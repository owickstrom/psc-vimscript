{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( modulePackName
  , genModule
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bool
import qualified Data.Char                        as Char
import           Data.Foldable
import           Data.Generics.Uniplate.Data
import           Data.List                        (intersperse)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Semigroup
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Version                     (Version)
import           GHC.Generics                     ()

import           Language.PureScript.AST.Literals
import           Language.PureScript.Comments
import qualified Language.PureScript.Constants    as PureScript
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString

import qualified Vimscript.AST                    as Vim
import           ZEncoding                        (zEncode)

modulePackName :: ModuleName -> Vim.PackName
modulePackName (ModuleName ns) =
  Vim.PackName (T.intercalate "." (map runProperName ns))

primModule :: ModuleName
primModule = moduleNameFromString "Prim"

type Gen a = StateT GenState (Reader GenEnv) a

type Constructors
   = Map ( Qualified (ProperName 'TypeName)
         , Qualified (ProperName 'ConstructorName)) [Ident]

type TopLevelDefinitions = Set (Qualified Ident)

data GenState = GenState
  { localNameN   :: Int
  , constructors :: Constructors
  , liftedBlocks :: [Vim.Block]
  }

type NameMappings = Map Ident Vim.ScopedName

data GenEnv = GenEnv
  { currentModuleName :: ModuleName
  , nameMappings      :: NameMappings
  , topLevelFunctions :: TopLevelDefinitions
  }

withNewMappings :: NameMappings -> Gen a -> Gen a
withNewMappings newMappings =
  local (\e -> e {nameMappings = Map.union (nameMappings e) newMappings})

fresh :: Gen Vim.Name
fresh = do
  st <- get
  let n = localNameN st
      name = Vim.Name ("G" <> T.pack (show n))
  put st {localNameN = n + 1}
  pure name

freshLocal :: Gen Vim.ScopedName
freshLocal = Vim.ScopedName Vim.Local <$> fresh

data Target
  = GenBlock Vim.Block
  | GenReturn Vim.Block
              Vim.Expr
  | GenLet Vim.ScopedName
           Vim.Block
           Vim.Expr
  | GenAssign Vim.AssignTarget
              Vim.Block
              Vim.Expr
  | GenCopy Vim.ScopedName
            Vim.Block
            Vim.Expr
  deriving (Show)

returnTarget :: Vim.Block -> Vim.Expr -> Gen Target
returnTarget block expr = pure (GenReturn block expr)

letTarget :: Vim.ScopedName -> Vim.Block -> Vim.Expr -> Gen Target
letTarget name block expr = pure (GenLet name block expr)

assignTarget :: Vim.AssignTarget -> Vim.Block -> Vim.Expr -> Gen Target
assignTarget tgt block expr = pure (GenAssign tgt block expr)

copyTarget :: Vim.ScopedName -> Vim.Block -> Vim.Expr -> Gen Target
copyTarget name block expr = pure (GenCopy name block expr)

projTarget :: Vim.Projection -> TargetRet -> TargetRet
projTarget proj ret block expr = ret block (Vim.Proj expr proj)

type TargetRet = Vim.Block -> Vim.Expr -> Gen Target

targetToBlock :: Target -> Vim.Block
targetToBlock =
  \case
    GenBlock block -> block
    GenReturn block expr -> block ++ [Vim.Return expr]
    GenLet name block expr -> block ++ [Vim.Let name expr]
    GenAssign tgt block expr -> block ++ [Vim.Assign tgt expr]
    GenCopy name block expr ->
      block ++
      [ Vim.Let
          name
          (Vim.Apply (Vim.Ref (Vim.ScopedName Vim.BuiltIn "deepcopy")) [expr])
      ]

textToName :: Text -> Vim.Name
textToName = Vim.Name . uppercaseFirst . zEncode
  where
    uppercaseFirst t =
      case T.uncons t of
        Just (c, t') -> T.cons (Char.toUpper c) t'
        Nothing      -> T.empty

identToName :: Ident -> Vim.Name
identToName = textToName . runIdent

unscopedName :: Ident -> Vim.ScopedName
unscopedName = Vim.ScopedName Vim.Unscoped . identToName

qualifiedName :: ModuleName -> Ident -> Vim.ScopedName
qualifiedName (ModuleName properNames) ident =
  Vim.ScopedName
    Vim.Global
    (Vim.Name (moduleName <> "#" <> zEncode (runIdent ident)))
  where
    moduleName :: Text
    moduleName = mconcat (intersperse "#" (map runProperName properNames))

psStringToText :: PSString -> Gen Text
psStringToText str =
  case decodeString str of
    Just t  -> pure t
    Nothing -> error ("Invalid field name: " ++ show str)

getConstructorFields ::
     Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Gen [Ident]
getConstructorFields tn cn =
  Map.lookup (tn, cn) <$> gets constructors >>= \case
    Just fields -> pure fields
    Nothing -> error ("Unknown constructor: " ++ show (tn, cn))

genSimpleExpr :: Expr Ann -> Gen (Vim.Block, Vim.Expr)
genSimpleExpr expr = do
  n <- freshLocal
  t <- genExpr (letTarget n) expr
  case targetToBlock t of
    [Vim.Let ln e]
      | ln == n -> pure ([], e)
    block -> pure (block, Vim.Ref n)

genSimpleExprs :: [Expr Ann] -> Gen (Vim.Block, [Vim.Expr])
genSimpleExprs exprs = do
  es <- mapM genSimpleExpr exprs
  pure (concatMap fst es, map snd es)

genLiteral :: TargetRet -> Literal (Expr Ann) -> Gen Target
genLiteral ret =
  \case
    NumericLiteral (Left i) -> ret [] (Vim.intExpr i)
    NumericLiteral (Right d) -> ret [] (Vim.floatingExpr d)
    StringLiteral s ->
      case decodeString s of
        Just t  -> ret [] (Vim.stringExpr t)
        Nothing -> error ("Invalid string literal: " ++ show s)
    CharLiteral c -> ret [] (Vim.stringExpr (T.pack [c]))
    BooleanLiteral b -> ret [] (Vim.intExpr (bool 1 0 b))
    ArrayLiteral exprs -> do
      (block, exprs') <- genSimpleExprs exprs
      ret block (Vim.listExpr exprs')
    ObjectLiteral pairs -> do
      (block, exprs) <- genSimpleExprs (map snd pairs)
      fs <- mapM (fmap Vim.Name . psStringToText . fst) pairs
      ret block (Vim.dictionaryExpr (zip fs exprs))

data CaseBranch = CaseBranch
  { branchConditions   :: [Vim.Expr]
  , branchStmts        :: Vim.Block
  , branchNameMappings :: NameMappings
  } deriving (Show, Eq)

instance Monoid CaseBranch where
  mempty = CaseBranch mempty mempty mempty
  mappend b1 b2 =
    CaseBranch
      (branchConditions b1 `mappend` branchConditions b2)
      (branchStmts b1 `mappend` branchStmts b2)
      (branchNameMappings b1 `mappend` branchNameMappings b2)

genGuardedBlock ::
     TargetRet
  -> NameMappings
  -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  -> Gen Target
genGuardedBlock ret newMappings guardedExprs =
  withNewMappings newMappings $
  case guardedExprs of
    Left exprs -> do
      bs <- mapM guardBlock exprs
      pure (GenBlock (concatMap targetToBlock bs))
    Right expr -> GenBlock . targetToBlock <$> genExpr ret expr
  where
    guardBlock :: (Guard Ann, Expr Ann) -> Gen Target
    guardBlock (guard', expr) = do
      (gb, ge) <- genSimpleExpr guard'
      e <- genExpr ret expr
      let cond =
            Vim.Cond
              (Vim.CondStmt (Vim.CondCase ge (targetToBlock e)) [] Nothing)
      pure (GenBlock (gb <> [cond]))

branchToBlock :: CaseBranch -> Target -> [Vim.Stmt]
branchToBlock CaseBranch {..} block =
  case branchConditions of
    [] -> branchStmts ++ targetToBlock block
    (x:xs) ->
      [ Vim.Cond
          (Vim.CondStmt
             (Vim.CondCase
                (foldl' Vim.andExpr x xs)
                (branchStmts ++ targetToBlock block))
             []
             Nothing)
      ]

genCaseAlternatives ::
     TargetRet -> [Vim.Expr] -> [CaseAlternative Ann] -> Gen Target
genCaseAlternatives ret exprs alts = do
  branches <- mapM (genAlt ret exprs) alts
  guardedBlocks <-
    zipWithM
      (genGuardedBlock ret)
      (map branchNameMappings branches)
      (map caseAlternativeResult alts)
  pure (GenBlock (fold (zipWith branchToBlock branches guardedBlocks)))

genAlt :: TargetRet -> [Vim.Expr] -> CaseAlternative Ann -> Gen CaseBranch
genAlt ret exprs alt =
  fold <$> zipWithM (genBinder ret) exprs (caseAlternativeBinders alt)

ctorTest ::
     Qualified (ProperName 'TypeName)
  -> Qualified (ProperName 'ConstructorName)
  -> Vim.Expr
  -> Vim.Expr
ctorTest (Qualified _ typeName) (Qualified _ ctorName) expr =
  fieldTest "_type" (runProperName typeName) `Vim.andExpr`
  fieldTest "_ctor" (runProperName ctorName)
  where
    fieldTest f n =
      Vim.BinOpApply
        Vim.Equals
        (Vim.Proj expr (Vim.ProjSingle (Vim.stringExpr f)))
        (Vim.stringExpr n)

genBinder :: TargetRet -> Vim.Expr -> Binder Ann -> Gen CaseBranch
genBinder ret expr =
  \case
    NullBinder _ -> pure mempty
    LiteralBinder _ lit -> genLiteralBinder ret expr lit
    VarBinder _ ident ->
      let sn = Vim.ScopedName Vim.Local (identToName ident)
      in pure
           mempty
           { branchStmts = [Vim.Let sn expr]
           , branchNameMappings = Map.singleton ident sn
           }
    ConstructorBinder (_, _, _, Just IsNewtype) _typeName _ctorName [binder] ->
      genBinder ret expr binder
    ConstructorBinder _ typeName ctorName binders -> do
      fields <- getConstructorFields typeName ctorName
      let test = ctorTest typeName ctorName expr
      branch <- fold <$> zipWithM (genFieldBinder ret expr) fields binders
      pure branch {branchConditions = test : branchConditions branch}
    NamedBinder _ ident binder -> do
      let sn = Vim.ScopedName Vim.Local (identToName ident)
      branch <- genBinder ret expr binder
      pure
        branch
        { branchStmts = Vim.Let sn expr : branchStmts branch
        , branchNameMappings = Map.insert ident sn (branchNameMappings branch)
        }

genLiteralBinder ::
     TargetRet -> Vim.Expr -> Literal (Binder Ann) -> Gen CaseBranch
genLiteralBinder ret expr =
  \case
    NumericLiteral (Left int) ->
      pure mempty {branchConditions = [Vim.eqExpr expr (Vim.intExpr int)]}
    NumericLiteral (Right double) ->
      pure
        mempty {branchConditions = [Vim.eqExpr expr (Vim.floatingExpr double)]}
    StringLiteral str -> do
      str' <- psStringToText str
      pure mempty {branchConditions = [Vim.eqExpr expr (Vim.stringExpr str')]}
    CharLiteral c ->
      pure
        mempty
        {branchConditions = [Vim.eqExpr expr (Vim.stringExpr (T.singleton c))]}
    BooleanLiteral b ->
      pure
        mempty
        {branchConditions = [Vim.eqExpr expr (bool Vim.true Vim.false b)]}
    ArrayLiteral binders -> do
      branch <-
        fold <$>
        zipWithM
          (genBinder ret . Vim.Proj expr . Vim.ProjSingle . Vim.intExpr)
          [0 ..]
          binders
      let lenCond =
            Vim.eqExpr
              (Vim.Apply (Vim.Ref (Vim.ScopedName Vim.BuiltIn "len")) [expr])
              (Vim.intExpr (fromIntegral (length binders)))
      pure (branch {branchConditions = lenCond : branchConditions branch})
    ObjectLiteral binders -> fold <$> mapM pairBranch binders
      where pairBranch (pStr, binder) = do
              str <- psStringToText pStr
              genBinder
                ret
                (Vim.Proj expr (Vim.ProjSingle (Vim.stringExpr str)))
                binder

genFieldBinder :: TargetRet -> Vim.Expr -> Ident -> Binder Ann -> Gen CaseBranch
genFieldBinder tgt dict field binder = do
  let proj = Vim.Proj dict (Vim.ProjSingle (Vim.stringExpr (runIdent field)))
  genBinder tgt proj binder

freeVariables :: Expr a -> Set Ident
freeVariables = free
  where
    (_, free, _, _) =
      everythingOnValues
        (<>)
        (const mempty)
        (\case
           Var _ (Qualified Nothing ident) -> Set.singleton ident
           _ -> mempty)
        (const mempty)
        (const mempty)

lookupIdent :: Ident -> Gen Vim.Expr
lookupIdent ident = do
  mappings <- asks nameMappings
  case Map.lookup ident mappings of
    Just sn -> pure (Vim.Ref sn)
    Nothing -> pure (Vim.Ref (Vim.ScopedName Vim.Script (identToName ident)))

liftBlock :: Vim.Block -> Gen ()
liftBlock block = modify (\s -> s {liftedBlocks = liftedBlocks s <> [block]})

genExpr :: TargetRet -> Expr Ann -> Gen Target
genExpr ret =
  \case
    Literal _ lit -> genLiteral ret lit
    Constructor _ typeName ctorName fieldNames -> do
      let typeField =
            (Vim.Name "_type", Vim.stringExpr (runProperName typeName))
          ctor = Vim.Name "_ctor"
          ctorField = (ctor, Vim.stringExpr (runProperName ctorName))
          names = map identToName fieldNames
          dictFields =
            map (\n -> (n, Vim.Ref (Vim.ScopedName Vim.Unscoped n))) names
          dict = Vim.dictionaryExpr ([typeField, ctorField] ++ dictFields)
      ret [] (foldr (\n -> Vim.Lambda [n]) dict names)
    Accessor _ field expr -> do
      t <- psStringToText field
      genExpr (projTarget (Vim.ProjSingle (Vim.stringExpr t)) ret) expr
    ObjectUpdate _ expr newValues -> do
      copyName <- freshLocal
      copy <- genExpr (copyTarget copyName) expr
      vals <- mapM (updateField copyName) newValues
      ret (targetToBlock copy <> foldMap targetToBlock vals) (Vim.Ref copyName)
      where updateField objName (field, newValue) = do
              f <- psStringToText field
              let assign =
                    Vim.AssignProj
                      (Vim.AssignName objName)
                      (Vim.ProjSingle (Vim.stringExpr f))
              genExpr (assignTarget assign) newValue
    Abs _ arg expr -> do
      localVariables <- asks (Map.keysSet . nameMappings)
      liftedName <- Vim.ScopedName Vim.Script <$> fresh
      let freeVars =
            Set.toList (freeVariables expr `Set.intersection` localVariables)
          origArg = identToName arg
          newArgs = map identToName freeVars
          mappings =
            Map.fromList
              (zip
                 (arg : freeVars)
                 (map (Vim.ScopedName Vim.Argument) (origArg : newArgs)))
      body <-
        local (\e -> e {nameMappings = mappings}) (genExpr returnTarget expr)
      liftBlock
        [ Vim.Function
            liftedName
            (origArg : newArgs)
            Vim.Regular
            (targetToBlock body)
        ]
      case freeVars of
        [] -> ret [] (Vim.FuncRef liftedName)
        _ -> do
          params <-
            (Vim.Ref (Vim.ScopedName Vim.Unscoped origArg) :) <$>
            mapM lookupIdent freeVars
          ret [] (Vim.Lambda [origArg] (Vim.Apply (Vim.Ref liftedName) params))
    App _ f p -> do
      (fb, fe) <- genSimpleExpr f
      (pb, pe) <- genSimpleExpr p
      ret (fb <> pb) (Vim.Apply fe [pe])
    Var _ (Qualified Nothing ident) -> lookupIdent ident >>= ret []
    Var _ (Qualified (Just mn) (Ident "undefined"))
      | mn == primModule -> ret [] (Vim.intExpr 0)
    Var _ qn@(Qualified (Just mn) ident)
      | mn == primModule -> ret [] (Vim.Ref (unscopedName ident))
      | otherwise -> do
        let sn = qualifiedName mn ident
        fns <- asks topLevelFunctions
        if Set.member qn fns
          then ret [] (Vim.FuncRef sn)
          else ret [] (Vim.Ref sn)
    Case _ exprs alts -> do
      (eb, es) <- genSimpleExprs exprs
      cases <- genCaseAlternatives ret es alts
      pure (GenBlock (eb <> targetToBlock cases))
    Let _ bindings expr -> do
      binds <- mapM genBind bindings
      let mappings = foldMap fst binds
      e <- withNewMappings mappings (genExpr ret expr)
      pure (GenBlock (foldMap (targetToBlock . snd) binds <> targetToBlock e))

genBind :: Bind Ann -> Gen (NameMappings, Target)
genBind =
  \case
    NonRec ann ident expr -> genBind' ann ident expr
    Rec binds -> do
      bs <- mapM (\((ann, ident), expr) -> genBind' ann ident expr) binds
      pure (foldMap fst bs, GenBlock (foldMap (targetToBlock . snd) bs))
  where
    genBind' _ ident expr = do
      let sn = Vim.ScopedName Vim.Local (identToName ident)
      e <- genExpr (letTarget sn) expr
      pure (Map.singleton ident sn, GenBlock (targetToBlock e))

genComment :: Comment -> Gen Vim.Block
genComment =
  \case
    LineComment t -> pure [Vim.LineComment t]
    BlockComment t -> pure (map Vim.LineComment (T.lines t))

singleTopLevelLet :: ModuleName -> Ident -> Expr Ann -> Gen (Maybe Vim.Block)
singleTopLevelLet moduleName ident expr = do
  let name = qualifiedName moduleName ident
  body <- genExpr (letTarget name) expr
  case targetToBlock body of
    [letStmt] -> pure (Just [letStmt])
    _         -> pure Nothing

wrapTopLevelLet :: ModuleName -> Ident -> Expr Ann -> Gen Vim.Block
wrapTopLevelLet moduleName ident expr = do
  fName <- Vim.ScopedName Vim.Script <$> fresh
  body <- genExpr returnTarget expr
  let f = Vim.Function fName [] Vim.Regular (targetToBlock body)
      name = qualifiedName moduleName ident
      invocation = Vim.Let name (Vim.Apply (Vim.Ref fName) [])
  pure [f, invocation]

genDecl :: ModuleName -> Bind Ann -> Gen Vim.Block
genDecl moduleName =
  \case
    NonRec ann ident expr -> genDecl' ann ident expr
    Rec binds ->
      concat <$> mapM (\((ann, ident), expr) -> genDecl' ann ident expr) binds
  where
    genDecl' _ ident (Abs _ arg expr) = do
      let name = qualifiedName moduleName ident
      let argName = identToName arg
      body <-
        withNewMappings
          (Map.singleton arg (Vim.ScopedName Vim.Argument argName))
          (genExpr returnTarget expr)
      pure [Vim.Function name [argName] Vim.Regular (targetToBlock body)]
    genDecl' _ ident expr =
      fromMaybe <$> wrapTopLevelLet moduleName ident expr <*>
      singleTopLevelLet moduleName ident expr

extractConstructors :: ModuleName -> Bind a -> Constructors
extractConstructors mn = getConstructors
  where
    (getConstructors, _, _, _) =
      everythingOnValues
        Map.union
        (const mempty)
        (\case
           Constructor _ typeName ctorName fieldNames ->
             Map.singleton
               (Qualified (Just mn) typeName, Qualified (Just mn) ctorName)
               fieldNames
           _ -> mempty)
        (const mempty)
        (const mempty)

extractTopLevelDefs :: ModuleName -> Module Ann -> TopLevelDefinitions
extractTopLevelDefs mn m = foldMap fromBind (moduleDecls m)
  where
    extract ident =
      \case
        Abs {} -> Set.singleton (Qualified (Just mn) ident)
        _ -> mempty
    fromBind =
      \case
        NonRec _ ident expr -> extract ident expr
        Rec binds -> fold [extract ident expr | ((_, ident), expr) <- binds]

packLoadedGuard :: Vim.Block
packLoadedGuard =
  [ Vim.Cond
      (Vim.CondStmt
         (Vim.CondCase (Vim.Exists sn) [Vim.BuiltInStmt "finish" Nothing])
         []
         Nothing)
  , Vim.Assign (Vim.AssignName sn) Vim.true
  ]
  where
    sn = Vim.ScopedName Vim.Script "purs_loaded"

runGen :: GenEnv -> GenState -> Gen a -> (a, GenState)
runGen env st gen = runReader (runStateT gen st) env

extractLifted :: Gen Vim.Block -> Gen Vim.Block
extractLifted gen = do
  env <- ask
  st <- get
  let (x, st') = runGen env st gen
  put st' {liftedBlocks = []}
  pure (concat (liftedBlocks st' <> [x]))

genDeclAndLifted :: ModuleName -> Bind Ann -> Gen Vim.Block
genDeclAndLifted moduleName = extractLifted . genDecl moduleName

removeUndefinedApp :: Vim.Program -> Vim.Program
removeUndefinedApp =
  transformBi $ \case
    (Vim.Apply fn ps) -> Vim.Apply fn (filter notUndefined ps)
    other -> other
  where
    notUndefined (Vim.Ref param) =
      param /= Vim.ScopedName Vim.Unscoped (textToName PureScript.undefined) &&
      param /= Vim.ScopedName Vim.Unscoped (textToName PureScript.__unused)
    notUndefined _ = True

removeUnusedFunctionArg :: Vim.Program -> Vim.Program
removeUnusedFunctionArg =
  transformBi $ \case
    Vim.Function name args Vim.Regular body ->
      Vim.Function name (filter notUnused args) Vim.Regular body
    other -> other
  where
    notUnused arg = arg /= textToName PureScript.__unused

removeUnusedLambdaArg :: Vim.Program -> Vim.Program
removeUnusedLambdaArg =
  transformBi $ \case
    (Vim.Lambda [arg] body)
      | arg == textToName PureScript.__unused -> Vim.Lambda [] body
    other -> other

-- TODO: Use this when it doesn't break Eff bind.
removeUnused :: Vim.Program -> Vim.Program
removeUnused = id
  -- removeUndefinedApp . removeUnusedLambdaArg . removeUnusedFunctionArg

genModule ::
     Map ModuleName (Module Ann)
  -> Maybe Text
  -> (Version, Module Ann)
  -> Vim.Program
genModule allModules prelude (_version, m) =
  removeUnused (fst (runGen initialEnv initialState gen))
  where
    moduleCtors = foldMap (extractConstructors (moduleName m)) (moduleDecls m)
    importedCtors =
      foldMap
        (\im -> foldMap (extractConstructors (moduleName im)) (moduleDecls im))
        allModules
    imports =
      [ Vim.PackAdd (modulePackName n)
      | (_, n) <- moduleImports m
      , n /= primModule && n /= moduleName m
      ]
    preludeBlock = maybe [] ((: []) . Vim.Raw) prelude
    initialState =
      GenState
      { localNameN = 0
      , constructors = moduleCtors <> importedCtors
      , liftedBlocks = []
      }
    initialEnv =
      GenEnv
      { nameMappings = mempty
      , currentModuleName = moduleName m
      , topLevelFunctions =
          foldMap (uncurry extractTopLevelDefs) (Map.toList allModules)
      }
    gen = do
      comments <- mapM genComment (moduleComments m)
      stmts <- concat <$> mapM (genDeclAndLifted (moduleName m)) (moduleDecls m)
      pure
        Vim.Program
        { programStmts =
            concat
              [packLoadedGuard, concat comments, imports, preludeBlock, stmts]
        }
