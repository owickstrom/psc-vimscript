{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( loadModule
  , modulePackName
  , genModule
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                          (Result (..), decode)
import           Data.Aeson.Types                    (parse)
import           Data.Bool
import qualified Data.ByteString.Lazy                as BS
import           Data.Foldable
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.List                           (intersperse)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Semigroup
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Version                        (Version)

import           Language.PureScript.AST.Literals
import           Language.PureScript.Comments
import           Language.PureScript.CoreFn
import           Language.PureScript.CoreFn.FromJSON
import           Language.PureScript.Names
import           Language.PureScript.PSString

import qualified Vimscript.AST                       as Vim

loadModule :: FilePath -> IO (Version, Module Ann)
loadModule path = do
  j <- BS.readFile path
  case decode j of
    Just val ->
      case parse moduleFromJSON val of
        Success m -> return m
        Error e   -> fail e
    Nothing -> fail "Couldn't read CoreFn file."

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
  { localNameN        :: Int
  , constructors      :: Constructors
  }

type NameMappings = HashMap Vim.Name Vim.ScopedName

data GenEnv = GenEnv
  { currentModuleName :: ModuleName
  , nameMappings      :: NameMappings
  , topLevelFunctions :: TopLevelDefinitions
  }

withNewMappings :: NameMappings -> Gen a -> Gen a
withNewMappings newMappings =
  local (\e -> e {nameMappings = HashMap.union (nameMappings e) newMappings})

fresh :: Gen Vim.Name
fresh = do
  st <- get
  let n = localNameN st
      name = Vim.Name ("G" <> T.pack (show n))
  put st {localNameN = n + 1}
  pure name

freshLocal :: Gen Vim.ScopedName
freshLocal = Vim.ScopedName Vim.Local <$> fresh

data TargetType
  = Statement
  | Expression

data Target (t :: TargetType) where
  GenBlock :: Vim.Block -> Target 'Statement
  GenReturn :: Vim.Block -> Vim.Expr -> Target 'Statement
  GenAssign :: Vim.ScopedName -> Vim.Block -> Vim.Expr -> Target 'Statement
  GenCopy :: Vim.ScopedName -> Vim.Block -> Vim.Expr -> Target 'Statement
  GenExpression :: Vim.Block -> Vim.Expr -> Target 'Expression

returnTarget :: Vim.Block -> Vim.Expr -> Gen (Target 'Statement)
returnTarget block expr = pure (GenReturn block expr)

assignTarget ::
     Vim.ScopedName -> Vim.Block -> Vim.Expr -> Gen (Target 'Statement)
assignTarget name block expr = pure (GenAssign name block expr)

copyTarget :: Vim.ScopedName -> Vim.Block -> Vim.Expr -> Gen (Target 'Statement)
copyTarget name block expr = pure (GenCopy name block expr)

expressionTarget :: Vim.Block -> Vim.Expr -> Gen (Target 'Expression)
expressionTarget block expr = pure (GenExpression block expr)

type TargetRet (t :: TargetType) = Vim.Block -> Vim.Expr -> Gen (Target t)

targetToBlock :: Target t -> Vim.Block
targetToBlock =
  \case
    GenBlock block -> block
    GenReturn block expr -> block ++ [Vim.Return expr]
    GenAssign name block expr -> block ++ [Vim.Let name expr]
    GenCopy name block expr ->
      block ++
      [ Vim.Let
          name
          (Vim.Apply (Vim.Ref (Vim.ScopedName Vim.BuiltIn "deepcopy")) [expr])
      ]
    GenExpression block _ -> block

targetToExpression :: Target 'Expression -> Vim.Expr
targetToExpression (GenExpression _ expr) = expr

identToName :: Ident -> Vim.Name
identToName = Vim.Name . runIdent

qualifiedName :: ModuleName -> Ident -> Vim.ScopedName
qualifiedName (ModuleName properNames) ident =
  Vim.ScopedName Vim.Global (Vim.Name (moduleName <> "#" <> runIdent ident))
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

genLiteral :: TargetRet t -> Literal (Expr Ann) -> Gen (Target t)
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
      targets <- mapM (genExpr expressionTarget) exprs
      ret
        (concatMap targetToBlock targets)
        (Vim.listExpr (map targetToExpression targets))
    ObjectLiteral pairs -> do
      fs <- mapM genField pairs
      ret
        (concat [b | (_, b, _) <- fs])
        (Vim.dictionaryExpr [(n, e) | (n, _, e) <- fs])
      where genField (field, expr) = do
              n <- Vim.Name <$> psStringToText field
              e <- genExpr expressionTarget expr
              pure (n, targetToBlock e, targetToExpression e)

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
     TargetRet 'Statement
  -> NameMappings
  -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  -> Gen (Target 'Statement)
genGuardedBlock ret newMappings guardedExprs =
  withNewMappings newMappings $
  case guardedExprs of
    Left exprs -> do
      bs <- mapM guardBlock exprs
      pure (GenBlock (concatMap targetToBlock bs))
    Right expr -> GenBlock . targetToBlock <$> genExpr ret expr
  where
    guardBlock :: (Guard Ann, Expr Ann) -> Gen (Target 'Statement)
    guardBlock (guard', expr) = do
      g <- genExpr expressionTarget guard'
      e <- genExpr ret expr
      let cond =
            Vim.Cond
              (Vim.CondStmt
                 (Vim.CondCase (targetToExpression g) (targetToBlock e))
                 []
                 Nothing)
      pure (GenBlock (targetToBlock g <> [cond]))

branchToBlock :: CaseBranch -> Target 'Statement -> Vim.Block
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
     TargetRet 'Statement
  -> [Vim.Expr]
  -> [CaseAlternative Ann]
  -> Gen (Target 'Statement)
genCaseAlternatives ret exprs alts = do
  branches <- mapM (genAlt ret exprs) alts
  guardedBlocks <-
    zipWithM
      (genGuardedBlock ret)
      (map branchNameMappings branches)
      (map caseAlternativeResult alts)
  pure (GenBlock (concat (zipWith branchToBlock branches guardedBlocks)))

genAlt :: TargetRet t -> [Vim.Expr] -> CaseAlternative Ann -> Gen CaseBranch
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

genBinder :: TargetRet t -> Vim.Expr -> Binder Ann -> Gen CaseBranch
genBinder ret expr =
  \case
    NullBinder _ -> pure mempty
    LiteralBinder _ _lit -> pure mempty
    VarBinder _ ident ->
      let name = identToName ident
          sn = Vim.ScopedName Vim.Local name
      in pure
           mempty
           { branchStmts = [Vim.Let sn expr]
           , branchNameMappings = HashMap.singleton name sn
           }
    ConstructorBinder (_, _, _, Just IsNewtype) _typeName _ctorName [binder] ->
      genBinder ret expr binder
    ConstructorBinder _ typeName ctorName binders -> do
      fields <- getConstructorFields typeName ctorName
      let test = ctorTest typeName ctorName expr
      branch <- fold <$> zipWithM (genFieldBinder ret expr) fields binders
      pure branch {branchConditions = test : branchConditions branch}
    NamedBinder _ ident binder -> do
      let n = identToName ident
          sn = Vim.ScopedName Vim.Local n
      branch <- genBinder ret expr binder
      pure
        branch
        { branchStmts = Vim.Let sn expr : branchStmts branch
        , branchNameMappings = HashMap.insert n sn (branchNameMappings branch)
        }

genFieldBinder ::
     TargetRet t -> Vim.Expr -> Ident -> Binder Ann -> Gen CaseBranch
genFieldBinder tgt dict field binder = do
  let proj = Vim.Proj dict (Vim.ProjSingle (Vim.stringExpr (runIdent field)))
  genBinder tgt proj binder

genExpr :: TargetRet t -> Expr Ann -> Gen (Target t)
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
      exprTarget <- genExpr expressionTarget expr
      t <- psStringToText field
      ret
        (targetToBlock exprTarget)
        (Vim.Proj
           (targetToExpression exprTarget)
           (Vim.ProjSingle (Vim.stringExpr t)))
    ObjectUpdate _ expr newValues -> do
      copyName <- freshLocal
      copy <- genExpr (copyTarget copyName) expr
      vals <- mapM (updateField copyName) newValues
      ret (targetToBlock copy <> foldMap targetToBlock vals) (Vim.Ref copyName)
      where updateField objName (field, newValue) = do
              f <- psStringToText field
              e <- genExpr expressionTarget newValue
              let assign =
                    Vim.Assign
                      (Vim.AssignProj
                         (Vim.AssignName objName)
                         (Vim.ProjSingle (Vim.stringExpr f)))
                      (targetToExpression e)
              pure (GenBlock (targetToBlock e <> [assign]))
    Abs _ arg expr -> do
      let argName = identToName arg
      closureName <- Vim.ScopedName Vim.Unscoped <$> fresh
      body <-
        withNewMappings
          (HashMap.singleton argName (Vim.ScopedName Vim.Argument argName))
          (genExpr returnTarget expr)
      ret
        [Vim.Function closureName [argName] Vim.Closure (targetToBlock body)]
        (Vim.FuncRef closureName)
    App _ f p -> do
      f' <- genExpr expressionTarget f
      p' <- genExpr expressionTarget p
      ret
        (targetToBlock f' <> targetToBlock p')
        (Vim.Apply (targetToExpression f') [targetToExpression p'])
    Var _ (Qualified Nothing ident) -> do
      mappings <- asks nameMappings
      let name = identToName ident
      case HashMap.lookup name mappings of
        Just sn -> ret [] (Vim.Ref sn)
        Nothing -> ret [] (Vim.Ref (Vim.ScopedName Vim.Script name))
    Var _ qn@(Qualified (Just mn) ident)
      | mn == primModule -> ret [] (Vim.Ref (qualifiedName mn ident))
      | otherwise -> do
        let sn = qualifiedName mn ident
        fns <- asks topLevelFunctions
        if Set.member qn fns
          then ret [] (Vim.FuncRef sn)
          else ret [] (Vim.Ref sn)
    Case _ exprs alts -> do
      n <- freshLocal
      es <- mapM (genExpr expressionTarget) exprs
      cases <-
        genCaseAlternatives (assignTarget n) (map targetToExpression es) alts
      ret (concatMap targetToBlock es <> targetToBlock cases) (Vim.Ref n)
    Let _ bindings expr -> do
      binds <- mapM genBind bindings
      let mappings = foldMap fst binds
      e <- withNewMappings mappings (genExpr expressionTarget expr)
      ret
        (foldMap (targetToBlock . snd) binds <> targetToBlock e)
        (targetToExpression e)

genBind :: Bind Ann -> Gen (NameMappings, Target 'Statement)
genBind =
  \case
    NonRec ann ident expr -> genBind' ann ident expr
    Rec binds -> do
      bs <- mapM (\((ann, ident), expr) -> genBind' ann ident expr) binds
      pure (foldMap fst bs, GenBlock (foldMap (targetToBlock . snd) bs))
  where
    genBind' _ ident expr = do
      let name = identToName ident
          sn = Vim.ScopedName Vim.Local name
      e <- genExpr (assignTarget sn) expr
      pure (HashMap.singleton name sn, GenBlock (targetToBlock e))

genComment :: Comment -> Gen Vim.Block
genComment =
  \case
    LineComment t -> pure [Vim.LineComment t]
    BlockComment t -> pure (map Vim.LineComment (T.lines t))

singleTopLevelLet :: ModuleName -> Ident -> Expr Ann -> Gen (Maybe Vim.Block)
singleTopLevelLet moduleName ident expr = do
  let name = qualifiedName moduleName ident
  body <- genExpr (assignTarget name) expr
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
          (HashMap.singleton argName (Vim.ScopedName Vim.Argument argName))
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
  , Vim.Assign (Vim.AssignName sn) (Vim.intExpr 1)
  ]
  where
    sn = Vim.ScopedName Vim.Script "purs_loaded"

genModule ::
     Map ModuleName (Module Ann)
  -> Maybe Text
  -> (Version, Module Ann)
  -> Vim.Program
genModule allModules prelude (_version, m) =
  runReader (evalStateT gen initialState) initialEnv
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
      GenState {localNameN = 0, constructors = moduleCtors <> importedCtors}
    initialEnv =
      GenEnv
      { nameMappings = mempty
      , currentModuleName = moduleName m
      , topLevelFunctions =
          foldMap (uncurry extractTopLevelDefs) (Map.toList allModules)
      }
    gen = do
      comments <- mapM genComment (moduleComments m)
      stmts <- concat <$> mapM (genDecl (moduleName m)) (moduleDecls m)
      pure
        Vim.Program
        { programStmts =
            concat
              [packLoadedGuard, concat comments, imports, preludeBlock, stmts]
        }
