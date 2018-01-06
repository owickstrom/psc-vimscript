{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( loadModule
  , genModule
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                          (Result (..), decode)
import           Data.Aeson.Types                    (parse)
import           Data.Bool
import qualified Data.ByteString.Lazy                as BS
import           Data.Foldable

-- import           Data.Generics.Uniplate.Data
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.List                           (intersperse)
import           Data.Semigroup
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

type NameMappings = HashMap Vim.Name Vim.ScopedName

type Gen a = StateT GenState (Reader NameMappings) a

type Constructors = HashMap (Text, Text) [Ident]

data GenState = GenState
  { localNameN   :: Int
  , constructors :: Constructors
  } deriving (Show, Eq)

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
  GenExpression :: Vim.Block -> Vim.Expr -> Target 'Expression

returnTarget :: Vim.Block -> Vim.Expr -> Gen (Target 'Statement)
returnTarget block expr = pure (GenReturn block expr)

assignTarget ::
     Vim.ScopedName -> Vim.Block -> Vim.Expr -> Gen (Target 'Statement)
assignTarget name block expr = pure (GenAssign name block expr)

expressionTarget :: Vim.Block -> Vim.Expr -> Gen (Target 'Expression)
expressionTarget block expr = pure (GenExpression block expr)

type TargetRet (t :: TargetType) = Vim.Block -> Vim.Expr -> Gen (Target t)

targetToBlock :: Target t -> Vim.Block
targetToBlock =
  \case
    GenBlock block -> block
    GenReturn block expr -> block ++ [Vim.Return expr]
    GenAssign name block expr -> block ++ [Vim.Let name expr]
    GenExpression block _ -> block

targetToExpression :: Target 'Expression -> Vim.Expr
targetToExpression (GenExpression _ expr) = expr

identToName :: Ident -> Vim.Name
identToName = Vim.Name . runIdent

genName :: Qualified Ident -> Gen Vim.ScopedName
genName (Qualified Nothing ident) = do
  mappings <- ask
  case HashMap.lookup (identToName ident) mappings of
    Just sn -> pure sn
    Nothing -> error ("Unknown name: " <> show (identToName ident)) -- pure (Vim.ScopedName Vim.Global (identToName ident))
genName (Qualified (Just (ModuleName properNames)) ident) =
  pure
    (Vim.ScopedName Vim.Global (Vim.Name (moduleName <> "#" <> runIdent ident)))
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
getConstructorFields (Qualified _mn tn) (Qualified _ cn) =
  HashMap.lookup (runProperName tn, runProperName cn) <$> gets constructors >>= \case
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
genGuardedBlock ret nameMappings guardedExprs =
  local (HashMap.union nameMappings) $
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

genBinder :: TargetRet t -> Vim.Expr -> Binder a -> Gen CaseBranch
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

genFieldBinder :: TargetRet t -> Vim.Expr -> Ident -> Binder a -> Gen CaseBranch
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
    Abs _ arg expr -> do
      let argName = identToName arg
      closureName <- freshLocal
      body <-
        local
          (HashMap.insert argName (Vim.ScopedName Vim.Unscoped argName))
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
    Var _ qn -> do
      n <- genName qn
      ret [] (Vim.Ref n)
    Case _ exprs alts -> do
      n <- freshLocal
      es <- mapM (genExpr expressionTarget) exprs
      cases <-
        genCaseAlternatives (assignTarget n) (map targetToExpression es) alts
      ret (concatMap targetToBlock es <> targetToBlock cases) (Vim.Ref n)
    Let _ bindings expr -> do
      binds <- mapM genBind bindings
      let mappings = foldMap fst binds
      e <- local (HashMap.union mappings) (genExpr expressionTarget expr)
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

genDecl :: ModuleName -> Bind Ann -> Gen Vim.Block
genDecl moduleName =
  \case
    NonRec ann ident expr -> genDecl' ann ident expr
    Rec binds ->
      concat <$> mapM (\((ann, ident), expr) -> genDecl' ann ident expr) binds
  where
    genDecl' _ ident (Abs _ arg expr) = do
      name <- genName (Qualified (Just moduleName) ident)
      let argName = identToName arg
      body <-
        local
          (HashMap.insert argName (Vim.ScopedName Vim.Argument argName))
          (genExpr returnTarget expr)
      pure [Vim.Function name [argName] Vim.Regular (targetToBlock body)]
    genDecl' _ ident expr = do
      name <- genName (Qualified (Just moduleName) ident)
      target <- genExpr (assignTarget name) expr
      pure (targetToBlock target)

-- argScope :: Vim.Name -> Vim.ScopedName -> Vim.ScopedName
-- argScope n =
--   \case
--     Vim.ScopedName Vim.Unscoped n'
--       | n == n' -> Vim.ScopedName Vim.Argument n'
--     sn -> sn
extractConstructors :: Bind a -> Constructors
extractConstructors = getConstructors
  where
    (getConstructors, _, _, _) =
      everythingOnValues
        HashMap.union
        (const mempty)
        (\case
           Constructor _ typeName ctorName fieldNames ->
             HashMap.singleton
               (runProperName typeName, runProperName ctorName)
               fieldNames
           _ -> mempty)
        (const mempty)
        (const mempty)

genModule :: (Version, Module Ann) -> Vim.Program
genModule (_version, Module {..}) =
  runReader (evalStateT gen initialState) mempty
  where
    initialState =
      GenState
      {localNameN = 0, constructors = foldMap extractConstructors moduleDecls}
    gen = do
      comments <- mapM genComment moduleComments
      stmts <- mapM (genDecl moduleName) moduleDecls
      pure (Vim.Program (concat comments ++ concat stmts))
