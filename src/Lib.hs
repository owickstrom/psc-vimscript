{-# LANGUAGE DataKinds         #-}
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
import           Data.Generics.Uniplate.Data
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

data Target
  = Return
  | Assign Vim.ScopedName
  | Value

singleExpr :: Vim.Expr -> Gen (Vim.Block, Vim.Expr)
singleExpr expr = pure ([], expr)

genTarget :: Target -> Vim.Expr -> Gen Vim.Block
genTarget t expr =
  case t of
    Return      -> pure [Vim.Return expr]
    Assign name -> pure [Vim.Let name expr]
    Value       -> pure [Vim.ExprStmt expr]

identToName :: Ident -> Vim.Name
identToName = Vim.Name . runIdent

genName :: Qualified Ident -> Gen Vim.ScopedName
genName (Qualified Nothing ident) = do
  mappings <- ask
  case HashMap.lookup (identToName ident) mappings of
    Just sn -> pure sn
    Nothing -> pure (Vim.ScopedName Vim.Script (identToName ident))
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

genLiteral :: Literal (Expr Ann) -> Gen (Vim.Block, Vim.Expr)
genLiteral =
  \case
    NumericLiteral (Left i) -> singleExpr (Vim.intExpr i)
    NumericLiteral (Right d) -> singleExpr (Vim.floatingExpr d)
    StringLiteral s ->
      case decodeString s of
        Just t  -> singleExpr (Vim.stringExpr t)
        Nothing -> error ("Invalid string literal: " ++ show s)
    CharLiteral c -> singleExpr (Vim.stringExpr (T.pack [c]))
    BooleanLiteral b -> singleExpr (Vim.intExpr (bool 1 0 b))
    ArrayLiteral exprs -> do
      es <- mapM genExpr exprs
      pure (concatMap fst es, Vim.listExpr (map snd es))
    ObjectLiteral pairs -> do
      ps <- mapM genPair pairs
      pure (concatMap fst ps, Vim.dictionaryExpr (map snd ps))
      where genPair (field, expr) = do
              n <- Vim.Name <$> psStringToText field
              (decls, expr') <- genExpr expr
              pure (decls, (n, expr'))

data CaseBranch = CaseBranch
  { branchConditions :: [Vim.Expr]
  , branchStmts      :: Vim.Block
  } deriving (Show, Eq)

instance Monoid CaseBranch where
  mempty = CaseBranch mempty mempty
  mappend b1 b2 =
    CaseBranch
      (branchConditions b1 `mappend` branchConditions b2)
      (branchStmts b1 `mappend` branchStmts b2)

genGuardedBlock ::
     Target -> Either [(Guard Ann, Expr Ann)] (Expr Ann) -> Gen Vim.Block
genGuardedBlock tgt =
  \case
    Left guardedExprs -> concat <$> mapM guardBlock guardedExprs
    Right expr -> do
      e <- genExpr expr
      block <- genTarget tgt (snd e)
      pure (fst e ++ block)
  where
    guardBlock (guard', expr) = do
      g <- genExpr guard'
      e <- genExpr expr
      exprBlock <- genTarget tgt (snd e)
      let cond =
            Vim.Cond
              (Vim.CondStmt
                 (Vim.CondCase (snd g) (fst e ++ exprBlock))
                 []
                 Nothing)
      pure (fst g ++ [cond])

branchToBlock :: CaseBranch -> Vim.Block -> Vim.Block
branchToBlock CaseBranch {..} block =
  case branchConditions of
    [] -> branchStmts ++ block
    (x:xs) ->
      [ Vim.Cond
          (Vim.CondStmt
             (Vim.CondCase (foldl' Vim.andExpr x xs) (branchStmts ++ block))
             []
             Nothing)
      ]

genCaseAlternatives ::
     Target -> [Vim.Expr] -> [CaseAlternative Ann] -> Gen Vim.Block
genCaseAlternatives tgt exprs alts = do
  branches <- mapM (genAlt tgt exprs) alts
  guardedBlocks <- mapM (genGuardedBlock tgt . caseAlternativeResult) alts
  let branches' = zipWith branchToBlock branches guardedBlocks
  pure (concat branches')

genAlt :: Target -> [Vim.Expr] -> CaseAlternative Ann -> Gen CaseBranch
genAlt tgt exprs alt =
  fold <$> zipWithM (genBinder tgt) exprs (caseAlternativeBinders alt)

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

genBinder :: Target -> Vim.Expr -> Binder a -> Gen CaseBranch
genBinder tgt expr =
  \case
    NullBinder _ -> pure mempty
    LiteralBinder _ _lit -> pure mempty
    VarBinder _ ident ->
      pure
        mempty
        { branchStmts =
            [Vim.Let (Vim.ScopedName Vim.Local (identToName ident)) expr]
        }
    ConstructorBinder _ typeName ctorName binders -> do
      fields <- getConstructorFields typeName ctorName
      let test = ctorTest typeName ctorName expr
      branch <- fold <$> zipWithM (genFieldBinder tgt expr) fields binders
      pure branch {branchConditions = test : branchConditions branch}
    NamedBinder _ ident binder -> do
      let n = Vim.ScopedName Vim.Local (identToName ident)
      branch <- genBinder tgt expr binder
      pure branch {branchStmts = Vim.Let n expr : branchStmts branch}

genFieldBinder :: Target -> Vim.Expr -> Ident -> Binder a -> Gen CaseBranch
genFieldBinder tgt dict field binder = do
  let proj = Vim.Proj dict (Vim.ProjSingle (Vim.stringExpr (runIdent field)))
  genBinder tgt proj binder

genExpr :: Expr Ann -> Gen (Vim.Block, Vim.Expr)
genExpr =
  \case
    Literal _ lit -> genLiteral lit
    Constructor _ typeName ctorName fieldNames -> do
      let typeField =
            (Vim.Name "_type", Vim.stringExpr (runProperName typeName))
          ctor = Vim.Name "_ctor"
          ctorField = (ctor, Vim.stringExpr (runProperName ctorName))
          names = map identToName fieldNames
          dictFields =
            map (\n -> (n, Vim.Ref (Vim.ScopedName Vim.Unscoped n))) names
          dict = Vim.dictionaryExpr ([typeField, ctorField] ++ dictFields)
      pure ([], foldr (\n -> Vim.Lambda [n]) dict names)
    Accessor _ field expr -> do
      (decls, e) <- genExpr expr
      t <- psStringToText field
      pure (decls, Vim.Proj e (Vim.ProjSingle (Vim.stringExpr t)))
    Abs _ arg expr -> do
      let argName = identToName arg
      (decls, e) <-
        local
          (HashMap.insert argName (Vim.ScopedName Vim.Unscoped argName))
          (genExpr expr)
      pure (decls, Vim.Lambda [argName] e)
    App _ f p -> do
      (fd, f') <- genExpr f
      (pd, p') <- genExpr p
      pure (fd ++ pd, Vim.Apply f' [p'])
    Var _ qn -> do
      n <- genName qn
      pure ([], Vim.Ref n)
    Case _ exprs alts -> do
      n <- freshLocal
      es <- mapM genExpr exprs
      stmts <- genCaseAlternatives (Assign n) (map snd es) alts
      -- TODO: Use target
      pure (concatMap fst es ++ stmts, Vim.Ref n)
    Let _ bindings expr -> do
      binds <- mapM genBind bindings
      let mappings = foldMap fst binds
      (decls, e) <- local (HashMap.union mappings) (genExpr expr)
      pure (foldMap snd binds ++ decls, e)

genBind :: Bind Ann -> Gen (NameMappings, Vim.Block)
genBind =
  \case
    NonRec ann ident expr -> genBind' ann ident expr
    Rec binds -> do
      bs <- mapM (\((ann, ident), expr) -> genBind' ann ident expr) binds
      pure (foldMap fst bs, foldMap snd bs)
  where
    genBind' _ ident expr = do
      let name = identToName ident
          sn = Vim.ScopedName Vim.Script name
      (decls, e) <- genExpr expr
      pure (HashMap.singleton name sn, decls ++ [Vim.Let sn e])

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
      (decls, e) <-
        local
          (HashMap.insert argName (Vim.ScopedName Vim.Argument argName))
          (genExpr expr)
      pure [Vim.Function name [argName] Vim.Regular (decls ++ [Vim.Return e])]
    genDecl' _ ident expr = do
      name <- genName (Qualified (Just moduleName) ident)
      genExpr expr >>= \case
        (decls, Vim.Lambda [arg] body) ->
          pure
            [ Vim.Function
                name
                [arg]
                Vim.Regular
                (decls ++ [Vim.Return (transformBi (argScope arg) body)])
            ]
        (decls, e) -> pure (decls ++ [Vim.Let name e])

argScope :: Vim.Name -> Vim.ScopedName -> Vim.ScopedName
argScope n =
  \case
    Vim.ScopedName Vim.Unscoped n'
      | n == n' -> Vim.ScopedName Vim.Argument n'
    sn -> sn

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
