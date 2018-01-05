{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( loadModule
  , genModule
  ) where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy                as BS
import           Data.Generics.Uniplate.Data
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.List                           (intersperse)
import           Data.Semigroup
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as T
import           Data.Version                        (Version)

import           Language.PureScript.AST.Literals
import           Language.PureScript.Comments
import           Language.PureScript.CoreFn
import           Language.PureScript.CoreFn.Expr
import           Language.PureScript.CoreFn.FromJSON
import           Language.PureScript.Names
import           Language.PureScript.PSString

import qualified Vimscript.AST                       as Vim

import           Debug.Trace

loadModule :: FilePath -> IO (Version, Module Ann)
loadModule path = do
  json <- BS.readFile path
  case decode json of
    Just val ->
      case parse moduleFromJSON val of
        Success m -> return m
        Error e   -> fail e
    Nothing -> fail "Couldn't read CoreFn file."

type Gen a = Reader (HashMap Vim.Name Vim.ScopedName) a

identToName :: Ident -> Vim.Name
identToName = Vim.Name . runIdent

genName :: Qualified Ident -> Gen Vim.ScopedName
genName (Qualified Nothing ident) =
  pure (Vim.ScopedName Vim.Script (identToName ident))
genName (Qualified (Just (ModuleName properNames)) ident) =
  pure
    (Vim.ScopedName Vim.Global (Vim.Name (moduleName <> "#" <> runIdent ident)))
  where
    moduleName :: Text
    moduleName = mconcat (intersperse "#" (map runProperName properNames))

fieldToText :: PSString -> Gen Text
fieldToText field =
  case decodeString field of
    Just t  -> pure t
    Nothing -> error ("Invalid field name: " ++ show field)

genLiteral :: Literal (Expr Ann) -> Gen (Vim.Block, Vim.Expr)
genLiteral =
  \case
    NumericLiteral (Left i) -> pure ([], Vim.intExpr i)
    NumericLiteral (Right d) -> pure ([], Vim.floatingExpr d)
    StringLiteral s ->
      case decodeString s of
        Just t  -> pure ([], Vim.stringExpr t)
        Nothing -> error ("Invalid string literal: " ++ show s)
    CharLiteral c -> pure ([], Vim.stringExpr (T.pack [c]))
    BooleanLiteral b ->
      pure
        ( []
        , Vim.intExpr
            (if b
               then 1
               else 0))
    ArrayLiteral exprs -> do
      es <- mapM genExpr exprs
      pure (concatMap fst es, Vim.listExpr (map snd es))
    ObjectLiteral pairs -> do
      ps <- mapM genPair pairs
      pure (concatMap fst ps, Vim.dictionaryExpr (map snd ps))
      where genPair (field, expr) = do
              n <- Vim.Name <$> fieldToText field
              (decls, expr) <- genExpr expr
              pure (decls, (n, expr))

genCaseAlternatives ::
     [Expr Ann] -> [CaseAlternative Ann] -> Gen (Vim.Block, Vim.Expr)
genCaseAlternatives exprs alts =
  pure ([], Vim.intExpr 0)

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
      t <- fieldToText field
      pure (decls, Vim.Proj e (Vim.ProjSingle (Vim.stringExpr t)))
    Abs _ arg expr -> do
      (decls, e) <- genExpr expr
      pure (decls, Vim.Lambda [identToName arg] e)
    App _ f p -> do
      (fd, f') <- genExpr f
      (pd, p') <- genExpr p
      pure (fd ++ pd, Vim.Apply f' [p'])
    Var _ qn -> do
      n <- genName qn
      pure ([], Vim.Ref n)
    Case _ exprs alts -> genCaseAlternatives exprs alts
    Let _ bindings expr -> do
      binds <- concat <$> mapM genBind bindings
      (decls, e) <- genExpr expr
      pure (binds ++ decls, e)

genBind :: Bind Ann -> Gen Vim.Block
genBind =
  \case
    NonRec ann ident expr -> genBind' ann ident expr
    Rec binds ->
      concat <$> mapM (\((ann, ident), expr) -> genBind' ann ident expr) binds
  where
    genBind' ann ident expr = do
      (decls, e) <- genExpr expr
      pure
        (decls ++ [Vim.Let (Vim.ScopedName Vim.Script (identToName ident)) e])

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
    genDecl' ann ident expr = do
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

genModule :: (Version, Module Ann) -> Vim.Program
genModule (version, Module {..}) = runReader gen mempty
  where
    gen = do
      comments <- mapM genComment moduleComments
      stmts <- mapM (genDecl moduleName) moduleDecls
      pure (Vim.Program (concat comments ++ concat stmts))
