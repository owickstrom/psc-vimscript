{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Vimscript.Render where

import           Data.Text                 (Text)
import           Prelude                   hiding (Ordering (..))
import           Text.PrettyPrint.Mainland

import           Vimscript.AST

indentWidth :: Int
indentWidth = 4

renderName :: Name -> Doc
renderName (Name n) = strictText n

renderScopedName :: ScopedName -> Doc
renderScopedName (ScopedName scope name) = prefix <> renderName name
  where
    prefix =
      case scope of
        Unscoped     -> empty
        BuiltIn      -> empty
        Global       -> "g:"
        Local        -> "l:"
        Script       -> "s:"
        Argument     -> "a:"
        Register     -> "@"
        Option       -> "&"
        LocalOption  -> "&l:"
        GlobalOption -> "&g:"
        Environment  -> "$"

renderBinOp :: BinOp -> Doc
renderBinOp =
  \case
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Equals -> "==#"
    LT -> "<"
    LTE -> "<="
    GT -> ">"
    GTE -> ">="
    Concat -> "."
    Or -> "||"
    And -> "&&"

commaSepIn :: Doc -> Doc -> [Doc] -> Doc
commaSepIn l r = enclose l r . folddoc (<>) . punctuate comma

renderStringLiteral :: Text -> Doc
renderStringLiteral = string . show

renderPrim :: Primitive -> Doc
renderPrim =
  \case
    Integer i -> integer i
    Floating d -> double d
    String t -> string (show t) -- Will this hold?!
    List exprs -> commaSepIn lbracket rbracket (map renderExpr exprs)
    Dictionary pairs -> commaSepIn lbrace rbrace (map renderPair pairs)
      where renderPair (Name field, expr) =
              renderStringLiteral field <> ":" <+> renderExpr expr

renderProjection :: Projection -> Doc
renderProjection =
  \case
    ProjSingle expr -> brackets (renderExpr expr)
    ProjFrom expr -> brackets (renderExpr expr <> ":")
    ProjTo expr -> brackets (":" <> renderExpr expr)
    ProjBoth e1 e2 -> brackets (renderExpr e1 <> ":" <> renderExpr e2)

renderExpr :: Expr -> Doc
renderExpr =
  \case
    BinOpApply op lhs rhs ->
      renderExpr lhs <+> renderBinOp op <+> renderExpr rhs
    Prim prim -> renderPrim prim
    Ref scopedName -> renderScopedName scopedName
    Apply expr params ->
      renderExpr expr <> commaSepIn lparen rparen (map renderExpr params)
    Proj expr proj -> renderExpr expr <> renderProjection proj
    FuncRef scopedName ->
      renderScopedName (ScopedName BuiltIn "funcref") <>
      parens (squotes (renderScopedName scopedName))
    Lambda args expr ->
      braces $ commasep (map renderName args) <+> "->" <+> renderExpr expr
    Exists sn ->
      "exists" <> parens (squotes (renderScopedName sn))

renderAssignTarget :: AssignTarget -> Doc
renderAssignTarget =
  \case
    AssignName sn -> renderScopedName sn
    AssignProj at ap -> renderAssignTarget at <> renderProjection ap

renderFunctionType :: FunctionType -> Doc
renderFunctionType =
  \case
    Regular -> empty
    Closure -> "closure"

renderStmt :: Stmt -> Doc
renderStmt =
  \case
    Let sn expr -> "let" <+> renderScopedName sn <+> "=" <+> renderExpr expr
    Return expr -> "return" <+> renderExpr expr
    Function scopedName args type' block ->
      "function!" <+>
      renderScopedName scopedName <>
      commaSepIn lparen rparen (map renderName args) <+>
      renderFunctionType type' </> indent indentWidth (renderBlock block) </>
      "endfunction"
    Break -> "break"
    Continue -> "continue"
    While expr bl ->
      "while" <+>
      renderExpr expr </> indent indentWidth (renderBlock bl) </> "endwhile"
    Call name params ->
      "call" <+>
      renderScopedName name <> commaSepIn lparen rparen (map renderExpr params)
    Cond (CondStmt (CondCase ifExpr ifBlock) elseIfCases mElseCase) ->
      renderCase ("if" <+> renderExpr ifExpr) ifBlock </>
      stack (map renderElseIf elseIfCases) </>
      maybe empty (renderCase "else") mElseCase </>
      "endif"
      where renderCase h block = h </> indent indentWidth (renderBlock block)
            renderElseIf (CondCase expr block) =
              renderCase ("elseif" <+> renderExpr expr) block
    Assign tgt expr ->
      "let" <+> renderAssignTarget tgt <+> "=" <+> renderExpr expr
    BuiltInStmt name expr ->
      renderName name <+> maybe mempty (parens . renderExpr) expr
    LineComment contents -> "\"" <+> strictText contents
    ExprStmt expr -> renderExpr expr
    PackAdd (PackName n) -> "packadd" <+> strictText n
    Raw code -> strictText code

renderBlock :: Block -> Doc
renderBlock = stack . map renderStmt

renderProgram :: Program -> Doc
renderProgram prg = stack (map renderStmt (programStmts prg))
