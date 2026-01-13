module Ceca.Pretty where

import Ceca.AST
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec.Pos (initialPos)

-- | Configuration for pretty printing
data PrettyConfig = PrettyConfig
  { indentWidth :: Int
  , maxLineWidth :: Int
  } deriving (Eq, Show)

-- | Default configuration
defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig {indentWidth = 2, maxLineWidth = 80}

-- | Render a Doc to Text using default layout
renderPretty :: Doc ann -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions

-- | Pretty print an expression
prettyExpr :: Expr -> Doc ann
prettyExpr (MkSpan _ _ node) = prettyExprNode node

prettyExprNode :: ExprNode -> Doc ann
prettyExprNode (EVar name) = pretty name
prettyExprNode (ELit lit) = prettyLit lit
prettyExprNode (EBuiltin name) = pretty "%%" <> pretty name <> pretty "%%"
prettyExprNode (EAbs pat body) = pretty "(" <> prettyPat pat <+> pretty "=>" <+> prettyExpr body <> pretty ")"
prettyExprNode (EApp e1 e2) = prettyExpr e1 <+> prettyExpr e2
prettyExprNode (ERecord fields) =
  group $ braces $ align $ vsep $ punctuate comma $
    map (\(n, e) -> pretty n <+> pretty "=" <+> prettyExpr e) fields
prettyExprNode (ETuple exprs) =
  group $ parens $ align $ vsep $ punctuate comma $ map prettyExpr exprs
prettyExprNode (EArray exprs) =
  group $ brackets $ align $ vsep $ punctuate comma $ map prettyExpr exprs
prettyExprNode (EProj e field) = prettyExpr e <> pretty "." <> pretty field
prettyExprNode (EExtend e field value) =
  braces $ pretty field <+> pretty "=" <+> prettyExpr value <+> pretty "|" <+> prettyExpr e
prettyExprNode (ERestrict e field) =
  braces $ pretty "-" <> pretty field <+> pretty "|" <+> prettyExpr e
prettyExprNode (ELet pat mtype expr body) =
  let typePart = case mtype of
        Just t -> space <> pretty ":" <+> prettyType t
        Nothing -> mempty
   in pretty "let" <+> prettyPat pat <> typePart <+> pretty "=" <+> prettyExpr expr <> semi <+> line <> prettyExpr body
prettyExprNode (EAnnot e t) = prettyExpr e <+> pretty ":" <+> prettyType t
prettyExprNode (EImport path) = pretty "@" <> pretty path

-- | Pretty print a pattern
prettyPat :: Pattern -> Doc ann
prettyPat (MkSpan _ _ node) = prettyPatNode node

prettyPatNode :: PatternNode -> Doc ann
prettyPatNode (PVar name) = pretty name
prettyPatNode PWildcard = pretty "_"
prettyPatNode (PLit lit) = prettyLit lit
prettyPatNode (PRecord fields mrest) =
  let fieldDocs = map (\(n, p) -> pretty n <+> pretty "=" <+> prettyPat p) fields
      restDoc = case mrest of
        Just rest -> [pretty "|" <+> prettyPat rest]
        Nothing -> []
      allDocs = fieldDocs ++ restDoc
   in braces $ align $ vsep $ punctuate comma allDocs
prettyPatNode (PTuple pats) =
  group $ parens $ align $ vsep $ punctuate comma $ map prettyPat pats
prettyPatNode (PArray pats) =
  group $ brackets $ align $ vsep $ punctuate comma $ map prettyPat pats

-- | Pretty print a literal
prettyLit :: Literal -> Doc ann
prettyLit (LInt n) = pretty n
prettyLit (LFloat n) = pretty n
prettyLit (LString s) = dquotes $ pretty s
prettyLit (LBool b) = pretty $ if b then "true" else "false"

-- | Pretty print a type
prettyType :: Type -> Doc ann
prettyType (MkSpan _ _ node) = prettyTypeNode node

prettyTypeNode :: TypeNode -> Doc ann
prettyTypeNode (TVar name) = pretty name
prettyTypeNode (TCon name) = pretty name
prettyTypeNode (TArrow t1 t2) = prettyType t1 <+> pretty "->" <+> prettyType t2
prettyTypeNode (TTuple ts) =
  group $ parens $ align $ vsep $ punctuate comma $ map prettyType ts
prettyTypeNode (TArray t) = brackets $ prettyType t
prettyTypeNode TRecordEmpty = pretty "{}"
prettyTypeNode (TRecordExtend label typ rest) =
  let (fields, mrest) = collectRecordFields [(label, typ)] rest
      fieldsPart = vsep $ punctuate comma $ map (\(l, t) -> pretty l <> pretty ":" <+> prettyType t) fields
   in case mrest of
        Nothing -> braces $ align fieldsPart
        Just rest' -> braces $ align $ fieldsPart <+> pretty "|" <+> prettyType rest'
 where
  collectRecordFields :: [(Text, Type)] -> Type -> ([(Text, Type)], Maybe Type)
  collectRecordFields acc t@(MkSpan _ _ TRecordEmpty) = (reverse acc, Nothing)
  collectRecordFields acc t@(MkSpan _ _ (TVar _)) = (reverse acc, Just t)
  collectRecordFields acc (MkSpan _ _ (TRecordExtend l t r)) =
    collectRecordFields ((l, t) : acc) r
  collectRecordFields acc _ = (reverse acc, Nothing)

-- | Convenience functions that return Text

-- | Pretty print an expression to Text
prettyPrintExpr :: Expr -> Text
prettyPrintExpr = renderPretty . prettyExpr

-- | Pretty print a pattern to Text
prettyPrintPattern :: Pattern -> Text
prettyPrintPattern = renderPretty . prettyPat

-- | Pretty print a type to Text
prettyPrintType :: Type -> Text
prettyPrintType = renderPretty . prettyType
