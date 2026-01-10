module Ceca.Normalizer where

import Data.Text (Text)
import qualified Data.Set as Set
import Ceca.AST
import Ceca.Desugar

normalize :: CoreExpr -> CoreExpr
normalize (CoreExpr e) = CoreExpr $ normalizeExpr e

normalizeExpr :: Expr -> Expr
normalizeExpr e = case spanNode e of
    EApp e1 e2 -> case spanNode (normalizeExpr e1) of
        EAbs (MkSpan _ _ (PVar x)) body -> normalizeExpr $ substitute x (normalizeExpr e2) body
        _ -> let e1' = normalizeExpr e1
                 e2' = normalizeExpr e2
             in e { spanNode = EApp e1' e2' }
    EAbs pat body -> e { spanNode = EAbs pat (normalizeExpr body) }
    ELet x mty e1 e2 -> e { spanNode = ELet x mty (normalizeExpr e1) (normalizeExpr e2) }
    ERecord fields -> e { spanNode = ERecord (map (\(l, ex) -> (l, normalizeExpr ex)) fields) }
    ETuple es -> e { spanNode = ETuple (map normalizeExpr es) }
    EArray es -> e { spanNode = EArray (map normalizeExpr es) }
    EProj ex l -> e { spanNode = EProj (normalizeExpr ex) l }
    EAnnot ex ty -> e { spanNode = EAnnot (normalizeExpr ex) ty }
    _ -> e

substitute :: Text -> Expr -> Expr -> Expr
substitute x replacement e = case spanNode e of
    EVar y | x == y -> replacement
    EApp e1 e2 -> e { spanNode = EApp (substitute x replacement e1) (substitute x replacement e2) }
    EAbs pat body -> e { spanNode = EAbs pat (substitute x replacement body) }
    ELet y mty e1 e2 -> e { spanNode = ELet y mty (substitute x replacement e1) (substitute x replacement e2) }
    ERecord fields -> e { spanNode = ERecord (map (\(l, ex) -> (l, substitute x replacement ex)) fields) }
    ETuple es -> e { spanNode = ETuple (map (substitute x replacement) es) }
    EArray es -> e { spanNode = EArray (map (substitute x replacement) es) }
    EProj ex l -> e { spanNode = EProj (substitute x replacement ex) l }
    EAnnot ex ty -> e { spanNode = EAnnot (substitute x replacement ex) ty }
    _ -> e