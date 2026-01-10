module Ceca.Desugar where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Ceca.AST

newtype CoreExpr = CoreExpr Expr
  deriving (Eq, Show)

coreExpr :: Expr -> Maybe CoreExpr
coreExpr e = if validateCore e then Just (CoreExpr e) else Nothing

validateCore :: Expr -> Bool
validateCore e = case spanNode e of
    EAbs pat _ -> case spanNode pat of
        PVar _ -> validateCoreBody e
        _ -> False
    _ -> all validateCore (subExprs e)
  where subExprs (MkSpan _ _ node) = case node of
            EApp e1 e2 -> [e1, e2]
            EAbs _ body -> [body]
            ELet _ _ e1 e2 -> [e1, e2]
            ERecord fields -> map snd fields
            ETuple es -> es
            EArray es -> es
            EProj e _ -> [e]
            EAnnot e _ -> [e]
            _ -> []
        validateCoreBody (MkSpan _ _ (EAbs _ body)) = validateCore body
        validateCoreBody _ = True

unCore :: CoreExpr -> Expr
unCore (CoreExpr e) = e

type DesugarM a = State Int a

freshVar :: DesugarM Text
freshVar = do
    n <- get
    put (n + 1)
    return $ pack ("p" ++ show n)

desugar :: Expr -> CoreExpr
desugar e = CoreExpr $ evalState (desugarExpr e) 0

desugarExpr :: Expr -> DesugarM Expr
desugarExpr (MkSpan s e node) = case node of
    EAbs pat body -> do
        (pat', lets) <- desugarPat pat
        body' <- desugarExpr body
        let body'' = foldr (\(x, proj) b -> MkSpan s e (ELet x Nothing proj b)) body' lets
        return $ MkSpan s e (EAbs pat' body'')
    EApp e1 e2 -> do
        e1' <- desugarExpr e1
        e2' <- desugarExpr e2
        return $ MkSpan s e (EApp e1' e2')
    ELet x mty e1 e2 -> do
        e1' <- desugarExpr e1
        e2' <- desugarExpr e2
        return $ MkSpan s e (ELet x mty e1' e2')
    ERecord fields -> do
        fields' <- mapM (\(l, ex) -> do ex' <- desugarExpr ex; return (l, ex')) fields
        return $ MkSpan s e (ERecord fields')
    ETuple es -> do
        es' <- mapM desugarExpr es
        return $ MkSpan s e (ETuple es')
    EArray es -> do
        es' <- mapM desugarExpr es
        return $ MkSpan s e (EArray es')
    EProj ex l -> do
        ex' <- desugarExpr ex
        return $ MkSpan s e (EProj ex' l)
    EAnnot ex ty -> do
        ex' <- desugarExpr ex
        return $ MkSpan s e (EAnnot ex' ty)
    _ -> return (MkSpan s e node)

desugarPat :: Pattern -> DesugarM (Pattern, [(Text, Expr)])
desugarPat (MkSpan s e (PVar x)) = return (MkSpan s e (PVar x), [])
desugarPat (MkSpan s e (PTuple ps)) = do
    p <- freshVar
    let pat' = MkSpan s e (PVar p)
    lets <- mapM (\(i, p_sub) -> do
        (_, lets_sub) <- desugarPat p_sub
        let proj = MkSpan s e (EProj (MkSpan s e (EVar p)) (pack (show i)))
        return (lets_sub ++ [(varFromPat p_sub, proj)])) (zip [0..] ps)
    return (pat', concat lets)
desugarPat (MkSpan s e (PRecord fields mrest)) = do
    p <- freshVar
    let pat' = MkSpan s e (PVar p)
    lets <- mapM (\(l, p_sub) -> do
        (_, lets_sub) <- desugarPat p_sub
        let proj = MkSpan s e (EProj (MkSpan s e (EVar p)) l)
        return (lets_sub ++ [(varFromPat p_sub, proj)])) fields
    case mrest of
        Nothing -> return (pat', concat lets)
        Just _ -> error "rest patterns not supported in desugaring"
desugarPat _ = error "unsupported pattern in desugaring"

varFromPat :: Pattern -> Text
varFromPat (MkSpan _ _ (PVar x)) = x
varFromPat _ = error "expected PVar"