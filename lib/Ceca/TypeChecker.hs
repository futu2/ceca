module Ceca.TypeChecker where

import Ceca.AST
import Ceca.Types
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Text.Megaparsec.Pos (initialPos)

type Env = Map Text Scheme

type TI a = ExceptT TypeError (ReaderT Env (StateT Int Identity)) a

runTI :: TI a -> Either TypeError a
runTI ti = evalState (runReaderT (runExceptT ti) Map.empty) 0

fresh :: TI Type
fresh = do
  n <- get
  put (n + 1)
  return $ MkSpan (initialPos "dummy") (initialPos "dummy") (TVar (pack ("?t" <> show n)))

unify :: Type -> Type -> TI Subst
unify t1 t2 = unify' (typeNode t1) (typeNode t2)

unify' :: TypeNode -> TypeNode -> TI Subst
unify' (TVar v) t = varBind v t
unify' t (TVar v) = varBind v t
unify' (TCon a) (TCon b)
  | a == b = return nullSubst
  | otherwise = throwError (UnificationFail (MkSpan undefined undefined (TCon a)) (MkSpan undefined undefined (TCon b)))
unify' (TArrow a1 a2) (TArrow b1 b2) = do
  s1 <- unify a1 b1
  s2 <- unify (apply s1 a2) (apply s1 b2)
  return (s2 `compose` s1)
unify' (TTuple as) (TTuple bs)
  | length as == length bs = unifyList as bs
  | otherwise = throwError (UnificationFail (MkSpan undefined undefined (TTuple as)) (MkSpan undefined undefined (TTuple bs)))
unify' (TArray a) (TArray b) = unify a b
unify' TRecordEmpty TRecordEmpty = return nullSubst
unify' r1@(TRecordExtend _ _ _) r2@(TRecordExtend _ _ _) = unifyRecords r1 r2
unify' r1@(TRecordExtend _ _ _) TRecordEmpty = unifyRecords r1 TRecordEmpty
unify' TRecordEmpty r2@(TRecordExtend _ _ _) = unifyRecords TRecordEmpty r2
unify' t1 t2 = throwError (UnificationFail (MkSpan undefined undefined t1) (MkSpan undefined undefined t2))

unifyList :: [Type] -> [Type] -> TI Subst
unifyList [] [] = return nullSubst
unifyList _ _ = error "unifyList called with mismatched lengths"

unifyRecords :: TypeNode -> TypeNode -> TI Subst
unifyRecords r1 r2 = do
  let (m1, rest1) = decomposeRecord r1
      (m2, rest2) = decomposeRecord r2
  if Map.keys m1 /= Map.keys m2 then throwError (UnificationFail (MkSpan undefined undefined r1) (MkSpan undefined undefined r2)) else return ()
  s1 <- unifyMaps (Map.toList m1) (Map.toList m2)
  s2 <- case (rest1, rest2) of
    (Nothing, Nothing) -> return nullSubst
    (Just v, Nothing) -> varBind v TRecordEmpty
    (Nothing, Just v) -> varBind v TRecordEmpty
    (Just v1, Just v2) -> unify' (TVar v1) (TVar v2)
  return (s2 `compose` s1)

unifyMaps :: [(Text, Type)] -> [(Text, Type)] -> TI Subst
unifyMaps [] [] = return nullSubst
unifyMaps ((_, t1) : ts1) ((_, t2) : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMaps (map (\(l, t) -> (l, apply s1 t)) ts1) (map (\(l, t) -> (l, apply s1 t)) ts2)
  return (s2 `compose` s1)

varBind :: Text -> TypeNode -> TI Subst
varBind v t
  | TVar v == t = return nullSubst
  | v `Set.member` ftv t = throwError (InfiniteType v (MkSpan undefined undefined t))
  | otherwise = return (Map.singleton v (MkSpan undefined undefined t))

instantiate :: Scheme -> TI Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList (zip as as')
  return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList (ftv t `Set.difference` ftv env)

litType :: Literal -> Type
litType (LInt _) = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "int"))
litType (LFloat _) = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "float"))
litType (LString _) = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "string"))
litType (LBool _) = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "bool"))

inferExpr :: Expr -> TI (Subst, Type)
inferExpr e = case spanNode e of
  ELit l -> return (nullSubst, litType l)
  EVar x -> do
    env <- ask
    case Map.lookup x env of
      Nothing -> throwError (UnboundVariable x)
      Just s -> do
        t <- instantiate s
        return (nullSubst, t)
  EAbs pat body -> do
    (s1, t1, env') <- inferPat pat
    (s2, t2) <- local (Map.union env' . apply s1) (inferExpr body)
    return (s2 `compose` s1, MkSpan (initialPos "dummy") (initialPos "dummy") (TArrow (apply s2 t1) t2))
  EApp e1 e2 -> do
    (s1, t1) <- inferExpr e1
    (s2, t2) <- local (apply s1) (inferExpr e2)
    tv <- fresh
    s3 <- unify (apply s2 t1) (MkSpan (initialPos "dummy") (initialPos "dummy") (TArrow t2 tv))
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  ELet x mty e1 e2 -> do
    (s1, t1) <- inferExpr e1
    env <- ask
    case mty of
      Just ty -> do
        s2 <- unify (apply s1 t1) ty
        let t1' = apply s2 t1
            sc = generalize (apply s2 env) t1'
            env' = Map.singleton x sc
        (s3, t2) <- local (Map.insert x sc . apply (s2 `compose` s1)) (inferExpr e2)
        return (s3 `compose` s2 `compose` s1, t2)
      Nothing -> do
        let t1' = apply s1 t1
            sc = generalize env t1'
            env' = Map.singleton x sc
        (s2, t2) <- local (Map.insert x sc . apply s1) (inferExpr e2)
        return (s2 `compose` s1, t2)
  ERecord fields -> do
    (ss, ts) <- unzip <$> mapM inferExpr (map snd fields)
    let s = foldr compose nullSubst ss
        t =
          foldl
            (\r (l, t') -> MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l (apply s t') r))
            (MkSpan (initialPos "dummy") (initialPos "dummy") TRecordEmpty)
            (zip (map fst fields) ts)
    return (s, t)
  ETuple es -> do
    (ss, ts) <- unzip <$> mapM inferExpr es
    let s = foldr compose nullSubst ss
    return (s, MkSpan (initialPos "dummy") (initialPos "dummy") (TTuple (map (apply s) ts)))
  EArray es -> do
    (ss, ts) <- unzip <$> mapM inferExpr es
    let s = foldr compose nullSubst ss
        t = head (map (apply s) ts) -- assume all same, but for simplicity
    return (s, MkSpan (initialPos "dummy") (initialPos "dummy") (TArray t))
  EAnnot e ty -> do
    (s1, t) <- inferExpr e
    s2 <- unify t ty
    return (s2 `compose` s1, apply s2 ty)
  EProj e l -> do
    (s1, t1) <- inferExpr e
    tv <- fresh
    rowVar <- fresh
    let rowType = MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l tv rowVar)
    s2 <- unify t1 rowType
    return (s2 `compose` s1, apply s2 tv)
  _ -> error "not implemented"

inferPat :: Pattern -> TI (Subst, Type, Env)
inferPat p = case spanNode p of
  PVar x -> do
    tv <- fresh
    return (nullSubst, tv, Map.singleton x (Forall [] tv))
  PTuple ps -> do
    (ss, ts, envs) <- unzip3 <$> mapM inferPat ps
    let s = foldr compose nullSubst ss
        t = MkSpan (initialPos "dummy") (initialPos "dummy") (TTuple (map (apply s) ts))
        env = foldr (Map.union . apply s) Map.empty envs
    return (s, t, env)
  PRecord fields mrest -> case mrest of
    Nothing -> do
      (ss, ts, envs) <- unzip3 <$> mapM inferPat (map snd fields)
      let s = foldr compose nullSubst ss
          env = foldr (Map.union . apply s) Map.empty envs
          t =
            foldl
              (\r (l, t') -> MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l (apply s t') r))
              (MkSpan (initialPos "dummy") (initialPos "dummy") TRecordEmpty)
              (zip (map fst fields) ts)
      return (s, t, env)
    Just _ -> error "rest pattern not implemented"
  _ -> error "pattern not implemented"

typeCheck :: Expr -> Either TypeError Type
typeCheck e = case runTI (inferExpr e) of
  Left err -> Left err
  Right (s, t) -> Right (apply s t)
