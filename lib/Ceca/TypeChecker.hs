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

runTIWith :: Env -> TI a -> Either TypeError a
runTIWith env ti = evalState (runReaderT (runExceptT ti) env) 0

runTI :: TI a -> Either TypeError a
runTI = runTIWith Map.empty

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
  | otherwise = throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") (TCon a)) (MkSpan (initialPos "dummy") (initialPos "dummy") (TCon b)))
unify' (TArrow a1 a2) (TArrow b1 b2) = do
  s1 <- unify a1 b1
  s2 <- unify (apply s1 a2) (apply s1 b2)
  return (s2 `compose` s1)
unify' (TTuple as) (TTuple bs)
  | length as == length bs = unifyList as bs
  | otherwise = throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") (TTuple as)) (MkSpan (initialPos "dummy") (initialPos "dummy") (TTuple bs)))
unify' (TArray a) (TArray b) = unify a b
unify' TRecordEmpty TRecordEmpty = return nullSubst
unify' r1@(TRecordExtend _ _ _) r2@(TRecordExtend _ _ _) = unifyRecords r1 r2
unify' r1@(TRecordExtend _ _ _) TRecordEmpty = unifyRecords r1 TRecordEmpty
unify' TRecordEmpty r2@(TRecordExtend _ _ _) = unifyRecords TRecordEmpty r2
unify' t1 t2 = throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") t1) (MkSpan (initialPos "dummy") (initialPos "dummy") t2))

unifyList :: [Type] -> [Type] -> TI Subst
unifyList [] [] = return nullSubst
unifyList _ _ = error "unifyList called with mismatched lengths"

unifyAll :: [Type] -> Type -> TI Subst
unifyAll [] _ = return nullSubst
unifyAll (t:ts) target = do
  s1 <- unify t target
  s2 <- unifyAll (map (apply s1) ts) (apply s1 target)
  return (s2 `compose` s1)

unifyRecords :: TypeNode -> TypeNode -> TI Subst
unifyRecords r1 r2 = do
  let (m1, rest1) = decomposeRecord r1
      (m2, rest2) = decomposeRecord r2
      keys1 = Set.fromList (Map.keys m1)
      keys2 = Set.fromList (Map.keys m2)
      common = Set.intersection keys1 keys2
      emptyRecord = MkSpan (initialPos "dummy") (initialPos "dummy") TRecordEmpty
      buildRecord fields tailRecord =
        foldl
          (\acc (l, t) -> MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l t acc))
          tailRecord
          fields
  s1 <- unifyMaps (Map.toList (Map.restrictKeys m1 common)) (Map.toList (Map.restrictKeys m2 common))
  let m1' = Map.map (apply s1) m1
      m2' = Map.map (apply s1) m2
      extra1 = Map.withoutKeys m1' common
      extra2 = Map.withoutKeys m2' common
  case (rest1, rest2) of
    (Nothing, Nothing) ->
      if Map.null extra1 && Map.null extra2
        then return s1
        else throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") r1) (MkSpan (initialPos "dummy") (initialPos "dummy") r2))
    (Just v1, Nothing) ->
      if Map.null extra1
        then do
          let extraRecord = buildRecord (Map.toList extra2) emptyRecord
          s2 <- unify (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v1)) extraRecord
          return (s2 `compose` s1)
        else throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") r1) (MkSpan (initialPos "dummy") (initialPos "dummy") r2))
    (Nothing, Just v2) ->
      if Map.null extra2
        then do
          let extraRecord = buildRecord (Map.toList extra1) emptyRecord
          s2 <- unify (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v2)) extraRecord
          return (s2 `compose` s1)
        else throwError (UnificationFail (MkSpan (initialPos "dummy") (initialPos "dummy") r1) (MkSpan (initialPos "dummy") (initialPos "dummy") r2))
    (Just v1, Just v2) ->
      if Map.null extra1 && Map.null extra2
        then do
          s2 <- unify (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v1)) (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v2))
          return (s2 `compose` s1)
        else do
          rowTail <- fresh
          let extraRecord1 = buildRecord (Map.toList extra1) rowTail
          let extraRecord2 = buildRecord (Map.toList extra2) rowTail
          s2 <- unify (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v1)) extraRecord2
          s3 <- unify (apply s2 (MkSpan (initialPos "dummy") (initialPos "dummy") (TVar v2))) (apply s2 extraRecord1)
          return (s3 `compose` s2 `compose` s1)

unifyMaps :: [(Text, Type)] -> [(Text, Type)] -> TI Subst
unifyMaps [] [] = return nullSubst
unifyMaps ((_, t1) : ts1) ((_, t2) : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMaps (map (\(l, t) -> (l, apply s1 t)) ts1) (map (\(l, t) -> (l, apply s1 t)) ts2)
  return (s2 `compose` s1)

varBind :: Text -> TypeNode -> TI Subst
varBind v t
  | TVar v == t = return nullSubst
  | v `Set.member` ftv t = throwError (InfiniteType v (MkSpan (initialPos "dummy") (initialPos "dummy") t))
  | otherwise = return (Map.singleton v (MkSpan (initialPos "dummy") (initialPos "dummy") t))

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
inferExpr e = withExprSpan e $ case spanNode e of
  ELit l -> return (nullSubst, litType l)
  EBuiltin _ -> do
    tv <- fresh
    return (nullSubst, tv)
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
  ELet pat mty e1 e2 -> do
    (s1, t1) <- inferExpr e1
    (s2, t_pat, env_pat) <- inferPat pat
    s3 <- unify (apply s2 t1) t_pat
    let t1' = apply s3 t1
    let env_pat_mono = Map.map (apply s3) env_pat
    case mty of
      Just ty -> do
        s4 <- unify t1' ty
        let env_pat_final = Map.map (apply s4) env_pat_mono
        (s5, t2) <- local (Map.union env_pat_final . apply (s4 `compose` s3 `compose` s2 `compose` s1)) (inferExpr e2)
        return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, t2)
      Nothing -> do
        env <- ask
        let env_pat_final = Map.map (\(Forall [] t) -> generalize (apply s3 env) t) env_pat_mono
        (s4, t2) <- local (Map.union env_pat_final . apply (s3 `compose` s2 `compose` s1)) (inferExpr e2)
        return (s4 `compose` s3 `compose` s2 `compose` s1, t2)
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
    t <- case ts of
      [] -> fresh
      _ -> pure (head (map (apply s) ts)) -- assume all same, but for simplicity
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
  EExtend e l val -> do
    (s1, t1) <- inferExpr e
    (s2, tVal) <- local (apply s1) (inferExpr val)
    tv <- fresh
    rowVar <- fresh
    let rowType = MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l tv rowVar)
    s3 <- unify (apply s2 t1) rowType
    s4 <- unify (apply s3 tVal) (apply s3 tv)
    return (s4 `compose` s3 `compose` s2 `compose` s1, apply s4 rowType)
  ERestrict e l -> do
    (s1, t1) <- inferExpr e
    tv <- fresh
    rowVar <- fresh
    let rowType = MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l tv rowVar)
    s2 <- unify t1 rowType
    return (s2 `compose` s1, apply s2 rowVar)
  EImport _ -> do
    tv <- fresh
    return (nullSubst, tv)

withExprSpan :: Expr -> TI a -> TI a
withExprSpan expr action =
  catchError action $ \err -> case err of
    TypeErrorAt _ _ -> throwError err
    _ -> throwError (TypeErrorAt expr err)

inferPat :: Pattern -> TI (Subst, Type, Env)
inferPat p = case spanNode p of
  PVar x -> do
    tv <- fresh
    return (nullSubst, tv, Map.singleton x (Forall [] tv))
  PWildcard -> do
    tv <- fresh
    return (nullSubst, tv, Map.empty)
  PLit lit -> return (nullSubst, litType lit, Map.empty)
  PTuple ps -> do
    (ss, ts, envs) <- unzip3 <$> mapM inferPat ps
    let s = foldr compose nullSubst ss
        t = MkSpan (initialPos "dummy") (initialPos "dummy") (TTuple (map (apply s) ts))
        env = foldr (Map.union . apply s) Map.empty envs
    return (s, t, env)
  PArray ps -> do
    (ss, ts, envs) <- unzip3 <$> mapM inferPat ps
    let s0 = foldr compose nullSubst ss
    tv <- fresh
    s1 <- unifyAll (map (apply s0) ts) tv
    let s = s1 `compose` s0
        env = foldr (Map.union . apply s) Map.empty envs
        t = MkSpan (initialPos "dummy") (initialPos "dummy") (TArray (apply s tv))
    return (s, t, env)
  PRecord fields mrest -> do
    (ss, ts, envs) <- unzip3 <$> mapM inferPat (map snd fields)
    let s0 = foldr compose nullSubst ss
        envFields = foldr (Map.union . apply s0) Map.empty envs
    (sRest, restType, envRest) <- case mrest of
      Nothing ->
        return (nullSubst, MkSpan (initialPos "dummy") (initialPos "dummy") TRecordEmpty, Map.empty)
      Just restPat -> inferPat restPat
    let s = sRest `compose` s0
        restType' = apply s restType
        fieldTypes = map (apply s) ts
        t =
          foldl
            (\r (l, t') -> MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend l t' r))
            restType'
            (zip (map fst fields) fieldTypes)
        env = Map.union (apply s envFields) (apply s envRest)
    return (s, t, env)

typeCheck :: Expr -> Either TypeError Type
typeCheck = typeCheckWithEnv Map.empty

typeCheckWithEnv :: Env -> Expr -> Either TypeError Type
typeCheckWithEnv env e = case runTIWith env (inferExpr e) of
  Left err -> Left err
  Right (s, t) -> Right (apply s t)
