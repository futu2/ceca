module Ceca.Core where

import Ceca.AST
import Ceca.TypeChecker
  ( TI
  , fresh
  , generalize
  , inferPat
  , instantiate
  , litType
  , runTI
  , unify
  )
import Ceca.Types
  ( Scheme (Forall)
  , Subst
  , TypeError (UnboundVariable)
  , apply
  , compose
  , decomposeRecord
  , nullSubst
  , typeNode
  )
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (get, put)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Text.Megaparsec.Pos (initialPos)

data CoreExpr = CoreExpr
  { coreType :: Type
  , coreNode :: CoreExprNode
  }
  deriving (Eq, Show)

data CoreExprNode
  = CVar Text
  | CLit Literal
  | CBuiltin Text
  | CAbs Text CoreExpr
  | CApp CoreExpr CoreExpr
  | CRecord [(Text, CoreExpr)]
  | CTuple [CoreExpr]
  | CArray [CoreExpr]
  | CProj CoreExpr Text
  | CExtend CoreExpr Text CoreExpr
  | CRestrict CoreExpr Text
  | CAnnot CoreExpr Type
  | CImport FilePath
  deriving (Eq, Show)

desugar :: Expr -> Either TypeError CoreExpr
desugar expr = case runTI (inferCoreExpr expr) of
  Left err -> Left err
  Right (s, core) -> Right (applySubstCore s core)

inferCoreExpr :: Expr -> TI (Subst, CoreExpr)
inferCoreExpr expr = case spanNode expr of
  ELit lit ->
    return (nullSubst, CoreExpr (litType lit) (CLit lit))
  EBuiltin name -> do
    tv <- fresh
    return (nullSubst, CoreExpr tv (CBuiltin name))
  EVar x -> do
    env <- ask
    case Map.lookup x env of
      Nothing -> throwError (UnboundVariable x)
      Just scheme -> do
        t <- instantiate scheme
        return (nullSubst, CoreExpr t (CVar x))
  EAbs pat body -> do
    (s1, tPat, envPat) <- inferPat pat
    (s2, coreBody) <- local (Map.union envPat . apply s1) (inferCoreExpr body)
    let tPat' = apply s2 tPat
        coreBody' = applySubstCore s2 coreBody
    (param, boundBody) <- desugarPattern pat tPat' coreBody'
    let lamType = mkType (TArrow tPat' (coreType boundBody))
    return (s2 `compose` s1, CoreExpr lamType (CAbs param boundBody))
  EApp e1 e2 -> do
    (s1, core1) <- inferCoreExpr e1
    (s2, core2) <- local (apply s1) (inferCoreExpr e2)
    tv <- fresh
    s3 <- unify (apply s2 (coreType core1)) (mkType (TArrow (coreType core2) tv))
    let s = s3 `compose` s2 `compose` s1
        core1' = applySubstCore s core1
        core2' = applySubstCore s core2
    return (s, CoreExpr (apply s3 tv) (CApp core1' core2'))
  ELet pat mty e1 e2 -> do
    (s1, core1) <- inferCoreExpr e1
    (s2, tPat, envPat) <- inferPat pat
    s3 <- unify (apply s2 (coreType core1)) tPat
    let t1' = apply s3 (coreType core1)
        envPatMono = apply s3 envPat
    case mty of
      Just ty -> do
        s4 <- unify t1' ty
        let envPatFinal = apply s4 envPatMono
        (s5, core2) <-
          local
            (Map.union envPatFinal . apply (s4 `compose` s3 `compose` s2 `compose` s1))
            (inferCoreExpr e2)
        let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
            core1' = applySubstCore s core1
            core2' = applySubstCore s core2
            tPat' = apply s tPat
        (param, boundBody) <- desugarPattern pat tPat' core2'
        let lamType = mkType (TArrow tPat' (coreType boundBody))
            lamExpr = CoreExpr lamType (CAbs param boundBody)
        return (s, CoreExpr (coreType boundBody) (CApp lamExpr core1'))
      Nothing -> do
        env <- ask
        let envPatFinal =
              Map.map
                (\(Forall [] t) -> generalize (apply s3 env) t)
                envPatMono
        (s4, core2) <-
          local
            (Map.union envPatFinal . apply (s3 `compose` s2 `compose` s1))
            (inferCoreExpr e2)
        let s = s4 `compose` s3 `compose` s2 `compose` s1
            core1' = applySubstCore s core1
            core2' = applySubstCore s core2
            tPat' = apply s tPat
        (param, boundBody) <- desugarPattern pat tPat' core2'
        let lamType = mkType (TArrow tPat' (coreType boundBody))
            lamExpr = CoreExpr lamType (CAbs param boundBody)
        return (s, CoreExpr (coreType boundBody) (CApp lamExpr core1'))
  ERecord fields -> do
    (ss, coreFields) <- unzip <$> mapM (inferCoreExpr . snd) fields
    let s = foldr compose nullSubst ss
        coreFields' = map (applySubstCore s) coreFields
        fields' = zip (map fst fields) coreFields'
        t =
          foldl
            (\r (l, e) -> mkType (TRecordExtend l (coreType e) r))
            (mkType TRecordEmpty)
            fields'
    return (s, CoreExpr t (CRecord fields'))
  ETuple es -> do
    (ss, coreEs) <- unzip <$> mapM inferCoreExpr es
    let s = foldr compose nullSubst ss
        coreEs' = map (applySubstCore s) coreEs
        t = mkType (TTuple (map coreType coreEs'))
    return (s, CoreExpr t (CTuple coreEs'))
  EArray es -> do
    (ss, coreEs) <- unzip <$> mapM inferCoreExpr es
    let s = foldr compose nullSubst ss
        coreEs' = map (applySubstCore s) coreEs
    elemType <- case coreEs' of
      [] -> fresh
      _ -> pure (coreType (head coreEs'))
    return (s, CoreExpr (mkType (TArray elemType)) (CArray coreEs'))
  EProj e l -> do
    (s1, core1) <- inferCoreExpr e
    tv <- fresh
    rowVar <- fresh
    let rowType = mkType (TRecordExtend l tv rowVar)
    s2 <- unify (coreType core1) rowType
    let s = s2 `compose` s1
        core1' = applySubstCore s core1
    return (s, CoreExpr (apply s2 tv) (CProj core1' l))
  EExtend e l val -> do
    (s1, core1) <- inferCoreExpr e
    (s2, coreVal) <- local (apply s1) (inferCoreExpr val)
    tv <- fresh
    rowVar <- fresh
    let rowType = mkType (TRecordExtend l tv rowVar)
    s3 <- unify (apply s2 (coreType core1)) rowType
    s4 <- unify (apply s3 (coreType coreVal)) (apply s3 tv)
    let s = s4 `compose` s3 `compose` s2 `compose` s1
        core1' = applySubstCore s core1
        coreVal' = applySubstCore s coreVal
        rowType' = apply s rowType
    return (s, CoreExpr rowType' (CExtend core1' l coreVal'))
  ERestrict e l -> do
    (s1, core1) <- inferCoreExpr e
    tv <- fresh
    rowVar <- fresh
    let rowType = mkType (TRecordExtend l tv rowVar)
    s2 <- unify (coreType core1) rowType
    let s = s2 `compose` s1
        core1' = applySubstCore s core1
    return (s, CoreExpr (apply s2 rowVar) (CRestrict core1' l))
  EAnnot e ty -> do
    (s1, core1) <- inferCoreExpr e
    s2 <- unify (coreType core1) ty
    let s = s2 `compose` s1
        core1' = applySubstCore s core1
        ty' = apply s2 ty
    return (s, CoreExpr ty' (CAnnot core1' ty'))
  EImport path -> do
    tv <- fresh
    return (nullSubst, CoreExpr tv (CImport path))

desugarPattern :: Pattern -> Type -> CoreExpr -> TI (Text, CoreExpr)
desugarPattern pat patType body = case spanNode pat of
  PVar x -> return (x, body)
  PWildcard -> do
    param <- freshVarName
    return (param, body)
  PLit _ -> error "literal patterns are not supported in core desugaring"
  PArray _ -> error "array patterns are not supported in core desugaring"
  _ -> do
    param <- freshVarName
    let paramExpr = CoreExpr patType (CVar param)
        bindings = patternBindings paramExpr pat patType
        boundBody = applyBindings bindings body
    return (param, boundBody)

patternBindings :: CoreExpr -> Pattern -> Type -> [(Text, CoreExpr)]
patternBindings value pat patType = case spanNode pat of
  PVar x -> [(x, value)]
  PWildcard -> []
  PLit _ -> error "literal patterns are not supported in core desugaring"
  PArray _ -> error "array patterns are not supported in core desugaring"
  PTuple ps -> case typeNode patType of
    TTuple ts
      | length ps == length ts ->
          concatMap
            (\(i, (p, t)) ->
              let proj = CoreExpr t (CProj value (pack (show i)))
               in patternBindings proj p t)
            (zip [0 :: Int ..] (zip ps ts))
    _ -> error "tuple pattern type mismatch during desugaring"
  PRecord fields mrest -> case mrest of
    Just _ -> error "rest patterns are not supported in core desugaring"
    Nothing ->
      let (fieldMap, _) = decomposeRecord (typeNode patType)
       in concatMap
            (\(l, p) -> case Map.lookup l fieldMap of
              Nothing -> error "record pattern field missing during desugaring"
              Just t ->
                let proj = CoreExpr t (CProj value l)
                 in patternBindings proj p t)
            fields

applyBindings :: [(Text, CoreExpr)] -> CoreExpr -> CoreExpr
applyBindings bindings body =
  foldr
    (\(x, expr) acc ->
      let lamType = mkType (TArrow (coreType expr) (coreType acc))
          lamExpr = CoreExpr lamType (CAbs x acc)
       in CoreExpr (coreType acc) (CApp lamExpr expr))
    body
    bindings

applySubstCore :: Subst -> CoreExpr -> CoreExpr
applySubstCore s (CoreExpr t node) =
  CoreExpr (apply s t) (applySubstNode s node)

applySubstNode :: Subst -> CoreExprNode -> CoreExprNode
applySubstNode s node = case node of
  CVar x -> CVar x
  CLit lit -> CLit lit
  CBuiltin name -> CBuiltin name
  CAbs x body -> CAbs x (applySubstCore s body)
  CApp e1 e2 -> CApp (applySubstCore s e1) (applySubstCore s e2)
  CRecord fields -> CRecord (map (\(l, e) -> (l, applySubstCore s e)) fields)
  CTuple es -> CTuple (map (applySubstCore s) es)
  CArray es -> CArray (map (applySubstCore s) es)
  CProj e l -> CProj (applySubstCore s e) l
  CExtend e l val -> CExtend (applySubstCore s e) l (applySubstCore s val)
  CRestrict e l -> CRestrict (applySubstCore s e) l
  CAnnot e ty -> CAnnot (applySubstCore s e) (apply s ty)
  CImport path -> CImport path

freshVarName :: TI Text
freshVarName = do
  n <- get
  put (n + 1)
  return (pack ("p" <> show n))

mkType :: TypeNode -> Type
mkType node = MkSpan (initialPos "dummy") (initialPos "dummy") node
