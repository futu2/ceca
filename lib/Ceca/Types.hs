{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Ceca.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Ceca.AST

typeNode :: Type -> TypeNode
typeNode (MkSpan _ _ n) = n

data Scheme = Forall [Text] Type

type Subst = Map Text Type

data TypeError = UnificationFail Type Type
               | InfiniteType Text Type
               | UnboundVariable Text
               deriving (Show, Eq)

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Types t where
    apply :: Subst -> t -> t
    ftv   :: t -> Set Text

instance Types Type where
    apply s t = t { spanNode = apply s (spanNode t) }
    ftv = ftv . spanNode

instance Types TypeNode where
     apply s (TVar n) = case Map.lookup n s of
                          Nothing -> TVar n
                          Just t -> typeNode t
     apply s (TCon n) = TCon n
     apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)
     apply s (TTuple ts) = TTuple (map (apply s) ts)
     apply s (TArray t) = TArray (apply s t)
     apply s (TRecordEmpty) = TRecordEmpty
     apply s (TRecordExtend n t r) = TRecordExtend n (apply s t) (apply s r)

     ftv (TVar n) = Set.singleton n
     ftv (TCon _) = Set.empty
     ftv (TArrow t1 t2) = ftv t1 `Set.union` ftv t2
     ftv (TTuple ts) = foldr (Set.union . ftv) Set.empty ts
     ftv (TArray t) = ftv t
     ftv (TRecordEmpty) = Set.empty
     ftv (TRecordExtend _ t r) = ftv t `Set.union` ftv r

instance Types Scheme where
    apply s (Forall as t) = Forall as (apply (foldr Map.delete s as) t)
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv = foldr (Set.union . ftv) Set.empty

decomposeRecord :: TypeNode -> (Map Text Type, Maybe Text)
decomposeRecord (TVar v) = (Map.empty, Just v)
decomposeRecord TRecordEmpty = (Map.empty, Nothing)
decomposeRecord (TRecordExtend l t r) = let (m, rest) = decomposeRecord (typeNode r)
                                         in (Map.insert l t m, rest)
decomposeRecord _ = error "decomposeRecord called on non-record type"

instance (Types v) => Types (Map.Map k v) where
    apply s = Map.map (apply s)
    ftv = Map.foldr (Set.union . ftv) Set.empty