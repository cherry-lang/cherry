{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typecheck.Solve where

import qualified Data.Map              as Map
import qualified Data.Set              as Set

import qualified Type                  as T
import           Typecheck.Constraint
import           Typecheck.Environment


newtype Subst
  = Subst (Map.Map T.Var T.Type)


type Unifier = (Subst, [Constraint])


class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set T.Var


instance Substitutable T.Type where
  apply _ (T.Con type')             = T.Con type'
  apply (Subst s) type'@(T.Var var) = Map.findWithDefault type' var s
  apply subst (t1 `T.Arrow` t2)     = apply subst t1 `T.Arrow` apply subst t2

  ftv T.Con{}         = Set.empty
  ftv (T.Var var)     = Set.singleton var
  ftv (T.Arrow t1 t2) = ftv t1 `Set.union` ftv t2


instance Substitutable T.Scheme where
  apply (Subst s) (T.Forall as type') = T.Forall as $ apply s' type'
    where s' = Subst $ foldr Map.delete s as

  ftv (T.Forall as type') = ftv type' `Set.difference` Set.fromList as


instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)

  ftv (t1, t2) = ftv t1 `Set.union` ftv t2


instance Substitutable a => Substitutable [a] where
  apply = map . apply

  ftv = foldr (Set.union . ftv) Set.empty


instance Substitutable Environment where
  apply s (Environment env type') = Environment (Map.map (apply s) env) type'

  ftv (Environment env type') = ftv $ Map.elems env
