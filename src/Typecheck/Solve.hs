{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typecheck.Solve where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import qualified Type                   as T
import           Typecheck.Constraint
import           Typecheck.Environment
import           Typecheck.Error


newtype Subst
  = Subst (Map.Map T.Var T.Type)
  deriving (Monoid)


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


emptySubst :: Subst
emptySubst = mempty


-- SOLVE

type Solve a = ExceptT Error Identity a


solve :: [Constraint] -> Either Error Subst
solve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)


compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s2


solver :: Unifier -> Solve Subst
solver (sub, cs) =
  case cs of
    [] ->
      return sub

    ((t1, t2):cs') -> do
      sub' <- unifies t1 t2
      solver (sub' `compose` sub, apply sub' cs')


unifyMany :: [T.Type] -> [T.Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (h1:t1) (h2:t2) = do
  sub1 <- unifies h1 h2
  sub2 <- unifyMany (apply sub1 t1) (apply sub1 t2)
  return (sub2 `compose` sub1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2


unifies :: T.Type -> T.Type -> Solve Subst
unifies t1 t2 | t1 == t2                = return emptySubst
unifies (T.Var var) type'               = var `bind` type'
unifies type' (T.Var var)               = var `bind` type'
unifies (T.Arrow t1 t2) (T.Arrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2                           = throwError $ TypeMismatch t1 t2


bind :: T.Var -> T.Type -> Solve Subst
bind var type' | type' == T.Var var    = return emptySubst
               | occursCheck var type' = throwError $ InfiniteType var type'
               | otherwise             = return $ Subst $ Map.singleton var type'


occursCheck :: Substitutable a => T.Var -> a -> Bool
occursCheck var t = var `Set.member` ftv t
