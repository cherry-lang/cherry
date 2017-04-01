{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Typecheck.Infer where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.List
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           System.IO.Unsafe

import qualified Syntax                as Ch
import qualified Type                  as T
import           Typecheck.Constraint
import           Typecheck.Environment
import qualified Typecheck.Error       as Err
import           Typecheck.Solve


data InferState
  = InferState { count :: Int }


type Infer a =
  (RWST
    Environment
    [Constraint]
    InferState
    (Except Err.Error)
    a
  )


emptyInfer :: InferState
emptyInfer = InferState 0


-- UTILS


uni :: T.Type -> T.Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer T.Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  let l = letters !! count s
  return $ T.var l


inEnv :: (String, T.Scheme) -> Infer a -> Infer a
inEnv (name, scheme) m = do
  let scope env = extend (name, scheme) (remove name env)
  local scope m


lookupEnv :: String -> Infer (Maybe T.Type)
lookupEnv name = do
  env <- ask
  case lookupVar name env of
    Nothing ->
      return Nothing

    Just scheme -> do
      instantiate scheme >>= return . Just


instantiate :: T.Scheme -> Infer T.Type
instantiate (T.Forall as type') = do
  as' <- mapM (\_ -> fresh) as
  let substitution = Subst $ Map.fromList $ zip as as'
  return $ apply substitution type'


closeOver :: T.Type -> T.Scheme
closeOver = normalize . generalize emptyEnv


normalize :: T.Scheme -> T.Scheme
normalize (T.Forall _ body) = T.Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map T.TV letters)

    fv (T.Var var)     = [var]
    fv (T.Con _)       = []
    fv (T.Arrow t1 t2) = fv t1 ++ fv t2

    normtype (T.Arrow t1 t2) = T.Arrow (normtype t1) (normtype t2)
    normtype (T.Con type')   = T.Con type'
    normtype (T.Var var)     =
      case Prelude.lookup var ord of
        Nothing ->
          error "type variable not in signature"

        Just var' ->
          T.Var var'


generalize :: Environment -> T.Type -> T.Scheme
generalize env type' = T.Forall as type'
  where as = Set.toList $ ftv type' `Set.difference` ftv env


-- INFERENCE


infer
  :: Environment
  -> Ch.Module
  -> Either Err.Error (Ch.Module, [Constraint])
infer env m = runInfer env $ module' m


runInfer :: Environment -> Infer a -> Either Err.Error (a, [Constraint])
runInfer env m =
  case runExcept $ evalRWST m env emptyInfer of
    Left err ->
      Left err

    Right result ->
      Right result


module' :: Ch.Module -> Infer Ch.Module
module' m@(Ch.Module { Ch.decls }) = do
  env <- topDecls decls
  return $ m { Ch.typeEnv = env }


imports :: [Ch.Assign] -> Infer a -> Infer a
imports [] m             = m
imports (Ch.Plain h:t) m = do
  tv <- fresh
  inEnv (h, T.Forall [] tv) (imports t m)


topDecls :: [Ch.Declaration] -> Infer Environment
topDecls [] = ask
topDecls (d:ds) =
  case d of
    Ch.TypeAnn _ (name, _) type' -> do
      inEnv (name, T.Forall (Set.toList $ ftv type') type') $ topDecls ds

    Ch.Func _ (name, _) params exprs -> do
      let rest  = init exprs
      let last' = last exprs

      env   <- ask
      type' <- param params $ do
        mapM_ expr rest
        t <- expr last'
        return t

      def <- lookupEnv name
      case def of
        Nothing ->
          inEnv (name, generalize env type') $ topDecls ds

        Just t ->
          uni t type' >> topDecls ds

    _ ->
      ask


param :: [String] -> Infer T.Type -> Infer T.Type
param [] m     = m
param (p:ps) m = fresh >>= \tv -> inEnv (p, T.Forall [] tv) $ do
  t <- param ps m
  return $ T.Arrow tv t


expr :: Ch.Expr -> Infer T.Type
expr e =
  case e of
    Ch.Var _ var -> do
      mt <- lookupEnv var
      case mt of
        Just t  -> return t
        Nothing -> throwError $ Err.UnboundVariable var

    Ch.Prop _ (var:ps) -> do
      mt <- lookupEnv var
      case mt of
        Just t  -> record var ps t
        Nothing -> throwError $ Err.UnboundVariable var

    Ch.Lit _ lit' ->
      lit lit'

    Ch.App _ e1 e2 -> do
      t1 <- expr e1
      t2 <- expr e2
      tv <- fresh
      uni t1 (t2 `T.Arrow` tv)
      return tv


record :: String -> [String] -> T.Type -> Infer T.Type
record _ [] _       = fail "Can't access record without a prop."
record var (p:ps) r =
  case r of
    T.Record props -> case Map.lookup p props of
      Just re@T.Record{} ->
        record var ps re

      Just t ->
        return t

      Nothing ->
        throwError $ Err.UnboundProperty var p

    _ ->
      throwError $ Err.TypeMismatch (T.Record $ Map.singleton (show p) $ T.var "a") r


lit :: Ch.Lit -> Infer T.Type
lit l =
  case l of
    Ch.String _ ->
      return T.string

    Ch.Int _ ->
      return T.int

    Ch.Float _ ->
      return T.float

    Ch.Bool _ ->
      return T.bool
