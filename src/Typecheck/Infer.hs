{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}

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
import           Utils                 ((|>))


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


uni :: Ch.Pos -> T.Type -> T.Type -> Infer ()
uni pos t1 t2 = tell [(pos, t1, t2)]


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer T.Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  let l = letters !! count s
  return $ T.var l


varInEnv :: (String, T.Scheme) -> Infer a -> Infer a
varInEnv (name, scheme) m = do
  let scope env = extend (name, scheme) (remove name env)
  local scope m


aliasInEnv :: (String, T.Alias) -> Infer a -> Infer a
aliasInEnv (name, a) m = do
  let scope env = alias name a env
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
    fv (T.Term _ vars) = []
    fv (T.Arrow t1 t2) = fv t1 ++ fv t2
    fv (T.Record ts)   = Map.elems ts |> map fv |> concat

    normtype (T.Record ts)   = T.Record $ Map.map normtype ts
    normtype (T.Arrow t1 t2) = T.Arrow (normtype t1) (normtype t2)
    normtype (T.Term t vars) = T.Term t $ Set.map normtype vars
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
module' m@Ch.Module { Ch.decls } = do
  env <- topDecls decls
  return $ m { Ch.typeEnv = env }


imports :: [Ch.Assign] -> Infer a -> Infer a
imports [] m             = m
imports (Ch.Plain h:t) m = do
  tv <- fresh
  varInEnv (h, T.Forall [] tv) (imports t m)


topDecls :: [Ch.Declaration] -> Infer Environment
topDecls [] = ask
topDecls (d:ds) =
  case d of
    Ch.TypeAlias pos name alias@(T.Alias _ type') -> do
      checkType pos type'
      aliasInEnv (name, alias) $ topDecls ds

    Ch.TypeAnn pos (name, _) type' -> do
      t <- checkType pos type'
      varInEnv (name, T.Forall (Set.toList $ ftv t) t) $ topDecls ds

    Ch.Func pos (name, _) params exprs -> do
      let rest  = init exprs
      let last' = last exprs

      def <- lookupEnv name

      case def of
        Nothing -> do
          env   <- ask
          type' <- param params $ do
            mapM_ expr rest
            expr last'
          varInEnv (name, generalize env type') $ topDecls ds

        Just t -> do
          type' <- func (T.arrowToList t) params $ do
            mapM_ expr rest
            expr last'

          uni pos t type'
          topDecls ds

    _ ->
      local id $ topDecls ds


checkType :: Ch.Pos -> T.Type -> Infer T.Type
checkType pos t =
  case t of
    T.Var{} ->
      return t

    arr@T.Arrow{} -> do
      ts <- mapM (checkType pos) $ T.arrowToList arr
      return $ T.toArrow ts

    T.Record rec ->
      mapM (checkType pos) rec
        >>= return . T.Record

    con@T.Term{} -> do
      env <- ask
      case resolveTypeAlias con env of
        Nothing ->
          throwError $ Err.UndefinedType pos con

        Just t' ->
          return t'


func :: [T.Type] -> [String] -> Infer T.Type -> Infer T.Type
func _ [] m          = m
func (t:ts) (p:ps) m =
  varInEnv (p, T.Forall [] t) $ func ts ps m
    >>= return . T.Arrow t



param :: [String] -> Infer T.Type -> Infer T.Type
param [] m     = m
param (p:ps) m =
  fresh
    >>= \tv -> varInEnv (p, T.Forall [] tv) $ param ps m
    >>= return . T.Arrow tv


expr :: Ch.Expr -> Infer T.Type
expr e =
  case e of
    Ch.Var pos var -> do
      mt <- lookupEnv var
      case mt of
        Just t  -> return t
        Nothing -> throwError $ Err.UnboundVariable pos var

    Ch.Prop pos (var:ps) -> do
      mt <- lookupEnv var
      case mt of
        Just t  -> record pos var ps t
        Nothing -> throwError $ Err.UnboundVariable pos var

    Ch.Lit _ lit' ->
      lit lit'

    Ch.App pos e1 e2 -> do
      t1 <- expr e1
      t2 <- expr e2
      tv <- fresh
      uni pos t1 (t2 `T.Arrow` tv)
      return tv

    Ch.Record pos props -> do
      props' <- mapM (\(k, v) -> expr v >>= return . (k,)) $ Map.toList props
      return $ T.Record $ Map.fromList props'


record :: Ch.Pos -> String -> [String] -> T.Type -> Infer T.Type
record _ _ [] _         = fail "Can't access record without a prop."
record pos var (p:ps) r =
  case r of
    T.Record props ->
      case Map.lookup p props of
        Just re@T.Record{} ->
          record pos var ps re

        Just t ->
          return t

        Nothing ->
          throwError $ Err.UnboundProperty pos var p

    T.Var _ -> do
      tv <- fresh
      uni pos r (T.Record $ Map.singleton (show p) tv)
      return tv

    _ ->
      throwError $ Err.TypeMismatch pos r (T.Record $ Map.singleton (show p) $ T.var "a")


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
