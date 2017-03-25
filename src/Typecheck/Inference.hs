module Typecheck.Inference where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.List
import qualified Data.Map              as Map
import qualified Data.Set              as Set

import qualified Syntax                as Ch
import qualified Type                  as T
import           Typecheck.Constraint
import           Typecheck.Environment
import           Typecheck.Error
import           Typecheck.Solve


data InferState
  = InferState { count :: Int }


type Infer a =
  (RWST
    Environment
    [Constraint]
    InferState
    (Except Error)
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


lookupEnv :: String -> Infer T.Type
lookupEnv name = do
  (Environment vars _) <- ask

  case Map.lookup name vars of
    Nothing ->
      throwError $ UnboundVariable name

    Just scheme -> do
      instantiate scheme >>= return


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


infer :: Environment -> Ch.Module -> Either Error (T.Type, [Constraint])
infer env m = runInfer env $ module' m


runInfer :: Environment -> Infer T.Type -> Either Error (T.Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env emptyInfer


module' :: Ch.Module -> Infer T.Type
module' (Ch.Module _ _ _ decls) = topDecls decls


topDecls :: [Ch.Declaration] -> Infer T.Type
topDecls [] = return $ T.var ""
topDecls (d:ds) =
  case d of
    Ch.Func _ (name, _) params exprs -> do
      let rest  = init exprs
      let last' = last exprs

      type' <- param params $ do
        mapM_ expr rest
        expr last' >>= return

      inEnv (name, T.Forall [] type') $ topDecls ds


param :: [String] -> Infer T.Type -> Infer T.Type
param [] m     = m
param (p:[]) m = fresh >>= \tv -> inEnv (p, T.Forall [] tv) m
param (p:ps) m = fresh >>= \tv -> inEnv (p, T.Forall [] tv) $ param ps m

expr :: Ch.Expr -> Infer T.Type
expr e =
  case e of
    Ch.Var _ var ->
      lookupEnv var

    Ch.Lit _ lit' ->
      lit lit'

    Ch.App _ e1 e2 -> do
      t1 <- expr e1
      t2 <- expr e2
      tv <- fresh
      uni t1 (t2 `T.Arrow` tv)
      return tv


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
