module Typecheck.Environment where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Type as T


type TypeVar = Map.Map String T.Scheme


data Environment
  = Environment
  { vars    :: TypeVar
  , aliases :: Map.Map String T.Type
  , types   :: Set.Set T.Type
  }
  deriving (Show)


emptyEnv :: Environment
emptyEnv =
  Environment Map.empty Map.empty $ Set.fromList T.primaryTypes


unionEnvs :: [Environment] -> Environment
unionEnvs = foldl u emptyEnv
  where u env env' =
          Environment
          { vars    = Map.union (vars env) (vars env')
          , aliases = Map.union (aliases env) (aliases env')
          , types   = Set.union (types env) (types env')
          }


extend :: (String, T.Scheme) -> Environment -> Environment
extend (name, scheme) env =
  env { vars = Map.insert name scheme $ vars env }


alias :: String -> T.Type -> Environment -> Environment
alias name t env =
  env { aliases = Map.insert name t $ aliases env }


remove :: String -> Environment -> Environment
remove name env = env { vars = Map.delete name $ vars env }


lookupVar :: String -> Environment -> Maybe T.Scheme
lookupVar var env = Map.lookup var $ vars env


lookupType :: T.Type -> Environment -> Maybe T.Type
lookupType t env =
  case t of
    T.Con{} ->
      if Set.member t $ types env
        then Just t
        else Nothing

    _ ->
      Just t


resolveType :: T.Type -> Environment -> Maybe T.Type
resolveType t env =
  case t of
    T.Con name ->
      case Map.lookup name (aliases env) of
        Nothing ->
          lookupType t env

        Just t' ->
          resolveType t' env

    _ ->
      lookupType t env
