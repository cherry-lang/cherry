module Typecheck.Environment where

import qualified Data.Map as Map

import qualified Type as T


type TypeVar = Map.Map String T.Scheme


data Environment
  = Environment { vars :: TypeVar, types :: [T.Type] }


emptyEnv :: Environment
emptyEnv = Environment Map.empty T.primaryTypes


extend :: (String, T.Scheme) -> Environment -> Environment
extend (name, scheme) env =
  env { vars = Map.insert name scheme $ vars env }


remove :: String -> Environment -> Environment
remove name env = env { vars = Map.delete name $ vars env }
