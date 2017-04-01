module Typecheck where

import qualified Syntax                as Ch
import           Typecheck.Environment (emptyEnv, unionEnvs)
import           Typecheck.Error
import           Typecheck.Infer       (infer)
import           Typecheck.Solve       (solve)


typecheck :: [Ch.Interface] -> Ch.Module -> Either Error Ch.Module
typecheck interfaces m =
  let
    env =
      unionEnvs $ map Ch._typeEnv interfaces
  in
    case infer env m of
      Left err ->
        Left err

      Right (m', constraints) ->
        case solve constraints of
          Left err ->
            Left err

          Right _  ->
            Right m'
