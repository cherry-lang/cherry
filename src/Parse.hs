module Parse where

import           Control.Monad.State
import           Text.Megaparsec     (parseErrorPretty, runParserT)

import qualified Parse.Module        as P
import qualified Parse.Parse         as P
import qualified Syntax              as Ch


type Filename = String


parse :: Filename -> String -> Either String (Ch.Module Ch.Source)
parse filename src =
  case evalState (runParserT P.module' filename src) P.emptyParserState of
    Left err ->
      Left $ parseErrorPretty err

    Right mod' ->
      Right mod'
