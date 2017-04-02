module Parse where

import           Control.Monad.State
import           Text.Megaparsec      (Token, ParseError, Dec, parseErrorPretty, runParserT)

import qualified Parse.Module         as P
import qualified Parse.Parse          as P
import qualified Syntax               as Ch


type ParseErr = ParseError (Token String) Dec


parse'
  :: FilePath
  -> String
  -> P.Parser a
  -> P.ParserState
  -> Either ParseErr a
parse' fp src p ps =
  case evalState (runParserT p fp src) ps of
    Left err ->
      Left err

    Right m ->
      Right m


parse
  :: FilePath
  -> String
  -> [Ch.Interface]
  -> P.ParserState
  -> Either ParseErr Ch.Module
parse fp src interfaces ps =
  parse' fp src (P.module' interfaces) ps


parseHeader
  :: FilePath
  -> String
  -> Either ParseErr Ch.Module
parseHeader fp src =
  parse' fp src P.header P.emptyParserState
