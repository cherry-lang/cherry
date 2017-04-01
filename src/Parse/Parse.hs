module Parse.Parse where

import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as Map
import           Text.Megaparsec        (Dec, ParsecT, Pos)
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.Pos    (unsafePos)

import qualified Syntax                 as Ch


data ParserState
  = ParserState
  { name        :: String
  , indentLevel :: Pos
  , infixes     :: Map.Map Int [Ch.Infix]
  }


type Parser = ParsecT Dec String (StateT ParserState Identity)


emptyParserState :: ParserState
emptyParserState = ParserState "" (unsafePos 0) Map.empty
