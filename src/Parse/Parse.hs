module Parse.Parse where

import           Control.Monad.Identity
import           Control.Monad.State
import           Text.Megaparsec        (Dec, ParsecT, Pos)
import           Text.Megaparsec.Pos    (unsafePos)
import qualified Text.Megaparsec.Lexer as L


data ParserState
  = ParserState { indentLevel :: Pos }


type Parser = ParsecT Dec String (StateT ParserState Identity)


emptyParserState :: ParserState
emptyParserState = ParserState $ unsafePos 0
