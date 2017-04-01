module Parse.Parse where

import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Text.Megaparsec        (Dec, ParsecT, Pos)
import           Text.Megaparsec.Pos    (unsafePos)

import qualified Syntax                 as Ch


data ParserState
  = ParserState
  { name        :: String
  , indentLevel :: Pos
  , infixes     :: Map.Map Int (Set.Set Ch.Infix)
  }


type Parser = ParsecT Dec String (StateT ParserState Identity)


emptyParserState :: ParserState
emptyParserState = ParserState "" (unsafePos 0) Map.empty


infixEq :: Ch.Infix -> Ch.Infix -> Bool
infixEq (Ch.Infix _ _ op) (Ch.Infix _ _ op') = op == op'


addInfix :: Ch.Infix -> Parser ()
addInfix inf =
  let
    removeInfix i s =
      Set.filter (not . infixEq i) s

    setForPrec prec m =
      Map.findWithDefault Set.empty prec m

    addInfix' i@(Ch.Infix _ prec _) m =
      Map.insert prec (Set.insert i $ setForPrec prec m) $ Map.map (removeInfix i) m
  in
    modify $ \s -> s { infixes = addInfix' inf $ infixes s }
