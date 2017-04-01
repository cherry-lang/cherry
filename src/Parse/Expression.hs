module Parse.Expression where

import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Text.Megaparsec      (try, (<|>))
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Expr as P

import qualified Parse.Lexer          as L
import           Parse.Parse
import qualified Syntax               as Ch
import           Utils


expr :: Parser Ch.Expr
expr = try app
   <|> try infixApp
   <|> try term


term :: Parser Ch.Expr
term = try $ L.parens expr
   <|> try lit
   <|> try prop
   <|> try var


prop :: Parser Ch.Expr
prop = do
  pos  <- L.pos
  prop <- L.prop
  return $ Ch.Prop pos prop


var :: Parser Ch.Expr
var = (Ch.Var <$> L.pos) <*> L.ident


infixApp :: Parser Ch.Expr
infixApp = opTable >>= \tbl -> P.makeExprParser (try app <|> term) tbl


opTable :: Parser [[P.Operator Parser Ch.Expr]]
opTable = gets infixes >>= \infs ->
  let
    p op = do
      pos <- L.pos
      inf <- L.sym op
      return $ \e e' -> Ch.App pos (Ch.App pos (Ch.Var pos inf) e) e'


    mapInfix (Ch.Infix assoc _ op) =
      case assoc of
        Ch.L ->
          P.InfixL $ p op

        Ch.N ->
          P.InfixN $ p op

        Ch.R ->
          P.InfixR $ p op
  in
    infs
      |> Map.toDescList
      |> map (Set.toList . snd)
      |> map (map mapInfix)
      |> return


app :: Parser Ch.Expr
app = do
  pos  <- L.pos
  func <- term
  args <- P.some term
  return $ foldl (\app arg -> Ch.App pos app arg) func args


lit :: Parser Ch.Expr
lit = Ch.Lit <$> L.pos <*>
     (try string
  <|> try bool
  <|> try float
  <|> try int
  <|> try void')



-- LITERALS


string :: Parser Ch.Lit
string = Ch.String <$> L.doubleQuotes (P.many $ P.noneOf ['"'])


bool :: Parser Ch.Lit
bool = try (L.rWord "True" >> return (Ch.Bool True))
   <|> try (L.rWord "False" >> return (Ch.Bool False))


int :: Parser Ch.Lit
int = Ch.Int . fromIntegral <$> L.integer


float :: Parser Ch.Lit
float = Ch.Float <$> L.float


void' :: Parser Ch.Lit
void' = L.sym "()" >> return Ch.Void
