module Parse.Type where

import           Data.Char
import qualified Data.Map        as Map
import           Text.Megaparsec (try, (<|>))
import qualified Text.Megaparsec as P

import qualified Parse.Lexer     as L
import           Parse.Parse
import qualified Syntax          as Ch
import qualified Type            as T


-- UTILS


toArrow :: [T.Type] -> T.Type
toArrow []     = undefined
toArrow (x:[]) = x
toArrow (x:xs) = T.Arrow x (toArrow xs)


toType :: String -> T.Type
toType type'@(h:_) =
  if isLower h
     then T.var type'
     else T.Con type'


depth :: T.Type -> Int
depth (T.Arrow _ arr@T.Arrow{}) = 1 + depth arr
depth _                         = 0


-- PARSE


typeAnnDecl :: Parser Ch.Declaration
typeAnnDecl = do
  pos         <- L.pos
  (name, ann) <- typeAnn
  return $ Ch.TypeAnn pos (name, depth ann) ann


typeAnn :: Parser (String, T.Type)
typeAnn = do
  name <- P.choice [L.parens L.infixOp, L.ident]
  L.colon
  ann  <- arrow
  return (name, ann)


type' :: Parser T.Type
type' = try record
    <|> try (L.parens arrow)
    <|> try tvar
    <|> try con


tvar :: Parser T.Type
tvar = L.lexeme $ (:) <$> P.lowerChar <*> (P.many P.alphaNumChar) >>= return . T.var


con :: Parser T.Type
con = L.lexeme $ (:) <$> P.upperChar <*> (P.many P.alphaNumChar) >>= return . T.var


record :: Parser T.Type
record = L.braces $ do
  props <- typeAnn `P.sepBy` L.comma
  return $ T.Record $ Map.fromList props


arrow :: Parser T.Type
arrow = toArrow <$> type' `P.sepBy` L.arrowr
