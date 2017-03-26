module Parse.Declaration where

import           Data.Char
import           Text.Megaparsec  ((<|>), try)
import qualified Text.Megaparsec  as P

import qualified Parse.Expression as P
import qualified Parse.Lexer      as L
import           Parse.Parse
import qualified Syntax           as Ch
import qualified Type             as T


decl :: Parser Ch.Declaration
decl = try func
   <|> try typeAnn


typeAnn :: Parser Ch.Declaration
typeAnn = do
  pos  <- L.pos
  name <- L.ident
  L.colon
  parts <- P.sepBy L.ident L.arrowr
  return $ Ch.TypeAnn pos (name, length parts) $ toArrow parts


toArrow :: [String] -> T.Type
toArrow []     = undefined
toArrow (x:[]) = toType x
toArrow (x:xs) = T.Arrow (toType x) (toArrow xs)


toType :: String -> T.Type
toType type'@(h:_) =
  if isLower h
     then T.var type'
     else T.Con type'



func :: Parser Ch.Declaration
func = L.refIndent >> do
  pos    <- L.pos
  name   <- L.ident
  params <- P.many L.ident
  L.equals *> L.scn
  exprs  <- P.some (L.indented *> P.expr <* L.scn)
  return $ Ch.Func pos (name, length params) params exprs
