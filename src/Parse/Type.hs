module Parse.Type where

import           Data.Char
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           Text.Megaparsec (try, (<|>))
import qualified Text.Megaparsec as P

import qualified Parse.Lexer     as L
import           Parse.Parse
import qualified Syntax          as Ch
import qualified Type            as T


-- UTILS


toType :: String -> T.Type
toType type'@(h:_) =
  if isLower h
     then T.var type'
     else T.Term type' []


depth :: T.Type -> Int
depth (T.Arrow _ arr@T.Arrow{}) = 1 + depth arr
depth _                         = 0


-- PARSE


typeAlias :: Parser Ch.Declaration
typeAlias = do
  pos <- L.pos
  L.rWord "type"
  L.rWord "alias"
  (T.Term n tvars) <- term
  L.sym "="
  t <- type'
  return $ Ch.TypeAlias pos n $ T.Alias (map (\(T.Var var) -> var) tvars) t


typeAnnDecl :: Parser Ch.Declaration
typeAnnDecl = do
  pos         <- L.pos
  (name, ann) <- typeAnn
  return $ Ch.TypeAnn pos (name, depth ann) ann


typeAnn :: Parser (String, T.Type)
typeAnn = do
  name <- P.choice [L.parens L.infixOp, L.ident]
  L.sym "::"
  ann  <- arrow
  return (name, ann)


type' :: Parser T.Type
type' = try record
    <|> try (L.parens arrow)
    <|> try tvar
    <|> try term


tvar :: Parser T.Type
tvar = L.leadingLowerCase >>= return . T.var


cons :: Parser T.Type
cons = L.leadingUpperCase >>= \t -> return $ T.Term t []


term :: Parser T.Type
term = do
  con    <- L.leadingUpperCase
  params <- P.many (try cons <|> tvar)
  return $ T.Term con params


record :: Parser T.Type
record = L.braces $ do
  props <- typeAnn `P.sepBy` L.comma
  return $ T.Record $ Map.fromList props


arrow :: Parser T.Type
arrow = T.toArrow <$> type' `P.sepBy` L.arrowr
