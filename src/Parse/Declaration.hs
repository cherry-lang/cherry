module Parse.Declaration where

import           Text.Megaparsec  (try, (<|>))
import qualified Text.Megaparsec  as P

import           Parse.Expression (string)
import qualified Parse.Expression as P
import qualified Parse.Lexer      as L
import           Parse.Parse
import qualified Parse.Type       as P
import qualified Syntax           as Ch


decl :: Parser Ch.Declaration
decl = try func
   <|> try importjs
   <|> try P.typeAnnDecl


func :: Parser Ch.Declaration
func = L.refIndent >> do
  pos    <- L.pos
  name   <- P.choice [infixName, L.ident]
  params <- P.many L.ident
  L.equals *> L.scn
  exprs  <- P.some (L.indented *> P.expr <* L.scn)
  return $ Ch.Func pos (name, length params) params exprs
  where
    infixName = L.parens L.infixOp


importjs :: Parser Ch.Declaration
importjs = do
  pos <- L.pos
  L.rWord "fromjs"
  (Ch.String src) <- string
  L.rWord "import"
  imports <- L.parens $ L.ident `P.sepBy` L.comma
  return $ Ch.ImportJs pos src $ map Ch.Plain imports
