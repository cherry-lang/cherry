module Parse.Declaration where

import           Data.Char
import           Text.Megaparsec  (try, (<|>))
import qualified Text.Megaparsec  as P

import qualified Parse.Expression as P
import qualified Parse.Lexer      as L
import           Parse.Parse
import qualified Parse.Type       as P
import qualified Syntax           as Ch
import qualified Type             as T


decl :: Parser Ch.Declaration
decl = try func
   <|> try P.typeAnnDecl


func :: Parser Ch.Declaration
func = L.refIndent >> do
  pos    <- L.pos
  name   <- L.ident
  params <- P.many L.ident
  L.equals *> L.scn
  exprs  <- P.some (L.indented *> P.expr <* L.scn)
  return $ Ch.Func pos (name, length params) params exprs
