module Parse.Declaration where

import           Text.Megaparsec  (try, (<|>))
import qualified Text.Megaparsec  as P

import qualified Parse.Expression as P
import qualified Parse.Lexer      as L
import           Parse.Parse      (Parser, addInfix)
import qualified Parse.Type       as P
import qualified Syntax           as Ch


decl :: Parser Ch.Declaration
decl = try func
   <|> try P.typeAnnDecl
   <|> try infixSpec


func :: Parser Ch.Declaration
func = L.refIndent >> do
  pos    <- L.pos
  name   <- P.choice [infixName, L.ident]
  params <- P.many L.ident
  L.equals *> L.scn
  exprs  <- P.some (L.indented *> P.expr <* L.scn)
  return $ Ch.Func pos (name, length params) params exprs
  where
    infixName = do
      op <- L.parens L.infixOp
      addInfix $ Ch.Infix Ch.L 9 op
      return op


infixSpec :: Parser Ch.Declaration
infixSpec = do
  assoc <- P.choice
    [ L.rWord "infixl" >> return Ch.L
    , L.rWord "infixr" >> return Ch.R
    , L.rWord "infix"  >> return Ch.N
    ]

  prec <- L.integer >>= return . fromIntegral
  op   <- L.infixOp

  let infix' = Ch.Infix assoc prec op

  addInfix infix'
  return $ Ch.InfixSpec infix'
