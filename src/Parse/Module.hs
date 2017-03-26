module Parse.Module where

import           Text.Megaparsec   (try)
import qualified Text.Megaparsec   as P

import qualified Parse.Declaration as P
import qualified Parse.Lexer       as L
import           Parse.Parse
import qualified Syntax            as Ch


module' :: Parser Ch.Module
module' = do
  L.rWord "module"
  name     <- L.ident
  exports' <- P.option [] exports
  runs'    <- P.option [] runs
  decls    <- P.many (L.scn *> P.decl <* L.scn)
  P.eof
  return $ Ch.Module name exports' runs' decls


exports :: Parser [Ch.Export]
exports =
  L.rWord "exports" *> L.parens (L.ident `P.sepBy` L.comma)


runs :: Parser [Ch.Runs]
runs = do
  L.rWord "runs"
  rs <- L.parens (run `P.sepBy` L.comma)
  return rs
  where
    run = do
      pos <- L.pos
      fun <- L.ident
      return $ Ch.App pos (Ch.Var pos fun) (Ch.Lit pos Ch.Void)
