module Parse.Module where

import           Text.Megaparsec     (try, (<|>))
import qualified Text.Megaparsec     as P

import qualified Parse.Declaration   as P
import qualified Parse.Lexer         as L
import           Parse.Parse
import qualified Syntax              as Ch


module' :: [Ch.Interface] -> Parser Ch.Module
module' interfaces = do
  header'  <- header
  mapM_ (integrate header') interfaces
  decls    <- P.many (L.scn *> P.decl <* L.scn)
  infixes' <- infixList
  P.eof
  return $ header'
    { Ch.decls = decls
    , Ch.fixities = infixes'
    }


integrate :: Ch.Module -> Ch.Interface -> Parser ()
integrate _ interface = mapM_ addInfix $ Ch._fixities interface


header :: Parser Ch.Module
header = do
  L.rWord "module"
  name'    <- L.identWith "/"
  exports' <- P.option [] exports
  runs'    <- P.option [] runs
  L.scn
  imports' <- P.many (L.scn *> imports <* L.scn)
  return $ Ch.emptyModule
    { Ch.name    = name'
    , Ch.exports = exports'
    , Ch.runs    = runs'
    , Ch.imports = imports'
    }


exports :: Parser [String]
exports =
  L.rWord "exports" *> L.parens (importName `P.sepBy` L.comma)


runs :: Parser [Ch.Expr]
runs = do
  L.rWord "runs"
  rs <- L.parens (run `P.sepBy` L.comma)
  return rs
  where
    run = do
      pos <- L.pos
      fun <- L.ident
      return $ Ch.App pos (Ch.Var pos fun) (Ch.Lit pos Ch.Void)


imports :: Parser Ch.Import
imports = do
  srcType <- importSrcType
  import' <- try (L.doubleQuotes L.string) <|> L.identWith "/"
  L.rWord "import"
  imports' <- L.parens $ importName `P.sepBy` L.comma
  return $ Ch.Import srcType import' $ map Ch.Plain imports'


importName :: Parser String
importName =
  let
    op  = L.parens L.infixOp
    id' = L.ident
  in
    try op <|> id'


importSrcType :: Parser Ch.SourceType
importSrcType =
  P.choice
    [ L.rWord "fromjs" >> return Ch.Js
    , L.rWord "from" >> return Ch.Cherry
    ]
