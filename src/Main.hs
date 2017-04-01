module Main where

import           System.Environment

import           Codegen.Javascript        (codegen)
import           Codegen.Javascript.Pretty (prettyPrint)
import           Dependency
import           Parse
import qualified Parse.Parse               as P
import           Typecheck


main :: IO ()
main = getArgs >>= return . head >>= \fp -> do
  interfaces <- deps fp
  src        <- readFile fp

  case parse fp src interfaces P.emptyParserState of
    Left err ->
      fail err

    Right m ->
      case typecheck interfaces m of
        Left err ->
          fail $ show err

        Right m' ->
          putStrLn $ prettyPrint $ codegen m'
