module Main where

import qualified Data.Aeson                as Aeson
import           System.Environment
import           System.Exit

import           Codegen.Javascript        (codegen)
import           Codegen.Javascript.Pretty (prettyPrint)
import           Dependency
import           Error
import           Parse
import qualified Parse.Parse               as P
import           Typecheck


main :: IO ()
main = getArgs >>= return . head >>= \fp -> do
  interfaces <- deps fp
  src        <- readFile fp

  case parse fp src interfaces P.emptyParserState of
    Left err -> do
      toFriendlyParseError err >>= putStrLn . show
      exitFailure

    Right m ->
      case typecheck interfaces m of
        Left err -> do
          toFriendlyTypeError err >>= putStrLn . show
          exitFailure

        Right m' -> do
          putStrLn $ prettyPrint $ codegen m'
