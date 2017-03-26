module Main where

import           System.Environment

import           Codegen.Javascript        (codegen)
import           Codegen.Javascript.Pretty (prettyPrint)
import           Parse
import Typecheck


main :: IO ()
main = getArgs >>= readFile . head >>= putStr . gen


gen :: String -> String
gen ctn =
  case parse "" ctn of
    Left err  -> show err
    Right ast -> case typecheck ast of
      Left err  -> show err
      Right ast -> show ast -- (prettyPrint . codegen) ast
