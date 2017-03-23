module Main where

import           System.Environment

import           Codegen.Javascript        (codegen)
import           Codegen.Javascript.Pretty (prettyPrint)
import           Parse


main :: IO ()
main = getArgs >>= readFile . head >>= putStr . gen


gen :: String -> String
gen ctn =
  case parse "" ctn of
    Left err -> show err
    Right ast -> (prettyPrint . codegen) ast
