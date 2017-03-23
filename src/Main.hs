module Main where

import System.Environment

import Parse
import Codegen.Javascript.Codegen (codegen)
import Codegen.Javascript.Pretty (prettyPrint)


main :: IO ()
main = getArgs >>= readFile . head >>= putStr . gen


gen :: String -> String
gen ctn =
  case parse "" ctn of
    Left err -> show err
    Right ast -> (prettyPrint . codegen) ast
