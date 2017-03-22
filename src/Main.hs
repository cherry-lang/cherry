module Main where

import System.Environment

import Parse
import Codegen.Javascript.Codegen (codegen)


main :: IO ()
main = getArgs >>= readFile . head >>= putStr . gen


gen :: String -> String
gen ctn =
  case parse "" ctn of
    Left err -> show err
    Right ast -> (show . codegen) ast
