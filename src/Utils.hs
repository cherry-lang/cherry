module Utils where

import Data.List
import Data.List.Split


(|>) :: a -> (a -> b) -> b
x |> f = f x


resolveImportPath :: String -> String -> String
resolveImportPath name import' =
  let
    np = splitOn "/" name
    ip = splitOn "/" import'

    part (nh:[]) ipt = concat $ intersperse "/" ipt
    part (nh:nt) (ih:it)
      | nh == ih  = part nt it
      | otherwise = "../" ++ (part nt (ih:it))

    path = (part np ip) ++ ".ch"
  in
    if head path == '.'
       then path
       else "./" ++ path
