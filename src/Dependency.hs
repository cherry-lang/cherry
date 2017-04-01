{-# LANGUAGE NamedFieldPuns #-}

module Dependency (deps) where

import Data.Tree
import Control.Monad.State

import qualified Syntax as Ch
import qualified Parse as P
import qualified Parse.Parse as P
import qualified Typecheck as T
import Utils



deps :: FilePath -> IO [Ch.Interface]
deps = buildDepTree >=> solveDeps


parseInterface :: [Ch.Interface] -> FilePath -> IO Ch.Interface
parseInterface interfaces fp = readFile fp >>= \src ->
  case P.parse fp src interfaces P.emptyParserState of
    Left err ->
      fail err

    Right m ->
      case T.typecheck interfaces m of
        Left err ->
          fail $ show err

        Right m' ->
          return $ Ch.moduleToInterface m'


solveDeps :: Tree FilePath -> IO [Ch.Interface]
solveDeps tree =
  levels tree
    |> tail
    |> reverse
    |> foldM (\ifs fps -> mapM (parseInterface ifs) fps) []


buildDepTree :: FilePath -> IO (Tree FilePath)
buildDepTree fp = do
  mod' <- parseModuleHeader fp

  let name' = Ch.name mod'
  let paths = Ch.imports mod' |> filter shouldFollowImport |> map (importToPath name')

  deps' <- mapM buildDepTree paths
  return $ Node fp deps'


parseModuleHeader :: FilePath -> IO Ch.Module
parseModuleHeader fp = do
  src <- readFile fp
  case P.parseHeader fp src of
    Left err ->
      fail err

    Right mod' ->
      return mod'


importToPath :: String -> Ch.Import -> FilePath
importToPath moduleName (Ch.Import _ name _) =
  resolveImportPath moduleName name


shouldFollowImport :: Ch.Import -> Bool
shouldFollowImport (Ch.Import Ch.Js _ _)     = False
shouldFollowImport (Ch.Import Ch.Cherry _ _) = True
