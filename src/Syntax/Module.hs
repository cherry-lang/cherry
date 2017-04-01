{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Syntax.Module where

import qualified Data.Map              as Map

import           Syntax.Declaration
import           Syntax.Expression
import           Typecheck.Environment


data Module
  = Module
  { name     :: String
  , path     :: FilePath
  , exports  :: [String]
  , imports  :: [Import]
  , runs     :: [Expr]
  , decls    :: [Declaration]
  , fixities :: [Infix]
  , typeEnv  :: Environment
  }
  deriving (Show)


data Import
  = Import SourceType String [Assign]
  deriving (Show)


data SourceType
  = Js
  | Cherry
  deriving (Eq, Show)


data Assign
  = DefaultAs String
  | As String String
  | Plain String
  deriving (Show)


data Interface
  = Interface
  { _name     :: String
  , _fixities :: [Infix]
  , _typeEnv  :: Environment
  }
  deriving (Show)


emptyModule :: Module
emptyModule = Module
  { name     = ""
  , path     = ""
  , exports  = []
  , imports  = []
  , runs     = []
  , decls    = []
  , fixities = []
  , typeEnv  = emptyEnv
  }


moduleToInterface :: Module -> Interface
moduleToInterface (Module { name, typeEnv, fixities, exports }) =
  let
    filterEnv env =
      env { vars = Map.filterWithKey (\k _ -> elem k exports) $ vars env }

    filterFixities fixs =
      filter (\(Infix _ _ op) -> elem op exports) fixs
  in
    Interface
      { _name = name
      , _fixities = filterFixities fixities
      , _typeEnv = filterEnv typeEnv
      }
