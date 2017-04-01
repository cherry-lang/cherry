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


filterEnv :: [String] -> Environment -> Environment
filterEnv exports env =
  env { vars = Map.filterWithKey (\k _ -> elem k exports) $ vars env }


filterFixities :: [String] -> [Infix] -> [Infix]
filterFixities exports fixs =
  filter (\(Infix _ _ op) -> elem op exports) fixs


moduleToInterface :: Module -> Interface
moduleToInterface (Module { name, typeEnv, fixities, exports }) =
  Interface
    { _name = name
    , _fixities = filterFixities exports fixities
    , _typeEnv = filterEnv exports typeEnv
    }


importFromInterface :: Interface -> Module -> Interface
importFromInterface inf m =
  let
    (Interface name infixes env) =
      inf

    imports' =
      filter (\(Import _ name' _) -> name == name') $ imports m

    importAssigns (Import _ _ assigns) =
      map (\(Plain name') -> name') assigns

    imports0 =
      foldl (++) [] $ map importAssigns imports'
  in
    Interface
      { _name = name
      , _fixities = filterFixities imports0 infixes
      , _typeEnv  = filterEnv imports0 env
      }














