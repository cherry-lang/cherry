module Syntax.Module where

import           Syntax.Declaration
import           Syntax.Expression
import           Typecheck.Environment


data Module phase
  = Module
  { name :: String
  , path :: FilePath
  , info :: phase
  }
  deriving (Show)


data Source
  = Source
  { srcExport :: [String]
  , srcRuns   :: [Expr]
  , srcDecls  :: [Declaration]
  }


data Info
  = Info
  { exports  :: [String]
  , runs     :: [Expr]
  , decls    :: [Declaration]
  , fixities :: [Infix]
  , typeEnv  :: Environment
  }
