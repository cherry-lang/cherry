module Syntax where

import qualified Type as T


type Name   = String
type Param  = String
type Export = String
type Runs   = Expr
type FuncId = (String, Int)


data Pos
  = Pos
  { srcFile :: FilePath
  , srcLine :: Int
  , srcCol  :: Int
  }
  deriving (Show)


data Module
  = Module Name [Export] [Runs] [Declaration]
  deriving (Show)


data ImportAssign
  = DefaultAs Name
  | As Name Name
  | Plain Name
  deriving (Show)


data Declaration
  = Func Pos FuncId [Param] [Expr]
  | TypeAnn Pos FuncId T.Type
  | Const Pos Name Expr
  | Import Pos Name [ImportAssign]
  | ImportJs Pos Name [ImportAssign]
  deriving (Show)


data Expr
  = Var Pos Name
  | Prop Pos [Name]
  | Lit Pos Lit
  | App Pos Expr Expr
  | Lambda [Param] Expr
  deriving (Show)


data Lit
  = Bool Bool
  | String String
  | Int Int
  | Float Double
  | Void
  deriving (Show)
