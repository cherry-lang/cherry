module Syntax where


type Name   = String
type Param  = String
type Export = String
type Runs   = String
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
  | Const Pos Name Expr
  | Import Pos Name [ImportAssign]
  | ImportJs Pos Name [ImportAssign]
  deriving (Show)


data Expr
  = Var Pos Name
  | Lit Pos Lit
  | App Pos Expr Expr
  | Lambda [Param] Expr
  | Arith Op Expr Expr
  deriving (Show)


data Op
  = Add
  | Subtract
  | Mulitply
  | Divide
  | PowerOf
  | Modulos
  deriving (Show)


data Lit
  = Bool Bool
  | String String
  | Int Int
  | Float Double
  | Void
  deriving (Show)
