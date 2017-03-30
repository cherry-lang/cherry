module Syntax.Declaration where

import           Syntax.Expression
import           Syntax.Position
import qualified Type              as T


type FuncId = (String, Int)


data ImportAssign
  = DefaultAs Name
  | As Name Name
  | Plain Name
  deriving (Show)


data Declaration
  = Func Pos FuncId [String] [Expr]
  | TypeAnn Pos FuncId T.Type
  | Const Pos Name Expr
  | Import Pos Name [ImportAssign]
  | ImportJs Pos Name [ImportAssign]
  | Inf Infix
  deriving (Show)


data Infix
  = Infix Assoc Int String
  deriving (Show)


data Assoc = L | N | R
  deriving (Show)

