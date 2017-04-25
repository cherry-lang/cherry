module Syntax.Declaration where

import           Syntax.Expression
import           Syntax.Position
import qualified Type              as T


type FuncId = (String, Int)


data Declaration
  = Func Pos FuncId [String] [Expr]
  | TypeAnn Pos FuncId T.Type
  | TypeAlias Pos String T.Type
  | Const Pos Name Expr
  | InfixSpec Infix
  deriving (Show)


data Infix
  = Infix Assoc Int String
  deriving (Ord, Eq, Show)


data Assoc = L | N | R
  deriving (Ord, Eq, Show)

