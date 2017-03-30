module Syntax.Expression where

import Syntax.Position


type Name = String


data Expr
  = Var Pos Name
  | Prop Pos [Name]
  | Lit Pos Lit
  | App Pos Expr Expr
  | Lambda [Name] Expr
  deriving (Show)


data Lit
  = Bool Bool
  | String String
  | Int Int
  | Float Double
  | Void
  deriving (Show)
