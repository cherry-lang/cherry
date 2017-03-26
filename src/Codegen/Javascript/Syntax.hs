module Codegen.Javascript.Syntax where


type Name = String
type Param = String


data Module
  = Module [Statement]
  deriving (Show)


data Statement
  = Expr Expr
  | Func Name [Param] [Statement]
  | Const Name Expr
  | Return Expr
  | Export Name
  | If [(Expr, [Statement])]
  | Skip
  deriving (Show)


data Comp
  = Eq
  deriving (Show)


data Expr
  = Lit Lit
  | Var Name
  | Prop [Name]
  | App Expr [Expr]
  | Lambda [Param] [Statement]
  | Comp Comp Expr Expr
  deriving (Show)


data Lit
  = String String
  | Bool Bool
  | Number Double
  | Void
  deriving (Show)
