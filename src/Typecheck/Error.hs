module Typecheck.Error where

import Syntax.Position
import Type as T


data Error
  = TypeMismatch { pos :: Pos, got :: T.Type, expected :: T.Type }
  | InfiniteType T.Var T.Type
  | UnificationMismatch [T.Type] [T.Type]
  | AmbiguousType T.Scheme
  | UndefinedType Pos T.Type
  | UnboundVariable Pos String
  | UnboundProperty { pos :: Pos, record :: String, property :: String }
  deriving (Show)
