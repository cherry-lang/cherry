module Typecheck.Error where

import Syntax.Position
import Type as T


data Error
  = TypeMismatch { pos :: Pos, got :: T.Type, expected :: T.Type }
  | InfiniteType T.Var T.Type
  | UnificationMismatch [T.Type] [T.Type]
  | AmbiguousType T.Scheme
  | UnboundVariable Pos String
  | UnboundProperty { record :: String, property :: String }
  deriving (Show)
