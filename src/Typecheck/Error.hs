module Typecheck.Error where

import Type as T


data Error
  = TypeMismatch { got :: T.Type, expected :: T.Type }
  | InfiniteType T.Var T.Type
  | UnificationMismatch [T.Type] [T.Type]
  | AmbiguousType T.Scheme
  | UnboundVariable String
  | UnboundProperty { record :: String, property :: String }
  deriving (Show)
