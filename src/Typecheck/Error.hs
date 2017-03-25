module Typecheck.Error where

import Type as T


data Error
  = TypeMismatch T.Type T.Type
  | InfiniteType T.Var T.Type
  | UnificationMismatch [T.Type] [T.Type]
  | AmbiguousType T.Scheme
  | UnboundVariable String
  deriving (Show)
