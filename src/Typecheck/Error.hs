module Typecheck.Error where

import Type as T


data Error
  = TypeMismatch T.Type T.Type
  | InfiniteType T.Scheme
  | AmbiguousType T.Scheme
  | UnboundVariable String
  deriving (Show)
