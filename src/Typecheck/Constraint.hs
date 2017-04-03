module Typecheck.Constraint where

import           Syntax.Position
import qualified Type            as T


type Constraint = (Pos, T.Type, T.Type)
