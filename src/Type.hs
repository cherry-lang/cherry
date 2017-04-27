module Type where

import qualified Data.Map as Map


newtype Var
  = TV String
  deriving (Show, Eq, Ord)


data Type
  = Var Var
  | Arrow Type Type
  | Term String [Type]
  | Record (Map.Map String Type)
  deriving (Show, Eq, Ord)


data Scheme
  = Forall [Var] Type
  deriving (Show, Eq, Ord)


data Alias
  = Alias [Var] Type
  deriving (Show)


var :: String -> Type
var = Var . TV


int, float, string, bool :: Type
int    = Term "Int" []
float  = Term "Float" []
string = Term "String" []
bool   = Term "Bool" []


primaryTypes :: [Type]
primaryTypes =
  [ int
  , float
  , string
  , bool
  ]


arrowToList :: Type -> [Type]
arrowToList t =
  case t of
    Arrow t1 (arrow@Arrow{}) ->
      t1 : (arrowToList arrow)

    Arrow t1 t2 ->
      [t1, t2]

    _ ->
      fail "not an arrow"


toArrow :: [Type] -> Type
toArrow []     = undefined
toArrow (x:[]) = x
toArrow (x:xs) = Arrow x (toArrow xs)
