module Test runs (main)

from Prelude import ((+), (/), add, divide)
from MyMod   import (greet)

type alias Name = String

type alias Record z b = { str :: z, gro :: b }


console :: { log :: a -> String }


toString :: a -> String
toString x = x


($) :: (a -> b) -> a -> b
($) f x = f x


infixl 5 $


toRecord :: String -> Record String Bool
toRecord x =
  { str = x + "!"
  , gro = 123
  , test = True
  }


fromRecord :: { str :: String, gro :: Int } -> String
fromRecord x = x.str


main =
  console.log (fromRecord (toRecord "Hello"))
