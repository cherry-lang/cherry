module Test runs (main)

from Prelude import ((+), (/), add, divide)
from MyMod   import (greet)


console : { log : a -> String }


toString : a -> String
toString x = x


($) : (a -> b) -> a -> b
($) f x = f x


infixl 5 $


toRecord : String -> { str : String, gro : Int }
toRecord x =
  { str = x + "!"
  , gro = 123
  }


fromRecord : { str : String } -> String
fromRecord x = x.str


main =
  console.log (fromRecord (toRecord "Hello"))
