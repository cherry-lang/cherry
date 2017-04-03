module Test runs (main)

from Prelude import ((+), (/), add, divide)
from MyMod   import (greet)


console : { log : a -> String }


toString : a -> String
toString x = x


($) : (a -> b) -> a -> b
($) f x = f x


infixl 5 $


addItTogether : Int -> Int
addItTogether x = x + 10


main =
  console.log $ toString True

  console.log
    "Hello2"
