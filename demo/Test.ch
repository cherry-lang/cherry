module Test runs (main)

from Prelude import ((+), (/), add, divide)
from MyMod   import (greet)


console : { log : a -> String }


toString : a -> String
toString x = x


($) : (a -> b) -> a -> b
($) f x = f x

infixl 5 $

main =
    console.log
        "Hello"

    console.log
        "Hello"
