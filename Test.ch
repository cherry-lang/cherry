module Test runs (main)

fromjs "./prelude" import (add, minus, divide)



console : { log : String -> String }
minus   : a -> a -> a

greet : String -> Int -> String
greet name asdf =
  name


main =
    console.log (greet "Hello there" (minus 10 5))
