module Test runs (main)

fromjs "./Prelude" import (add, minus, divide)


console : { log : String -> String }
minus   : a -> a -> a


infix 1 (-) = minus



greet : String -> Int -> String
greet name asdf = name



main =
    console.log (greet "Hello there" (minus 10 5))
