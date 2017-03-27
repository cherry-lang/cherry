module Test runs (main)

fromjs "./Prelude" import (add, minus, divide)


console : { log : String -> String }
minus   : a -> a -> a
add     : a -> a -> a


greet : String -> Int -> String
greet name asdf = name


main =
    console.log (greet (add "Hello there" "!") (minus 10 5))
