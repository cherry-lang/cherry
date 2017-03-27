module Test runs (main)

fromjs "./Prelude" import (add, minus, divide)


console : { log : String -> String }
add     : a -> a -> a


(+) : String -> String -> String
(+) x y = add x y


main =
    console.log ("Hello" + "World" + "!")
