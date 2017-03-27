module Test runs (main)

fromjs "./Prelude" import (add, minus, divide)


console : { log : String -> String }
add     : a -> a -> a


(+) : a -> a -> a 
(+) x y = add x y


(|>) : a -> (a -> b) -> b
(|>) x f = f x


addExclm : String -> String
addExclm str = str + "!"

main =
    "asdf" |> addExclm |> console.log
