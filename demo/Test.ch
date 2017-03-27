module Test runs (main)

fromjs "./Prelude" import (add, minus, divide, length)


console : { log : a -> String }
add     : a -> a -> a
length : a -> Int

(+) : a -> a -> a 
(+) x y = add x y


(|>) : a -> (a -> b) -> b
(|>) x f = f x



main =
    "asdf" |> length |> console.log
