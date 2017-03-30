module Test runs (main)

fromjs "./Prelude" import (add, minus, divide, length)

from MyMod import (greet)


console : { log : a -> String }


add    : a -> a -> a
minus  : a -> a -> a
divide : a -> a -> a
length : a -> Int

(+) : a -> a -> a 
(+) x y = add x y


greet : String -> String
greet fname = "Hello " + fname


main =
  console.log (greet "Gunnar")
