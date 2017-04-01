module Test runs (main)

fromjs "./Prelude" import (add, minus, divide, length)

from MyMod import (greet)


console : { log : a -> String }


add    : a -> a -> a
minus  : a -> a -> a
divide : a -> a -> a
length : a -> Int

toString : a -> String
toString x = x


(+) : a -> a -> a 
(+) x y = add x y

(/) : a -> a -> a
(/) x y = divide x y

infixl 8 +

main =
  console.log (toString (10 + 5 / 2))
