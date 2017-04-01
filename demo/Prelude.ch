module Prelude exports ((/), (+), add)

fromjs "./Js/Prelude" import (add, minus, divide, length)


add    : a -> a -> a
divide : a -> a -> a


(+) : a -> a -> a
(+) x y = add x y


(/) : a -> a -> a
(/) x y = divide x y


infixl 8 +
infixl 9 /
