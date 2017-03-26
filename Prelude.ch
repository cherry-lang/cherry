module Prelude exports
  ( (+), add
  , (-), minus
  , (*), multiply
  , (/), divide
  , (^), exponentiate
  , (%), modulos
  , (++), concat
  )

fromjs "./Prelude" import *


add : a -> a -> a
(+) = add


minus : a -> a -> a
(-) = minus


multiply : a -> a -> a
(*) = multiply


divide : a -> a -> a
(/) = divide


concat : [a] -> [a] -> [a]
(++) = concat


equals : a -> a -> Bool
(==) = equals


and : Bool -> Bool -> Bool
(&&) = and


not : Bool -> Bool
not True  = False
not False = True


id : a -> a
id x = x
