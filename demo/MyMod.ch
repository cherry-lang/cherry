module MyMod exports (greet, ms)

fromjs "./Prelude" import (add)

from Sub/Mod import (ms)

add : a -> a -> a



greet : String -> String
greet name = add (ms name) name