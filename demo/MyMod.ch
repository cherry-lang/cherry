module MyMod exports (greet)

from Sub/Mod import (ms)

(+) : a -> a -> a


greet : String -> String
greet name = (ms name) + name