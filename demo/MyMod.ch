module MyMod exports (greet, greetNicely, ms)

from Sub/Mod import (ms)
from Prelude import ((+), add)


greet : String -> String
greet name = add (ms name) name


greetNicely : String -> String
greetNicely name = "Greetings Mr. " + name