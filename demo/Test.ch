module Test runs (main)


from Prelude import (add)
from MyMod   import (greet)



console : { log : a -> String }


toString : a -> String
toString x = x


($) : (a -> b) -> a -> b
($) f x = f x



main =
    console.log $ toString (10 + 5 / 2)
