module Test runs (main)


greet : String -> Int -> String
greet name asdf =
  name

console : { log : String -> String, m : { id : Int -> Int, mount : Bool -> Bool } }

main = console.m.mount 123.012 
