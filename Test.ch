module Test runs (main)


greet : String -> Int -> String
greet name asdf =
  name

console : { log : String -> String }


main = console.log (greet "asdf" 123)
