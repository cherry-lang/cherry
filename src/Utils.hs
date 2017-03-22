module Utils where


(|>) :: a -> (a -> b) -> b
x |> f = f x
