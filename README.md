# Cherry

---

### Define a module

A module can either export functions.

```cherry
module Test exports (test)
```

Or run functions (which can not take any arguments).

```cherry
module Main runs (main)
```

### Imports

```cherry
from Test import (test)
```

### Type annotation

```cherry
test :: Int -> a -> { k :: Int, v :: a }
```

### Type alias

```cherry
type alias KV = { k :: Int, v :: String }
```

### Functions

```cherry
greet name = "Hello " + name
```

### Infix functions

```cherry
(|>) x f = f x

infixl 7 |>
```

### Records

```cherry
{ str  = "Test"
, int  = 123
, bool = True
}
```
