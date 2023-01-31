# diss_implementation

An interpreter for a simple functional language that will eventually support automatic parallelisation

## Syntax Examples

```
let rec fact : int -> int = \x : int.
  if x == 0 then 1 else x * fact (x - 1)
in
fact 5
```

## Todo list
* Declarations
* Lists
* Better typechecker
* Concurrency stuff
