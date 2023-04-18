fn main : int =
  fib 10


fn fib x : int -> int =
  if x < 2 then x else
  fib (x - 1) + fib (x - 2)
