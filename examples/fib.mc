fn main : int =
  fib 30

fn fib n : int -> int =
  if n < 2 then n else fib (n - 1) + fib (n - 2)
