fn normal_fib n : int -> int =
  if n < 2 then n else
  normal_fib (n - 1) + normal_fib (n - 2)
  
fn fib bn : int -> int =
  let n = bn in
  if n < 20 then normal_fib n else
  let f1 = (fib ((n - 1))) in
  let f2 = (fib ((n - 2))) in
  f1 + f2

fn main : int =
  fib (35)
