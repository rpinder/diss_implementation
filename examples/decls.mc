fn normal_fib n =
  if n < 2 then n else
  normal_fib (n - 1) + normal_fib (n - 2)
  
fn fib bn =
  let box n <- bn in
  if n < 2 then n else
  if n < 20 then normal_fib n else
  let box f1 <- (box (fib (box (n - 1)))) in
  let box f2 <- (box (fib (box (n - 2)))) in
  (f1 + f2)

fn main =
   fib (box 40)
