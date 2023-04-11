fn gcd a b : int -> int -> int =
  if a == b then
    a
  else
    if a > b then
      gcd (a - b) b
    else
      gcd a (b - a)

fn main : int =
  gcd 36 27
