fn main : int =
  fact 1 1

fn fact n x : int -> int -> int =
  if x < 1 then
    1
  else
    (n + x) * fact (n - 1) (x - 1)
