fn main : int =
  fact 5

fn fact x : int -> int =
  if x == 0 then
    1
  else
    x * fact (x - 1)
