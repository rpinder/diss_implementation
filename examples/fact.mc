fn main : int =
  fact 15

fn fact x : int -> int =
  if x < 1 then
    1
  else
    x * fact (x - 1)
