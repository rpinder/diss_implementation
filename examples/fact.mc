fn main : int =
  fact 20

fn fact x : int -> int =
  if x < 1 then
    1
  else
    x * fact (x - 1)
