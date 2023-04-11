fn main : int =
  5

fn factorial x : int -> int =
  if x < 1 then 1 else x * factorial (x - 1)

