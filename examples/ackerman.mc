fn ackermann a b : int -> int -> int =
  if a < 1 then b + 1
  else if b < 1 then ackermann (a - 1) 1
  else ackermann (a - 1) (ackermann a (b - 1)) 

fn main : int =
  ackermann 3 2
