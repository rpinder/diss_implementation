fn ackermann a b : int -> int -> int =
  if a < 1 then b + 1
  else (if (true) then ackermann (a - 1) 1 else 5)

fn main : int =
  ackermann 1 0
