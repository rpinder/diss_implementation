fn ackermann m n : int -> int -> int =
  if m < 1 then n + 1
  else if n < 1 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

fn main : int =
  ackermann 3 3
