fn callfunc a b : int -> int -> int =
  if a < 1 then b + 1
  else callfunc (a - 1) (callfunc 0 (b - 1))

fn main : int =
  callfunc 1 2
