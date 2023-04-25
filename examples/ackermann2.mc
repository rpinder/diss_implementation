fn ackerman ba bb : box int -> box int -> int =
  let box a <- ba in
  let box b <- bb in
  if a < 1 then b + 1
  else if b < 1 then
    ackerman (box (a - 1)) (box 1)
  else
    ackerman (box (a - 1)) (box (ackerman (box a) (box (b - 1))))

fn main : int =
  ackerman (box 3) (box 3)
