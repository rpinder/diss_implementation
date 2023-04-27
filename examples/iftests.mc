fn main : a =
  let box a <- box 8 in
  let box cond <- box (a > 7) in
  if cond then true else false
