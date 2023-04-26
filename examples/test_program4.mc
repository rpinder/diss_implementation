fn main : int =
  func (box 7)

fn func bn : a =
  let box a <- bn in
  let box cond <- box (a < 6) in
  if cond then 4 else 3
