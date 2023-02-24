fn main =
  let box n <- box 3 in
  let box g <- box (n + 1) in
  const g 3

fn const x y = x
