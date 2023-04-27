fn main : int =
  (let box x17 <- (func (box (5))) in (x17 (box (3))))

fn func : a =
  \x1. let box a <- x1 in
  box (\x2. let box b <- x2 in
  let box x3 <- box (a == 0) in
  if x3 then
    box (b + 1)
  else
    let box x4 <- ((\x. let box u <- x in u) (box (a - 1))) in
    box x4)
