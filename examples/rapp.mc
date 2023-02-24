fn rapp f x =
  let box g <- f in
  let box y <- x in
  box (g y)

fn bring x =
  let box u <- x in
  u

fn main =
  bring (rapp (box (\x. (x + 1))) (box 3))
