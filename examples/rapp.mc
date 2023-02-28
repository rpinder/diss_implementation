fn rapp f x : box (a -> b) -> box a -> box b =
  let box g <- f in
  let box y <- x in
  box (g y)

fn bring x : box a -> a =
  let box u <- x in
  u

fn main : int =
  bring (rapp (box (\x. x + 1)) (box 3))


