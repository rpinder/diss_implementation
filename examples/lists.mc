fn rapp f x : box (a -> b) -> box a -> box b =
  let box g <- f in
  let box y <- x in
  box (g y)

fn pmap f xs : box (a -> b) -> list (box a) -> list b =
  case xs of
  | nil
  | \y. \ys.
    let box u <- rapp f y in
    let rest = pmap f ys in
    u :: rest

fn main : list int =
  let bxs = box 3 :: box 4 :: box 5 :: nil in
  pmap (box (\x. x * 2)) bxs
