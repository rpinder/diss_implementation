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

fn fib n : int -> int =
  if n < 2 then n else
  fib (n - 1) + fib (n - 2)

fn main : list int =
  let bxs = box 30 :: box 31 :: box 32 :: box 29 :: nil in
  pmap (box fib) bxs

