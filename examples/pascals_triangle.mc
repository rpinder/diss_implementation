fn area_old r h : int -> int -> int =
  2 * pi * r * h + 2 * pi * r * r

fn area r h : int -> int -> int =
  let v0 = 2 * pi * r * h in
  let v1 = 2 * pi * r * r in
  v0 + v1
 
fn pi : int = 3

fn main : int = area 10 5

fn apmap f xs : (a -> b) -> list a -> list b =
  let box xs <- lift xs
  case xs of
  | nil
  | \y. \ys.
    let v0 = f y in
    let v1 = apmap f ys in
    v0 :: v1

fn apmap2 f xs : (a -> b) -> list a -> list b =
  let box xs <- lift xs
  case xs of
  | nil
  | \y. \ys.
    let box y <- lift y in
    let box ys <- lift ys in
    let box v0 <- f y in
    let box v1 <- apmap2 f ys in
    v0 :: v1
