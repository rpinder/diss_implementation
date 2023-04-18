fn main : list int =
  (* map (\x. x) (1 :: 2 :: 3 :: nil) *)
  case 1 :: nil of
  | 0
  | \y. \ys. 7

fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys
