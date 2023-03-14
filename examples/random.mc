fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys

fn filter f xs : (a -> bool) -> list a -> list a =
  case xs of
  | nil
  | \y. \ys. if f y then y :: filter f ys else filter f ys

fn reduce f init xs : (a -> b -> b) -> b -> list a -> b =
  case xs of
  | init
  | \y. \ys. f y (reduce f init ys) 

fn main : int =
  reduce (\x. \y. x + y) 0 (3 :: 4 :: 1 :: 2 :: 1 :: nil)
