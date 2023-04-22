fn main : int =
  sum (map (\x. x * 2) (1 :: 2 :: nil))

fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys

fn fib x : int -> int =
  if x < 2 then x else
  fib (x - 1) + fib (x - 2)
