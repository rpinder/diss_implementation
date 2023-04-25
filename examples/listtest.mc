fn main : int =
  sum (map fact (1 :: 2 :: 3 :: nil)) 

fn fact x : int -> int =
  if x == 0 then 1 else x * fact (x - 1)
 
fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys 
