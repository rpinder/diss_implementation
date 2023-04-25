fn main : int =
   sum (map (\x. x * 2) (1 :: 2 :: 3 :: nil))
(*   case 1 :: nil of
   | 5
   | \y. \ys. y *)
 
fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys 

