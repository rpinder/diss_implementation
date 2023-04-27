fn main : int =
  sum (map fib (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: 24 :: 25 :: 26 :: 27 :: 28 :: 29 :: 30 :: 31 :: 32 :: nil))

(* fn fact x : int -> int =
  if x == 0 then 1 else x * fact (x - 1) *)
 
fn fib x : int -> int =
  if x < 2 then x else fib (x - 1) + fib (x - 2)

fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys 

