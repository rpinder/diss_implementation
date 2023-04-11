fn main : list int =
  map (\x. x) (map (\x. x) (1 :: 2 :: 3 :: nil))

fn sum xs : list int -> int =
  case xs of
  | 0
  | \y. \ys. y + sum ys

fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys

(*

case 3 :: nil of | 4 | \y. \ys. 8

let box xs' <- box (b


nil -> box nil
3 :: nil -> box (box 3 :: nil)
3 :: 4 :: nil -> box 3 :: box 4 :: nil
case xs of | transform this |


function expects list a
but will get list (box a)

list needs to go from

3 :: 4 :: nil to

box (box 3 :: box (box 4 :: box nil)

cons needs to do something special as the list might already be boxed

how can case deal with box (box 3 :: box 4 :: nil)

case xs of | 5 | \y. \ys. 8
let box xs' <- xs in case xs' of | box 5 | \a1'. \a2'. (\y. \ys. 8) a1' (box a2') 

*)
