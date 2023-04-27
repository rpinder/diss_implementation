fn map f xs : (a -> b) -> list a -> list b =
  case xs of
  | nil
  | \y. \ys. f y :: map f ys

fn filter f xs : (a -> bool) -> list a -> list a =
  case xs of
  | nil
  | \y. \ys. if f y then y :: filter f ys else filter f ys
 

fn list_to x : int -> list int =
  if x == 0 then nil
  else x :: list_to (x - 1)
 

fn reverse xs : list a -> list a =
  reverse_helper xs nil

fn reverse_helper xs acc : list a -> list a -> list a =
  case xs of
  | acc
  | \y. \ys. reverse_helper ys (y :: acc)

fn sum xs : list int -> int = 
  case xs of
  | 0
  | \y. \ys. y + sum ys 

fn main : int =
  sum (map (\x. x * x) (filter (\x. x > 2) (reverse (list_to 200)))) 
