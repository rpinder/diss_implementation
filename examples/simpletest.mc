fn main : list int =
  case 1 :: 2 :: nil of
  | 3 :: 4 :: nil
  | \y. \ys. ys
