fn main : int =
  let rapp = \f. \x. let box g <- f in let box y <- x in box (g y) in
  let bring = \x. let box u <- x in u in
  bring (rapp (box (\x. (x + 1))) (box 3))
  (* (\x. let box u <- x in u) (rapp (box (\x. (x + 1))) (box 3)) *)
