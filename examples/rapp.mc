let rapp = \f. \x.
  let box g <- f in
  let box y <- x in
  box (g y)
in
rapp (\x. x)
