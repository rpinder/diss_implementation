let rec fib = \x.
  if x == 0 then
    0
  else if x == 1 then
    1
  else if x < 20 then
    fib (x - 1) + fib (x - 2)
  else
    let box y <- box (fib (x - 1)) in
    let box z <- box (fib (x - 2)) in
    y + z
in fib 30
