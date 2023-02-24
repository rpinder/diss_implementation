let rec fix = \f. \x.
  let y = f x in
  if x == y then
    x
  else
    fix (f y
in
fix
#let rec fib = \bn. let box n <- bn in
#  if n < 2 then
#    n
#  else
#    let box f1 <- box (fib (box (n-1))) in
#    let box f2 <- box (fib (box (n-2))) in
#    f1 + f2
#in fib (box 5)
