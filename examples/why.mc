fn main : box int =
  fac (box 2)

fn fac : box int -> box int =
let box res <- box (\xp.
let box x <- xp in
let box cond <- (let box n <- box (1) in box ((x < n))) in
if cond then
  box (1)
else
  let box n <- (fac (let box n <- box (1) in box ((x - n)))) in box ((x * n))) in res

fn fac2 : box int -> box int =
\xp.
let box x <- xp in
let box cond <- (let box n <- box (1) in box ((x < n))) in
if cond then
  box (1)
else
  let box n <- (fac2 (let box n <- box (1) in box ((x - n)))) in box ((x * n))
