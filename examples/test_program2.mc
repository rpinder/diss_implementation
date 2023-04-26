fn main : int =
  let a = 5 in
  let b = a + 7 in
  let c = 5 * a in
  a + b + c

(*

Translated

let box a <- box 5 in
let box b <- (let box x8 <- box a in (let box x9 <- box 7 in box (x8 + x9))) in
let box c <- (let box x6 <- box 5 in (let box x7 <- box a in box (x6 * x7)) in
let box x2 <- (let box x4 <- box a in (let box x5 <- box b in box (x4 + x5))) in
let box x3 <- box c in
box (x2 + x3)


FINAL

let box b <- box (5 + 7) in
let box c <- box (5 * 5) in
let box x2 <- box (5 + b) in
box (x2 + c)

*)
