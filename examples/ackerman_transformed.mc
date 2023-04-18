fn main : box int =
  (let box f <- (ackermann2 (box (1))) in (f (box (0))))

fn ackermann : a =
let box actual <- (box ((\x. (let box a <- x in
  (box ((\x. (let box b <- x in
    (let box cond <- (box ((a < 1))) in
    (if cond then
      (box ((b + 1)))
    else
      (let box cond <- (box ((b < 1))) in
      (if cond then
        (let box f <- (ackermann (box ((a - 1)))) in
        (f (box (1))))
      else
        (box (5)))))))))))))) in actual

fn ackermann2 : a =
let box actual <- (box ((\x. (let box a <- x in
  (box ((\x. (let box b <- x in
    (let box cond <- (box ((a < 1))) in
    (if cond then
      (box ((b + 1)))
    else
      (let box cond <- (box ((true))) in
      (if cond then
        (let box f <- (ackermann2 (box ((a - 1)))) in
        (f (box (1))))
      else
        (box (5)))))))))))))) in actual

(*

non transformed
(let box f <- (ackermann (box (1))) in (f (box (0))))
(1 < 1)
false, going down -> (if (b < 1) then ((ackermann (a - 1)) 1) else 5)
(0 < 1)
true, going down -> ((ackermann (a - 1)) 1)
(1 - 1)
(0 < 1)
true, going down -> (b + 1)
(1 + 1)
2

transformed
(let box f <- (ackermann (box (1))) in (f (box (0))))
(1 < 1)
false, going down -> (let box cond <- (box ((b < 1))) in (if cond then (let box f <- (ackermann (box ((<0> - 1)))) in (f (box (1)))) else (box (5))))
(0 < 1)
true, going down -> (let box f <- (ackermann (box ((<0> - 1)))) in (f (box (1))))
(1 - 1)
(0 < 1)
(0 < 1) --- These two lines are duplicates of previous?????????
(1 - 1) ---
true, going down -> (box ((<9> + 1)))
(0 + 1)
1
*)
