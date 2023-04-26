(* fn callfunc a b : int -> int -> int =
  if a < 1 then b + 1
  else callfunc (a - 1) (callfunc 0 (b - 1))

fn main : int =
  callfunc 1 2 

fn main : int =
  let box res <- (let box x17 <- (callfunc (box (1))) in (x17 (box (2)))) in res

fn callfunc : box int -> box int -> int =
  ((\x2. (let box a <- x2 in (box ((\x3. (let box b <- x3 in (let box x4 <- (box ((a < 1))) in (if x4 then (box ((b + 1))) else (let box x5 <- (callfunc (box ((a - 1)))) in (x5 (let box x6 <- (callfunc (box (0))) in (x6 (box ((b - 1))))))))))))))))

*)

fn main : int =
  let box res <- (let box x17 <- (callfunc (box (1))) in (x17 (box (2)))) in res

fn callfunc : box int -> box int -> int =
  ((\x2. (let box a <- x2 in
  (box ((\x3. (let box b <- x3 in
  (let box x4 <- (box ((a == 0))) in
  (if x4 then
    (box ((b + 1)))
   else
    (let box x5 <- (callfunc (box ((a - 1)))) in
    (x5 (let box x6 <- (callfunc (box (0))) in (x6 (box ((b - 1))))))))))))))))

