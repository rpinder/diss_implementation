open Base

type t =
  | Var of info * string
  | Int of info * int
  | Bool of info * bool
  | Abs of info * string * t
  | App of info * t * t
  | Let of info * string * t * t
  | LetRec of info * string * t * t
  | Cls of t * t Environment.t
  | If of info * t * t * t
  | BinOp of info * t * t * op
  | Eq of info * t * t
  | NEq of info * t * t
  | LetBox of info * string * t * t
  | Box of info * t
and op =
  | Plus
  | Minus
  | Multiply
  | LessThan
  | GreaterThan
  | LTEQ
  | GTEQ
and info =
  { line_number : int
  ; column_number : int
  }
let equal t1 t2 =
  match (t1, t2) with
  | (Int (_, x), Int (_, y)) -> x = y
  | (Bool (_, x), Bool (_, y)) -> Bool.(=) x y
  | _ -> false

let rec to_string = function
  | Var (_, name) -> name
  | Int (_, x) -> Printf.sprintf "%d" x
  | Bool (_, x) -> Printf.sprintf "%b" x
  | Abs (_, name, body) ->
     Printf.sprintf "(\\%s. %s)" name (to_string body)
  | App (_, t1, t2) ->
     let t1_s = to_string t1 in
     let t2_s = to_string t2 in
     Printf.sprintf "(%s %s)" t1_s t2_s
  | Let (_, s, t1, t2) ->
     Printf.sprintf "(let %s = %s in %s)" s (to_string t1) (to_string t2)
  | LetRec (_, s, t1,t2) -> Printf.sprintf "(let %s = %s in %s)" s (to_string t1) (to_string t2)
  | Cls (abs, _) -> to_string abs
  | If (_, pred, t1, t2) ->
     Printf.sprintf "(if %s then %s else %s)" (to_string pred) (to_string t1) (to_string t2)
  | BinOp (_, t1, t2, _) -> Printf.sprintf "(%s <op> %s)" (to_string t1) (to_string t2)
  | Eq (_, t1, t2) -> Printf.sprintf "(%s == %s)" (to_string t1) (to_string t2)
  | NEq (_, t1, t2) -> Printf.sprintf "(%s != %s)" (to_string t1) (to_string t2)
  | LetBox (_, s, t1, t2) -> Printf.sprintf "(let box %s = %s in %s)" s (to_string t1) (to_string t2)
  | Box (_, t1) -> Printf.sprintf "box (%s)" (to_string t1)

let empty_info = { line_number = 0; column_number = 0 }
