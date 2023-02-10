module Typ : sig
  type t =
    | Arr of t * t
    | Int
    | Bool

  val equal : t -> t -> bool

  val to_string : t -> string
end

type t =
  | Var of info * string
  | Int of info * int
  | Bool of info * bool
  | Abs of info * string * Typ.t * t
  | App of info * t * t
  | Let of info * string * t * t
  | LetRec of info * string * Typ.t * t * t
  | Cls of t * t Environment.t
  | If of info * t * t * t
  | BinOp of info * t * t * op
  | Eq of info * t * t
  | NEq of info * t * t
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

val equal : t -> t -> bool

