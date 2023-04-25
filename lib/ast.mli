type t =
  | Var of string
  | MVar of int
  | Int of int
  | Bool of bool
  | Abs of string * t
  | App of t * t
  | Let of string * t * t
  | Cls of t * t Environment.t
  | If of t * t * t
  | BinOp of t * t * op
  | Eq of t * t
  | NEq of t * t
  | LetBox of string * t * t
  | Box of t
  | OBox of t
  | Nil
  | Cons of t * t
  | Case of t * t * t
  | NilP
  | ConsP of t * t
  | CaseP of t * t * t
and infoAST =
  | IVar of info * string
  | IInt of info * int
  | IBool of info * bool
  | IAbs of info * string * infoAST
  | IApp of info * infoAST * infoAST
  | ILet of info * string * infoAST * infoAST
  | IIf of info * infoAST * infoAST * infoAST
  | IBinOp of info * infoAST * infoAST * op
  | IEq of info * infoAST * infoAST
  | INEq of info * infoAST * infoAST
  | ILetBox of info * string * infoAST * infoAST
  | IBox of info * infoAST
  | INil of info
  | ICons of info * infoAST * infoAST
  | ICase of info * infoAST * infoAST * infoAST
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

val empty_info : info

val equal : t -> t -> bool

val to_string : t -> string
val info_to_string : infoAST -> string
val convert : infoAST -> t
val to_single_threaded : infoAST -> t

