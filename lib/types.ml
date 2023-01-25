type info =
  { line_number : int
  ; column_number : string
  }

type typ =
  | Arr of typ * typ
  | Nat

type term =
  | Var of info * string
  | Abs of info * string * typ * term * term Environment.t
  | App of info * term * term
