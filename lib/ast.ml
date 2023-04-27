open Base

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
  | OBox of t * int
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
let equal t1 t2 =
  match (t1, t2) with
  | (Int x, Int y) -> x = y
  | (Bool x, Bool y) -> Bool.(=) x y
  | _ -> false

let op_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | LessThan -> "<"
  | GreaterThan -> ">"
  | LTEQ -> "<="
  | GTEQ -> ">="

let rec to_string = function
  | Var name -> name
  | MVar name -> "<" ^ Int.to_string name ^ ">"
  | Int x -> Printf.sprintf "%d" x
  | Bool x -> Printf.sprintf "%b" x
  | Abs (name, body) ->
     Printf.sprintf "(\\%s. %s)" name (to_string body)
  | App (t1, t2) ->
     let t1_s = to_string t1 in
     let t2_s = to_string t2 in
     Printf.sprintf "(%s %s)" t1_s t2_s
  | Let (s, t1, t2) ->
     Printf.sprintf "(let %s = %s in\n%s)" s (to_string t1) (to_string t2)
  | Cls (abs, _) -> to_string abs
  | If (pred, t1, t2) ->
     Printf.sprintf "(if %s then %s else %s)" (to_string pred) (to_string t1) (to_string t2)
  | BinOp (t1, t2, op) -> Printf.sprintf "(%s %s %s)" (to_string t1) (op_to_string op) (to_string t2)
  | Eq (t1, t2) -> Printf.sprintf "(%s == %s)" (to_string t1) (to_string t2)
  | NEq (t1, t2) -> Printf.sprintf "(%s != %s)" (to_string t1) (to_string t2)
  | LetBox (s, t1, t2) -> Printf.sprintf "(let box %s <- %s in\n%s)" s (to_string t1) (to_string t2)
  | Box (t1) -> Printf.sprintf "(box (%s))" (to_string t1)
  | OBox (t1, c) -> Printf.sprintf "obox<%s> (%s)" (Int.to_string c) (to_string t1)
  | Nil -> "nil"
  | Cons (t1, t2) -> Printf.sprintf "%s :: %s" (to_string t1) (to_string t2)
  (* | Cons _ as c -> Printf.sprintf "[%s]" (list_to_string (to_ocaml_list c)) *)
  | Case (t1, t2, t3) -> Printf.sprintf "(case %s of | %s | %s)" (to_string t1) (to_string t2) (to_string t3)
  | NilP -> "NilP"
  | ConsP (t1, t2) -> Printf.sprintf "%s :P %s" (to_string t1) (to_string t2)
  | CaseP (t1, t2, t3) -> Printf.sprintf "(caseP %s of | %s | %s)" (to_string t1) (to_string t2) (to_string t3)
and info_to_string = function
  | IVar (_, name) -> name
  | IInt (_, x) -> Printf.sprintf "%d" x
  | IBool (_, x) -> Printf.sprintf "%b" x
  | IAbs (_, name, body) ->
     Printf.sprintf "(\\%s. %s)" name (info_to_string body)
  | IApp (_, t1, t2) ->
     let t1_s = info_to_string t1 in
     let t2_s = info_to_string t2 in
     Printf.sprintf "(%s %s)" t1_s t2_s
  | ILet (_, s, t1, t2) ->
     Printf.sprintf "(let %s = %s in\n%s)" s (info_to_string t1) (info_to_string t2)
  | IIf (_, pred, t1, t2) ->
     Printf.sprintf "(if %s then %s else %s)" (info_to_string pred) (info_to_string t1) (info_to_string t2)
  | IBinOp (_, t1, t2, op) -> Printf.sprintf "(%s %s %s)" (info_to_string t1) (op_to_string op)(info_to_string t2)
  | IEq (_, t1, t2) -> Printf.sprintf "(%s == %s)" (info_to_string t1) (info_to_string t2)
  | INEq (_, t1, t2) -> Printf.sprintf "(%s != %s)" (info_to_string t1) (info_to_string t2)
  | ILetBox (_, s, t1, t2) -> Printf.sprintf "(let box %s <- %s in\n%s)" s (info_to_string t1) (info_to_string t2)
  | IBox (_, t1) -> Printf.sprintf "box (%s)" (info_to_string t1)
  | INil _ -> "nil"
  | ICons _ as c -> Printf.sprintf "[%s]" (list_to_string (to_ocaml_list (convert c)))
  | ICase (_, t1, t2, t3) -> Printf.sprintf "(case %s of | %s | %s)" (info_to_string t1) (info_to_string t2) (info_to_string t3)
and list_to_string xs =
  match xs with
  | y :: [] -> Printf.sprintf "%s" (to_string y)
  | y :: ys -> Printf.sprintf "%s, %s" (to_string y) (list_to_string ys)
  | [] -> ""
and to_ocaml_list xs =
  match xs with
  | Cons (t1, t2) -> t1 :: to_ocaml_list t2
  | Nil -> []
  | _ -> failwith "TO_OCAML_LIST"
and convert (term : infoAST) : t =
  match term with
  | IVar (_, s) -> Var s
  | IInt (_, i) -> Int i
  | IBool (_, b) -> Bool b
  | IAbs (_, s, t) -> Abs (s, convert t)
  | IApp (_, t1, t2) -> App (convert t1, convert t2)
  | ILet (_, s, t1, t2) -> Let (s, convert t1, convert t2)
  | IIf (_, t1, t2, t3) -> If (convert t1, convert t2, convert t3)
  | IBinOp (_, t1, t2, op) -> BinOp (convert t1, convert t2, op)
  | IEq (_, t1, t2) -> Eq (convert t1, convert t2)
  | INEq (_, t1, t2) -> NEq (convert t1, convert t2)
  | ILetBox (_, s, t1, t2) -> LetBox (s, convert t1, convert t2)
  | IBox (_, t) -> Box (convert t)
  | INil _ -> Nil
  | ICons (_, t1, t2) -> Cons (convert t1, convert t2)
  | ICase (_, t1, t2, t3) -> Case (convert t1, convert t2, convert t3)
and to_single_threaded (term : infoAST) : t =
  match term with
  | IVar (_, s) -> Var s
  | IInt (_, i) -> Int i
  | IBool (_, b) -> Bool b
  | IAbs (_, s, t) -> Abs (s, to_single_threaded t)
  | IApp (_, t1, t2) -> App (to_single_threaded t1, to_single_threaded t2)
  | ILet (_, s, t1, t2) -> Let (s, to_single_threaded t1, to_single_threaded t2)
  | IIf (_, t1, t2, t3) -> If (to_single_threaded t1, to_single_threaded t2, to_single_threaded t3)
  | IBinOp (_, t1, t2, op) -> BinOp (to_single_threaded t1, to_single_threaded t2, op)
  | IEq (_, t1, t2) -> Eq (to_single_threaded t1, to_single_threaded t2)
  | INEq (_, t1, t2) -> NEq (to_single_threaded t1, to_single_threaded t2)
  | ILetBox (_, s, t1, t2) -> Let (s, to_single_threaded t1, to_single_threaded t2)
  | IBox (_, t) -> to_single_threaded t
  | INil _ -> Nil
  | ICons (_, t1, t2) -> Cons (to_single_threaded t1, to_single_threaded t2)
  | ICase (_, t1, t2, t3) -> Case (to_single_threaded t1, to_single_threaded t2, to_single_threaded t3)




let empty_info = { line_number = 0; column_number = 0 }
