open Base

type info =
  { line_number : int
  ; column_number : int
  }

module Typ = struct
  type t =
    | Arr of t * t
    | Int
    | Bool

  (* TODO get [@@deriving eq] to work *)
  let rec equal t1 t2 =
    match (t1, t2) with
    | (Int, Int) -> true
    | (Bool, Bool) -> true
    | (Arr (x11, x12), Arr (x21, x22)) -> equal x11 x21 && equal x12 x22
    | _ -> false

  let rec to_string = function
    | Int -> "int"
    | Bool -> "bool"
    | Arr (t1, t2) ->
       let t1_s = to_string t1 in
       let t2_s = to_string t2 in
       Printf.sprintf "%s -> %s" t1_s t2_s
end

module Terms = struct
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
  and op =
    | Plus
    | Minus
    | Multiply

  let equal t1 t2 =
    match (t1, t2) with
    | (Int (_, x), Int (_, y)) -> x = y
    | (Bool (_, x), Bool (_, y)) -> Bool.(=) x y
    | _ -> false

  let rec to_string = function
    | Var (_, name) -> name
    | Int (_, x) -> Printf.sprintf "%d" x
    | Bool (_, x) -> Printf.sprintf "%b" x
    | Abs (_, name, ty, body) ->
       Printf.sprintf "(\\%s : %s. %s)" name (Typ.to_string ty) (to_string body)
    | App (_, t1, t2) ->
       let t1_s = to_string t1 in
       let t2_s = to_string t2 in
       Printf.sprintf "(%s %s)" t1_s t2_s
    | Let (_, s, t1, t2) ->
       Printf.sprintf "(let %s = %s in %s)" s (to_string t1) (to_string t2)
    | LetRec (_, s, typ, t1,t2) -> Printf.sprintf "(let %s : %s = %s in %s)" s (Typ.to_string typ) (to_string t1) (to_string t2)
    | Cls (abs, _) -> to_string abs
    | If (_, pred, t1, t2) ->
       Printf.sprintf "(if %s then %s else %s)" (to_string pred) (to_string t1) (to_string t2)
    | BinOp (_, t1, t2, _) -> Printf.sprintf "(%s <op> %s)" (to_string t1) (to_string t2)
    | Eq (_, t1, t2) -> Printf.sprintf "(%s == %s)" (to_string t1) (to_string t2)
end

(*let binop f t1 t2 =
  match (t1, t2) with
  | (Terms.Nat (fi, t1i), Terms.Nat (_, t2i)) -> Terms.Nat (fi, f t1i t2i)
  | _ -> failwith "BINOP EXPECTS TWO NATS"

(* DO SOMETHING WITH THIS *)
let builtins =
  let funs = [
      ("plus", (binop (+), Typ.Arr (Typ.Nat, Typ.Arr (Typ.Nat, Typ.Nat))));
    ]
  in
  match Hashtbl.of_alist (module String) funs with
  | `Ok x -> x
  | _ -> failwith "Problem with builtins"
 *) 

let eval_operator op =
  match op with
  | Terms.Plus -> (+)
  | Terms.Minus -> (-)
  | Terms.Multiply -> ( * )

let rec eval env t =
  match t with
  | Terms.Var (_, name) -> (match Environment.get env name with
                     | Some x -> x
                     | _ -> failwith ("IMPLEMENT A RESOLVER " ^ "CAN'T FIND " ^ name))
  | Terms.Int (_,_) as x -> x
  | Terms.Bool (_,_) as x -> x
  | Terms.Abs (_, _, _, _) as x -> Terms.Cls (x, env)
  | Terms.Cls (_, _) as x -> x
  | Terms.App (_, Terms.Cls (Terms.Abs (_, param, _, body), closure), arg) ->
     let arg' = eval env arg in
     let env' = Environment.createWithEnclosing closure in
     Environment.define env' param arg';
     eval env' body
  | Terms.App (fi, t1, t2) ->
     let t1 = eval env t1 in
     let t2 = eval env t2 in
     eval env (Terms.App (fi, t1, t2))
  | Terms.Let (_, name, body, rest) ->
     let body' = eval env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval env' rest
  | Terms.LetRec (_, name, _, body, rest) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body;
     let body' = eval env' body in
     Environment.define env' name body';
     eval env' rest
  | Terms.If (fi, predicate, true_term, false_term) ->
     (match predicate with
     | Terms.Bool (_, b) ->
        eval env (if b then true_term else false_term)
     | pred ->
        let pred' = eval env pred in
        eval env (Terms.If (fi, pred', true_term, false_term)))
  | Terms.BinOp (_, t1, t2, op) ->
     let t1' = eval env t1 in
     let t2' = eval env t2 in
     let f = eval_operator op in
     (match (t1', t2') with
     | (Terms.Int (fi, x), Terms.Int (_, y)) -> Terms.Int (fi, f x y)
     | _ -> failwith "Error")
  | Terms.Eq (fi, t1, t2) ->
     let t1' = eval env t1 in
     let t2' = eval env t2 in
     if Terms.equal t1' t2' then
       Terms.Bool (fi, true)
     else
       Terms.Bool (fi, false)

     

let rec typeof env t =
  match t with
  | Terms.Var (_, name) -> (match Environment.get env name with
                            | Some x -> x
                            | _ -> failwith ("IMPLEMENT A RESOLVER! " ^ "CAN'T FIND " ^ name))
  | Terms.Int (_, _) -> Typ.Int
  | Terms.Bool (_, _) -> Typ.Bool
  | Terms.Abs (_, arg, arg_type, body) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' arg arg_type;
     let body_type = typeof env' body in
     Typ.Arr (arg_type, body_type)
  | Terms.App (_, term1, term2) ->
     let term1_type = typeof env term1 in
     let term2_type = typeof env term2 in
     (match term1_type with
      | Typ.Arr (term1_arg_type, term1_body_type) when Typ.equal term2_type term1_arg_type ->
         term1_body_type
      | _ -> failwith "(include info stuff) Type Error") 
  | Terms.Let (_, s, term1, term2) ->
     let bodytype = typeof env term1 in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' s bodytype;
     typeof env' term2
  | Terms.LetRec (_, s, arg_type, term1, term2) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' s  arg_type;
     let bodytype = typeof env' term1 in
     if not (Typ.equal bodytype arg_type) then
       failwith "Type Error, bodytype not equal arg_type"
     else 
       typeof env' term2
  | Terms.Cls (abs, _) -> typeof env abs
  | Terms.If (_, predicate, t1, t2) ->
     let pred_type = typeof env predicate in
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if not (Typ.equal pred_type Typ.Bool) then
       failwith "predicate must be of type Bool"
     else
       if not (Typ.equal t1_type t2_type) then
         failwith "operands should be same type"
       else
         t1_type
  | Terms.BinOp (_, t1, t2, _) ->
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if not (Typ.equal t1_type t2_type && Typ.equal t1_type Typ.Int) then
       failwith "TypeError"
     else
       Typ.Int
  | Terms.Eq (_, t1, t2) ->
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if Typ.equal t1_type t2_type then
       Typ.Bool
     else
       failwith "TypeError"

(* Test things - TODO add real tests*)

let empty_info = { line_number = 0; column_number = 0 }

(*let term_id = Terms.Abs (empty_info, "x", Typ.Nat, Terms.Var (empty_info, "x"), Environment.create ())
let term_xyx = Terms.Abs (empty_info, "x", Typ.Nat, Terms.Abs (empty_info, "y", Typ.Nat, Terms.Var (empty_info, "x"), Environment.create ()), Environment.create ())
let term1 = Terms.App (empty_info, term_id, Terms.Nat (empty_info, 5))
let term2 = Terms.App (empty_info, term_id, term_id)*)
