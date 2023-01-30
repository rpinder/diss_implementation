open Base

type info =
  { line_number : int
  ; column_number : int
  }

module Typ = struct
  type t =
    | Arr of t * t
    | Nat

  (* TODO get [@@deriving eq] to work *)
  let rec equal t1 t2 =
    match (t1, t2) with
    | (Nat, Nat) -> true
    | (Arr (x11, x12), Arr (x21, x22)) -> equal x11 x21 && equal x12 x22
    | _ -> false

  let rec to_string = function
    | Nat -> "Nat"
    | Arr (t1, t2) ->
       let t1_s = to_string t1 in
       let t2_s = to_string t2 in
       Printf.sprintf "%s -> %s" t1_s t2_s
end

module Terms = struct
  type t =
    | Var of info * string
    | Nat of info * int
    | Abs of info * string * Typ.t * t * t Environment.t
    | App of info * t * t
    | Let of info * string * t * t

  let rec to_string = function
    | Var (_, name) -> name
    | Nat (_, x) -> Printf.sprintf "%d" x
    | Abs (_, _, _, _, _) -> "<fun>"
    | App (_, t1, t2) ->
       let t1_s = to_string t1 in
       let t2_s = to_string t2 in
       Printf.sprintf "(%s) %s" t1_s t2_s
    | Let (_, s, t1, t2) ->
       Printf.sprintf "let %s = %s in %s" s (to_string t1) (to_string t2)
end

let binop f t1 t2 =
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

let rec eval env t =
  match t with
  | Terms.Var (_, name) -> (match Environment.get env name with
                     | Some x -> x
                     | _ -> failwith "IMPLEMENT A RESOLVER")
  | Terms.Nat (_,_) as x -> x
  | Terms.Abs (_, _, _, _, _) as x -> x
  | Terms.App (_, Abs (_, param, _, body, env), arg) ->
     let arg' = eval env arg in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' param arg';
     eval env' body
  | Terms.Let (_, name, body, rest) ->
     let body' = eval env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval env' rest
  | _ -> failwith "Shouldn't be possible"

let rec typeof env t =
  match t with
  | Terms.Var (_, name) -> (match Environment.get env name with
                            | Some x -> x
                            | _ -> failwith "IMPLEMENT A RESOLVER!")
  | Terms.Nat (_, _) -> Typ.Nat
  | Terms.Abs (_, arg, arg_type, body, _) ->
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
     
(* Test things - TODO add real tests*)

let empty_info = { line_number = 0; column_number = 0 }

let term_id = Terms.Abs (empty_info, "x", Typ.Nat, Terms.Var (empty_info, "x"), Environment.create ())
let term1 = Terms.App (empty_info, term_id, Terms.Nat (empty_info, 5))
let term2 = Terms.App (empty_info, term_id, term_id)
