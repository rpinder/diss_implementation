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
end

module Terms = struct
  type t =
    | Var of info * string
    | Nat of info * int
    | Abs of info * string * Typ.t * t * t Environment.t
    | App of info * t * t
end

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
     
(* Test things - TODO add real tests*)

let empty_info = { line_number = 0; column_number = 0 }

let term_id = Terms.Abs (empty_info, "x", Typ.Nat, Terms.Var (empty_info, "x"), Environment.create ())
let term1 = Terms.App (empty_info, term_id, Terms.Nat (empty_info, 5))
let term2 = Terms.App (empty_info, term_id, term_id)
