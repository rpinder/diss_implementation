type info =
  { line_number : int
  ; column_number : int
  }

type typ =
  | Arr of typ * typ
  | Nat

type term =
  | Var of info * string
  | Nat of info * int
  | Abs of info * string * typ * term * term Environment.t
  | App of info * term * term

let rec eval env t =
  match t with
  | Var (_, name) -> (match Environment.get env name with
                     | Some x -> x
                     | _ -> failwith "IMPLEMENT A RESOLVER")
  | Nat (_,_) as x -> x
  | Abs (_, _, _, _, _) as x -> x
  | App (_, Abs (_, param, _, body, env), arg) ->
     let arg' = eval env arg in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' param arg';
     eval env' body
  | _ -> failwith "Shouldn't be possible"
     
(* Test things - TODO add real tests*)

let empty_info = { line_number = 0; column_number = 0 }

let term_id = Abs (empty_info, "x", Nat, Var (empty_info, "x"), Environment.create ())
let term1 = App (empty_info, term_id, Nat (empty_info, 5))
let term2 = App (empty_info, term_id, term_id)
