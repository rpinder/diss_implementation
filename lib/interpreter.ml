open Base

let eval_operator op fi =
  let createint x = Ast.Int (fi, x) in
  let createbool x = Ast.Bool (fi, x) in
  fun x y ->
  match op with
  | Ast.Plus -> createint (x + y)
  | Ast.Minus -> createint (x - y)
  | Ast.Multiply -> createint (x * y)
  | Ast.LessThan -> createbool (x < y)
  | Ast.GreaterThan -> createbool (x > y)
  | Ast.LTEQ -> createbool (x <= y)
  | Ast.GTEQ -> createbool (x >= y)

let rec eval env t =
  match t with
  | Ast.Var (_, name) -> (match Environment.get env name with
                     | Some x -> x
                     | _ -> failwith ("IMPLEMENT A RESOLVER " ^ "CAN'T FIND " ^ name))
  | Ast.Int (_,_) as x -> x
  | Ast.Bool (_,_) as x -> x
  | Ast.Abs (_, _, _, _) as x -> Ast.Cls (x, env)
  | Ast.Cls (_, _) as x -> x
  | Ast.App (_, Ast.Cls (Ast.Abs (_, param, _, body), closure), arg) ->
     let arg' = eval env arg in
     let env' = Environment.createWithEnclosing closure in
     Environment.define env' param arg';
     eval env' body
  | Ast.App (fi, t1, t2) ->
     let t1 = eval env t1 in
     let t2 = eval env t2 in
     eval env (Ast.App (fi, t1, t2))
  | Ast.Let (_, name, body, rest) ->
     let body' = eval env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval env' rest
  | Ast.LetRec (_, name, _, body, rest) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body;
     let body' = eval env' body in
     Environment.define env' name body';
     eval env' rest
  | Ast.If (fi, predicate, true_term, false_term) ->
     (match predicate with
     | Ast.Bool (_, b) ->
        eval env (if b then true_term else false_term)
     | pred ->
        let pred' = eval env pred in
        eval env (Ast.If (fi, pred', true_term, false_term)))
  | Ast.BinOp (_, t1, t2, op) ->
     let t1' = eval env t1 in
     let t2' = eval env t2 in
     (match (t1', t2') with
     | (Ast.Int (fi, x), Ast.Int (_, y)) -> eval_operator op fi x y
     | _ -> failwith "Error")
  | Ast.Eq (fi, t1, t2) ->
     let t1' = eval env t1 in
     let t2' = eval env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, true)
     else
       Ast.Bool (fi, false)
  | Ast.NEq (fi, t1, t2) ->
     let t1' = eval env t1 in
     let t2' = eval env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, false)
     else
       Ast.Bool (fi, true)

     

let rec typeof env t =
  match t with
  | Ast.Var (_, name) -> (match Environment.get env name with
                            | Some x -> x
                            | _ -> failwith ("IMPLEMENT A RESOLVER! " ^ "CAN'T FIND " ^ name))
  | Ast.Int (_, _) -> Ast.Typ.Int
  | Ast.Bool (_, _) -> Ast.Typ.Bool
  | Ast.Abs (_, arg, arg_type, body) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' arg arg_type;
     let body_type = typeof env' body in
     Ast.Typ.Arr (arg_type, body_type)
  | Ast.App (_, term1, term2) ->
     let term1_type = typeof env term1 in
     let term2_type = typeof env term2 in
     (match term1_type with
      | Ast.Typ.Arr (term1_arg_type, term1_body_type) when Ast.Typ.equal term2_type term1_arg_type ->
         term1_body_type
      | _ -> failwith "(include info stuff) Ast.Type Error") 
  | Ast.Let (_, s, term1, term2) ->
     let bodytype = typeof env term1 in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' s bodytype;
     typeof env' term2
  | Ast.LetRec (_, s, arg_type, term1, term2) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' s  arg_type;
     let bodytype = typeof env' term1 in
     if not (Ast.Typ.equal bodytype arg_type) then
       failwith "Ast.Type Error, bodytype not equal arg_type"
     else 
       typeof env' term2
  | Ast.Cls (abs, _) -> typeof env abs
  | Ast.If (_, predicate, t1, t2) ->
     let pred_type = typeof env predicate in
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if not (Ast.Typ.equal pred_type Ast.Typ.Bool) then
       failwith "predicate must be of type Bool"
     else
       if not (Ast.Typ.equal t1_type t2_type) then
         failwith "operands should be same type"
       else
         t1_type
  | Ast.BinOp (_, t1, t2, _) ->
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if not (Ast.Typ.equal t1_type t2_type && Ast.Typ.equal t1_type Ast.Typ.Int) then
       failwith "Ast.TypeError"
     else
       Ast.Typ.Int
  | Ast.Eq (_, t1, t2) ->
     let t1_type = typeof env t1 in
     let t2_type = typeof env t2 in
     if Ast.Typ.equal t1_type t2_type then
       Ast.Typ.Bool
     else
       failwith "Ast.TypeError"
  | Ast.NEq (fi, t1, t2) -> typeof env (Ast.Eq (fi, t1, t2))

(* Test things - TODO add real tests*)

(*let term_id = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ())
let term_xyx = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Abs (empty_info, "y", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ()), Environment.create ())
let term1 = Ast.App (empty_info, term_id, Ast.Nat (empty_info, 5))
let term2 = Ast.App (empty_info, term_id, term_id)*)
