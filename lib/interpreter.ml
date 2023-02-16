open Base

module T = Domainslib.Task

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

let rec eval pool env t =
  match t with
  | Ast.Var (_, name) -> (match Environment.get env name with
                          | Some (Ast.WaitingOn _ as p) -> eval pool env p
                          | Some x -> x
                          | _ -> failwith ("IMPLEMENT A RESOLVER " ^ "CAN'T FIND " ^ name))
  | Ast.Int (_,_) as x -> x
  | Ast.Bool (_,_) as x -> x
  | Ast.Abs (_, _, _) as x -> Ast.Cls (x, env)
  | Ast.Cls (_, _) as x -> x
  | Ast.App (_, Ast.Cls (Ast.Abs (_, param, body), closure), arg) ->
     let arg' = eval pool env arg in
     let env' = Environment.createWithEnclosing closure in
     Environment.define env' param arg';
     eval pool env' body
  | Ast.App (fi, t1, t2) ->
     let t1 = eval pool env t1 in
     let t2 = eval pool env t2 in
     eval pool env (Ast.App (fi, t1, t2))
  | Ast.Let (_, name, body, rest) ->
     let body' = eval pool env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval pool env' rest
  | Ast.LetRec (_, name, body, rest) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body;
     let body' = eval pool env' body in
     Environment.define env' name body';
     eval pool env' rest
  | Ast.If (fi, predicate, true_term, false_term) ->
     (match predicate with
     | Ast.Bool (_, b) ->
        eval pool env (if b then true_term else false_term)
     | pred ->
        let pred' = eval pool env pred in
        eval pool env (Ast.If (fi, pred', true_term, false_term)))
  | Ast.BinOp (_, t1, t2, op) ->
     let t1' = eval pool env t1 in
     let t2' = eval pool env t2 in
     (match (t1', t2') with
     | (Ast.Int (fi, x), Ast.Int (_, y)) -> eval_operator op fi x y
     | _ -> failwith "Error")
  | Ast.Eq (fi, t1, t2) ->
     let t1' = eval pool env t1 in
     let t2' = eval pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, true)
     else
       Ast.Bool (fi, false)
  | Ast.NEq (fi, t1, t2) ->
     let t1' = eval pool env t1 in
     let t2' = eval pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, false)
     else
       Ast.Bool (fi, true)
  | Ast.LetBox (_, s, t1, t2) ->
     (match eval pool env t1 with
      | Ast.Box (_, x) ->
         let env' = Environment.createWithEnclosing env in
         let promise = T.async pool (fun _ -> eval pool env x) in
         Environment.define env' s (Ast.WaitingOn promise);
         eval pool env' t2
      | _ -> failwith "ERROR")
  | Ast.Box _ as b -> b (* TODO CHECK IF NO FREE VARIABLES *)
  | Ast.WaitingOn p -> T.await pool p

let interpret n env t =
  let pool = T.setup_pool ~num_domains:n () in
  let res = T.run pool (fun _ -> eval pool env t) in
  T.teardown_pool pool;
  res

(* Test things - TODO add real tests*)

(*let term_id = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ())
let term_xyx = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Abs (empty_info, "y", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ()), Environment.create ())
let term1 = Ast.App (empty_info, term_id, Ast.Nat (empty_info, 5))
let term2 = Ast.App (empty_info, term_id, term_id)*)
