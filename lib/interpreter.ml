open Core

module T = Domainslib.Task

exception RuntimeError of string

type t =
  { globals : Ast.t Environment.t
  ; counter : int Atomic.t
  ; table : (int, Ast.t T.promise) Hashtbl.t
  ; table_mutex : Error_checking_mutex.t
  }

let create globals =
  { globals
  ; counter = Atomic.make 0
  ; table = Hashtbl.create (module Int)
  ; table_mutex = Error_checking_mutex.create ()
  }

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

let rec convert_mvar t env internal_seen term =
  match term with
  | Ast.MVar _ as v -> v
  | Ast.Var (_, name) as v -> (match Environment.get env name with
                               | Some (Ast.MVar _ as mv) -> mv
                               | _ when Option.is_some (Environment.get t.globals name) -> v
                               | _ ->
                                  if Set.mem internal_seen name then v
                                  else raise (RuntimeError ("Local binding (" ^ name ^ ") not allowed in box")))
  | Ast.App (fi, e1, e2) -> Ast.App (fi, convert_mvar t env internal_seen e1, convert_mvar t env internal_seen e2)
  | Ast.Abs (fi, s, e1) ->
     let internal' = Set.add internal_seen s in
     let e1' = convert_mvar t env internal' e1 in
     Ast.Abs(fi, s, e1')
  | Ast.BinOp (fi, e1, e2, op) -> Ast.BinOp (fi, convert_mvar t env internal_seen e1, convert_mvar t env internal_seen e2, op)
  | Ast.Int _ as i -> i
  | Ast.Box (fi, t1) -> Ast.Box (fi, convert_mvar t env internal_seen t1)
  | x -> failwith ("convert_mvar not yet implemented for " ^ (Ast.to_string x))
                          

let rec eval t pool env term = 
  match term with
  | Ast.Var (_, name) -> (match Environment.get env name with
                          | Some (Ast.MVar _ as mv) ->
                             (* Out_channel.output_string stdout ("FIRST: " ^ name ^ "\n"); *)
                             (* Out_channel.flush stdout; *)
                             eval t pool env mv
                          | Some x ->
                             (* Out_channel.output_string stdout "SECOND\n"; *)
                             (* Out_channel.flush stdout; *)
                             eval t pool env x
                          | _ -> (match Environment.get t.globals name with
                                  | Some x ->
                                     (* Out_channel.output_string stdout "THIRD\n"; *)
                                     (* Out_channel.flush stdout; *)
                                     eval t pool env x
                                  | _ -> raise (RuntimeError ("IMPLEMENT A RESOLVER " ^ "CAN'T FIND " ^ name))))
  | Ast.MVar (_, name) ->
     Error_checking_mutex.lock t.table_mutex;
     let res = Hashtbl.find t.table name in
     Error_checking_mutex.unlock t.table_mutex;
     (match res with
                           | Some x ->
                              (* Out_channel.output_string stdout ("STARTING " ^ Int.to_string name ^ "\n"); *)
                              (* Out_channel.flush stdout; *)
                              let res = T.await pool x in
                              (* Out_channel.output_string stdout "END\n"; *)
                              (* Out_channel.flush stdout; *)
                              res
                           | None -> raise (RuntimeError ("Unknown MVAR")))
  | Ast.Int (_,_) as x -> x
  | Ast.Bool (_,_) as x -> x
  | Ast.Abs (_, _, _) as x -> Ast.Cls (x, env)
  | Ast.Cls (_, _) as x -> x
  | Ast.App (_, Ast.Cls (Ast.Abs (_, param, body), closure), arg) ->
     let arg' = eval t pool env arg in
     let env' = Environment.createWithEnclosing closure in
     Environment.define env' param arg';
     eval t pool env' body
  | Ast.App (fi, t1, t2) ->
     let t1 = eval t pool env t1 in
     let t2 = eval t pool env t2 in
     eval t pool env (Ast.App (fi, t1, t2))
  | Ast.Let (_, name, body, rest) ->
     let body' = eval t pool env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval t pool env' rest
  | Ast.LetRec (_, name, body, rest) ->
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body;
     let body' = eval t pool env' body in
     Environment.define env' name body';
     eval t pool env' rest
  | Ast.If (fi, predicate, true_term, false_term) ->
     (match predicate with
      | Ast.Bool (_, b) ->
         eval t pool env (if b then true_term else false_term)
      | pred ->
         let pred' = eval t pool env pred in
         eval t pool env (Ast.If (fi, pred', true_term, false_term)))
  | Ast.BinOp (_, t1, t2, op) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     (match (t1', t2') with
      | (Ast.Int (fi, x), Ast.Int (_, y)) -> eval_operator op fi x y
      | _ ->
         let s = Printf.sprintf "%s and %s should be integers" (Ast.to_string t1') (Ast.to_string t2') in
         raise (RuntimeError s))
  | Ast.Eq (fi, t1, t2) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, true)
     else
       Ast.Bool (fi, false)
  | Ast.NEq (fi, t1, t2) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool (fi, false)
     else
       Ast.Bool (fi, true)
  | Ast.LetBox (_, s, t1, t2) ->
     (* Out_channel.output_string stdout "letbox\n"; *)
     (* Out_channel.flush stdout; *)
     (match eval t pool env t1 with
      | Ast.Box (fi, x) ->
         (* Out_channel.output_string stdout "it was a box\n"; *)
         (* Out_channel.flush stdout; *)
         let env' = Environment.createWithEnclosing env in
         (* Out_channel.output_string stdout "creating promise\n"; *)
         (* Out_channel.flush stdout; *)
         let promise = T.async pool (fun _ -> let res = eval t pool (Environment.create ()) x in (*Out_channel.output_string stdout "FINISHED\n";*) res) in
         let count = Atomic.fetch_and_add t.counter 1 in
         Error_checking_mutex.lock t.table_mutex;
         Hashtbl.set t.table ~key:count ~data:promise;
         Error_checking_mutex.unlock t.table_mutex;
         Environment.define env' s (Ast.MVar (fi, count));
         (* Out_channel.output_string stdout ("evaluating t2 : " ^ (Ast.to_string t2) ^ "\n"); *)
         (* Out_channel.flush stdout; *)
         eval t pool env' t2
      | x -> let s = Printf.sprintf "%s is not a box" (Ast.to_string x) in raise (RuntimeError s))
  | Ast.Box (fi, t1) ->
     let t1' = convert_mvar t env (Set.empty (module String)) t1 in
     Ast.Box (fi, t1') 

let interpret n env term =
  let pool = T.setup_pool ~num_domains:n () in
  let t = create env in
  let res = T.run pool (fun _ -> eval t pool env term) in
  T.teardown_pool pool;
  res

    (* Test things - TODO add real tests*)

    (*let term_id = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ())
      let term_xyx = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Abs (empty_info, "y", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ()), Environment.create ())
      let term1 = Ast.App (empty_info, term_id, Ast.Nat (empty_info, 5))
      let term2 = Ast.App (empty_info, term_id, term_id)*)
