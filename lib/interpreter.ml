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

let eval_operator op =
  let createint x = Ast.Int x in
  let createbool x = Ast.Bool x in
  fun x y ->
  match op with
  | Ast.Plus -> createint (x + y)
  | Ast.Minus -> createint (x - y)
  | Ast.Multiply -> createint (x * y)
  | Ast.LessThan -> createbool (x < y)
  | Ast.GreaterThan -> createbool (x > y)
  | Ast.LTEQ -> createbool (x <= y)
  | Ast.GTEQ -> createbool (x >= y)

let rec replace_variable term v_old v_new =
  match term with
  | Ast.Var v when String.(=) v v_old -> Ast.Var v_new
  | Ast.Var _ as v -> v
  | Ast.LetBox (v, t1, t2) when String.(=) v v_old ->
     Out_channel.output_string stdout ("ONE\n"); Out_channel.flush stdout;
     Ast.LetBox (v, replace_variable t1 v_old v_new, t2)
  | Ast.LetBox (v, t1, t2) when String.(=) v v_new ->
     Out_channel.output_string stdout ("TWO\n"); Out_channel.flush stdout;
     Ast.LetBox (v_new, replace_variable t1 v_old v_new, t2)
  | Ast.LetBox (v, t1, t2) ->
     Out_channel.output_string stdout ("THREE\n"); Out_channel.flush stdout;
     Ast.LetBox (v, replace_variable t1 v_old v_new, replace_variable t2 v_old v_new)
  | Ast.Abs (v, t1) when String.(=) v v_old -> Ast.Abs (v_new, t1)
  | Ast.Abs (v, t1) when String.(=) v v_new -> Ast.Abs (v_new, t1)
  | Ast.Abs (v, t1) -> Ast.Abs (v, replace_variable t1 v_old v_new)
  | Ast.App (t1, t2) -> Ast.App (replace_variable t1 v_old v_new, replace_variable t2 v_old v_new)
  | Ast.BinOp (t1, t2, op) ->
     Out_channel.output_string stdout ("BINOP: " ^ Ast.to_string t1 ^ " and " ^ Ast.to_string t2 ^ "\n"); Out_channel.flush stdout;
     Ast.BinOp (replace_variable t1 v_old v_new, replace_variable t2 v_old v_new, op)
  | Ast.If (t1, t2, t3) -> Ast.If (replace_variable t1 v_old v_new, replace_variable t2 v_old v_new, replace_variable t3 v_old v_new)
  | Ast.Box x -> Ast.Box (replace_variable x v_old v_new)
  | x ->
     Out_channel.output_string stdout ("FALLTHROUGH: " ^ Ast.to_string x ^ "\n"); Out_channel.flush stdout;
     x

(* let rec replace term v_old new_term = *)
(*   match term with *)
(*   | Ast.Var v when String.(=) v v_old -> new_term *)
(*   | Ast.Var _ as v -> v *)
(*   | Ast.LetBox (v, t1, t2) when String.(=) v v_old-> Ast.LetBox (v_new, replace t1 v_old v_new, replace t2 v_old v_new) *)
(*   | Ast.LetBox (v, t1, t2) when String.(=) v v_new -> Ast.LetBox (v_new, replace t1 v_old v_new, t2) *)
(*   | Ast.LetBox (v, t1, t2) -> Ast.LetBox (v, replace t1 v_old v_new, replace t2 v_old v_new) *)
(*   | Ast.Abs (v, t1) when String.(=) v v_old -> Ast.Abs (v_new, replace t1 v_old v_new) *)
(*   | Ast.Abs (v, t1) when String.(=) v v_new -> Ast.Abs (v_new, t1) *)
(*   | Ast.Abs (v, t1) -> Ast.Abs (v, replace t1 v_old v_new) *)
(*   | x -> x *)


let rec optimize term =
  match term with
  | Ast.LetBox (v1, Ast.Box (Ast.Var v2), t2) -> optimize (replace_variable t2 v1 v2)
  (* | Ast.LetBox (v1, Ast.Box (Ast.Int _ as i), t2) -> (\* Ast.Let (v1, i, optimize t2) *\) *)
  (* | Ast.LetBox (v1, Ast.Box (Ast.Bool _ as b), t2) -> Ast.Let (v1, b, optimize t2) *)
  | Ast.LetBox (v1, t1, t2) -> Ast.LetBox (v1, optimize t1, optimize t2)
  | Ast.Box x -> Ast.Box (optimize x)
  | Ast.Abs (s, t1) -> Ast.Abs (s, optimize t1)
  | Ast.Let (s, t1, t2) -> Ast.Let (s, optimize t1, optimize t2)
  | Ast.BinOp (t1, t2, op) -> Ast.BinOp (optimize t1, optimize t2, op)
  | Ast.If (pred, t1, t2) -> Ast.If (optimize pred, optimize t1, optimize t2)
  | Ast.App (t1, t2) -> Ast.App (optimize t1, optimize t2)
  | x ->
     Out_channel.output_string stdout ("OPTIMIZE FALLTHROUGH: " ^ Ast.to_string x ^ "\n"); Out_channel.flush stdout;
     x



let rec convert_mvar t pool env internal_seen term =
  match term with
  | Ast.MVar _ as v -> v
  | Ast.Var name as v -> (match Environment.get env name with
                               | Some (Ast.MVar _ as mv) -> mv
                               | Some (Ast.OBox _ as b) -> make_mvar t pool b
                               | _ when Option.is_some (Environment.get t.globals name) -> v
                               | _ ->
                                  if Set.mem internal_seen name then v
                                  else raise (RuntimeError ("Local binding (" ^ name ^ ") not allowed in box")))
  | Ast.Let (s, t1, t2) ->
     let internal' = Set.add internal_seen s in
     Ast.Let (s, convert_mvar t pool env internal' t1, convert_mvar t pool env internal' t2)
  | Ast.App (e1, e2) -> Ast.App (convert_mvar t pool env internal_seen e1, convert_mvar t pool env internal_seen e2)
  | Ast.Abs (s, e1) ->
     let internal' = Set.add internal_seen s in
     let e1' = convert_mvar t pool env internal' e1 in
     Ast.Abs(s, e1')
  (* | Ast.Cls (fi, s, e1, _) -> *)
  (*    let internal' = Set.add internal_seen s in *)
  (*    let e1' = convert_mvar t pool env internal' e1 in *)
  (*    Ast.Cls (fi, s, e1', Environment.create ()) *)
  | Ast.BinOp (e1, e2, op) -> Ast.BinOp (convert_mvar t pool env internal_seen e1, convert_mvar t pool env internal_seen e2, op)
  | Ast.Int _ as i -> i
  | Ast.Bool _ as b -> b
  | Ast.Box t1 -> Ast.Box (convert_mvar t pool env internal_seen t1)
  | Ast.Nil -> Ast.Nil
  | Ast.Cons (t1, t2) -> Ast.Cons (convert_mvar t pool env internal_seen t1, convert_mvar t pool env internal_seen t2)
  | Ast.Case (t1, t2, t3) -> Ast.Case (convert_mvar t pool env internal_seen t1, convert_mvar t pool env internal_seen t2, convert_mvar t pool env internal_seen t3)
  | Ast.LetBox (s, t1, t2) ->
     let internal' = Set.add internal_seen s in
     Ast.LetBox (s, convert_mvar t pool env internal_seen t1, convert_mvar t pool env internal' t2)
  | Ast.If (cond, e1, e2) ->
     let f = convert_mvar t pool env internal_seen in
     Ast.If (f cond, f e1, f e2)
  | x -> raise (RuntimeError ("convert_mvar not yet implemented for " ^ (Ast.to_string x)))

and make_mvar t pool term = 
  match term with
  | Ast.OBox b ->
     let promise = T.async pool (fun _ -> eval t pool (Environment.create ()) b) in
     let count = Atomic.fetch_and_add t.counter 1 in
     Error_checking_mutex.lock t.table_mutex;
     Hashtbl.set t.table ~key:count ~data:promise;
     Error_checking_mutex.unlock t.table_mutex;
     Ast.MVar count
 | t -> raise (RuntimeError ("make_mvar requires OBox but got " ^ (Ast.to_string t)))

and mvar_or_not t pool term =
  match term with
  | Ast.OBox _ as b -> make_mvar t pool b
  | x -> x

and force_obox t pool term =
  match term with
  | Ast.Nil -> Ast.Nil
  | Ast.OBox b -> force_obox t pool (eval t pool (Environment.create ()) b)
  | Ast.Cons (t1, t2) ->
     let t1 = T.async pool (fun _ -> force_obox t pool t1) in
     let t2 = T.async pool (fun _ -> force_obox t pool t2) in
     let t1' = T.await pool t1 in
     let t2' = T.await pool t2 in
     Ast.Cons (t1', t2')
  | Ast.BinOp (t1, t2, op) ->
     let t1 = T.async pool (fun _ -> force_obox t pool t1) in
     let t2 = T.async pool (fun _ -> force_obox t pool t2) in
     let t1' = T.await pool t1 in
     let t2' = T.await pool t2 in
     Ast.BinOp (t1', t2', op)
  | Ast.App (t1, t2) ->
     let t1 = T.async pool (fun _ -> force_obox t pool t1) in
     let t2 = T.async pool (fun _ -> force_obox t pool t2) in
     let t1' = T.await pool t1 in
     let t2' = T.await pool t2 in
     Ast.App (t1', t2')
  | Ast.Abs (s, t1) ->
    Ast.Abs(s, force_obox t pool t1)
  | Ast.Int _ as i -> i
  | Ast.Bool _ as b -> b
  | Ast.Box (t1) -> Ast.Box (force_obox t pool t1)
  | Ast.MVar _ as c -> eval t pool (Environment.create ()) c
  | Ast.Cls (t1, env) -> eval t pool env t1
  | Ast.Var _ as v -> v
  | Ast.LetBox _ as t1 ->
     force_obox t pool (eval t pool (Environment.create ()) t1)
  | x -> raise (RuntimeError ("force_obox not implemented: " ^ (Ast.to_string x)))
and eval t pool env term =
  match term with
  | Ast.Var name -> (match Environment.get env name with
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
  | Ast.MVar name ->
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
                           | None -> Out_channel.output_string stdout "NEVER HEARD OT HIS MAN IN MY LIFE\n"; Out_channel.flush stdout;raise (RuntimeError ("Unknown MVAR")))
  | Ast.Int _ as x -> x
  | Ast.Bool _ as x -> x
  | Ast.Abs _ as x -> Ast.Cls (x, env)
  | Ast.Cls _ as x -> x
  | Ast.App (Ast.Cls (Ast.Abs (param, body) as l, closure), arg) ->
     let arg' = eval t pool env arg in
     Out_channel.output_string stdout ("APP: " ^ Ast.to_string l ^ " and " ^ Ast.to_string arg ^ "\n"); Out_channel.flush stdout;
     let env' = Environment.createWithEnclosing closure in
     Environment.define env' param arg';
     eval t pool env' body
  | Ast.App (t1, t2) ->
     (* Out_channel.output_string stdout "APP\n"; *)
     (* Out_channel.flush stdout; *)
     let t1 = mvar_or_not t pool (eval t pool env t1) in
     let t2 = eval t pool env t2 in
     eval t pool env (Ast.App (t1, t2))
  | Ast.Let (name, body, rest) ->
     let body' = eval t pool env body in
     let env' = Environment.createWithEnclosing env in
     Environment.define env' name body';
     eval t pool env' rest
  | Ast.If (predicate, true_term, false_term) ->
     (match predicate with
      | Ast.Bool b ->
         eval t pool env (if b then true_term else false_term)
      | pred ->
         let pred' = eval t pool env (mvar_or_not t pool pred) in
         eval t pool env (Ast.If (pred', true_term, false_term)))
  | Ast.BinOp (t1, t2, op) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     (match (t1', t2') with
      | (Ast.Int x, Ast.Int y) ->
         Out_channel.output_string stdout (Ast.to_string (Ast.BinOp (t1', t2', op)) ^ "\n");
         eval_operator op x y
      | (x, y) -> eval t pool env (Ast.BinOp (mvar_or_not t pool x, mvar_or_not t pool y, op)))
      (* | _ ->
         let s = Printf.sprintf "%s and %s should be integers" (Ast.to_string t1') (Ast.to_string t2') in
         raise (RuntimeError s)) *)
  | Ast.Eq (t1, t2) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool true
     else
       Ast.Bool false
  | Ast.NEq (t1, t2) ->
     let t1' = eval t pool env t1 in
     let t2' = eval t pool env t2 in
     if Ast.equal t1' t2' then
       Ast.Bool false
     else
       Ast.Bool true
  | Ast.LetBox (s, t1, t2) ->
     (* Out_channel.output_string stdout "letbox\n"; *)
     (* Out_channel.flush stdout; *)
     (match eval t pool env t1 with
      | Ast.Box x -> 
         (* Out_channel.output_string stdout ("it was a box: " ^ Ast.to_string x ^ "\n"); *)
         (* Out_channel.flush stdout; *)
         (*
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
         eval t pool env' t2 *)
         let obox = Ast.OBox x in
         let env' = Environment.createWithEnclosing env in
         Environment.define env' s obox;
         Out_channel.output_string stdout ("ffff: " ^ Ast.to_string x ^ "\n"); Out_channel.flush stdout;
         let res = eval t pool env' t2 in
         Out_channel.output_string stdout ("iiii: " ^ Ast.to_string x ^ "\n"); Out_channel.flush stdout; res
      | x -> let s = Printf.sprintf "%s is not a box" (Ast.to_string x) in raise (RuntimeError s))
  | Ast.Box t1 ->
     (* Out_channel.output_string stdout ("box: " ^ Ast.to_string t1 ^ "\n"); Out_channel.flush stdout; *)
     let t1' = convert_mvar t pool env (Set.empty (module String)) t1 in
     Ast.Box t1' 
  | Ast.OBox _ as x -> (* Out_channel.output_string stdout ("gggg " ^ Ast.to_string x ^ "\n"); Out_channel.flush stdout; *) x
     (* )let t1' = convert_mvar t env (Set.empty (module String)) t1 in
     Ast.OBox t1' *)
  | Ast.Nil ->
     (* Out_channel.output_string stdout "nil\n"; Out_channel.flush stdout; *)
     Ast.Nil
  | Ast.Cons (e1, e2) ->
     (* Out_channel.output_string stdout "cons\n"; Out_channel.flush stdout; *)
     let e1' = eval t pool env e1 in
     let e2' = eval t pool env e2 in
     Ast.Cons (e1', e2')
  | Ast.Case (e1, empty_case, other_case) ->
     (* Out_channel.output_string stdout "aaa\n"; Out_channel.flush stdout; *)
     let e1' = eval t pool env (mvar_or_not t pool e1) in
     (* Out_channel.output_string stdout "bbb\n"; Out_channel.flush stdout; *)
     (match e1' with
      | Ast.Nil ->
         (* Out_channel.output_string stdout "evaluating a nil\n"; Out_channel.flush stdout; *)
         eval t pool env empty_case
     | Ast.Cons (a, b) ->
        (* Out_channel.output_string stdout "evaluating a cons\n"; Out_channel.flush stdout; *)
        let other_case' = eval t pool env other_case in
        (* Out_channel.output_string stdout "ddd\n"; Out_channel.flush stdout; *)
        let e2 = Ast.App (Ast.App (other_case', a), b) in
        (* Out_channel.output_string stdout "nnn\n"; Out_channel.flush stdout; *)
        eval t pool env e2
     | Ast.OBox _ as b -> (* Out_channel.output_string stdout "AAAAHHH\n"; Out_channel.flush stdout; *) eval t pool env (Ast.Case (make_mvar t pool b, empty_case, other_case))
     | x -> raise (RuntimeError ("Non list type used in case: " ^ Ast.to_string x)))

let interpret n env term =
  let pool = T.setup_pool ~num_domains:n () in
  let t = create env in
  let res = T.run pool (fun _ -> eval t pool env term) in
  let res' = T.run pool (fun _ -> force_obox t pool res) in
  T.teardown_pool pool;
  res'

    (* Test things - TODO add real tests*)

    (*let term_id = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ())
      let term_xyx = Ast.Abs (empty_info, "x", Ast.Typ.Nat, Ast.Abs (empty_info, "y", Ast.Typ.Nat, Ast.Var (empty_info, "x"), Environment.create ()), Environment.create ())
      let term1 = Ast.App (empty_info, term_id, Ast.Nat (empty_info, 5))
      let term2 = Ast.App (empty_info, term_id, term_id)*)
