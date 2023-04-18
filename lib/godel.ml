exception GodelError of string

module Fresh : sig
  val gen : unit -> string
end = struct
  let counter = ref 1

  let gen () =
    counter := !counter + 1;
    "'x" ^ (Int.to_string !counter)
end

let rec transform term =
  match term with
  | Ast.IInt (_, i) -> Ast.Box (Ast.Int i)
  | Ast.IBool (_, b) -> Ast.Box (Ast.Bool b)
  | Ast.IIf (_, cond, m, n) ->
     let fresh = Fresh.gen () in
     Ast.LetBox (fresh, transform cond, Ast.If (Ast.Var fresh, transform m, transform n))
  | Ast.IVar (_, s) -> Ast.Box (Ast.Var s)
  | Ast.IAbs (_, x, m) ->
     let fresh = Fresh.gen () in
     Ast.Box (Ast.Abs (fresh, Ast.LetBox (x, Ast.Var fresh, transform m)))
  | Ast.IApp (_, m, n) ->
     let fresh = Fresh.gen () in
     Ast.LetBox (fresh, transform m, Ast.App (Ast.Var fresh, transform n))
  | Ast.IBinOp (_, m, n, op) ->
     let fresh1 = Fresh.gen () in
     let fresh2 = Fresh.gen () in
     Ast.LetBox (fresh1, transform m, Ast.LetBox (fresh2, transform n, Ast.Box (Ast.BinOp (Ast.Var fresh1, Ast.Var fresh2, op))))
  | Ast.IEq (_, m, n) ->
     let fresh1 = Fresh.gen () in
     let fresh2 = Fresh.gen () in
     Ast.LetBox (fresh1, transform m, Ast.LetBox (fresh2, transform n, Ast.Box (Ast.Eq (Ast.Var fresh1, Ast.Var fresh2))))
  | Ast.INEq (_, m, n) ->
     let fresh1 = Fresh.gen () in
     let fresh2 = Fresh.gen () in
     Ast.LetBox (fresh1, transform m, Ast.LetBox (fresh2, transform n, Ast.Box (Ast.NEq (Ast.Var fresh1, Ast.Var fresh2))))
(* LISTS *)
  | Ast.INil _ -> Ast.Box Ast.Nil
  | Ast.ICons (_, t1, t2) ->
     let fresh1 = Fresh.gen () in
     let fresh2 = Fresh.gen () in
     Ast.LetBox (fresh1, transform t1, Ast.LetBox (fresh2, transform t2, Ast.Box (Ast.Cons (Ast.Box (Ast.Var fresh1), Ast.Var fresh2))))
  | Ast.ICase (_, t1, t2, t3) ->
     let xs = Fresh.gen () in
     (* let one = Fresh.gen () in *)
     (* let two = Fresh.gen () in *)
     (* let f' = Fresh.gen () in *)
     (* let f'' = Fresh.gen () in *)
     (* Ast.LetBox (xs, transform t1, Ast.Case (Ast.Var xs, transform t2, Ast.Abs (one, Ast.Abs (two, Ast.LetBox (f'', Ast.App (Ast.LetBox (f', transform t3, Ast.Var f'), Ast.Var one), Ast.App (Ast.Var f'', Ast.Var two)))))) *)
     (* Let xs = Fresh.gen () in *)
     (* let z = Fresh.gen () in *)
     (* let zs = Fresh.gen () in *)
     (* let other_case = in *)
     (* Ast.LetBox (xs, transform t1, Ast.Case (Ast.Var xs, transform t2, other_case) *)
     (match t3 with
     | Ast.IAbs (_, y, Ast.IAbs (_, ys, q)) ->
        let fresh1 = Fresh.gen () in
        (* let fresh2 = Fresh.gen () in *)
        let other_case = Ast.Abs (fresh1, Ast.LetBox (y, Ast.Var fresh1, Ast.Abs (ys, Ast.convert q)))in
        Ast.LetBox (xs, transform t1, Ast.Case (Ast.Var xs, transform t2, other_case))
     | _ -> raise (GodelError "Last term of case was not correct form"))
     
  | Ast.ILet (_, s, t1, t2) -> Ast.LetBox (s, transform t1, transform t2)
  | _ -> failwith "Not yet implemented for godel transform"



