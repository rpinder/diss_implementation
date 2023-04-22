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
  | Ast.IInt (_, i) -> (Ast.Int i)
  | Ast.IVar (_, s) -> (Ast.Var s)
  | Ast.IAbs (_, x, m) ->
     let fresh = Fresh.gen () in
     (Ast.Abs (fresh, Ast.LetBox (x, Ast.Var fresh, transform m)))
  | Ast.IApp (_, m, n) -> Ast.App (transform m, Ast.Box (transform n))
  | Ast.IIf (_, pred, case1, case2) -> Ast.If (transform pred, transform case1, transform case2)
  | Ast.IBinOp (_, m, n, op) ->
     Ast.BinOp(transform m, transform n, op)
  | Ast.INil _ -> Ast.Box Ast.Nil
  | Ast.ICons (_, t1, t2) -> Ast.Box (Ast.Cons (Ast.Box (transform t1), Ast.Box (transform t2)))
  (* | Ast.ICase (_, t1, t2, t3) -> *)
     (* (match t3 with *)
      (* | (Ast.IAbs (_, y, Ast.IAbs (_, ys, rest))) -> Ast.CaseP (transform t1, transform t2, Ast.Abs (y, Ast.Abs (ys, transform rest))) *)
      (* | _ -> failwith ("Wrong type of func in case")) *)
  | Ast.ICase (_, t1, t2, t3) ->
     let fresh = Fresh.gen () in
     Ast.LetBox (fresh, transform t1, 
     Ast.Case (Ast.Var fresh, transform t2, transform t3))

  | x -> failwith (Ast.info_to_string x ^ " Not yet implemented for girard transform")
