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
  | Ast.IVar (_, s) -> Ast.Box (Ast.Var s)
  (* | Ast.IIf (_, cond, m, n) -> Ast.LetBox ("'cond", transform cond, Ast.If (Ast.Var "'cond", transform m, transform n)) *)
  | Ast.IAbs (_, x, m) ->
     let fresh = Fresh.gen () in
     (Ast.Abs (fresh, Ast.LetBox (x, Ast.Var fresh, transform m)))
  | Ast.IApp (_, m, n) -> Ast.App (transform m, Ast.Box (transform n))
  | Ast.IBinOp (_, m, n, op) ->
     let fresh1 = Fresh.gen () in
     let fresh2 = Fresh.gen () in
     Ast.LetBox (fresh1, transform m, Ast.LetBox (fresh2, transform n, Ast.Box (Ast.BinOp (Ast.Var fresh1, Ast.Var fresh2, op))))
(*   | Ast.INil _ -> Ast.Box Ast.Nil
  | Ast.ICons (_, t1, t2) -> Ast.Box (Ast.Cons (transform t1, transform t2))
  | Ast.ICase (_, t1, t2, t3) ->
     Ast.LetBox ("'xs", transform t1, Ast.Case (Ast.Var "'xs", transform t2, transform t3)) *)
  | x -> failwith (Ast.info_to_string x ^ " Not yet implemented for girard transform")
