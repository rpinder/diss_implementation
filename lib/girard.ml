let rec transform term =
  match term with
  | Ast.IInt (_, i) -> Ast.Box (Ast.Int i)
  | Ast.IBool (_, b) -> Ast.Box (Ast.Bool b)
  | Ast.IVar (_, s) -> Ast.Box (Ast.Var s)
  | Ast.IIf (_, cond, m, n) -> Ast.LetBox ("'cond", transform cond, Ast.If (Ast.Var "'cond", transform m, transform n))
  | Ast.IAbs (_, x, m) -> (Ast.Abs ("'x", Ast.LetBox (x, Ast.Var "'x", transform m)))
  | Ast.IApp (_, m, n) -> LetBox ("'f", transform m, Ast.App (Ast.Var "'f", transform n))
  | Ast.IBinOp (_, m, n, op) -> Ast.LetBox ("'m", transform m, Ast.LetBox ("'n", transform n, Ast.Box (Ast.BinOp (Ast.Var "'m", Ast.Var "'n", op))))
  | Ast.INil _ -> Ast.Box Ast.Nil
  | Ast.ICons (_, t1, t2) -> Ast.Box (Ast.Cons (transform t1, transform t2))
  | Ast.ICase (_, t1, t2, t3) ->
     Ast.LetBox ("'xs", transform t1, Ast.Case (Ast.Var "'xs", transform t2, transform t3))
  | _ -> failwith "Not yet implemented for girard transform"
