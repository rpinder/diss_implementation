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
     Ast.Cons (Ast.Box (transform t1), transform t2)
  | Ast.ICase (_, t1, t2, t3) ->
     let y = Fresh.gen () in
     let ys = Fresh.gen () in
     let f = Fresh.gen () in
     let g = Fresh.gen () in
     Ast.Case (transform t1, transform t2, Ast.Abs (y, Ast.Abs (ys, Ast.LetBox (f, transform t3, Ast.LetBox (g, Ast.App (Ast.Var f, Ast.Var y), Ast.App (Ast.Var g, Ast.Var ys))))))
  | Ast.ILet (_, s, t1, t2) -> Ast.LetBox (s, transform t1, transform t2)
  | _ -> failwith "Not yet implemented for godel transform"



