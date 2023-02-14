open Core

exception TypeError of string

module Var = struct
  module T = struct
    type t = V of string
                    [@@deriving compare, sexp_of, hash, equal]
  end
  include T
  include Comparator.Make(T)
end

module Typ = struct
  type t =
    | Var of Var.t
    | Con of string
    | Arr of t * t
    | Box of t
  [@@deriving equal]

  let rec to_string = function
    | Var (V s) -> s
    | Con s -> s
    | Arr (Arr _ as t1, t2) -> "(" ^ (to_string t1) ^ ") -> " ^ (to_string t2)
    | Arr (t1, t2) -> (to_string t1) ^ " -> " ^ (to_string t2)
    | Box t1 -> "box (" ^ to_string t1 ^ ")"
end

type scheme = Forall of Var.t list * Typ.t

module Fresh : sig
  val gen : unit -> Typ.t
end = struct
  let counter = ref 0

  let gen () =
    counter := !counter + 1;
    Typ.Var (V ("t" ^ (Int.to_string !counter)));
end

let rec type_ftv = function
  | Typ.Var v -> Set.singleton (module Var) v
  | Typ.Con _ -> Set.empty (module Var)
  | Typ.Arr (t1, t2) -> Set.union (type_ftv t1) (type_ftv t2)
  | Typ.Box t1 -> type_ftv t1

let rec type_apply s = function
  | Typ.Var n as v ->
     (match Map.find s n with
      | Some x -> x
      | None -> v)             
  | Typ.Arr (t1, t2) -> Typ.Arr (type_apply s t1, type_apply s t2)
  | x -> x

let scheme_ftv (Forall (vars, typ)) =
  let ftv_t = type_ftv typ in
  let freeVars = Set.of_list (module Var) vars in
  Set.diff ftv_t freeVars

(*let scheme_apply s (Forall (vars, typ)) : scheme =
  let s' = List.fold vars ~init:s ~f:Map.remove in
  Forall (vars, type_apply s' typ)*)

type subst = (Var.t, Typ.t, Var.comparator_witness) Map.t

let emptysubst : subst = Map.empty (module Var)

let compose_subst (s1 : subst) (s2 : subst) : subst =
  let s1' = Map.map ~f:(fun x -> type_apply s2 x) s1 in
  Map.merge s1' s2 ~f:(fun ~key:_ opt ->
      match opt with
      | `Left v | `Right v -> Some v
      | `Both (v, _) -> Some v)

type typeenv = (Var.t, scheme, Var.comparator_witness) Map.t

let env_ftv (env : typeenv) : (Var.t, Var.comparator_witness) Set.t =
  let f l = List.fold (List.map ~f:(scheme_ftv) l) ~init:(Set.empty (module Var)) ~f:Set.union in
  f (Map.data env)

(*let env_apply (s : subst) env =
  Map.map env ~f:(scheme_apply s)
 *)

let generalise (env : typeenv) (t : Typ.t) : scheme =
  let diff = Set.diff (type_ftv t) (env_ftv env) in
  Forall (Set.to_list diff, t)

let instantiate (Forall (vars, typ) : scheme) =
  let vars' = List.map vars ~f:(fun _ -> Fresh.gen ()) in
  let zipped = List.zip_exn vars vars' in
  let sub = Map.of_alist_exn (module Var) zipped in
  type_apply sub typ

let var_bind v t =
  match t with
  | (Typ.Var v') when Var.equal v' v -> emptysubst
  | _ when Set.mem (type_ftv t) v -> raise (TypeError "Occurs check fails")
  | _ -> Map.singleton (module Var) v t

(*let e0 = Ast.Let (Ast.empty_info, "id", Ast.Abs (Ast.empty_info, "x", Ast.Var (Ast.empty_info, "x")), Ast.Var (Ast.empty_info, "id"))*)

let lookupEnv env (var : Var.t) =
  let (V s) = var in
  match Map.find env var with
  | None -> raise (TypeError ("UnboundVariable: " ^ s))
  | Some sigma ->
     let t = instantiate sigma in
     t

let binops op = match op with
  | Ast.Plus | Ast.Minus | Ast.Multiply -> Typ.Arr (Typ.Con "int", Typ.Arr (Typ.Con "int", Typ.Con "int"))
  | Ast.LessThan | Ast.GreaterThan | Ast.LTEQ | Ast.GTEQ -> Typ.Arr (Typ.Con "int", Typ.Arr (Typ.Con "int", Typ.Con "bool"))

module Inference = struct
    type t = {
        mutable constraints : (Typ.t * Typ.t) list
      }

    type constra = Typ.t * Typ.t
    type unifier = subst * constra list

    let create () = { constraints = [] }

    let add_constraint t con = 
      let constraints' = con :: t.constraints in
      t.constraints <- constraints'


    let list_type_apply (s : subst) (xs : Typ.t list) =
      List.map xs ~f:(type_apply s)

    let constraint_apply (s : subst) ((t1, t2) : constra) : constra = (type_apply s t1, type_apply s t2)

    let list_constraint_apply (s : subst) (xs : constra list) : constra list =
      List.map xs ~f:(constraint_apply s)

    let rec unifies t1 t2 : unifier =
      match (t1, t2) with
      | (t1, t2) when Typ.equal t1 t2 -> emptysubst, []
      | (Typ.Var v, t) -> var_bind v t, []
      | (t, Typ.Var v) -> var_bind v t, []
      | (Typ.Arr (t1, t2), Typ.Arr (t3, t4)) -> unify_list [t1; t2] [t3; t4]
      | (Typ.Box t1, Typ.Box t2) -> unifies t1 t2
      | _ -> raise (TypeError (Printf.sprintf "Unification failed - Cannot unify\n  %s\nwith\n  %s\n" (Typ.to_string t1) (Typ.to_string t2)))
    and unify_list l1 l2 : unifier =
      match (l1, l2) with
      | [], [] -> emptysubst, []
      | t1::ts1, t2::ts2 ->
         let (su1, cs1) = unifies t1 t2 in
         let (su2, cs2) = unify_list (list_type_apply su1 ts1) (list_type_apply su1 ts2) in
         (compose_subst su2 su1, cs1 @ cs2)
      | _ -> raise (TypeError "Unification Mismatch")

    let rec solver (u : unifier) =
      let (su, cs) = u in
      match cs with
      | [] -> su
      | ((t1, t2) :: cs0) ->
         let (su1, cs1) = unifies t1 t2 in
         solver (compose_subst su1 su, cs1 @ (list_constraint_apply su1 cs0))

    let rec infer t env ex =
      match ex with
      | Ast.Int _ -> Typ.Con "int"
      | Ast.Bool _ -> Typ.Con "bool"
      | Ast.Var (_, x) -> lookupEnv env (V x)
      | Ast.Abs (_, x, e) ->
         let tv = Fresh.gen () in
         let env' = Map.add_exn env ~key:(V x) ~data:(Forall ([], tv)) in
         let t = infer t env' e in
         Typ.Arr (tv, t)
      | Ast.App (_, e1, e2) ->
         let t1 = infer t env e1 in
         let t2 = infer t env e2 in
         let tv = Fresh.gen () in
         add_constraint t (t1, Typ.Arr (t2, tv));
         tv
      | Ast.Let (_, x, e1, e2) ->
         let t' = create () in
         let t0 = infer t' env e1 in
         let sub = solver (emptysubst, t'.constraints) in
         let t1 = type_apply sub t0 in
         let sc = generalise env t1 in
         let env' = Map.add_exn env ~key:(V x) ~data:sc in
         let t2 = infer t env' e2 in
         t2
      | Ast.LetRec (_, x, e1, e2) ->
         let t' = create () in
         let tv = Fresh.gen () in
         let gen = generalise env tv in
         let env' = Map.add_exn env ~key:(V x) ~data:gen in
         let t0 = infer t' env' e1 in
         add_constraint t' (tv, t0);
         let sub = solver (emptysubst, t'.constraints) in
         let t1 = type_apply sub t0 in
         let sc = generalise env t1 in
         let env'' = Map.set env' ~key:(V x) ~data:sc in
         let t2 = infer t env'' e2 in
         t2
      | Ast.If (_, pred, e1, e2) ->
         let tpred = infer t env pred in
         add_constraint t (tpred, Typ.Con "bool");
         let t1 = infer t env e1 in
         let t2 = infer t env e2 in
         add_constraint t (t1, t2);
         t1
      | Ast.Eq (_, e1, e2) | Ast.NEq (_, e1, e2) ->
         let t1 = infer t env e1 in
         let t2 = infer t env e2 in
         add_constraint t (t1, t2);
         let tv = Fresh.gen () in
         add_constraint t (tv, Typ.Con "bool");
         tv
      | Ast.BinOp (_, e1, e2, op) ->
         let t1 = infer t env e1 in
         let t2 = infer t env e2 in
         let tv = Fresh.gen () in
         let u1 = Typ.Arr (t1, Typ.Arr (t2, tv)) in
         let u2 = binops op in
         add_constraint t (u1, u2);
         tv
      | Ast.LetBox (_, x, e1, e2) ->
         let t1 = infer t env e1 in
         let tv = Fresh.gen () in
         add_constraint t (t1, Typ.Box tv);
         let sc = generalise env tv in
         let env' = Map.add_exn env ~key:(V x) ~data:sc in
         let t2 = infer t env' e2 in
         Out_channel.output_string stdout (Printf.sprintf "%s | %s | %s\n" (Typ.to_string t1) (Typ.to_string tv ) (Typ.to_string t2));
         t2
      | Ast.Box (_, e1) ->
         let t1 = infer t env e1 in
         Out_channel.output_string stdout (" || " ^ Typ.to_string t1 ^ "\n");
         let tv = Fresh.gen () in
         add_constraint t (tv, Typ.Box t1);
         tv
      | _ -> failwith "Not yet implemented"

    (* change this to support any number *)
    let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

    let _normalise ((Forall (_, body)) : scheme) : scheme =
      let ord = List.mapi (Set.to_list (type_ftv body)) ~f:(fun i x -> (x, List.nth_exn letters i)) in
      let rec normtype = function
        | Typ.Arr (t1, t2) -> Typ.Arr (normtype t1, normtype t2)
        | Typ.Con _ as x -> x
        | Typ.Box x -> Typ.Box (normtype x)
        | Typ.Var a ->
           (match Caml.List.assoc_opt a ord with
            | Some x -> Typ.Var (V x)
            | None -> raise (TypeError "type variable not in signature"))
      in
      Forall (List.map ord ~f:(fun x -> Var.V (snd x)), normtype body)

    let typeof (ex : Ast.t) : Typ.t =
      let env = Map.empty (module Var) in
      let t = create () in
      let typ = infer t env ex in
      List.iter t.constraints ~f:(fun (t1, t2) -> Out_channel.output_string stdout (Typ.to_string t1 ^ " and " ^ Typ.to_string t2 ^ "\n"));
      let subst = solver (emptysubst, t.constraints) in
      let Forall (_, body) = (generalise env (type_apply subst typ))
      in body
  end
