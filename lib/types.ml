open Base

exception TypeError of string

type lit = LInt of int | LBool of bool

module Var = struct
  module T = struct
    type t = V of string
                    [@@deriving compare, sexp_of, hash, equal]
  end
  include T
  include Comparator.Make(T)
end

type expr =
  | Var of Var.t
  | App of expr * expr
  | Lam of Var.t * expr
  | Lit of lit
  | Let of Var.t * expr * expr

module Typ = struct
  type t =
    | Var of Var.t
    | Con of string
    | Arr of t * t

  let rec to_string = function
    | Var (V s) -> s
    | Con s -> s
    | Arr (t1, t2) -> (to_string t1) ^ " -> " ^ (to_string t2)
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

let scheme_apply s (Forall (vars, typ)) : scheme =
  let s' = List.fold vars ~init:s ~f:Map.remove in
  Forall (vars, type_apply s' typ)

type subst = (Var.t, Typ.t, Var.comparator_witness) Map.t

let emptysubst : subst = Map.empty (module Var)

let compose_subst (s1 : subst) (s2 : subst) : subst =
  let s1' = Map.map ~f:(fun x -> type_apply s1 x) s2 in
  Map.merge_skewed s1' s2 ~combine:(fun ~key:_ v1 _ -> v1)

type typeenv = (Var.t, scheme, Var.comparator_witness) Map.t

let env_ftv (env : typeenv) : (Var.t, Var.comparator_witness) Set.t =
  let f l = List.fold (List.map ~f:(scheme_ftv) l) ~init:(Set.empty (module Var)) ~f:Set.union in
  f (Map.data env)

let env_apply (s : subst) env =
  Map.map env ~f:(scheme_apply s)

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

let rec mgu (t1 : Typ.t) (t2 : Typ.t) : subst =
  match (t1, t2) with
  | (Typ.Arr (l, r)), (Typ.Arr (l', r')) ->
     let s1 = mgu l l' in
     let s2 = mgu (type_apply s1 r) (type_apply s1 r') in
     compose_subst s1 s2
  | (Typ.Var u, t) | (t, Typ.Var u)-> var_bind u t
  | (Typ.Con s1), (Typ.Con s2) when String.(=) s1 s2 -> emptysubst
  | _ -> raise (TypeError ("types do not unify: " ^ (Typ.to_string t1) ^ " and " ^ (Typ.to_string t2)))

let infer_lit l =
  let t = match l with
    | LInt _ -> "int"
    | LBool _ -> "bool"
  in
  (emptysubst, Typ.Con t)

let rec infer (env : typeenv) (ex : expr) : subst * Typ.t =
  match ex with
  | Var n ->
     let (V s) = n in
     (match Map.find env n with
      | None -> raise (TypeError ("unbound variable: " ^ s))
      | Some sigma ->
         let t = instantiate sigma in
         (emptysubst, t))
  | Lit l -> infer_lit l
  | Lam (n, e) ->
     let tv = Fresh.gen () in
     let env' = Map.remove env n in
     let env'' = Map.add_exn env' ~key:n ~data:(Forall ([], tv)) in
     let (s1, t1) = infer env'' e in
     (s1, Typ.Arr (type_apply s1 tv, t1))
  | App (e1, e2) ->
     let tv = Fresh.gen () in
     let (s1, t1) = infer env e1 in
     let (s2, t2) = infer (env_apply s1 env) e2 in
     let s3 = mgu (type_apply s2 t1) (Typ.Arr (t2, tv)) in
     (compose_subst s3 (compose_subst s2 s1), type_apply s3 tv)
  | Let (x, e1, e2) ->
     let (s1, t1) = infer env e1 in
     let env' = Map.remove env x in
     let t' = generalise (env_apply s1 env) t1 in
     let env'' = Map.add_exn env' ~key:x ~data:t' in
     let (s2, t2) = infer (env_apply s1 env'') e2 in
     (compose_subst s1 s2, t2)

let type_inference env e =
  let (s, t) = infer env e in
  type_apply s t

let e0 = Let ((V "id"), Lam (V "x", Var (V "x")), Var (V "id"))

let test ex =
  Typ.to_string (type_inference (Map.empty (module Var)) ex)
