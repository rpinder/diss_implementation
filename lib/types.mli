open Base

exception TypeError of string

module Var : sig
  type t = V of string
end

module Typ : sig
  type t =
    | Var of Var.t
    | Con of string
    | Arr of t * t
    | Box of t
    | List of t
  [@@deriving equal]

  val to_string : t -> string
end
module Inference : sig
  type t

  val typeof : Ast.t -> Typ.t
  val typecheck_program : (string * Ast.t * Typ.t) list -> Typ.t
end 
