open Base

exception TypeError of string

module Var : sig
  type t
end

module Typ : sig
  type t =
    | Var of Var.t
    | Con of string
    | Arr of t * t
    | Box of t
  [@@deriving equal]

  val to_string : t -> string
end
module Inference : sig
  type t

  val typeof : Ast.t -> Typ.t
end 
