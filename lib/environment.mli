(* Environment interface *)

type 'a t

val create : unit -> 'a t 

val createWithEnclosing : 'a t -> 'a t

val get : 'a t -> string -> 'a option

val define : 'a t -> string -> 'a -> unit
