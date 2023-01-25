open Base

type 'a t =
  { values : (string, 'a) Hashtbl.t
  ; enclosing : 'a t option
  }

let create () =
  let values = Hashtbl.create (module String) in
  let enclosing = None in
  { values; enclosing }

let createWithEnclosing env = 
  let values = Hashtbl.create (module String) in
  let enclosing = Some env in
  { values; enclosing }

let rec get t str =
  match Hashtbl.find t.values str with
  | Some x -> Some x
  | None -> match t.enclosing with
            | Some x -> get x str
            | None -> None

let define t str term =
  Hashtbl.set t.values ~key:str ~data:term
