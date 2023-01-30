open Core

let rec loop () =
  Out_channel.output_string stdout "> ";
  Out_channel.flush stdout;
  let s = match In_channel.input_line In_channel.stdin with
    | Some x -> x
    | None -> failwith "):"
  in
  let t = match Diss_implementation.Parsing.attempt s with
    | Some x -> x
    | None -> failwith ":("
  in
  let t' = Diss_implementation.Interpreter.eval (Diss_implementation.Environment.create ()) t in
  let typeof = Diss_implementation.Interpreter.typeof (Diss_implementation.Environment.create ()) t' in
  let str = ((Diss_implementation.Interpreter.Terms.to_string t') ^ "\n" ^ (Diss_implementation.Interpreter.Typ.to_string typeof)) in
  
  Out_channel.output_string stdout (str ^ "\n");
  loop ()

let () = loop ()
