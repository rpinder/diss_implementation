open Core
open Diss_implementation

let rec loop () =
  Out_channel.output_string stdout "> ";
  Out_channel.flush stdout;
  let s = match In_channel.input_line In_channel.stdin with
    | Some x -> x
    | None -> ""
  in
  (match Parsing.attempt s with
  | Some t ->
     let typeof = Interpreter.typeof (Environment.create ()) t in
     let t' = Interpreter.eval (Environment.create ()) t in
     let str = ((Interpreter.Terms.to_string t') ^ "\n" ^ (Interpreter.Typ.to_string typeof)) in
     
     Out_channel.output_string stdout (str ^ "\n");
  | None -> ());
  loop ()

let run file =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  match Parser.prog Lexer.read lexbuf with
  | Some t -> 
     let typeof = Interpreter.typeof (Environment.create ()) t in
     let t' = Interpreter.eval (Environment.create ()) t in
     let str = ((Interpreter.Terms.to_string t') ^ "\n" ^ (Interpreter.Typ.to_string typeof)) in
     
     Out_channel.output_string stdout (str ^ "\n");
  | None -> ()

let command =
  Command.basic
    ~summary:"Programming Language Interpreter"
    Command.Param.(
    map
      (anon (maybe ("filename" %: Filename_unix.arg_type)))
      ~f:(fun filename () -> match filename with
                             | Some x -> run x
                             | None -> loop ()))

let () = Command_unix.run command
