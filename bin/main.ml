open Core
open Diss_implementation

let rec loop num_threads () =
  Out_channel.output_string stdout "> ";
  Out_channel.flush stdout;
  let s = match In_channel.input_line In_channel.stdin with
    | Some x -> x
    | None -> ""
  in
  (match Parsing.attempt s with
   | Some t ->
      (try
         let typeof = Types.Inference.typeof t in
         let t' = Interpreter.interpret num_threads (Environment.create ()) t in
         let str = ((Ast.to_string t') ^ "\n" ^ (Types.Typ.to_string typeof)) in
         
         Out_channel.output_string stdout (str ^ "\n");
       with
       | Types.TypeError x -> Out_channel.output_string stdout (x ^ "\n"))
   | None -> ());
  loop num_threads ()

let run file num_threads =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  match Parser.prog Lexer.read lexbuf with
  | Some t -> 
     (try
        let typeof = Types.Inference.typeof  t in
        let t' = Interpreter.interpret num_threads (Environment.create ()) t in
        let str = ((Ast.to_string t') ^ "\n" ^ (Types.Typ.to_string typeof)) in
        
        Out_channel.output_string stdout (str ^ "\n");
      with
      | Types.TypeError x -> Out_channel.output_string stdout (x ^ "\n"))
  | None -> ()

let command =
  Command.basic
    ~summary:"Programming Language Interpreter"
    (let%map_open.Command number_threads = flag "-t" (optional int) ~doc:" number of threads"
     and filename = anon (maybe ("filename" %: Filename_unix.arg_type)) in
     fun () -> let num_threads = Option.value number_threads ~default:1 in
               match filename with
               | Some x -> run x num_threads
               | None -> loop num_threads ())

let () = Command_unix.run command
