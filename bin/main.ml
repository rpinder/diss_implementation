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
         let t' = Interpreter.interpret num_threads (Environment.create ()) (Ast.convert t) in
         let str = ((Ast.to_string t') ^ "\n" ^ (Types.Typ.to_string typeof)) in
         
         Out_channel.output_string stdout (str ^ "\n");
       with
       | Types.TypeError x -> Out_channel.output_string stdout (x ^ "\n"))
   | None -> ());
  loop num_threads ()

let run file num_threads godel =
  let transform_func = if godel then Godel.transform else Ast.convert in
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  let decls = 
    try
      Parser.prog Lexer.read lexbuf
    with
    | Parser.Error ->
       let str = sprintf "Syntax Error on line %d at char %d\n" lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_bol in
       Out_channel.output_string stdout str;
       exit 1
  in
  let env = Environment.create () in
  (if godel then
     List.iter decls ~f:(fun (name, term, _) ->
         let term' = (match transform_func term with
                      | Ast.Box x -> x
                      | x -> Ast.LetBox ("'res", x, Ast.Var "'res"))
         in
         Environment.define env name ((term')))
   else 
     List.iter decls ~f:(fun (name, term, _) -> Environment.define env name (transform_func term)));
  let main = match Environment.get env "main" with
    | Some x -> x
    | None ->  failwith "No main function"
  in
  try
    let typeof = Types.Inference.typecheck_program decls in
    let t' =
      (* if godel then *)
        (* Interpreter.interpret num_threads env (Ast.LetBox ("'res", main, Ast.Var "'res")) *)
      (* else *)
        Interpreter.interpret num_threads env main
    in
    let str = ((Ast.to_string t') ^ "\n" ^ (Types.Typ.to_string typeof)) in
    
    Out_channel.output_string stdout (str ^ "\n");
  with
  | Types.TypeError x -> Out_channel.output_string stdout (x ^ "\n")


let command =
  Command.basic
    ~summary:"Programming Language Interpreter"
    (let%map_open.Command number_threads = flag "-t" (optional int) ~doc:" number of threads"
     and godel = flag "-g" no_arg ~doc:" Use the Gödel translation"
     and filename = anon (maybe ("filename" %: Filename_unix.arg_type)) in
     fun () -> let num_threads = Option.value number_threads ~default:1 in
               match filename with
               | Some x -> run x num_threads godel
               | None -> loop num_threads ())

let () = Command_unix.run command
