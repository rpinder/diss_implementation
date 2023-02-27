 open Lexer 

let attempt s =
  try
    Parser.single Lexer.read (Lexing.from_string s)
  with
  | SyntaxError msg -> Printf.printf "Syntax Error: %s\n" msg; None
  | _ -> print_endline "Parsing Error (make a better error)"; None
