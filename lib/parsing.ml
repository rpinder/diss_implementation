
let attempt s =
  try
    Parser.prog Lexer.read (Lexing.from_string s)
  with
  | _ -> print_endline "Parsing Error (make a better error)"; None
