
let attempt s =
  Parser.prog Lexer.read (Lexing.from_string s)
