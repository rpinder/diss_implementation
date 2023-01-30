{
open Lexing
open Parser
open Core

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let natlit = ['0'-'9'] ['0'-'9']*
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | natlit { NATLIT (int_of_string (lexeme lexbuf)) }
  | "nat" { NAT }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQUAL }
  | id { ID (lexeme lexbuf) }
  | ":" { COLON }
  | "->" { ARROW }
  | "\\" { SLASH }
  | "." { DOT }
  | "(" { BOPEN }
  | ")" { BCLOSE }
  | _ { raise (SyntaxError ("Unexpected token")) }
  | eof { EOF }