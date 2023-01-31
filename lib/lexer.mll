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
  | "true" { TRUE }
  | "false" { FALSE }
  | "nat" { NAT }
  | "bool" { BOOL }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "==" { EQUALITY }
  | "=" { EQUAL }
  | id { ID (lexeme lexbuf) }
  | ":" { COLON }
  | "->" { ARROW }
  | "\\" { SLASH }
  | "." { DOT }
  | "(" { BOPEN }
  | ")" { BCLOSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLY }
  | _ { raise (SyntaxError ("Unexpected token")) }
  | eof { EOF }