{
open Lexing
open Parser
open Core

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let intlit = ['0'-'9'] ['0'-'9']*
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | intlit { INTLIT (int_of_string (lexeme lexbuf)) }
  | "fn" { FN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "int" { INT }
  | "bool" { BOOL }
  | "box" { BOX }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "nil" { NIL }
  | "list" { LIST }
  | "case" { CASE }
  | "of" { OF }
  | "::" { CONS }
  | "|" { PIPE }
  | "==" { EQUALITY }
  | "!=" { NOTEQUALITY }
  | ">" { GREATERTHAN }
  | "<" { LESSTHAN }
  | ">=" { GTEQ }
  | "<=" { LTEQ }
  | "=" { EQUAL }
  | id { ID (lexeme lexbuf) }
  | ":" { COLON }
  | "->" { ARROW }
  | "<-" { BACKARROW }
  | "\\" { SLASH }
  | "." { DOT }
  | "(" { BOPEN }
  | ")" { BCLOSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLY }
  | "(*" { comment lexbuf; read lexbuf }
  | _ { raise (SyntaxError ("Unexpected token")) }
  | eof { EOF }
and comment =
  parse
  | "*)" { () }
  | _ { comment lexbuf }
  | eof { () }