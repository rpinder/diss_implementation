%{
open Interpreter
%}

%token <int> NATLIT
%token <string> ID
%token NAT
%token COLON
%token ARROW
%token SLASH
%token DOT
%token EOF
%token BOPEN
%token BCLOSE
%token LET
%token EQUAL
%token IN
%start <Terms.t option> prog
%nonassoc NATLIT ID BOPEN
%right SLASH DOT LET IN
%left Application
%%

prog:
  | EOF { None }
  | t = term EOF { Some t }
  ;

types:
  | NAT; ARROW; t2 = types { Typ.Arr (Typ.Nat,t2) }
  | NAT { Interpreter.Typ.Nat }
  ; 

term:
  | i = NATLIT { Terms.Nat (empty_info, i) }
  | s = ID { Terms.Var (empty_info, s) }
  | SLASH; arg = ID; COLON; arg_type = types; DOT; body = term { Terms.Abs (empty_info, arg, arg_type, body) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Terms.Let (empty_info, s, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | t1 = term; t2 = term; { Terms.App (empty_info, t1, t2)} %prec Application
  ;
