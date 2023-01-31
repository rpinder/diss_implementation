%{
open Interpreter
%}

%token <int> NATLIT
%token <string> ID
%token NAT
%token BOOL
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
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token PLUS
%token MINUS
%token MULTIPLY
%token EQUALITY
%start <Terms.t option> prog
%nonassoc NATLIT ID BOPEN
%right SLASH DOT LET IN IF ELSE
%left Application
%%

prog:
  | EOF { None }
  | t = term EOF { Some t }
  ;

typ:
  | NAT { Typ.Nat }
  | BOOL { Typ.Bool }

types:
  | t = typ { t }
  | t1 = typ; ARROW; t2 = types { Typ.Arr (t1,t2) }

binop:
  | PLUS { Terms.Plus }
  | MINUS { Terms.Minus }
  | MULTIPLY { Terms.Multiply }

term:
  | i = NATLIT { Terms.Nat (empty_info, i) }
  | t1 = term; EQUALITY; t2 = term { Terms.Eq (empty_info, t1, t2)}
  | t1 = term; op = binop; t2 = term { Terms.BinOp (empty_info, t1, t2, op)}
  | TRUE { Terms.Bool (empty_info, true) }
  | FALSE { Terms.Bool (empty_info, false) }
  | s = ID { Terms.Var (empty_info, s) }
  | SLASH; arg = ID; COLON; arg_type = types; DOT; body = term { Terms.Abs (empty_info, arg, arg_type, body) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Terms.Let (empty_info, s, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | IF; b = term; THEN; t1 = term; ELSE; t2 = term { Terms.If (empty_info, b, t1, t2)}
  | t1 = term; t2 = term; { Terms.App (empty_info, t1, t2)} %prec Application
  ;
