%{
open Interpreter
%}

%token <int> INTLIT
%token <string> ID
%token INT
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
%token LESSTHAN
%token GREATERTHAN
%token GTEQ
%token LTEQ
%token NOTEQUALITY
%token REC

%token APP

%right IN
%nonassoc ELSE
%left EQUALITY NOTEQUALITY
%left LESSTHAN GREATERTHAN GTEQ LTEQ
%left PLUS MINUS
%left MULTIPLY
%nonassoc INTLIT TRUE FALSE ID SLASH LET BOPEN IF DOT
%nonassoc APP

%start <Terms.t option> prog

%type <Terms.t> term
%type <Typ.t> typ
%type <Typ.t> types

%%

prog:
  | EOF { None }
  | t = term EOF { Some t }
  ;

typ:
  | INT { Typ.Int }
  | BOOL { Typ.Bool }

types:
  | t = typ { t }
  | t1 = typ; ARROW; t2 = types { Typ.Arr (t1,t2) }

%inline binop:
  | PLUS { Terms.Plus }
  | MINUS { Terms.Minus }
  | MULTIPLY { Terms.Multiply }
  | LESSTHAN { Terms.LessThan }
  | GREATERTHAN { Terms.GreaterThan }
  | LTEQ { Terms.LTEQ }
  | GTEQ { Terms.GTEQ }

term:
  | t1 = term; op = binop; t2 = term { Terms.BinOp (empty_info, t1, t2, op)}
  | i = INTLIT { Terms.Int (empty_info, i) }
  | t1 = term; EQUALITY; t2 = term { Terms.Eq (empty_info, t1, t2)}
  | t1 = term; NOTEQUALITY; t2 = term { Terms.NEq (empty_info, t1, t2)}
  | TRUE { Terms.Bool (empty_info, true) }
  | FALSE { Terms.Bool (empty_info, false) }
  | s = ID { Terms.Var (empty_info, s) }
  | SLASH; arg = ID; COLON; arg_type = types; DOT; body = term { Terms.Abs (empty_info, arg, arg_type, body) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Terms.Let (empty_info, s, t1, t2)}
  | LET; REC; s = ID; COLON; typ = types; EQUAL; t1 = term; IN; t2 = term { Terms.LetRec (empty_info, s, typ, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | IF; b = term; THEN; t1 = term; ELSE; t2 = term { Terms.If (empty_info, b, t1, t2)}
  | t1 = term; t2 = term %prec APP { Terms.App (empty_info, t1, t2)}
  ;


