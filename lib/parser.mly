%{
open Ast
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

%start <t option> prog

%type <t> term

%%

prog:
  | EOF { None }
  | t = term EOF { Some t }
  ;

/*typ:
  | INT { Typ.Int }
  | BOOL { Typ.Bool }

types:
  | t = typ { t }
  | t1 = typ; ARROW; t2 = types { Typ.Arr (t1,t2) }*/

%inline binop:
  | PLUS { Ast.Plus }
  | MINUS { Ast.Minus }
  | MULTIPLY { Ast.Multiply }
  | LESSTHAN { Ast.LessThan }
  | GREATERTHAN { Ast.GreaterThan }
  | LTEQ { Ast.LTEQ }
  | GTEQ { Ast.GTEQ }

term:
  | t1 = term; op = binop; t2 = term { Ast.BinOp (empty_info, t1, t2, op)}
  | i = INTLIT { Ast.Int (empty_info, i) }
  | t1 = term; EQUALITY; t2 = term { Ast.Eq (empty_info, t1, t2)}
  | t1 = term; NOTEQUALITY; t2 = term { Ast.NEq (empty_info, t1, t2)}
  | TRUE { Ast.Bool (empty_info, true) }
  | FALSE { Ast.Bool (empty_info, false) }
  | s = ID { Ast.Var (empty_info, s) }
  | SLASH; arg = ID; DOT; body = term { Ast.Abs (empty_info, arg, body) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Ast.Let (empty_info, s, t1, t2)}
  | LET; REC; s = ID; EQUAL; t1 = term; IN; t2 = term { Ast.LetRec (empty_info, s, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | IF; b = term; THEN; t1 = term; ELSE; t2 = term { Ast.If (empty_info, b, t1, t2)}
  | t1 = term; t2 = term %prec APP { Ast.App (empty_info, t1, t2)}
  ;


