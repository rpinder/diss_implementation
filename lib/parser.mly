%{
open Ast
open Types

let rec unroll args body =
  match args with
  | [] -> body
  | x :: xs -> Ast.Abs (empty_info, x, unroll xs body)

%}

%token <int> INTLIT
%token <string> ID
%token INT
%token BOOL
%token LIST
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
%token BOX
%token BACKARROW
%token FN
%token NIL
%token CONS
%token CASE
%token OF
%token PIPE

%token APP

%right ARROW
%nonassoc DOT
%right IN PIPE
%nonassoc ELSE
%left EQUALITY NOTEQUALITY
%left LESSTHAN GREATERTHAN GTEQ LTEQ
%nonassoc NIL
%nonassoc LIST
%right CONS
%nonassoc BOX
%left PLUS MINUS
%left MULTIPLY
%nonassoc INTLIT TRUE FALSE ID SLASH LET BOPEN IF CASE
%nonassoc APP

%start <(string * Ast.t * Typ.t) list> prog
%start <Ast.t option> single

%type <Ast.t> term
%type <Typ.t> typ
%type <Typ.t> types
%type <string * Ast.t * Typ.t> declaration
%type <(string * Ast.t * Typ.t) list> list(declaration)
%type <string list> list(ID)

%%

prog:
  | defns = list(declaration); EOF { defns }
  ;

single:
  | EOF { None }
  | t = term; EOF { Some t }

declaration:
  | FN; s = ID; args = list(ID); COLON; t = types; EQUAL; body = term { (s, unroll args body, t) }


typ:
  | INT { Typ.Con "int" }
  | BOOL { Typ.Con "bool" }
  | s = ID { Typ.Var (V s) }

types:
  | t = typ { t }
  | t1 = types; ARROW; t2 = types { Typ.Arr (t1,t2) }
  | BOX; t = types { Typ.Box t } 
  | LIST; t = types { Typ.List t } 
  | BOPEN; t = types; BCLOSE { t }

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
  | BOX; t1 = term { Ast.Box (empty_info, t1) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Ast.Let (empty_info, s, t1, t2)}
  | LET; REC; s = ID; EQUAL; t1 = term; IN; t2 = term { Ast.LetRec (empty_info, s, t1, t2)}
  | LET; BOX; s = ID; BACKARROW; t1 = term; IN; t2 = term { Ast.LetBox (empty_info, s, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | IF; b = term; THEN; t1 = term; ELSE; t2 = term { Ast.If (empty_info, b, t1, t2)}
  | NIL { Ast.Nil }
  | t1 = term; CONS; t2 = term { Ast.Cons (empty_info, t1, t2) }
  | CASE; t1 = term; OF; PIPE; t2 = term; PIPE t3 = term { Ast.Case (empty_info, t1, t2, t3)}
  | t1 = term; t2 = term %prec APP { Ast.App (empty_info, t1, t2)}
  ;


