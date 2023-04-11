%{
open Ast
open Types

let rec unroll args body =
  match args with
  | [] -> body
  | x :: xs -> Ast.IAbs (empty_info, x, unroll xs body)

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
%left PLUS
%left MULTIPLY MINUS
%nonassoc INTLIT TRUE FALSE ID SLASH LET BOPEN IF CASE
%nonassoc APP

%start <(string * Ast.infoAST * Typ.t) list> prog
%start <Ast.infoAST option> single

%type <Ast.infoAST> term
%type <Typ.t> typ
%type <Typ.t> types
%type <string * Ast.infoAST * Typ.t> declaration
%type <(string * Ast.infoAST * Typ.t) list> list(declaration)
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
  | t1 = term; op = binop; t2 = term { Ast.IBinOp (empty_info, t1, t2, op)}
  | i = INTLIT { Ast.IInt (empty_info, i) }
  | t1 = term; EQUALITY; t2 = term { Ast.IEq (empty_info, t1, t2)}
  | t1 = term; NOTEQUALITY; t2 = term { Ast.INEq (empty_info, t1, t2)}
  | TRUE { Ast.IBool (empty_info, true) }
  | FALSE { Ast.IBool (empty_info, false) }
  | s = ID { Ast.IVar (empty_info, s) }
  | SLASH; arg = ID; DOT; body = term { Ast.IAbs (empty_info, arg, body) }
  | BOX; t1 = term { Ast.IBox (empty_info, t1) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Ast.ILet (empty_info, s, t1, t2)}
  | LET; BOX; s = ID; BACKARROW; t1 = term; IN; t2 = term { Ast.ILetBox (empty_info, s, t1, t2)}
  | BOPEN; t = term; BCLOSE { t }
  | IF; b = term; THEN; t1 = term; ELSE; t2 = term { Ast.IIf (empty_info, b, t1, t2)}
  | NIL { Ast.INil empty_info }
  | t1 = term; CONS; t2 = term { Ast.ICons (empty_info, t1, t2) }
  | CASE; t1 = term; OF; PIPE; t2 = term; PIPE t3 = term { Ast.ICase (empty_info, t1, t2, t3)}
  | t1 = term; t2 = term %prec APP { Ast.IApp (empty_info, t1, t2)}
  ;


