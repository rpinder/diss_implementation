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
%start <Interpreter.Terms.t option> prog

%left Application
%%

prog:
  | EOF { None }
  | t = term { Some t }
  ;

types:
  | NAT; ARROW; t2 = types { Interpreter.Typ.Arr (Interpreter.Typ.Nat,t2) }
  | NAT { Interpreter.Typ.Nat }
  ; 

term:
  | i = NATLIT { Interpreter.Terms.Nat (Interpreter.empty_info, i) }
  | s = ID { Interpreter.Terms.Var (Interpreter.empty_info, s) }
  | SLASH; arg = ID; COLON; arg_type = types; DOT; body = term { Interpreter.Terms.Abs (Interpreter.empty_info, arg, arg_type, body, (Environment.create ())) }
  | LET; s = ID; EQUAL; t1 = term; IN; t2 = term { Interpreter.Terms.Let (Interpreter.empty_info, s, t1, t2)}
  /*| t1 = term; t2 = term { Interpreter.Terms.App (Interpreter.empty_info, t1, t2) } %prec Application*/
  ;
