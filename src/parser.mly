%{
open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE FALSE
%token LEQ PLUS TIMES
%token LET EQUALS IN
%token IF THEN ELSE
%token LPAREN RPAREN
%token EOF

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | x = ID { Var x }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e = expr; RPAREN { e }
