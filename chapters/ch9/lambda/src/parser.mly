%{
open Ast
%}

%token <string> ID
%token LPAREN RPAREN ARROW FUN EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | e = simple_expr { e }
  | e = simple_expr; es = simple_expr+ { make_apply e es }
  | FUN; x = ID; ARROW; e = expr { Fun (x, e) }


simple_expr:
  | x = ID { Var x }
  | LPAREN; e = expr; RPAREN { e }
  ;