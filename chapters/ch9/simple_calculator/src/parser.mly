%token <int> INT
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left TIMES

%type <Ast.expr> prog
%start prog

%%

prog: 
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | el = expr; TIMES; er = expr { Binop (Mul, el, er) }
  | el = expr; PLUS; er = expr { Binop (Add, el, er) }
  | LPAREN; e = expr; RPAREN { e }
  ;