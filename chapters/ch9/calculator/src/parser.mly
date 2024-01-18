%token <int> INT
%token PLUS
%token SUB
%token TIMES
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS SUB
%left TIMES DIV

%type <Ast.expr> prog
%start prog

%%

prog: 
  | e = expr; EOF { e }
  ;

expr:
  | i = INT                       { Int i }
  | el = expr; DIV;   er = expr   { Binop (Div, el, er) }
  | el = expr; TIMES; er = expr   { Binop (Mul, el, er) }
  | el = expr; PLUS;  er = expr   { Binop (Add, el, er) }
  | el = expr; SUB;   er = expr   { Binop (Sub, el, er) }
  | PLUS;   e = expr              { Binop (Add, Int 0, e) }
  | SUB;    e = expr              { Binop (Sub, Int 0, e) }
  | LPAREN; e = expr; RPAREN      { e }
  ;