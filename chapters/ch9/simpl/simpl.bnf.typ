#smallcaps[Stmatics]
$
x  ::= &"<identifiers>" \

i  ::= &"<integers>" \

b ::= &"true" | "false" \

"bop" ::= & + | * | \/ | <= \

e ::= &  i | b \ 
       |& e_1 "bop" e_2 \
       | & "if" e_1 "then" e_2 "else" e_3 \
       | & "let" x = e_1 "in" e_2 \



$
/*
#let e1 = $e_1$
#let bop = math.op("bop")
#let e2 = $e_2$
#let e3 = $e_3$
#let then = math.op[then]
#let eelse = math.op[else]
#let eif = math.op[if]
#let etrue = math.op[true]
#let efalse = math.op[false]

#smallcaps[steps:] \
$
i cancel(-->) \
b cancel(-->) \
x cancel(-->) \
\
e1 bop e2 --> e1' bop e2  \
  "if" e1 --> e1' \
v_1 bop e2 --> v_1 bop e2' \
  "if" e2 --> e2' \
v_1 bop v_2 --> v  \

\

"if" e1 then e2 eelse_ e3 --> eif e1' then e2  eelse e3 \
  eif e1 --> e1' \
eif etrue  then e2 eelse e3 --> e2 \
eif efalse then e2 eelse e3 --> e3 \

\

"let" x = e1 in e2 --> "let" x = e1' in e2  \
  eif e1 --> e1' \
"let" x = v in e2 --> e2{v/x} \
$
*/

#smallcaps[Substitution:] \
$
  i{e\/x} =& i \
  b{e\/x} =& b \
  x{e\/x} =& e \
  y{e\/x} =& y \
  (e_1 "bop" e_2){e\/x} =& e_1{e\/x} "bop" e_2{e\/x} \
  ("if" e_1 "then" e_2 "else" e_3){e\/x} =& "if" e_1{e\/x} "then" e_2{e\/x} "else" e_3{e\/x}  \
  ("let" y = e_1 "in" e_2){e\/x} =& "let" y = e_1{e\/x} "in" e_2{e\/x} \
  ("let" x = e_1 "in" e_2){e\/x} =& "let" x = e_1{e\/x} "in" e_2 quad "(shadowing)"
$