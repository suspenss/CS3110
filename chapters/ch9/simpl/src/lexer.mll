{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
    parse (* here is keyword _*parse*_ *)
    | white { read lexbuf }
    | "True" { TRUE }
    | "False" { FALSE }
    | "<=" { LEQ }
    | "*" { TIMES }
    | "+" { PLUS }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "let" { LET }
    | "=" { EQUALS }
    | "in" { IN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | id { ID (Lexing.lexeme lexbuf) }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }

(*
    let x = 12 in x + 2
    <LET> <VAR: x> <EQUALS> <INT 12> <IN> <EXPR< <VAR: x> <PLUS> <INT 2> >>
 *)