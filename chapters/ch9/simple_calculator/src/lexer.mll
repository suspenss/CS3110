{
  open Parser
}

let whitespace = [' ' 't']+
let digit = ['0'-'9']
let int = '-'?digit+

rule read =
  parse
  | whitespace { read lexbuf }
  | ")" { RPAREN }
  | "(" { LPAREN }
  | "+" { PLUS }
  | "*" { TIMES }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }

