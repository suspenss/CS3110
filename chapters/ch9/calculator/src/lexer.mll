{
  open Parser
}

let digit      = ['0'-'9']
let int        = digit+
let whitespace = [' ' 't']+

rule read =
  parse
  | whitespace { read lexbuf }
  | "-" { SUB    }
  | ")" { RPAREN }
  | "(" { LPAREN }
  | "+" { PLUS   }
  | "*" { TIMES  }
  | "/" { DIV    }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF    }

