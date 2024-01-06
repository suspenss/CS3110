type expr = 
  | Int      of int
  | Binop of binaryOp * expr * expr

and binaryOp = 
  | Add 
  | Sub
  | Mul