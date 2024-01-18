type binaryOp =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | Int of int
  | Binop of binaryOp * expr * expr
