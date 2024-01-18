open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | _ -> false
;;

(** [subst e v x] is [e{v/x}]. *)
let subst _ _ _ = failwith "See next section"

(** [step] is the [-->] relation, that is, a single step of
    evaluation. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Can't step"
  | Var _ -> failwith "Unbound variable"
  (* Binary operation expression *)
  | Binop (op, e1, e2) ->
    if is_value e1 && is_value e2 then
      step_binop (op, e1, e2)
    else if is_value e1 then
      Binop (op, e1, step e2)
    else
      Binop (op, step e1, e2)
  (* If expression *)
  | If (e1, e2, e3) ->
    if is_value e1 then (
      match e1 with
      | Bool true -> e2
      | Bool false -> e2
      | _ -> failwith "Error type, gurd must be bool type"
    ) else
      If (step e1, e2, e3)
    (* Let expression *)
  | Let (x, e1, e2) ->
    if is_value e1 then
      subst e2 e1 x
    else
      Let (x, step e1, e2)

and step_binop : bop * expr * expr -> expr = function
  | Add, Int a, Int b -> Int (a + b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"
;;
