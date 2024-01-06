open Ast

(* [parse s] is parse s to Ast *)
let parse : string -> expr = fun s ->
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

(* [string_of_val e] convert expr [e] to string *)
let string_of_val : expr -> string = function
  | Int i -> string_of_int i
  | Binop _ ->  failwith "Invaild token"
;;

let is_val : expr -> bool = function
  | Int _ -> true
  | Binop _ -> false
;;

let rec step : expr -> expr = function
  | Int _i -> failwith "todo"
  | Binop (_, el, er) as e when is_val el && is_val er -> step_binop e
  | Binop (op, el, er) when is_val el -> Binop (op, el, step er) 
  | Binop (op, el, er) -> Binop (op, step el, er)

and step_binop = function
  | Binop (Add, Int a, Int b) -> Int (a + b)
  | Binop (Mul, Int a, Int b) -> Int (a * b)
  | _ -> failwith ""
;;


(* [eval e] evaluate expr [e] to a value *)
let rec eval : expr -> expr = fun e -> 
  if is_val e then e else e |> step |> eval
;;

(* [interp s] *)
let interp : string -> string = fun s -> 
  s |> parse |> eval |> string_of_val
;;