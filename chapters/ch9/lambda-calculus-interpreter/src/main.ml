open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let unbound_var_err = "unbound variable error"

module Fv = struct
  (** fv *)
  module VarSet = Set.Make (String)

  open VarSet

  let rec fv : expr -> VarSet.t = function
    | Var x -> singleton x
    | App (e1, e2) -> union (fv e1) (fv e2)
    | Fun (x, e) -> diff (fv e) (singleton x)
  ;;

  let is_fv (x : string) (e : expr) : bool = mem x (fv e)
end

(** [gensym ()] is generate a new variable name *)
let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "$x" ^ string_of_int !counter
;;

(** [is_val e] is true if e is a identifier of function *)
let is_val : expr -> bool = function
  | Var _ | Fun _ -> true
  | App _ -> false
;;

(** [replace e older newer] is [e] replace [newer] with [older] *)
let rec replace (e : expr) (older : string) (newer : string) : expr =
  match e with
  | Var x -> if x = older then Var newer else e
  | App (e1, e2) -> App (replace e1 older newer, replace e2 older newer)
  | Fun (x, e') -> Fun ((if x = older then newer else x), replace e' older newer)
;;

(** [subst e v x] is a expression whici substution [x] with [v] *)
let rec subst (e : expr) (v : expr) (x : string) : expr =
  match e with
  | Var y -> if y = x then v else e
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Fun (y, e') ->
    if y = x then
      e
    else if not (Fv.is_fv y v) then
      Fun (y, subst e' v x)
    else (
      let fresh = gensym () in
      let new_body = replace e' y fresh in
      Fun (fresh, subst new_body v x))
;;

(** [step e] is e' which is e evaluate signle step *)
let rec step : expr -> expr = function
  | Var _ | Fun _ -> failwith unbound_var_err
  | App (e1, e2) ->
    (* call by value*)
    if is_val e2 then (
      match e1 with
      | Fun (x, e) -> subst e e2 x
      | App _ -> App (step e1, e2)
      | _ -> failwith "Application only with function")
    else
      App (e1, step e2)
;;

let rec steps (e : expr) : expr =
  if is_val e then
    e
  else
    e |> step |> steps
;;

let interp (s : string) : expr = s |> parse |> steps
