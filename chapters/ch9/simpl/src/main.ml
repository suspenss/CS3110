open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

(** [typ] represents the type of an expression. *)
type typ =
  | TInt
  | TBool

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the [then] and [else] branches
    of an [if] do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

(** A [Context] is a mapping from variable names to
    types, aka a symbol table, aka a typing environment. *)
module type Context = sig
  (** [t] is the type of a context. *)
  type t

  (** [empty] is the empty context. *)
  val empty : t

  (** [lookup ctx x] gets the binding of [x] in [ctx].
      Raises: [Failure unbound_var_err] if [x] is
      not bound in [ctx]. *)
  val lookup : t -> string -> typ

  (** [extend ctx x ty] is [ctx] extended with a binding
      of [x] to [ty]. *)
  val extend : t -> string -> typ -> t
end

(** The [Context] module implements the [Context] signature
    with an association list. *)
module Context : Context = struct
  type t = (string * typ) list

  let empty = []

  let lookup ctx x =
    try List.assoc x ctx with
    | Not_found -> failwith unbound_var_err
  ;;

  let extend ctx x ty = (x, ty) :: ctx
end

open Context

(** [typeof ctx e] is the type of [e] in context [ctx].
    Raises: [Failure] if [e] is not well typed in [ctx]. *)
let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> lookup ctx x
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3

(** Helper function for [typeof]. *)
and typeof_let ctx x e1 e2 =
  let t1 = typeof ctx e1 in
  let ctx' = extend ctx x t1 in
  typeof ctx' e2

(** Helper function for [typeof]. *)
and typeof_bop ctx bop e1 e2 =
  let t1, t2 = typeof ctx e1, typeof ctx e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt | Mul, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> failwith bop_err

(** Helper function for [typeof]. *)
and typeof_if ctx e1 e2 e3 =
  if typeof ctx e1 = TBool then (
    let t2 = typeof ctx e2 in
    if t2 = typeof ctx e3 then
      t2
    else
      failwith if_branch_err
  ) else
    failwith if_guard_err
;;

(** [typecheck e] checks whether [e] is well typed in
    the empty context. Raises: [Failure] if not. *)
let typecheck e = ignore (typeof empty e)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | _ -> false
;;

(** [subst e v x] is [e{v/x}]. *)
let rec subst (e : expr) (v : expr) (x : string) =
  match e with
  | Int _ | Bool _ -> e
  | Var y ->
    if y = x then
      v
    else
      e
  | Binop (op, e1, e2) -> Binop (op, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
  | Let (y, e1, e2) ->
    if y = x then
      Let (y, subst e1 v x, e2)
    else
      Let (y, subst e1 v x, subst e2 v x)
;;

(** [step] is the [-->] relation, that is, a single step of
    evaluation. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Can't step"
  | Var _ -> failwith unbound_var_err
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
      | _ -> failwith if_guard_err
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
  | _ -> failwith bop_err
;;

(** [eval_small e] is the [e -->* v] relation.  That is,
    keep applying [step] until a value is produced. *)
let rec steps (e : expr) : expr =
  if is_value e then
    e
  else
    e |> step |> steps
;;

(** [interp_small s] interprets [s] by parsing, type-checking,
    and evaluating it with the small-step model. *)
let interp_small (s : string) : expr =
  let e = parse s in
  typecheck e;
  steps e
;;

(* Big step *)

(** [eval_big e] is the [e ==> v] relation. *)
let rec bigstep = function
  | (Int _ | Bool _) as e -> e
  | Var _ -> failwith unbound_var_err
  | Binop (op, e1, e2) -> eval_bop op e1 e2
  | Let (x, e1, e2) -> eval_let x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop op e1 e2 =
  match op, bigstep e1, bigstep e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 =
  match bigstep e1, e2, e3 with
  | Bool true, e1, _ -> bigstep e1
  | Bool false, _, e2 -> bigstep e2
  | _ -> failwith if_guard_err

(** [eval_let x e1 e2] is the [e] such that [let x = e1 in e2 ==> v ]. *)
and eval_let x e1 e2 = subst e2 (bigstep e1) x |> bigstep

(** [interp_big s] interprets [s] by parsing, type-checking,
    and evaluating it with the big-step model. *)
let interp_big (s : string) : expr =
  let e = parse s in
  typecheck e;
  bigstep e
;;
