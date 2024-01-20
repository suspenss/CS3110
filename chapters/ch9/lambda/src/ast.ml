type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr

let rec make_apply e = function
  | [] -> failwith "no argument"
  | [ e' ] -> App (e, e')
  | e' :: _ :: _ as t -> make_apply (App (e, e')) t
;;
