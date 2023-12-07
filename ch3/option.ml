(********************************************
  Option implentation by hand
  ********************************************)

(* maybe type definition, has polymorphic paramater ['a]
   represent it has a value which type is 'a or has Nothing *)
type 'a maybe =
  | Nothing
  | Just of 'a

let take = function
  | Nothing -> failwith "Nothing"
  | Just x -> x
;;

let take_or_use default = function
  | Nothing -> default
  | Just x -> x
;;

(* eg function *)
let rec list_max = function
  | [] -> Nothing
  | [ x ] -> Just x
  | x :: xs -> Just (list_max xs |> take |> max x)
;;

let x = list_max [ 1; 2 ]

(* list_max option implementation *)
let rec list_max' = function
  | [] -> None
  | h :: t ->
    (match list_max' t with
     | None -> Some h
     | Some m -> Some (max h m))
;;
