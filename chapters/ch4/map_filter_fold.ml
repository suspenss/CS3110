(* Map *)

let rec map' f = function
  | [] -> []
  | x :: xs -> f x :: map' f xs
;;

let add1 = map' (fun x -> x + 1)
let concat_bang = map' (fun x -> x ^ "!")
let a = map' (fun x -> x + 1) [ 1; 2; 3 ]

let rec map f = function
  | [] -> []
  | x :: xs ->
    let x' = f x in
    x' :: map f xs
;;

let map_tr f lst =
  let rec aux acc f = function
    | [] -> List.rev acc
    | x :: xs -> aux (f x :: acc) f xs
  in
  aux [] f lst
;;

(* filter *)
let rec filter f = function
  | [] -> []
  | x :: xs -> if f x then x :: filter f xs else filter f xs
;;

let even x = x mod 2 = 0
let odd x = x mod 2 <> 0
let evens = filter even
let odds = filter odd

let filter_tr f lst =
  let rec aux acc f = function
    | [] -> List.rev acc
    | x :: xs -> aux (if f x then x :: acc else acc) f xs
  in
  aux [] f lst
;;

(* fold or combina *)
let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let rec concat = function
  | [] -> ""
  | x :: xs -> x ^ concat xs
;;

let rec combina init f = function
  | [] -> init
  | x :: xs -> f x (combina init f xs)
;;

let rec combina_tr acc f = function
  | [] -> acc
  | x :: xs -> combina_tr (f acc x) f xs
;;

let foldl = List.fold_left

(* foldl f acc [a; b; c] -> (f (f (f acc a) b) c) *)
(* foldr -> (f a (f b (f c acc))) *)
(* ok, i think the foldr's tail recursion version is foldl *)

(* the foldr struct equate with combina *)
let rec foldr f lst acc =
  match lst with
  | [] -> acc
  | x :: xs -> f x (foldr f xs acc)
;;

(* the foldl struct equate with combina_tr *)
let rec foldl f acc lst =
  match lst with
  | [] -> acc
  | x :: xs -> foldl f (f acc x) xs
;;

(* Using Fold to Implement Other Functions *)

let length lst = List.fold_left (fun acc _ -> acc + 1) 0 lst
let rev lst = List.fold_left (fun acc x -> x :: acc) [] lst
let map_fold f lst = List.fold_right (fun x acc -> f x :: acc) lst []

let filter_fold f lst =
  List.fold_right (fun x acc -> if f x then x :: acc else acc) lst []
;;

(* fold version will process all element of the list *)
let list_and lst = List.fold_left (fun acc x -> x && acc) true lst
let list_any lst = List.fold_left (fun acc x -> x || acc) false lst

(* lib version *)
let list_and' lst = List.for_all Fun.id lst
let list_any' lst = List.exists Fun.id lst
