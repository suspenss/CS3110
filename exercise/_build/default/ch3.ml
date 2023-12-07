(* Exercise: list expressions 
   let e1_list = [ 1; 2; 3; 4; 5 ];
   let e1_list = 1 :: 2 :: 3 :: 4 :: 5 :: []; 
   let e1_list = [1; 2] @ [3; 4; 5];
*)

(* product *)
let rec product = function
  | [] -> 1
  | x :: xs -> x * product xs
;;

(* concat *)
let rec concat = function
  | [] -> ""
  | x :: xs -> x ^ concat xs
;;

(* patterns *)
let fst_string_is_bigred = function
  | [] -> false
  | x :: _ -> x = "bigred"
;;

let len_is_two_or_four lst =
  let rec aux acc = function
    | [] -> acc = 2 || acc = 4
    | _ :: xs -> aux (acc + 1) xs
  in
  aux 0 lst
;;

let fst_eq_snd_elem = function
  | [ _ ] | [] -> false
  | x :: y :: _ -> x = y
;;

(* library *)
let fifth_element lst =
  match List.length lst with
  | x when x >= 5 -> List.nth lst 4
  | _ -> 0
;;

let sort_by_desecnding lst = lst |> List.sort Stdlib.compare |> List.rev

(*  library puzzle *)
let last_element lst = List.nth lst (List.length lst - 1)
let any_zeroes lst = List.mem 0 lst

(* take drop *)
let rec take num lst =
  match num, lst with
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs
;;

let rec drop num list =
  match num, list with
  | 0, xs -> xs
  | _, [] -> []
  | n, _ :: xs -> drop (n - 1) xs
;;

(* take drop tail *)
let take_tr num lst =
  let rec aux acc num lst =
    match num, lst with
    | 0, _ | _, [] -> List.rev acc
    | n, x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] num lst
;;

(* drop is already tail recursion *)

(* unimodal *)

let is_unimodal lst =
  let rec continous = function
    | lst when List.length lst <= 1 -> List.length lst
    | x :: (y :: _ as lst) when x <= y -> 1 + continous lst
    | _ -> 1
  in
  continous lst + continous (List.rev lst) - 1 = List.length lst
;;
