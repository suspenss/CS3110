(* twice, no arguments *)
let twice f x = f (f x)
let double x = x * 2
let quad = twice double
(* the quad function is take no arguments but in function body
   it provide a function [double] to [twice].
   the [twice] take a one argument function as argument, and returns a function
   which do twice the argument function, and the type is same as the argument *)

(* mystery operator 1 *)

let ( $ ) f x = f x
(* the operatoe apply the second argument as the argument with first function
   argument. *)

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f
(* the syntax with the opt [@@] is f1 @@ f2 that produce a function which
   take a argument and do f2 x then do f1 x *)

(* repeat *)
let rec repeat f n x =
  match n with
  | t when t <= 0 -> t
  | t -> repeat f (t - 1) (f x)
;;

(* product *)

let product_left = List.fold_left ( *. ) 1.0
let product_right lst = List.fold_right ( *. ) lst 1.0

(* sum_cube_odd *)
let rec ( -- ) i n = if i > n then [] else i :: (i + 1 -- n)

let sum_cube_odd lst =
  List.fold_left
    (fun acc x -> (x * x * x * x) + acc)
    0
    (List.filter (fun x -> x mod 2 <> 0) lst)
;;

let sco_pipeline lst =
  lst
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.fold_left (fun acc x -> (x * x * x * x) + acc) 0
;;

(* exists *)
let rec exists f = function
  | [] -> false
  | x :: xs -> f x || exists f xs
;;

let exists' f = List.fold_left (fun acc x -> f x || acc) false
let exists'' = List.exists

(* library function uncurry *)
let uncurry_append (lst1, lst2) = List.append lst1 lst2
let ucr_cmp (lhs, rhs) = Char.compare lhs rhs
let ucr_max (lhs, rhs) = Stdlib.max lhs rhs

(* map composition *)
let map_composition f g lst =
  List.fold_left (fun acc x -> f x :: acc) [] (List.map g lst)
;;

(* more list fun
   - Find those elements of a list of strings
     whose length is strictly greater than 3.

   - Add 1.0 to every element of a list of floats.

   - Given a list of strings strs and another string sep,
     produce the string that contains every element of
     strs separated by sep. For example, given inputs
     ["hi";"bye"] and ",", produce "hi,bye", being sure
     not to produce an extra comma either at the beginning
     or end of the result string.
*)
let filter_str_gtr3 = List.filter (fun x -> String.length x > 3)
let addone = List.map (( +. ) 1.)

let join sep = function
  | [] -> ""
  | x :: xs -> List.fold_left (fun acc x -> acc ^ sep ^ x) x xs
;;

(* association list keys *)
let unique_keys lst = List.sort_uniq compare (List.map fst lst)

(* Unit test below function !!!  todo !!! *)
(*  valid matrix *)
let is_valid_matrix mat =
  let fst_vec_len = mat |> List.hd |> List.length in
  List.for_all (fun vec -> List.length vec = fst_vec_len) mat
;;

(*  valid matrix *)
let is_valid_matrix' = function
  | vec :: vecs ->
    let len = List.length vec in
    List.for_all (fun v -> List.length v = len) vecs
  | _ -> false
;;

(* row vector add *)
let row_vec_add lst1 lst2 = List.map2 ( + ) lst1 lst2

(* matrix add *)
let get_row_size m1 = List.length (List.hd m1)
let get_col_size m1 = List.length m1
let get_row_col_size m1 = get_row_size m1, get_col_size m1
let same_size m1 m2 = get_row_col_size m1 = get_row_col_size m2

let mat_addition mat1 mat2 =
  match mat1, mat2 with
  | m1, m2 when not (same_size m1 m2) -> raise (Failure " ")
  | m1, m2 -> List.map2 row_vec_add m1 m2
;;

(* matrix multiply *)
(* 1. transform matrix
   {v
    [[1; 2; 3]        [[1; 4; 7]       
    ;[4; 5; 6]   ->   ;[2; 5; 8]         
    ;[7; 8; 9]]       ;[3; 6; 9]]         
   v}
*)
let rec transpose = function
  | [] | [] :: _ -> []
  | lst -> List.map List.hd lst :: transpose (List.map List.tl lst)
;;

let dot v1 v2 = List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2

let mul_matrix m1 m2 =
  match m1, transpose m2 with
  | x, y when get_col_size x <> get_col_size y -> raise (failwith "")
  | x, y -> List.map (fun row -> List.map (dot row) y) x
;;
