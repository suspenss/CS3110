(** @author epoche *)

let x : string = "3310"
let inc x = x + 1

(** [even n] is whether [n] is even.
    Requires: [n >= 0]. *)
let rec even n = n = 0 || odd (n - 1)

(* * [odd n] is whether [n] is odd.
   Requires: [n >= 0]. *)
and odd n = n <> 0 && even (n - 1)

(** [count] is count [n] times naive version.
    Requires: [n >= 0].*)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

(** [count_aux] is count the [n] tail recursion version.
    Requires: [n]. *)
let rec count_aux n acc = if n = 0 then acc else count_aux (n - 1) (acc + 1)

let count n = count_aux n 0

(* [fact n] is [n] factorial *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)

(* change fact function to tail recursion version *)
let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (acc * n)
let fact_tr n = fact_aux n 1

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let rec length = function
  | [] -> 0
  | _ :: xs -> 1 + length xs
;;

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | x :: xs -> x :: append xs lst2
;;

let inc_list = function
  | [] -> []
  | x :: xs -> (x + 1) :: xs
;;

let head_list lst =
  match lst with
  | [] -> []
  | x :: xs -> x
;;

let rec sum_tl_aux acc = function
  | [] -> acc
  | x :: xs -> sum_tl_aux (acc + x) xs
;;

let rec sum_tl lst = sum_tl_aux 0 lst

and sum_tl_aux acc = function
  | [] -> acc
  | x :: xs -> sum_tl_aux (acc + x) xs
;;

(** operator [--] return a continuous list start with [i] and end with [j] *)
let rec ( -- ) i j = from i j []

and from i j lst = if j < i then lst else from i (j - 1) (j :: lst)

let _ = assert (0 -- 100 = List.init 100 Fun.id)
