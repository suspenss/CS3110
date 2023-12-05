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
let rec count_aux n acc =
    if n = 0 then acc else count_aux (n - 1) (acc + 1)

let count n = count_aux n 0

(* [fact n] is [n] factorial *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)

(* change fact function to tail recursion version *)
let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (acc * n)

let fact_tr n = fact_aux n 1
