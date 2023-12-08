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

(* powerset *)
let rec powerset = function
  | [] -> [ [] ]
  | x :: xs -> (fun a b -> a @ List.map (List.cons b) a) (powerset xs) x
;;

(* Exercise: print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    h |> string_of_int |> print_endline;
    print_int_list t
;;

(* Exercise: print int list iter *)
let print_int_list_iter lst =
  List.iter (fun x -> x |> string_of_int |> prerr_endline) lst
;;

(* student *)
type student =
  { first_name : string
  ; last_name : string
  ; gpa : float
  }

(*student

  student -> string * string (a function that extracts the student’s name)

  string -> string -> float -> student (a function that creates a student record)
*)

let xiaoming : student = { first_name = "xiao"; last_name = "ming"; gpa = 3.0 }
let get_full_name student = student.first_name, student.last_name

let student_constructor first_name last_name gpa =
  { first_name; last_name; gpa }
;;

(* safe hd and tl *)

let safe_hd = function
  | [] -> None
  | x :: _ -> Some x
;;

let safe_tl = function
  | [] -> None
  | xs -> xs |> List.rev |> safe_hd
;;

(* pokerecord *)
type poketype =
  | Normal
  | Fire
  | Water

type pokemon =
  { name : string
  ; hp : float
  ; poketype : poketype
  }

let charizard : pokemon = { name = "charizard"; hp = 78.; poketype = Fire }
let squirtle : pokemon = { name = "squirtle"; hp = 44.; poketype = Water }

(* pokefun *)

let default : pokemon = { name = "unknow"; hp = 0.; poketype = Normal }

let max_hp =
  let rec unsafe_fn = function
    | [] -> default
    | x :: xs -> (fun a b -> if a.hp > b.hp then a else b) x (unsafe_fn xs)
  in
  function
  | [] -> None
  | lst -> Some (unsafe_fn lst)
;;

(* date before *)
let is_before d1 d2 =
  match d1, d2 with
  | (y1, m1, d1), (y2, m2, d2) ->
    y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)
;;

let take_option = function
  | None -> failwith "None take_option"
  | Some x -> x
;;

(* earliest date *)
let rec earlist = function
  | [] -> None
  | x :: xs ->
    (fun a b -> if is_before a b then Some a else Some b)
      x
      (take_option (earlist xs))
;;

(* cards *)
type suit =
  | Hearts
  | Clubs
  | Diamonds
  | Spades

type rank =
  | Number of int
  | Jack
  | Queen
  | King
  | Ace

type card =
  { suit : suit
  ; rank : rank
  }

let card1 = { suit = Clubs; rank = Ace }
let card2 = { suit = Spades; rank = Number 10 }

(* quadrant *)
type quad =
  | I
  | II
  | III
  | IV

type sign =
  | Neg
  | Zero
  | Pos

let sign (x : int) : sign =
  match x with
  | 0 -> Zero
  | x when x > 0 -> Pos
  | x when x < 0 -> Neg
  | _ -> failwith "No sign known"
;;

let quadrant (x, y) =
  match sign x, sign y with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None
;;

let sign_pair (x, y) = sign x, sign y

let quadrant_when : int * int -> quad option = function
  | x when sign_pair x = (Pos, Pos) -> Some I
  | x when sign_pair x = (Neg, Pos) -> Some II
  | x when sign_pair x = (Neg, Neg) -> Some III
  | x when sign_pair x = (Pos, Neg) -> Some IV
  | _ -> None
;;

(* quadrant poly *)

let sign_poly : int -> [> `Neg | `Pos | `Zero ] = function
  | 0 -> `Zero
  | x when x > 0 -> `Pos
  | x when x < 0 -> `Neg
  | _ -> failwith "No sign_poly known"
;;

let quadrant_poly (x, y) : [> `I | `II | `III | `IV ] option =
  match sign_poly x, sign_poly y with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
;;

(* depth *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)
;;

(* shape *)
let rec shape t1 t2 =
  match t1, t2 with
  | Node _, Leaf | Leaf, Node _ -> false
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> true && shape l1 l2 && shape r1 r2
;;

(* list max exn *)
let rec list_max_exn : int list -> int = function
  | [] -> Failure "empty" |> raise
  | x :: xs -> max x (list_max_exn xs)
;;

let list_max_strinng : int list -> string = function
  | [] -> "empty"
  | x :: xs -> list_max_exn xs |> max x |> string_of_int
;;

(* is_bst *)
type ('a, 'b) tree' =
  | Leaf
  | Node of ('a * 'b) * ('a, 'b) tree' * ('a, 'b) tree'

let rec is_bst = function
  | Leaf -> true
  | Node ((key, _), l, r) ->
    calc_subtree ( > ) key l && calc_subtree ( < ) key r

and calc_subtree opt fa_key = function
  | Leaf -> true
  | Node ((key, _), l, r) ->
    if opt fa_key key then true && is_bst l && is_bst r else false
;;
