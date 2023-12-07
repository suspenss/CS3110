(* Variants
   like Sun, Mon ...
   These value just are tags rather than types
*)

type day =
  | Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

(* the Red bind the int type, Green bind the int type too
   in varients, they are different thing, however they are same type,
   as above, Red, Green just TAG, point a type, and the Tags are different
   but types are same.

   varients are algebraic data type (ADT) which can contain sum type (tags)
   and product type (tags bind type)
*)
type color =
  | Red of int
  | Green of int
  | Blue of int

type point = float * float

type shape =
  | Rectangle of point * point
  | Square of point * point
  | Circle of
      { center : point
      ; radius : float
      }

let avg a b = (a +. b) /. 2.

let center = function
  | Rectangle ((x_ll, y_ll), (x_ur, y_ur)) | Square ((x_ll, y_ll), (x_ur, y_ur))
    -> avg x_ll x_ur, avg y_ll y_ur
  | Circle { center; radius } -> center
;;

let area = function
  | Rectangle ((x_ll, y_ll), (x_ur, y_ur)) | Square ((x_ll, y_ll), (x_ur, y_ur))
    -> abs_float (x_ll -. x_ur) *. abs_float (y_ll -. y_ur)
  | Circle { center; radius } -> Float.pi *. (radius ** 2.)
;;

(* static string of int list *)
type string_or_int =
  | String of string
  | Int of int

type soril = string_or_int list

let rec sum_of_soil = function
  | [] -> 0
  | String x :: xs -> int_of_string x + sum_of_soil xs
  | Int x :: xs -> x + sum_of_soil xs
;;

let _ =
  let soril1 = [ Int 1; String "1"; String "251" ] in
  assert (sum_of_soil soril1 = 253)
;;

(* recursion varient *)
type intlist =
  | Nil
  | Cons of int * intlist

let rec length_intlist = function
  | Nil -> 0
  | Cons (x, xs) -> 1 + length_intlist xs
;;

let rec sum_intlist = function
  | Nil -> 0
  | Cons (x, xs) -> x + sum_intlist xs
;;

let empty_inlist intlist = intlist = Nil

let _ =
  let intlist1 = Cons (1, Cons (2, Cons (1, Nil))) in
  assert (length_intlist intlist1 = 3);
  assert (empty_inlist intlist1 = false);
  assert (sum_intlist intlist1 = 4)
;;

type mylist =
  | Nil
  | Node of node

and node =
  { value : int
  ; next : mylist
  }

(* poloymorphism varient *)
type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist

let char_lilist_mylist = Cons ('a', Cons ('b', Nil))
let int_list = Cons (1, Cons (3, Nil))
let list_mylist = Cons ([ 1 ], Cons ([ 3; 4 ], Nil))

(* parametric polymorphism function with [length : intlist -> int] previous *)
let rec length mylist = mylist_lenth_tr 0 mylist

and mylist_lenth_tr acc = function
  | Nil -> acc
  | Cons (x, xs) -> mylist_lenth_tr (acc + 1) xs
;;

(* polymorphic varients *)
let sign = function
  | 0 -> `Zero
  | x when x > 0 -> `Positive
  | x when x < 0 -> `Negivite
  | _ -> failwith " faild pattern "
;;
