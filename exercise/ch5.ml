(* Exercise: complex encapsulation *)
module type COMPLEX = sig
  type t = float * float

  val zero : t
  val make : float -> float -> t
  val add : t -> t -> t
  val str : t -> string
end

module Complex : COMPLEX = struct
  type t = float * float

  (** the zero *)
  let zero = 0., 0.

  let make a b = a, b
  let ( + ) (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
  let add = ( + )

  (** the str *)
  let str (real, img) = Printf.sprintf "%f + %fi" real img
end

(* Exercise: big list queue *)

(** Creates a ListQueue filled with [n] elements. *)
module ListQueue = struct
  type 'a t = 'a list

  let enqueue x q = q @ [ x ]
  let empty = []
end

let fill_listqueue n =
  let rec loop n q =
    if n = 0 then
      q
    else
      loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty
;;

(* Exercise: big batched queue *)
module BatchQueue = struct
  type 'a t =
    { out : 'a list
    ; inbox : 'a list
    }

  let empty = { out = []; inbox = [] }

  let enqueue x = function
    | { out = []; _ } -> { out = [ x ]; inbox = [] }
    | { out; inbox } -> { out; inbox = x :: inbox }
  ;;

  let dequeue = function
    | { out = []; _ } -> raise (Failure "Empty")
    | { out = [ _ ]; inbox } -> { out = List.rev inbox; inbox = [] }
    | { out = _ :: xs; inbox } -> { out = xs; inbox }
  ;;

  let front = function
    | { out = []; _ } -> raise (Failure "Empty")
    | { out = x :: _; _ } -> x
  ;;
end

module type BQTESTER = sig
  val fill_queue : int -> int BatchQueue.t
end

module BatchedQueueTester : BQTESTER = struct
  let rec loop n q =
    match n with
    | 0 -> q
    | _ -> loop (n - 1) (BatchQueue.enqueue n q)
  ;;

  let fill_queue n = loop n BatchQueue.empty
end

(* Exercise: binary search tree map *)

(* Exercise: fraction *)

module type FRACTION = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction : FRACTION = struct
  type t = int * int

  let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a mod b)
  ;;

  let make num den =
    let gcd_ = gcd num den in
    num / gcd_, den / gcd_
  ;;

  let numerator (num, _) = num
  let denominator (_, denom) = denom
  let to_string (num, den) = Printf.sprintf "%i/%i" num den
  let to_float (num, den) = float_of_int num /. float_of_int den
  let add (n1, d1) (n2, d2) = make (n1 + n2) (d1 + d2)
  let mul (n1, d1) (n2, d2) = make (n1 * n2) (d1 * d2)
end

(* Exercise: make char map *)
module CharMap = Map.Make (struct
    type t = char

    let compare = Char.compare
  end)

let charMap1 =
  CharMap.(
    empty
    |> add 'a' "Alpha"
    |> add 'E' "Echo"
    |> add 's' "Sierra"
    |> add 'V' "Victor")
;;

let _ = CharMap.find_opt 'F' charMap1
let _ = CharMap.remove 'E' charMap1
let _ = CharMap.mem 'E' charMap1

(* is for *)
let is_for = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v)

(* Data *)

module type DATE = sig
  type t =
    { month : int
    ; day : int
    }

  val compare : t -> t -> int
  val make_date : int -> int -> t
  val get_month : t -> int
  val get_day : t -> int
  val to_string : t -> string
  val format : Format.formatter -> t -> unit
end

module Date : DATE = struct
  type t =
    { month : int
    ; day : int
    }

  let compare lhs rhs =
    if lhs.month = rhs.month then
      lhs.day - rhs.day
    else
      lhs.month - rhs.month
  ;;

  let make_date month day = { month; day }
  let get_month d = d.month
  let get_day d = d.day
  let to_string d = string_of_int d.month ^ "/" ^ string_of_int d.day
  let format formatter d = Format.fprintf formatter "%s" (to_string d)
end

module DateMap = Map.Make (Date)

type calender = string DateMap.t

let calenders =
  DateMap.(
    empty
    |> add (Date.make_date 12 11) "wash clothes"
    |> add (Date.make_date 12 12) "do homework")
;;

let print_calender =
  DateMap.iter (fun data (plan : string) ->
    Printf.printf "%S in Month: %i, Day: %i\n" plan data.month data.day)
;;

(* Exercise: first after *)
let find_first cld data =
  DateMap.find_first (fun key -> Date.compare key data <= 0) cld
;;

(* sets *)

module StringSet = Set.Make (struct
    type t = string

    let compare lhs rhs =
      String.(compare (lowercase_ascii lhs) (lowercase_ascii rhs))
    ;;
  end)

let _ = StringSet.(empty |> add "Hello" |> add "hello" |> elements)

(* ToString *)
module type ToString = sig
  type t

  val to_string : t -> string
end

module Printor (M : ToString) = struct
  let print x = Printf.printf "%s" (M.to_string x)
end

module PrintInt = Printor (struct
    type t = int

    let to_string x = string_of_int x
  end)

module PrintString = Printor (struct
    type t = string

    let to_string = Fun.id
  end)

module StringWithPrint = struct
  include String
  include PrintString
end

(* -------------- BST -------------- *)
module BST = struct
  module type BSTorderedType = sig
    type t

    val compare : t -> t -> int
  end

  module Make (Ord : BSTorderedType) = struct
    type key = Ord.t

    type 'a t =
      | Leaf
      | Node of key * 'a * 'a t * 'a t

    let compare = Ord.compare
    let empty = Leaf

    let rec add k v = function
      | Leaf -> Node (k, v, Leaf, Leaf)
      | Node (tk, _, l, r) when compare tk k = 0 -> Node (tk, v, l, r)
      | Node (tk, tv, l, r) ->
        if compare tk k > 0 then
          Node (tk, tv, add k v l, r)
        else
          Node (tk, tv, l, add k v r)
    ;;

    let rec find k = function
      | Leaf -> None
      | Node (tk, tv, _, _) when compare tk k = 0 -> Some tv
      | Node (tk, _, l, r) ->
        if compare tk k > 0 then
          find k l
        else
          find k r
    ;;
  end
end

(* test BST *)
module BstIntMap = BST.Make (Int)

let t1 = BstIntMap.empty |> BstIntMap.add 12 "hello"
let bstTestIntMap = BstIntMap.(empty |> add 1 "hello" |> add 2 "world")
