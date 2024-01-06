type day =
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

let int_of_day = function
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7
;;

module DayMap = Map.Make (struct
    type t = day

    let compare day1 day2 = int_of_day day1 - int_of_day day2
  end)

(* module DayMap = Map.Make (OrderedDay) *)

let m1 =
  DayMap.(empty |> add Mon "go to singlechip lab" |> add Tue "go to playground")
;;

module IntMap = Map.Make (struct
    type t = int

    let compare x y = y - x
  end)
(*
   similar with cpp map definition:

  // T represent the value type is unspecified
  using IngMap = std::map<int, T, [] (int a, int b) {
    return a > b;
  }>;
*)

module FloatMap = Map.Make (Float)
module FloatMap = Map.Make (String)

type egtype =
  { str : string
  ; fn : int -> int
  ; base : int
  }

module IntTransformerMap = Map.Make (struct
    type t = egtype

    let compare x y = x.base - y.base
  end)

let x =
  IntTransformerMap.(
    empty |> add { str = "add 1 to base"; fn = (fun x -> x + 1); base = 0 } Mon)
;;

let binding_to_x = IntTransformerMap.bindings x
