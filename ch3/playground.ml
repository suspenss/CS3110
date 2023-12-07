let concat_list = [ 1; 2; 10 ] @ [ 1; 2; 3 ]

(* some pattern matching *)
let _ =
  match 'c' with
  (* pattern guard *)
  | x when x = 'c' -> 1
  (* character range pattern *)
  | 'd' .. 'x' -> 2
  (* any constant pattern like literals or booleans *)
  | 'z' -> 3
  (* explicit type *)
  | ('y' : char) -> 4
  | _ -> 0
;;

type pokemonType =
  | Common
  | Fire
  | Water

type pokemonEffect =
  | Normal
  | Double
  | Half

type pokemon =
  { name : string
  ; hp : float
  ; monType : pokemonType
  }

let mult_effect = function
  | Normal -> 1.0
  | Double -> 2.0
  | Half -> 0.5
;;

let get_effect = function
  | Fire, Fire | Fire, Water | Water, Water -> Half
  | Water, Fire -> Double
  | _ -> Normal
;;

let get_hp mon = mon.hp

(* instantiation*)
let picaqiu = { name = "picapica"; hp = 32.0; monType = Common }

(* third apply to [tuple] get the third element *)
let third (_, _, z) = z

type name = string
type location = string

let x : name = "he"

type id = string

let y = "he"

(* 3.8. Association Lists *)
let d = [ "rectangle", 4; "nonagon", 9; "icosagon", 20 ]

(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t
;;
