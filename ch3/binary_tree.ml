type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let t1 = Node (1, Node (1, Leaf, Leaf), Node (2, Leaf, Leaf))

let rec size_naive = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size_naive l + size_naive r
;;

let rec size' tree = size_tr_aux 0 tree

and size_tr_aux acc = function
  | Leaf -> acc
  | Node (_, l, r) -> size_tr_aux (acc + 1 + size' l) r
;;

(* prefer sytle: nested definition
   0 - let rec可以capture
   1 - let rec表明了‘这是个helper，其他人不要看不要用’
*)
let rec size'' tree =
  let rec size_tr_aux' acc = function
    | Leaf -> acc
    | Node (_, l, r) -> size_tr_aux' (acc + 1 + size'' l) r
  in
  size_tr_aux' 0 tree
;;

(* the code below constructs this tree:
   {v
              4
            /  \
           2    5
         / \   / \
        1  3  6   7
   v} *)

let t =
  Node
    ( 4
    , Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))
    , Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )
;;

(* -------------------------------------------------------------------------- *)

(* representation with Records *)
type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node =
  { value : 'a
  ; left : 'a tree
  ; right : 'a tree
  }

let t =
  Node
    { value = 1
    ; left = Node { value = 10; left = Leaf; right = Leaf }
    ; right = Leaf
    }
;;

let rec mem x = function
  | Leaf -> false
  | Node { value; left; right } -> value = x || mem x left || mem x right
;;

let rec size_rc tree =
  let rec aux acc = function
    | Leaf -> acc
    | Node { value; left; right } -> aux (acc + 1 + size_rc left) right
  in
  aux 0 tree
;;

let rec size_rc' tree = aux_re 0 tree

and aux_re acc = function
  | Leaf -> acc
  | Node { value; left; right } -> aux_re (acc + 1 + size_rc' left) right
;;

let rec preorder = function
  | Leaf -> []
  | Node { value; right; left } -> [ value ] @ preorder left @ preorder right
;;

(* -------------------------------------------------------------------------- *)

type nat =
  | Zero
  | Succ of nat

let iszero nat = nat = Zero

let pred = function
  | Zero -> failwith "zero does not have pred"
  | Succ x -> x
;;

let rec add nat1 nat2 =
  match nat1 with
  | Zero -> nat2
  | Succ x -> add (Succ nat2) x
;;

let rec int_of_nat = function
  | Zero -> 0
  | Succ x -> 1 + int_of_nat x
;;

let rec nat_of_int = function
  | 0 -> Zero
  | x when x > 0 -> Succ (nat_of_int (x - 1))
  | _ -> failwith "negivate number is not nature"
;;

let rec even = function
  | Zero -> true
  | Succ x -> odd x

and odd = function
  | Zero -> false
  | Succ x -> even x
;;
