module type set = sig
  (** ['a t] is the type of a set whose element type is ['a] *)
  type 'a t

  (** [empty] is the empty set without elements *)
  val empty : 'a t

  (** [size set] is the number of elements in set
      [size empty] is [0] *)
  val size : 'a t -> int

  (** [add x set] is a set which contains all element of [set] as well as [x] *)
  val add : 'a -> 'a t -> 'a t

  (** [rem x set] is a set which contains all element of [set] without [x] *)
  val rem : 'a -> 'a t -> 'a t

  (** [union s1 s2] is the set which contains all element of [s1] and [s2] *)
  val union : 'a t -> 'a t -> 'a t

  (** [inter s1 s2] is a set contains elements which both from [s1] and [s2] *)
  val inter : 'a t -> 'a t -> 'a t

  (** [mem x s] is a bool represent wether the [x] in [s] *)
  val mem : 'a -> 'a t -> bool
end

module BstSet = struct
  type 'a t =
    | Leaf
    | Node of 'a * 'a t * 'a t

  let empty = Leaf

  let rec size = function
    | Leaf -> 0
    | Node (_, left, right) -> 1 + size left + size right
  ;;

  let rec add x = function
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (v, l, r) when v > x -> Node (v, add x l, r)
    | Node (v, l, r) when v < x -> Node (v, l, add x r)
    | t -> t
  ;;

  let rec mem x = function
    | Leaf -> false
    | Node (v, l, _) when v > x -> mem x l
    | Node (v, _, r) when v < x -> mem x r
    | _ -> true
  ;;

  let rec remove_union lt = function
    | Leaf -> lt
    | Node (v, l, r) -> Node (v, remove_union lt l, r)
  ;;

  let rec remove x = function
    | Leaf -> failwith "tree doesn't contain the removal element"
    | Node (v, l, r) when v > x -> Node (v, remove x l, r)
    | Node (v, l, r) when v < x -> Node (v, l, remove x r)
    | Node (_, l, r) -> remove_union l r
  ;;

  let update x x' t = t |> remove x |> add x'
end

(** Red-Black Tree Representation Invariant :
    - is a Binary Search Tree
    - Local invariant : red node shoud not have red child
    - global invariant : both path from root to leaf
      have same number of black nodes *)
module RBTree = struct
  type color =
    | R
    | B

  type 'a tree =
    | Leaf
    | Node of (color * 'a * 'a tree * 'a tree)

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (_, v, l, _) when v > x -> mem x l
    | Node (_, v, _, r) when v < x -> mem x r
    | _ -> true
  ;;

  (** {v
                1             2             3             4
               Bz            Bz            Bx            Bx
              / \           / \           / \           / \
             Ry  d         Rx  d         a   Rz        a   Ry
            /  \          / \               /  \          /  \
          Rx   c         a   Ry            Ry   d        b    Rz
        /  \               /  \          / \                /  \
       a    b             b    c        b   c              c    d
      v} *)
  let balance : color * 'a * 'a tree * 'a tree -> 'a tree = function
    | B, z, Node (R, y, Node (R, x, a, b), c), d
    | B, z, Node (R, x, a, Node (R, y, b, c)), d
    | B, x, a, Node (R, z, Node (R, y, b, c), d)
    | B, x, a, Node (R, y, b, Node (R, z, c, d)) ->
      Node (R, y, Node (B, x, a, b), Node (B, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)
  ;;

  let insert x t =
    let rec insert_aux x = function
      | Leaf -> Node (R, x, Leaf, Leaf)
      | Node (c, v, l, r) when c > x -> balance (c, v, insert_aux x l, r)
      | Node (c, v, l, r) when c < x -> balance (c, v, l, insert_aux x r)
      | node -> node
    in
    match insert_aux x t with
    | Leaf -> failwith "error"
    | Node (_, v, l, r) -> Node (B, v, l, r)
  ;;
end
