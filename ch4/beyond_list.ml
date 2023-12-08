type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec map f = function
  | Leaf -> Leaf
  | Node (value, left, right) -> Node (f value, map f left, map f right)
;;

let addone t = map succ t

let rec fold_tree f t acc =
  match t with
  | Leaf -> acc
  | Node (value, left, right) ->
    f value (fold_tree f left acc) (fold_tree f right acc)
;;

let tree_sum t = fold_tree (fun v l r -> v + l + r) t 0
let tree_size t = fold_tree (fun _ l r -> 1 + l + r) t 0
let tree_depth t = fold_tree (fun _ l r -> 1 + max l r) t 0
let preoder t = fold_tree (fun x l r -> [ x ] @ l @ r) t []

let rec tree_filter f = function
  | Leaf -> Leaf
  | Node (value, left, right) ->
    if f value
    then Node (value, tree_filter f left, tree_filter f right)
    else Leaf
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
