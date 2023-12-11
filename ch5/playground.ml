(* my linked-list *)
module Mylist = struct
  type 'a mylist =
    | Nil
    | Cons of 'a * 'a mylist

  let rec map f = function
    | Nil -> Nil
    | Cons (value, next) -> Cons (f value, map f next)
  ;;
end

module BinaryTree = struct
  (* my tree *)
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  let rec map f = function
    | Leaf -> Leaf
    | Node (value, l, r) -> Node (f value, map f l, map f r)
  ;;
end

let _ = Mylist.(map succ (Cons (1, Nil)))
let _ : int Mylist.mylist = Cons (1, Nil)
let t = BinaryTree.Node (1, Leaf, Leaf)

module MyModule = struct
  type primary_color =
    | Red
    | Green
    | Blue

  let inc x = x + 1

  exception Oops
end

module type MATH = sig
  (** [fact n] is [n!]. *)
  val fact : int -> int
end

module Math_ = struct
  (** [fact_aux n acc] is [n! * acc]. *)
  let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (n * acc)

  let fact n = fact_aux n 1
end

module Math : MATH = Math_

let x = Math.fact 5
