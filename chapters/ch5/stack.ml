module type STACK = sig
  type 'a t

  exception EmptyStack

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t
  val size : 'a t -> int
end

module VarientStack : STACK = struct
  type 'a t =
    | Empty
    | Entry of 'a * 'a t

  exception EmptyStack

  let empty = Empty
  let is_empty s = Empty = s
  let push x stk = Entry (x, stk)

  let peek = function
    | Empty -> None
    | Entry (x, _) -> Some x
  ;;

  let pop = function
    | Empty -> raise EmptyStack
    | Entry (_, rest) -> rest
  ;;

  let rec size_aux acc = function
    | Empty -> acc
    | Entry (_, next) -> size_aux (1 + acc) next
  ;;

  let size s = size_aux 0 s
end

module ListStack : STACK = struct
  type 'a t = 'a list * int

  exception EmptyStack

  let empty = [], 0
  let is_empty (_, size) = size = 0

  let peek = function
    | [], _ -> None
    | x :: _, _ -> Some x
  ;;

  let push x (s, size) = x :: s, size + 1

  let pop = function
    | [], _ -> raise EmptyStack
    | x :: xs, size -> xs, size - 1
  ;;

  let size (_, size_) = size_
end

let stk1 = VarientStack.empty
let stk2 = VarientStack.push 1 stk1
let lstk1 = ListStack.empty
let lstk2 = ListStack.push 2 lstk1
let top_stk2 = VarientStack.peek stk2
let egstk = ListStack.(empty |> push 2 |> push 4 |> push 5)
