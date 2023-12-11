(* ------ Queue ------- *)
module type QUEUE = sig
  type 'a t

  (** [empty] is return a empty Queue datastruct *)
  val empty : 'a t

  (** [is_empty] is take a [queue]
      return [bool] wheather the queue is empty *)
  val is_empty : 'a t -> bool

  (** [enqueue] take ['a] to the front of queue*)
  val enqueue : 'a -> 'a t -> 'a t

  (** [dequeue] is pop the front element of a queue *)
  val dequeue : 'a t -> 'a t option

  (** return the front element of the queue*)
  val front : 'a t -> 'a option
end

module Queue : QUEUE = struct
  type 'a t = 'a list

  let empty = []
  let is_empty q = [] = q
  let enqueue x q = [ x ] @ q

  let dequeue = function
    | [] -> None
    | _ :: xs -> Some xs
  ;;

  let front = function
    | [] -> None
    | x :: _ -> Some x
  ;;
end

let unpack = function
  | None -> failwith "Option is None"
  | Some x -> x
;;

let ( ||> ) opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)
;;

let queue = Queue.(empty |> enqueue 1 |> dequeue |> unpack |> enqueue 2)

let q =
  Queue.(empty |> enqueue 1 |> dequeue ||> enqueue 10 ||> enqueue 100 ||> front)
;;

module BatchedQueue : QUEUE = struct
  (** [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
      is not. This implies that if [o] is empty, [i] must also be empty. *)
  type 'a t =
    { o : 'a list
    ; i : 'a list
    }

  exception Empty

  let empty = { o = []; i = [] }

  let is_empty = function
    | { o = [] } -> true
    | _ -> false
  ;;

  let enqueue x = function
    | { o = [] } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }
  ;;

  let front = function
    | { o = []; _ } -> None
    | { o = h :: _; _ } -> Some h
  ;;

  let dequeue = function
    | { o = [] } -> None
    | { o = [ _ ]; i } -> Some { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> Some { o = t; i }
  ;;

  let size { o; i } = List.(length o + length i)
  let to_list { o; i } = o @ List.rev i
end
