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

  val rem : 'a -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool
end

module ListSet : set = struct
  type 'a t = 'a list

  let empty = []
  let size lst = List.(lst |> sort_uniq Stdlib.compare |> length)
  let mem = List.mem
  let add = List.cons
  let rem x = List.filter (fun e -> e <> x)
  let union l1 l2 = l1 @ l2
  let inter l1 l2 = List.filter (fun x -> mem x l2) l1
end

module UniqueListSet = struct
  type 'a t = 'a list

  let empty = []
  let size = List.length
  let mem = List.mem

  let add x set =
    match mem x set with
    | true -> set
    | false -> x :: set
  ;;

  let rem x = List.filter (fun e -> e <> x)
  let union l1 l2 = l1 @ l2
  let inter l1 l2 = List.filter (fun x -> mem x l2) l1
end

module IO = struct
  let read_int = Scanf.scanf "%d" Fun.id

  let get_sequence separator =
    read_line () |> String.split_on_char separator |> List.map int_of_string
  ;;
end
