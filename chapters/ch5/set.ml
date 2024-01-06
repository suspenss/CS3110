module type SET = sig
  type 'a t
  (** ['a t] is the type of sets whose elements are of type ['a]. *)

  val empty : 'a t
  (** [empty] is the empty set *)

  val contains : 'a -> 'a t -> bool
  (** [mem x s] is whether [x] is an element of [s]. *)

  val insert : 'a -> 'a t -> 'a t
  (** [add x s] is the set that contains [x] and all the elements of [s]. *)

  val elements : 'a t -> 'a list
  (** [elements s] is a list containing the elements of [s].  No guarantee
      is made about the ordering of that list, but each is guaranteed to
      be unique. *)
end

module UniqueListSet : SET = struct
  type 'a t = 'a list

  let empty = []
  let contains = List.mem
  let insert x set = if contains x set then set else x :: set
  let elements = Fun.id
end

module MultipleListSet : SET = struct
  type 'a t = 'a list

  let empty = []
  let contains = List.mem
  let insert = List.cons
  let elements set = List.sort_uniq Stdlib.compare set
end

module type SET_EXTENDED = sig
  include SET

  val of_list : 'a list -> 'a t
end

module ListSetExtended : SET_EXTENDED = struct
  include UniqueListSet

  let of_list lst = List.fold_right insert lst empty
end
