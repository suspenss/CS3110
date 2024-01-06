module type SET = sig
  (** ['a t] is the type of set whose element type is ['a] *)
  type 'a t

  (** [empty] is a empty set *)
  val empty : 'a t

  (** [size set] is the size of the set which equal with
      the number of element in the set*)
  val size : 'a t -> int

  (** [mem e set] is [true] if the element [e] in the [set] *)
  val mem : 'a -> 'a t -> bool

  (** [add x set] is  insert [x] to [set] *)
  val add : 'a -> 'a t -> 'a t

  (** [rem x s] is the set remove [x] from [s] *)
  val rem : 'a -> 'a t -> 'a t

  (** [unios s1 s2] is a set which has all element of [s1] and [s2] *)
  val union : 'a t -> 'a t -> 'a t
end
