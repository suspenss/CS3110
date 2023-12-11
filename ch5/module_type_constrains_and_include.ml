module type RING = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t (* additive inverse *)
  val to_string : t -> string
end

module type INT_RING = RING with type t = int
module type FLOAT_RING = RING with type t = float

module IntRing : INT_RING = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

module FloatRing : FLOAT_RING = struct
  type t = float

  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

module type FIELD = sig
  include RING

  val ( / ) : t -> t -> t
end

module IntField : FIELD = struct
  (* include IntRing *)
  (** if [include IntRing] the un seal the type version in the [IntFiled],
      the type of IntRing is [IntRing.t] and the module can't get the type*)

  include IntRing

  let ( / ) = Stdlib.( / )
end

module FloatField : FIELD = struct
  include FloatRing

  let ( / ) = Stdlib.( /. )
end
