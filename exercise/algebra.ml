module type Ring = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring

  val ( / ) : t -> t -> t
end

module IntRingImpl = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
  let of_int n = n
end

module IntFieldImpl = struct
  include IntRingImpl

  let ( / ) = ( / )
end

module IntField : Field with type t = int = IntFieldImpl

module FloatRingImpl = struct
  type t = float

  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
  let of_int n = float_of_int n
end

module FloatFieldImpl = struct
  include FloatRingImpl

  type t = float

  let ( / ) = ( /. )
end

module FloatField : Field with type t = float = FloatFieldImpl

module MakeRational (T : Field) = struct
  type t = T.t * T.t

  let zero = T.zero, T.zero
  let one = T.one, T.one
  let ( + ) (a, b) (c, d) = T.( + ) (T.( * ) a d) (T.( * ) c b), T.( * ) b d
  let ( ~- ) (a, b) = T.( ~- ) a, b
  let ( / ) (a, b) (c, d) = T.( * ) a d, T.( * ) b c
  let ( * ) (a, b) (c, d) = T.( * ) a c, T.( * ) b d
  let to_string (a, b) = T.to_string a ^ "/" ^ T.to_string b
  let of_int n = T.of_int n, T.one
end

module IntRation : Field = MakeRational (IntField)
module FloatRation : Field = MakeRational (FloatField)
