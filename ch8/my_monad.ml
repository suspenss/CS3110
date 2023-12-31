module type Monad = sig
  type 'a t

  (** [return x] is the value [Monad(x)] *)
  val return : 'a -> 'a t

  (** [bind x f] is the value [f x] which the function [f] take [x] as argument
      and return a value [y] which warped in monad *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [x >>= f] is a symbol with same function as [bind] *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe = struct
  type 'a maybe =
    | Nothing
    | Just of 'a

  module MaybeMonad : Monad with type 'a t = 'a maybe = struct
    type 'a t = 'a maybe

    let return (x : 'a) : 'a t = Just x

    let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
      match x with
      | Nothing -> Nothing
      | Just v -> f v
    ;;

    let ( >>= ) = bind
  end

  module type BinaryMonad = sig
    include Monad

    val binary_bind : ('a -> 'b -> 'c t) -> 'a t -> 'b t -> 'c t
    val binary_return : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t
  end

  module MaybeBinaryMonad : BinaryMonad with type 'a t = 'a maybe = struct
    include MaybeMonad

    (** [binary_bind] is make the operation [f] take two argument [x y] and
        calculate the answer in monad *)
    let binary_bind f x y = x >>= fun a -> y >>= fun b -> f a b

    let binary_return f x y = return (f x y)
  end

  module type Num = sig
    type t

    val zero : t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
  end

  module MakeMaybeMonad (X : Num) = struct
    let div_opt : X.t -> X.t -> X.t MaybeBinaryMonad.t =
      fun x y -> if y = X.zero then Nothing else Just (X.div x y)
    ;;

    open MaybeBinaryMonad

    (** [binary_bind] is make the operation [f] take two argument [x y] and
        calculate the answer in monad *)
    let binary_bind f x y = x >>= fun a -> y >>= fun b -> f a b

    let binary_return f x y = return (f x y)

    (* Here are operations in Int *)

    let ( + ) = binary_bind (binary_return X.add)
    let ( - ) = binary_bind (binary_return X.sub)
    let ( * ) = binary_bind (binary_return X.mul)
    let ( / ) = binary_bind div_opt
  end

  module IntMaybeMonad = MakeMaybeMonad (Int)
  module FloatMaybeMonad = MakeMaybeMonad (Float)
end
