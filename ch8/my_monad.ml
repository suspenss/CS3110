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

module Log = struct
  module Playground = struct
    (** two simple function *)
    let inc x = x + 1

    let dec x = x - 1

    (** we can define a compose function *)
    let compose f g x = f (g x)

    (** [f @. g] is the compose function which take a argument [x] and calculate [g x]
        firstly, then calculate the [f x], and the operation is
        right association : [f @. g @. h] = [f @. (g @. h)] *)
    let ( @. ) = compose

    (** Use above most simple function : inc, dec, we can compose them eaisly*)

    let id x = x |> dec |> inc
    let id = dec @. inc

    (** upgrade above function with log *)

    let inc_log x = x + 1, Printf.sprintf "Called inc on %i; " x
    let dec_log x = x - 1, Printf.sprintf "Called dec on %i; " x

    (** but we can't compose them like [ x |> inc_log |> dec_log ]
        because of the type of arguments are not equal *)
    let id_log = inc_log @. fst @. dec_log

    (** or *)

    let dec_log' (x, s) = x - 1, Printf.sprintf "%s Called dec on %i; " s x
    let id_log' = dec_log' @. inc_log

    let log (name : string) (f : int -> int) : int -> int * string =
      fun x -> f x, Printf.sprintf "Called %s on %i; " name x
    ;;

    let loggable name f : int * string -> int * string =
      fun (x, s1) ->
      let y, s2 = log name f x in
      y, s1 ^ s2
    ;;

    let inc' : int * string -> int * string = loggable "inc" inc
    let dec' : int * string -> int * string = loggable "dec" dec
    let id' : int * string -> int * string = inc' @. dec'
  end

  module type MonadLogSig = Monad with type 'a t = 'a * string

  module LogMonad : MonadLogSig = struct
    type 'a t = 'a * string

    let return x = x, ""

    let bind (x, s) f =
      let y, s' = f x in
      y, s ^ s'
    ;;

    let ( >>= ) = bind
  end

  module Log = struct
    open LogMonad

    let log_int (name : string) (f : int -> int) : int -> int * string =
      fun x -> f x, Printf.sprintf "Called %s on %i; " name x
    ;;

    let loggable : string -> (int -> int) -> int t -> int t =
      fun name f m -> m >>= log_int name f
    ;;

    let inc = loggable "inc" Playground.inc
    let dec = loggable "dec" Playground.dec
    let id x = return x |> inc |> dec
  end
end

module Compose = struct
  let inc x = Some (x + 1)
  let dec x = Some (x - 1)

  let ( >>= ) x f =
    match x with
    | None -> None
    | Some v -> f v
  ;;

  let compose f g x = f x >>= g
  let ( >=> ) = compose
  let id = inc >=> dec >=> inc >=> dec
end

module type MonadCompose = sig
  include Monad

  val compose : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end
