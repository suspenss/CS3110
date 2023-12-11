module type X = sig
  val x : int
  val do_something : int -> int
end

module M : X = struct
  let x = 1
  let do_something x = x + 10
end

module Inc (M : X) (MM : X) = struct
  include M

  let x = M.x + 1
  let y = MM.x
end
(*
   syntax suger:
   module Mname = functor (ParaM : ParaSig) -> struct
   something ...
   end

   be equavite
   module Mname (ParaM : ParaSig) = struct  ... end
*)

(*
   you must be specifing the [PareSig], but you don't always seal the
   para module as ParaSig.
*)

module A = Inc (M) (M)

module type INC = sig
  val do_something : int -> int
  val x : int
  val y : int
end

module FT : functor (_ : X) (_ : X) -> INC = Inc

module type og1 = sig
  type t
end

module M1 = struct
  type t = int list
end

module M2 = struct
  type t =
    | Nothing
    | Just of int
end

module Get (X : og1) (Y : og1) = struct
  type t = X.t * Y.t
end

module Target = Get (M1) (M2)
