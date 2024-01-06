module Nats = struct
  type nat =
    | Zero
    | Succ of nat
end

module Sequence = struct
  (** An ['a sequence] is an infinite list of values of type ['a].
      AF: [Cons (x, f)] is the sequence whose head is [x] and tail is [f ()].
      RI: none. *)
  type 'a sequence = Cons of 'a * (unit -> 'a sequence)

  (** [from n] is a sequence from [n] to infinity *)
  let rec from : int -> int sequence =
    fun num -> Cons (num, fun () -> from (num + 1))
  ;;

  (** [hd s] is the head element of sequence [s] *)
  let hd (Cons (h, _)) = h

  (** [tl s] is the elements without head element of [s] *)
  let tl (Cons (_, t)) = t ()

  let rec take : int -> 'a sequence -> 'a list =
    fun n s -> if n = 0 then [] else hd s :: take (n - 1) (tl s)
  ;;

  let rec map : ('a -> 'b) -> 'a sequence -> 'b sequence =
    fun f s -> Cons (f (hd s), fun () -> map f (tl s))
  ;;

  let rec map2 f s1 s2 =
    Cons (f (hd s1) (hd s2), fun () -> map2 f (tl s1) (tl s2))
  ;;

  (** some tricky function *)
  let square = map (fun x -> x * x)

  let sum = map2 ( + )
  let rec fibs = Cons (1, fun () -> Cons (1, fun () -> sum fibs (tl fibs)))
end

module LazyFibs = struct
  type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

  let hd : 'a lazysequence -> 'a = fun (Cons (h, _)) -> h

  let tl : 'a lazysequence -> 'a lazysequence =
    fun (Cons (_, t)) -> Lazy.force t
  ;;

  let rec take_aux n s lst =
    match n with
    | 0 -> lst
    | _ -> take_aux (n - 1) (tl s) (hd s :: lst)
  ;;

  let take : int -> 'a lazysequence -> 'a list =
    fun n s -> List.rev (take_aux n s [])
  ;;

  let nth : int -> 'a lazysequence -> 'a =
    fun n s -> List.hd (take_aux (n + 1) s [])
  ;;

  let rec map : ('a -> 'b) -> 'a lazysequence -> 'b lazysequence =
    fun f s -> Cons (f (hd s), lazy (map f (tl s)))
  ;;

  let rec sum : int lazysequence -> int lazysequence -> int lazysequence =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
    Cons (h_a + h_b, lazy (sum (Lazy.force t_a) (Lazy.force t_b)))
  ;;

  let rec fibs = Cons (1, lazy (Cons (1, lazy (sum (tl fibs) fibs))))
  let nth_fib n = nth n fibs
end
