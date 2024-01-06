module HashTable = struct
  (* Exercise: hash insert [★★] *)

  (* Exercise: relax bucket RI [★★] *)

  (** [Answer]:
      - [insert] will be const time
      - [find] no change
      - [remove] will be destory because we don't know which one will be delete *)

  (* Exercise: strengthen bucket RI [★★] *)

  (** [Answer]:
      - [insert] will be O(k) which is the k is the order of the bucket
      - [find] and [remove] no change *)

  (* Exercise: hashtbl usage [★★] *)

  let table = Hashtbl.create 16

  let () =
    Seq.(ints 1 |> map (fun x -> x, string_of_int x) |> take 31)
    |> Hashtbl.add_seq table
  ;;

  let _ = Hashtbl.find_opt table 31

  (* Exercise: hashtbl stats [★] *)

  (* [Answer]: 
  # Hashtbl.stats table;;
  - : Hashtbl.statistics =
  {Hashtbl.num_bindings = 31; num_buckets = 16; max_bucket_length = 4;
  bucket_histogram = [|3; 3; 3; 6; 1|]}
  *)

  (* Exercise: hashtbl bindings [★★] *)

  let bindings t = Hashtbl.fold (fun k v acc -> (k, v) :: acc) t []

  (* Exercise: hashtbl load factor [★★] *)

  let load_factor table =
    let stats = Hashtbl.stats table in
    float_of_int stats.num_bindings /. float_of_int stats.num_buckets
  ;;

  (* Exercise: functorial interface [★★★] *)
  module SH = Hashtbl.Make (struct
      type t = string

      let equal l r = String.(lowercase_ascii l = lowercase_ascii r)
      let hash s = Hashtbl.hash (String.lowercase_ascii s)
    end)

  (* Exercise: equals and hash [★★] *)
  (** [Answer] : Because in the abstraction of hash table algebra data type, we
      need a injection function between key and index of array that storage the
      bucket to provide we storage the (key, value) pair into the HashTable and
      find or search or modify the (key, value) in it, the hash function can
      calculate a same hash number which is the index of the array based
      hash table implemention *)

  (* Exercise: linear probing [★★★★] *)
  module Probing = struct
    type key

    type 'a state =
      | Empty
      | Deleted
      | Value of (key * 'a)

    type 'a map =
      { mutable buckets : 'a state array
      ; mutable size : int
      }

    (* Empty = None, Delete = Some None, Value = Some x*)
    let creat size = { size = 0; buckets = Array.make size Empty }

    let load_factor t =
      float_of_int t.size /. float_of_int (Array.length t.buckets)
    ;;

    (* [capacity map] is the length of buckets *)
    let capacity table = Array.length table.buckets
    let index table k = Hashtbl.hash k mod capacity table

    let rec find_empty_slot t i =
      match t.buckets.(i) with
      | Empty -> i
      | _ -> find_empty_slot t (i + (1 mod capacity t))
    ;;

    let findi t k =
      let rec find_aux i =
        match t.buckets.(i) with
        | Value (k', _v) when k' = k -> Some i
        | Value _ -> find_aux (i + (1 mod capacity t))
        | _ -> None
      in
      find_aux (index t k)
    ;;

    let get_value x =
      match x with
      | Value (k, v) -> Some (k, v)
      | _ -> None
    ;;

    let find t k =
      match findi t k with
      | Some i -> get_value t.buckets.(i)
      | _ -> None
    ;;

    let insert_naive t k v =
      let empty_index = find_empty_slot t (index t k) in
      t.buckets.(empty_index) <- Value (k, v)
    ;;

    let remove_naive t k =
      match findi t k with
      | Some i -> t.buckets.(i) <- Deleted
      | None -> failwith "no operation"
    ;;

    let rehash t new_len =
      let old_buckets = t.buckets in
      let get_then_insert = function
        | Value (k, v) -> insert_naive t k v
        | _ -> ()
      in
      t.buckets <- Array.make new_len Empty;
      Array.iter (fun v -> get_then_insert v) old_buckets
    ;;

    let resize_if_needed table =
      match load_factor table with
      | x when x >= 0.5 -> rehash table (capacity table * 2)
      | x when x <= 0.25 -> rehash table (capacity table / 2)
      | _ -> ()
    ;;

    let rehash_then_do table f =
      resize_if_needed table;
      f table
    ;;

    let insert table = rehash_then_do table insert_naive
    let remove table = rehash_then_do table remove_naive
  end
end

module RedBlackTree = struct
  (* Exercise: functorized BST [★★★] *)

  module BstSet = struct
    type ord =
      | Lt
      | Eq
      | Gt

    module type T = sig
      type t

      val compare : t -> t -> ord
    end

    module Make (M : T) = struct
      type key = M.t

      type t =
        | Leaf
        | Node of key * t * t

      let cmp = M.compare
      let empty = Leaf

      let rec size = function
        | Leaf -> 0
        | Node (_, left, right) -> 1 + size left + size right
      ;;

      let rec add x = function
        | Leaf -> Node (x, Leaf, Leaf)
        | Node (v, l, r) when cmp v x = Gt -> Node (v, add x l, r)
        | Node (v, l, r) when cmp v x = Lt -> Node (v, l, add x r)
        | t -> t
      ;;

      let rec mem x = function
        | Leaf -> false
        | Node (v, l, _) when cmp v x = Gt -> mem x l
        | Node (v, _, r) when cmp v x = Lt -> mem x r
        | _ -> true
      ;;

      let rec remove_union lt = function
        | Leaf -> lt
        | Node (v, l, r) -> Node (v, remove_union lt l, r)
      ;;

      let rec remove x = function
        | Leaf -> failwith "tree doesn't contain the removal element"
        | Node (v, l, r) when cmp v x = Gt -> Node (v, remove x l, r)
        | Node (v, l, r) when cmp v x = Lt -> Node (v, l, remove x r)
        | Node (_, l, r) -> remove_union l r
      ;;

      let update x x' t = t |> remove x |> add x'
    end
  end

  (* Exercise: efficient traversal [★★★] *)
  module Traversal = struct
    type tree =
      | Leaf
      | Node of tree * int * tree

    let preorder t =
      let rec preorder_aux acc = function
        | Leaf -> List.rev acc
        | Node (l, v, r) -> preorder_aux (preorder_aux (v :: acc) l) r
      in
      preorder_aux [] t
    ;;

    let inorder t =
      let rec go acc = function
        | Leaf -> List.rev acc
        | Node (l, v, r) -> go (v :: go acc l) r
      in
      go [] t
    ;;

    let postorder t =
      let rec go acc = function
        | Leaf -> List.rev acc
        | Node (l, v, r) -> v :: go (go acc l) r
      in
      go [] t
    ;;

    let t =
      Node
        ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))
        , 4
        , Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )
    ;;

    (*{v
      t is    
              4
            /   \
            2     6
          / \   / \
         1   3 5   7
      v}*)

    let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])
    let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])
    let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])
  end
end

module Sequences = struct
  (* Exercise: pow2 [★★] *)
  type 'a sequence = Cons of 'a * (unit -> 'a sequence)

  let pow2 =
    let rec from n = Cons (n, fun () -> from (2 * n)) in
    from 1
  ;;

  (* Exercise: more sequences [★★] *)
  let _evens =
    let rec from n = Cons (n, fun () -> from (n + 2)) in
    from 0
  ;;

  let rec from_with f n = Cons (n, fun () -> from_with f (f n))
  let evens = from_with (fun x -> x + 2) 0

  let repeat_alphabet =
    from_with (fun x -> if x = 'z' then 'a' else Char.(chr (code x + 1))) 'a'
  ;;

  let rec pseudorandom_gen n =
    Cons (n * (n lsl 3) mod 50 > 25, fun () -> pseudorandom_gen (n + 1))
  ;;

  let coin_flip = pseudorandom_gen 0

  (* Exercise: nth [★★] ; hd tl [★★] *)
  let hd (Cons (h, _)) = h
  let tl (Cons (_, t)) = t ()

  let rec take : int -> 'a sequence -> 'a list =
    fun n s -> if n = 0 then [] else hd s :: take (n - 1) (tl s)
  ;;

  let rec nth : 'a sequence -> int -> 'a =
    fun s n -> if n = 0 then hd s else nth (tl s) (n - 1)
  ;;

  let seq_cons h l = Cons (h, fun () -> l)

  let rec filter f s =
    if f (hd s) then seq_cons (hd s) (filter f (tl s)) else filter f (tl s)
  ;;

  (* Exercise: interleave [★★★] *)

  let rec interleave_first : 'a sequence -> 'a sequence -> 'a sequence =
    fun s1 s2 -> seq_cons (hd s1) (interleave_second (tl s1) s2)

  and interleave_second : 'a sequence -> 'a sequence -> 'a sequence =
    fun s1 s2 -> seq_cons (hd s2) (interleave_first s1 (tl s2))
  ;;

  (* Exercise: sift [★★★] *)

  let sift n s = filter (fun x -> x mod n <> 0) s

  (* Exercise: primes [★★★] *)
  let rec primes_gen n = Cons (n, fun () -> sift n (primes_gen (n + 1)))
  let primes = primes_gen 2

  (* Exercise: approximately e [★★★★]
     AND
     Exercise: better e [★★★★]

     TODO TODO !!!
  *)

  (* Exercise: different sequence rep [★★★] *)
  module MoreLazySeq = struct
    type 'a sequence = Cons of (unit -> 'a * 'a sequence)

    let hd (Cons s) = fst (s ())
    let tl (Cons s) = snd (s ())

    (* cons *)
    let cons h t = Cons (fun () -> h, t)

    (* nats *)
    let rec nats_gen n = cons n (nats_gen (n + 1))
    let nats = nats_gen 1

    (* map *)
    let rec map f s = cons (f (hd s)) (map f (tl s))

    (* [Answer] : Because this type even lazy to calculate the first value *)
  end
end

module Lazy = struct
  (* Exercise: lazy hello [★] *)
  let lazy_hello = lazy (print_string "hello lazy world")

  (* Exercise: lazy and [★★] *)
  let ( &&& ) lhs rhs = Lazy.force lhs && Lazy.force rhs

  (* Exercise: lazy sequence [★★★] *)
  type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

  let hd (Cons (h, _)) = h
  let tl (Cons (_, t)) = Lazy.force t

  (* cons *)
  let cons h t = Cons (h, lazy t)

  (* map *)
  let rec map f seq = cons (f (hd seq)) (map f (tl seq))

  (* filter *)
  let rec filter f seq =
    match f (hd seq) with
    | true -> cons ((hd seq) (filter f (tl seq)))
    | false -> filter f (tl seq)
  ;;
end

module Lwt_Promise = struct
  (** A signature for Lwt-style promises, with better names *)
  module type PROMISE = sig
    type 'a state =
      | Pending
      | Resolved of 'a
      | Rejected of exn

    type 'a promise
    type 'a resolver

    (** [make ()] is a new promise and resolver. The promise is pending. *)
    val make : unit -> 'a promise * 'a resolver

    (** [return x] is a new promise that is already resolved with value
        [x]. *)
    val return : 'a -> 'a promise

    (** [state p] is the state of the promise *)
    val state : 'a promise -> 'a state

    (** [resolve r x] resolves the promise [p] associated with [r] with
        value [x], meaning that [state p] will become [Resolved x].
        Requires: [p] is pending. *)
    val resolve : 'a resolver -> 'a -> unit

    (** [reject r x] rejects the promise [p] associated with [r] with
        exception [x], meaning that [state p] will become [Rejected x].
        Requires: [p] is pending. *)
    val reject : 'a resolver -> exn -> unit

    (** [p >>= c] registers callback [c] with promise [p].
        When the promise is resolved, the callback will be run
        on the promises's contents.  If the promise is never
        resolved, the callback will never run. *)
    val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
  end

  module Promise : PROMISE = struct
    type 'a state =
      | Pending
      | Resolved of 'a
      | Rejected of exn

    (** RI: the input may not be [Pending] *)
    type 'a handler = 'a state -> unit

    (** RI: if [state <> Pending] then [handlers = []]. *)
    type 'a promise =
      { mutable state : 'a state
      ; mutable handlers : 'a handler list
      }

    let enqueue (handler : 'a state -> unit) (promise : 'a promise) : unit =
      promise.handlers <- handler :: promise.handlers
    ;;

    type 'a resolver = 'a promise

    (** [write_once p s] changes the state of [p] to be [s].  If [p] and [s]
        are both pending, that has no effect.
        Raises: [Invalid_arg] if the state of [p] is not pending. *)
    let write_once p s =
      match p.state with
      | Pending -> p.state <- s
      | _ -> invalid_arg "cannot write twice"
    ;;

    let make () =
      let p = { state = Pending; handlers = [] } in
      p, p
    ;;

    let return x = { state = Resolved x; handlers = [] }
    let state p = p.state

    (** requires: [st] may not be [Pending] *)
    let resolve_or_reject (r : 'a resolver) (st : 'a state) =
      assert (st <> Pending);
      let handlers = r.handlers in
      r.handlers <- [];
      write_once r st;
      List.iter (fun f -> f st) handlers
    ;;

    let reject r x = resolve_or_reject r (Rejected x)
    let resolve r x = resolve_or_reject r (Resolved x)

    let handler (resolver : 'a resolver) : 'a handler = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x -> resolve resolver x
    ;;

    let handler_of_callback callback resolver : 'a handler = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x ->
        let promise = callback x in
        (match promise.state with
         | Resolved y -> resolve resolver y
         | Rejected exc -> reject resolver exc
         | Pending -> enqueue (handler resolver) promise)
    ;;

    let ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise =
      fun input_promise callback ->
      match input_promise.state with
      | Resolved x -> callback x
      | Rejected exc -> { state = Rejected exc; handlers = [] }
      | Pending ->
        let output_promise, output_resolver = make () in
        enqueue (handler_of_callback callback output_resolver) input_promise;
        output_promise
    ;;
  end

  (* Exercise: promise and resolve [★★] *)
  module Exercise = struct
    let () =
      let open Promise in
      let p, r = make () in
      let f x =
        print_int x;
        return ()
      in
      let _ = p >>= f in
      resolve r 42
    ;;

    let () =
      let open Lwt.Infix in
      let p, r = Lwt.wait () in
      let _ = p >>= fun x -> Lwt_io.printl (string_of_int x) in
      Lwt.wakeup r 42
    ;;
  end

  module TimeChallenge = struct
    (** [delay s] is a promise that resolves after about [s] seconds. *)
    let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec

    (* Exercise: timing challenge 1 [★★] *)
    let delay_then_print () : unit Lwt.t =
      let open Lwt.Infix in
      delay 3. >>= fun () -> Lwt_io.printl "done"
    ;;

    (* Exercise: timing challenge 2 [★★★] *)
    open Lwt.Infix

    let timing2 () =
      let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
      let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
      let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
      Lwt_io.printl "all done"
    ;;

    (* [Answer] : Will delay 20 second *)

    (* Exercise: timing challenge 3 [★★★] *)
    let timing3 () =
      delay 1.
      >>= fun () ->
      Lwt_io.printl "1"
      >>= fun () ->
      delay 10.
      >>= fun () ->
      Lwt_io.printl "2"
      >>= fun () ->
      delay 20.
      >>= fun () -> Lwt_io.printl "3" >>= fun () -> Lwt_io.printl "all done"
    ;;

    (* [Answer] : Will delay 31 second *)

    (* Exercise: timing challenge 4 [★★★] *)
    let timing4 () =
      let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
      let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
      let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
      Lwt.join [ t1; t2; t3 ] >>= fun () -> Lwt_io.printl "all done"
    ;;
    (* [Answer]: will delay 20 second, and print '1' then '2' then '3'
       and 'all done' finally.
    *)
  end
end

module Monad_ = struct
  module type Monad = sig
    type 'a t

    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module MaybeImpl = struct
    type 'a t = 'a option

    let return x = Some x

    let ( >>= ) m f =
      match m with
      | Some x -> f x
      | None -> None
    ;;
  end

  module Maybe : Monad = MaybeImpl

  (* Exercise: add opt [★★] *)
  let add x y =
    let open Maybe in
    x >>= fun a -> y >>= fun b -> return (a + b)
  ;;

  (* Exercise: fmap and join [★★] *)
  module type ExtMonad = sig
    type 'a t

    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val join : 'a t t -> 'a t
  end

  module ExtMaybe : ExtMonad = struct
    include MaybeImpl

    let ( >>| ) x f =
      match x with
      | None -> None
      | Some v -> Some (f v)
    ;;

    let ( >>|| ) x f = x >>= fun v -> return (f v)

    let join = function
      | None -> None
      | Some x -> x
    ;;

    let join' x = x >>= Fun.id
  end

  (* Exercise: bind from fmap+join [★★★] *)
  module type FmapJoinMonad = sig
    type 'a t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val join : 'a t t -> 'a t
    val return : 'a -> 'a t
  end

  module type BindMonad = sig
    type 'a t

    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
    type 'a t = 'a M.t

    let return = M.return

    (* bind *)
    let ( >>= ) (x : 'a t) (f : 'a -> 'b t) : 'b t = M.(join (x >>| f))
  end

  (* Exercise: list monad [★★★] *)
  module ListMonad : ExtMonad = struct
    type 'a t = 'a list

    let return x = [ x ]

    let rec join = function
      | x :: xs -> x @ join xs
      | _ -> []
    ;;

    let rec ( >>| ) v f =
      match v with
      | [] -> []
      | x :: xs -> f x :: (xs >>| f)
    ;;

    let ( >>= ) x f = join (x >>| f)
  end

  (* Exercise: trivial monad laws [★★★] *)
  module Trivial : Monad = struct
    type 'a t = Wrap of 'a

    let return x = Wrap x
    let ( >>= ) (Wrap x) f = f x
  end

  (*
     - Law 1: [return x >>= f] behaves the same as [f x].
       Proof.
       forall x in any type, [return x] is [Wrap x]
       [Wrap x >>= f] is Destruct the x as ['a t] and take the contents as argument.
       So [Wrap x >>= f] is [f x].
       Qed.

     - Law 2: [m >>= return] behaves the same as [m].
       Proof.
       forall m is a Trivial type ['a t], which is [Wrap x] : [m = Wrap x]
       [m >>= return] is take the [x] and [return x] which is [Wrap x],
       [Wrap x = Wrap x],
       Qed.

     - Law 3: [(m >>= f) >>= g] behaves the same as [m >>= (fun x -> f x >>= g)].
       Proof.
       [(m >>= f) >>= g] is take the contents of [m] as [Wrap x] and aply [x]
       to [f] then get [f x] which is the [Wrap] type, [f x >>= g] is take same
       thing as [m >>= f],
       [m >>= (fun x -> f x >>= g)] is get the contents of [m] as [x] and aply
       [x] to [f] get [f x] then bind [f x] to [g], which is same as above.
       Qed.
  *)
end
