module Fibonacci = struct
  let fib_memoized n =
    let open Printf in
    let fibs = Array.make (n + 1) None in
    let rec get_v_lazy i =
      match fibs.(i) with
      | Some x -> x
      | None ->
        let v = get_value (i - 1) + get_value (i - 2) in
        printf "%d is calculated\n" v;
        fibs.(i) <- Some v;
        v
    and get_value n =
      if n < 2 then
        1
      else
        get_v_lazy n
    in
    get_value n
  ;;
end

let fibs100 = Fibonacci.fib_memoized 100

module PoorObject = struct
  let memo f =
    let dict = Hashtbl.create 11 in
    fun x ->
      try Hashtbl.find dict x with
      | Not_found ->
        let v = f x in
        Hashtbl.add dict x v;
        v
  ;;

  let memo_rec f =
    let h = Hashtbl.create 20 in
    let rec g x =
      try Hashtbl.find h x with
      | Not_found ->
        let v = f g x in
        Hashtbl.add h x v;
        v
    in
    g
  ;;

  let memo_fib =
    let aux g x =
      if x < 2 then
        1
      else
        g (x - 1) + g (x - 2)
    in
    memo_rec aux
  ;;
end

let t =
  match List.hd [ 1; 2 ] with
  | v -> string_of_int v
  | exception Failure s -> s
;;

module MaxIndependentSet = struct
  type tree =
    | Empty
    | Node of int * tree * tree

  let rec max_independent_set : tree -> int =
    fun tree -> max (mis_in tree) (mis_out tree)

  and mis_in = function
    | Empty -> 0
    | Node (v, l, r) -> v + mis_out l + mis_out r

  and mis_out = function
    | Empty -> 0
    | Node (_, l, r) -> max_independent_set l + max_independent_set r
  ;;
end

module MISMemoized = struct
  (* This version memoizes the optimal fun value for each tree node. It
     also remembers the best invite list. Each tree node has the name of
     the employee as a string. *)
  type tree =
    | Empty
    | Node of int * string * tree * tree * (int * string list) option ref

  let rec party t : int * string list =
    let get_best_list t =
      let (infun, innames), (outfun, outnames) = party_in t, party_out t in
      if infun > outfun then
        infun, innames
      else
        outfun, outnames
    in
    let cache memo result =
      memo := Some result;
      result
    in
    let get_cached memo =
      match !memo with
      | Some result -> result
      | None -> cache memo (get_best_list t)
    in
    match t with
    | Empty -> 0, []
    | Node (_, _, _, _, memo) -> get_cached memo

  and party_in = function
    | Empty -> 0, []
    | Node (v, name, l, r, _) ->
      let (lfun, lnames), (rfun, rnames) = party_out l, party_out r in
      v + lfun + rfun, (name :: lnames) @ rnames

  and party_out = function
    | Empty -> 0, []
    | Node (_, _, l, r, _) ->
      let (lfun, lnames), (rfun, rnames) = party l, party r in
      lfun + rfun, lnames @ rnames
  ;;
end

module LazyFibs = struct
  let rec f = function
    | 0 | 1 -> lazy 1
    | x -> lazy (Lazy.force (f (x - 1)) + Lazy.force (f (x - 2)))
  ;;
end
