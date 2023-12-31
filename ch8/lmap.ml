module type map = sig
  (** [('k, 'v) t] is the type of maps that bind keys of type
      ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] is the same map as [m], but with an additional
      binding from [k] to [v].  If [k] was already bound in [m],
      that binding is replaced by the binding to [v] in the new map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [find k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] is the same map as [m], but without any binding of [k].
      If [k] was not bound in [m], then the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  (** [empty] is the empty map. *)
  val empty : ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as
      association list [lst].
      Requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (** [bindings m] is an association list containing the same
      bindings as [m]. There are no duplicates in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module ListMap : map = struct
  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map {k1 : v1, k2 : v2,
      ..., kn : vn}. If a key appears more than once in the list, then in the
      map it is bound to the left-most occurrence in the list. For example,
      [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
      the empty map.
      RI: none. *)
  type ('k, 'v) t = ('k * 'v) list

  (** Efficiency: O(1). *)
  let insert k v m = (k, v) :: m

  (** Efficiency: O(n). *)
  let find = List.assoc_opt

  (** Efficiency: O(n). *)
  let remove k lst = List.filter (fun (k', _) -> k <> k') lst

  (** Efficiency: O(1). *)
  let empty = []

  (** Efficiency: O(1). *)
  let of_list lst = lst

  (** [keys m] is a list of the keys in [m], without
      any duplicates.
      Efficiency: O(n log n). *)
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] is [(k, v)], where [v] is the value that [k]
      binds in [m].
      Requires: [k] is a key in [m].
      Efficiency: O(n). *)
  let binding m k = k, List.assoc k m

  (** Efficiency: O(n log n) + O(n) * O(n), which is O(n^2). *)
  let bindings m = List.map (binding m) (keys m)
end

module HashMap = struct
  type ('k, 'v) map =
    { hash : 'k -> int
    ; mutable size : int
    ; mutable buckets : ('k * 'v) list array
    }

  let capacity table = Array.length table.buckets

  let load_foactor table =
    float_of_int table.size /. float_of_int (capacity table)
  ;;

  let creat hash n = { hash; size = 0; buckets = Array.make n [] }
  let index k table = table.hash k mod capacity table

  let insert_no_resize k v table =
    let i = index k table in
    let old_bucket = table.buckets.(i) in
    table.buckets.(i) <- (k, v) :: List.remove_assoc k old_bucket;
    if List.mem_assoc k old_bucket then table.size <- table.size + 1;
    ()
  ;;

  let rehash table n =
    let insert_no_check k v table =
      let i = index k table in
      table.buckets.(i) <- (k, v) :: table.buckets.(i);
      table.size <- table.size + 1
    in
    let rebinding_bucket =
      List.iter (fun (k, v) -> insert_no_check k v table)
    in
    let old_buckets = table.buckets in
    table.buckets <- Array.make n [];
    table.size <- 0;
    Array.iter rebinding_bucket old_buckets
  ;;

  let resize_if_needed table =
    match load_foactor table with
    | x when x >= 2.0 -> rehash table (capacity table * 2)
    | x when x <= 0.5 -> rehash table (capacity table / 2)
    | _ -> ()
  ;;

  let insert k v table =
    resize_if_needed table;
    insert_no_resize k v table
  ;;

  let find k table = List.assoc_opt k table.buckets.(index k table)

  let remove_no_resize k table =
    let i = index k table in
    let old_bucket = table.buckets.(i) in
    match List.mem_assoc k old_bucket with
    | true ->
      table.buckets.(i) <- List.remove_assoc k old_bucket;
      table.size <- table.size - 1
    | false -> ()
  ;;

  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab
  ;;

  let bindings table =
    Array.fold_left
      (fun acc b -> List.fold_left (fun acc (k, v) -> (k, v) :: acc) acc b)
      []
      table.buckets
  ;;

  let of_list hash lst =
    let table = creat hash (List.length lst) in
    List.iter (fun (k, v) -> insert k v table) lst;
    table
  ;;
end
