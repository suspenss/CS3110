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

      let equal l r = compare l r = 0
      let hash = Hashtbl.hash
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

    let rec findi t k =
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
