module MutLinkList = struct
  type 'a node =
    { value : 'a
    ; mutable next : 'a node option
    }

  type 'a linklist = { mutable head : 'a node option }

  let empty () : 'a linklist = { head = None }

  let insert_front (x : 'a) (lst : 'a linklist) : unit =
    lst.head <- Some { value = x; next = lst.head }
  ;;

  let rec to_list_aux (lst : 'a node option) : 'a list =
    match lst with
    | None -> []
    | Some { value; next } -> value :: to_list_aux next
  ;;

  let to_list lst = to_list_aux lst.head

  let rec insert_aux i x lst =
    match lst, i with
    | None, _ -> invalid_arg "Out of Bound"
    | Some cur, 0 -> cur.next <- Some { value = x; next = cur.next }
    | Some { next }, _ -> insert_aux (i - 1) x next
  ;;

  let insert i x lst = insert_aux i x lst.head

  let pop_front lst =
    match lst.head with
    | None -> None
    | Some cur ->
      lst.head <- Some cur;
      Some cur.value
  ;;

  let rec get_aux i lst =
    match lst, i with
    | None, _ -> invalid_arg "Out of Bound"
    | Some cur, 0 -> cur.value
    | Some { next }, _ -> get_aux (i - 1) next
  ;;

  let get i lst = get_aux i lst.head
end

module type MutableStack = sig
  (** ['a t] is the type of mutable stacks whose elements have type ['a].
      The stack is mutable not in the sense that its elements can
      be changed, but in the sense that it is not persistent:
      the operations [push] and [pop] destructively modify the stack. *)
  type 'a t

  (** Raised if [peek] or [pop] encounter the empty stack. *)
  exception Empty

  (** [empty ()] is the empty stack *)
  val empty : unit -> 'a t

  (** [push x s] modifies [s] to make [x] its top element.
      The rest of the elements are unchanged. *)
  val push : 'a -> 'a t -> unit

  (**[peek s] is the top element of [s].
     Raises: [Empty] if [s] is empty. *)
  val peek : 'a t -> 'a

  (** [pop s] removes the top element of [s].
      Raises: [Empty] if [s] is empty. *)
  val pop : 'a t -> unit
end
