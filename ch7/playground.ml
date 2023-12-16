(* syntax *)
let x = ref 1
let () = x := 10
let y = !x

let next_val =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
;;

(* pointer *)
module Pointer = struct
  type 'a t = 'a ref option

  exception SegmantationFault

  let null : 'a t = None
  let malloc (x : 'a) = Some (ref x)

  let deref (ptr : 'a t) =
    match ptr with
    | None -> raise SegmantationFault
    | Some r -> !r
  ;;

  let ( ~* ) = deref

  let assign (ptr : 'a t) (v : 'a) =
    match ptr with
    | None -> raise SegmantationFault
    | Some r -> r := v
  ;;

  let ( =* ) = assign
end

let y = Pointer.malloc 12
let dy = Pointer.deref y

module RecRef = struct
  let fact0 = ref (fun x -> x)
  let fact x = if x = 1 then 1 else x * !fact0 (x - 1)
  let () = fact0 := fact
end

module RefList = struct
  type 'a mylist = 'a node option ref

  and 'a node =
    { value : 'a
    ; next : 'a mylist
    }

  let empty : 'a mylist = ref None

  let insert_first (v : 'a) (lst : 'a mylist) : unit =
    lst := Some { value = v; next = ref !lst }
  ;;

  let ref set lst i x =
    match lst, i with
    | None, _ -> invalid_arg "Out of Bound"
    | Some { value }, 0 -> value := x
    | Some { next }, _ -> set next (i - 1) x
  ;;

  let rec to_list (lst : 'a mylist) : 'a list =
    match !lst with
    | None -> []
    | Some { value; next } -> value :: to_list next
  ;;
end

module MutableField = struct
  type point =
    { x : int
    ; y : int
    ; mutable c : string
    }

  let p1 = { x = 1; y = 0; c = "red" }
  let () = p1.c <- "black"

  module MutableLinkList = struct
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

  module MutableStack = struct
    type 'a t = 'a MutableLinkList.linklist

    exception Empty

    let empty () = MutableLinkList.empty
    let push = MutableLinkList.insert_front
    let pop = MutableLinkList.pop_front
    let peek = MutableLinkList.get 0
  end
end

module ArrayChapter = struct
  let arr = [| 1; 2 |]
  let print_arr = Array.iter (Printf.printf "%F ")
  let arr1 = [| 1.; 2.; 3. |]

  let () =
    for i = 0 to Array.length arr1 - 1 do
      ()
    done
  ;;

  let add_vec = Array.map2 ( + )
end
