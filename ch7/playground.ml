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
