(* mutable field *)
type student =
  { name : string
  ; mutable gpa : float
  }

let john = { name = "john"; gpa = 3.6 }
let () = john.gpa <- 4.0

(* ref *)
let ref1 = ref true
let ref2 = ref [ 1; 2; 3 ]
let ref3 = [ ref 1; ref 2 ]

(* inc *)
let inc = ref (fun x -> x + 1)
let _ = !inc 3110

(* addition assignmention *)
let ( +:= ) x y = x := !x + y

let () =
  let x = ref 0 in
  x +:= 10
;;

module Norm = struct
  (* https://en.wikipedia.org/wiki/Norm_(mathematics)#Euclidean_norm *)
  type vector = float array

  let norm vec =
    vec |> Array.fold_left (fun acc x -> acc +. (x ** 2.)) 0. |> sqrt
  ;;

  let normalize vec = (fun x -> Array.map (( /. ) x) vec) (norm vec)

  let norm_loop vec =
    let res = ref 0. in
    for i = 0 to Array.length vec - 1 do
      res := !res +. (vec.(i) ** 2.)
    done;
    !res
  ;;

  let normalize_loop vec =
    let dist = norm_loop vec in
    Array.init (Array.length vec) (fun i -> vec.(i) /. dist)
  ;;
end

module InitMatrix = struct
  type 'a matrix = 'a array array

  let init_matrix (r : int) (c : int) (f : int -> int -> 'a) : 'a matrix =
    Array.init r (fun i -> Array.init c (fun j -> f i j))
  ;;
end
