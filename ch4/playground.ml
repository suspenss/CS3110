let twice f x = x |> f |> f
let quad = twice (fun x -> x * x)

(* compose *)
let compose f g x = f (g x)
let ( |- ) = compose

(* pipeline *)
let rec sum_quare n =
  let rec loop acc = function
    | x when x > 0 -> acc
    | x -> loop (acc + (x * x)) (x - 1)
  in
  loop 0 n
;;

let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let square x = x * x
let sum = List.fold_left ( + ) 0

let sum_sq n =
  0 -- n (* [0;1;2;...;n] *)
  |> List.map square (* [0;1;4;...;n*n] *)
  |> sum (*  0+1+4+...+n*n *)
;;
