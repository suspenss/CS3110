(* Exercise: values *)
let _ : int = 7 * (1 + 2 + 3)
let _ : string = "CS" ^ string_of_int 3110

(* Exercise: operators *)
let _ : int = 42 / 10
let _ : float = 3.14 *. 2.0
let _ : float = 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2

(* Exercise: equality *)
let _ : bool = 42 = 42
let _ : bool = "hi" = "hi"

(* physical equality, return false *)
let _ : bool = "hi" == "hi"

(* Exercise: assert
   when print 'assert true' the topleve print unit ().
   when print 'assert false' the topleve get Excpetion.
*)

(* Exercise: double fun *)
let double x = x * 2

(* Exercise: more fun *)
let cube l = l ** 3.

let sign x =
  if x > 0 then
    1
  else if x = 0 then
    0
  else
    -1
;;

let area radius = Float.pi *. (radius ** 2.)
let _ = assert (12.56 -. area 2. < 1e-5)

(* Exercise: RMS *)
let rms a b = sqrt (((a *. a) +. (b *. b)) /. 2.)

let _ =
  let res = rms 2. 3. in
  assert (res -. 2.54950975679639225 < 1e-5)
;;

(* Exercise: date fun *)
let data_fun d m =
  if
    m = "Jan"
    || m = "Mar"
    || m = "May"
    || m = "Jul"
    || m = "Aug"
    || m = "Oct"
    || m = "Dec"
  then
    1 <= d && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov" then
    1 <= d && d <= 30
  else if m = "Feb" then
    1 <= d && d <= 28
  else
    false
;;

(* Exercise: fib *)
let rec fib n =
  if n = 1 then
    1
  else if n = 2 then
    1
  else
    fib (n - 1) + fib (n - 2)
;;

let rec fib_match n =
  match n with
  | 1 -> 1
  | 2 -> 1
  | _ -> fib_match (n - 1) + fib_match (n - 2)
;;

let _ = assert (fib_match 10 = fib 10)

(* Exercise: fib fast *)
let rec fib_fast_aux n pp p =
  match n with
  | 1 -> p
  | _ -> fib_fast_aux (n - 1) p (pp + p)
;;

let fib_fast n = fib_fast_aux n 0 1

(* Exercise: poly types *)
let f (x : bool) : bool =
  if x then
    x
  else
    x
;;

let g (x : 'a) (y : bool) : 'a =
  if y then
    x
  else
    x
;;

let h (x : bool) (y : 'a) (z : 'a) : 'a =
  if x then
    y
  else
    z
;;

let i (x : bool) (y : 'a) (_z : 'b) : 'a =
  if x then
    y
  else
    y
;;

(*Exercise: divide *)
let divide ~(numerator : float) ~(denominator : float) =
  numerator /. denominator
;;

(*Exercise: associativity *)
let ( +/. ) f1 f2 = (f1 +. f2) /. 2.
