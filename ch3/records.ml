(* gender infomation is a varient *)
type gender =
  | Male
  | Female

(* student information *)
type student =
  { name : string
  ; id : string
  ; grad_year : int
  ; gender : gender
  ; score : float list
  }

(* employee information *)
type employee =
  { age : int
  ; name : string
  ; location : string
  ; id : int
  }

let ss =
  { name = "ss"
  ; id = "120"
  ; grad_year = 1999
  ; gender = Male
  ; score = [ 20.; 100.; 0. ]
  }
;;

let sss = { ss with name = "sss" }

(* this match represent the partten fields can less than record fields *)
let _ =
  match sss with
  | { name; id } -> Printf.sprintf "name : %s, id is %s, \n%!" name id
;;




