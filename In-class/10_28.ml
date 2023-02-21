type value 
  = Int of int
  | Bool of bool
type expr 
  = Val of value
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

let rec eval (e: expr) : value = match e with
  | Val v -> v
  | Add (e1, e2) ->
      ( match (eval e1, eval e2) with
        | Int i1, Int i2 -> Int (i1 + i2)
        | _, _ -> failwith "Oh nuts, expected integer values"
      )
  | Eq (e1, e2) ->
     ( match (eval e1, eval e2) with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> failwith "Oh nuts, mismatched types"
     )

let e1 = Add (Val (Int 1), Val (Int 2))
let e2 = Add (Val (Int 1), Val (Bool false))

type int_expr =
  | Int of int
  | Add of int_expr * int_expr  
  | Sub of int_expr * int_expr
  | Mul of int_expr * int_expr
  | Div of int_expr * int_expr
type bool_expr =
  | True
  | False
  | Lt of int_expr * int_expr
  | Eq of int_expr * int_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr
