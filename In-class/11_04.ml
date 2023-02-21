(* Same as before *)

type value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * value_env


and value_env = (string * value) list

and expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

  | App of expr * expr
  | Lam of string * expr
         
type typ
  = Int_type   (* int *)
  | Bool_type  (* bool *)
  | Func_type of typ * typ   (* int -> int, int -> (int -> int) *)

type type_environment = (string * typ option) list 