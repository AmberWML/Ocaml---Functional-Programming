type expr
  = Int of int
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
         
let e1 = Add (Mul (Int 2, Int 4), Int 5)

let rec lookup (env: (string * int) list) (n: string) : int =
  match env with
  | [] -> failwith ("The name " ^ n ^ " is not bound by a let.")
  | (n',v)::rest when n' = n -> v
  | (_,_)::rest -> lookup rest n
  
let rec eval (e: expr) : int =
  match e with
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Div (e1, e2) -> eval e1 / eval e2