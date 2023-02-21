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

  | Let of string * expr * expr
  | Id of string

type typ
  = Int_type
  | Bool_type

type result
  = OK of typ
  | Errs of (expr * string) list

(* infer types 
  1 + 3   : integer 
  false && (1 > 3) : bool
  let x = 4 in x + 5 : int
  let x = 4 in x && false :  [ (x && false, "bools required for &&"  ) ]
     or as we implemented [ (x, "bool value expected, got an integer" ) ]
 *)

let expect_Int (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> []
  | OK Bool_type ->  [(e, "expected Int, got Bool") ]
  | Errs errs -> errs

let expect_Bool (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Bool_type -> []
  | OK Int_type ->  [(e, "expected Bool, got Int") ]
  | Errs errs -> errs

type typ_env = (string * typ option) list
             
let rec lookup_typ (x: string)  (t_env: typ_env) : result =
  match t_env with
  | [] -> Errs ( [(Id x, "identifier not bound")] )
  | (y, Some ty)::ys -> if x = y then OK ty else lookup_typ x ys
                
let rec type_infer (e: expr) (t_env: typ_env) : result =
  match e with
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
     ( match type_infer e1 t_env, type_infer e2 t_env with
       | OK Int_type, OK Int_type -> OK Int_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  | Lt (e1, e2) ->
     ( match type_infer e1 t_env, type_infer e2 t_env with
       | OK Int_type, OK Int_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  | And (e1, e2) ->
     ( match type_infer e1 t_env, type_infer e2 t_env with
       | OK Bool_type, OK Bool_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Bool r1 e1 @ expect_Bool r2 e2)
     )
  | Id x -> lookup_typ x t_env
          
  | Let (x, e1, e2) ->
     let re1 = type_infer e1 t_env
     in
     match re1 with
     | Errs errs -> Errs errs
     | OK ty -> type_infer e2 ( (x,Some ty) :: t_env ) 
  
let infer (e: expr) : result = type_infer e []
                    



let free_vars (e: expr) : string list = 
 let rec fv (e:expr) (bound_vars: string list) : string list =
   match e with
   | Val _ -> []
   | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2)
   | Lt (e1, e2) | Eq (e1, e2) | And (e1, e2) 
     -> fv e1 bound_vars @ fv e2 bound_vars

   | Let (x, e1, e2) -> fv e1 bound_vars @ fv e2 (x::bound_vars) 

   | Id x -> if List.mem x bound_vars then [] else [x]
 
 in
 fv e []
        

let e1 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )
       
let e2 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )
       
let e3 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              And (Id "x", Val (Bool true))
           )

let e4 = Let ("x", Add (Id "x", Val (Int 4)),
              And (Id "y", Val (Bool true))
           )



type value_env = (string * value) list
               
let rec lookup_val (x: string) (v_env: value_env) : value =
  failwith "finish"

let rec eval (v_env: value_env) (e: expr) : value =
  match e with
  | Val v -> v
  | Add (e1, e2) ->
      ( match (eval e1, eval e2) with
        | Int i1, Int i2 -> Int (i1 + i2)
        | _, _ -> failwith "Oh nuts, expected integer values"
      )

  | _ -> failwith "finish"

(* ////// *)
(* Evaluation with functions, and static type checking *)

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