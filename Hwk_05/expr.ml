(* Hwk 05.  Extend the construts below as specified.
  Amber Wong
 *)



type value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * value_environment
  | Ref of value ref

and value_environment = (string * value) list
                               
and expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * typ * expr * expr
  | Id  of string

  | App of expr * expr
  | Lam of string * typ * expr
  
  | LetRec of string * typ * expr * expr
  | If of expr * expr * expr

and typ = Int_type 
        | Bool_type
        | Func_type of typ * typ

type type_environment = (string * typ option) list 


(* Part 1. Complete unparse *)
let rec changetype (t:typ): string =
  match t with
  | Int_type ->"int"
  | Bool_type ->"bool"
  | Func_type(t1,t2) -> "(" ^ changetype t1 ^ " -> " ^ changetype t2 ^ ")"

let rec unparse (e: expr) : string =
  match e with
  | Val (Int i) -> string_of_int i
  | Val (Bool b) -> string_of_bool b
  | Val (Closure (s,exp,envir)) -> "<fun>"
  | Val (Ref s) -> "reference"
  | Add (x,y) -> "(" ^ unparse x ^ " + " ^ unparse y ^ ")"
  | Sub (x,y) -> "(" ^ unparse x ^ " - " ^ unparse y ^ ")"
  | Mul (x,y) -> "(" ^ unparse x ^ " * " ^ unparse y ^ ")"
  | Div (x,y) -> "(" ^ unparse x ^ " / " ^ unparse y ^ ")"
  | Lt  (x,y) -> "(" ^ unparse x ^ " < " ^ unparse y ^ ")"
  | Eq  (x,y) -> "(" ^ unparse x ^ " = " ^ unparse y ^ ")"
  | And (x,y) -> "(" ^ unparse x ^ " && " ^ unparse y ^ ")"
  | Not x -> "<>(" ^ unparse x ^ ")"
  (* | Let (s,typ,e1,e2) -> match typ with 
        | Int_type ->  "(let " ^ s ^ " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
        | Bool_type -> "(let " ^ s ^ " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
        | Func_type (type1,type2) -> "(let " ^ s ^ " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")" *)
  | Let (s,typ,e1,e2) ->  "(let " ^ s ^" : "^ changetype typ ^" = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
  | Id s -> s
  | App(x,y) -> "(" ^ unparse x ^ " " ^ unparse y ^ ")"
  (* | Lam(s,typ,exp) ->  match typ with 
        | Int_type ->  "(fun (" ^ s ^ ": int) -> " ^ unparse dexpr ^ ")"
        | Bool_type -> "(fun (" ^ s ^ ": bool) -> " ^ unparse dexpr ^ ")"
        | Func_type (type1,type2) -> "(fun (" ^ s ^ ": f) -> " ^ unparse dexpr ^ ")" *)
  | Lam(s,typ,exp) ->  "(fun (" ^ s ^ ": "^ changetype typ ^") -> " ^ unparse exp ^ ")"
  | LetRec(s,typ,e1,e2) ->  "(let rec " ^ s ^ " : "^ changetype typ ^" = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
  | If (x, y, z) ->  "(if " ^ unparse x ^ " then " ^ unparse y ^ " else " ^ unparse z ^ ")"



(* Part 2. Complete freevars *)       
let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> []
  | Add (x,y)| Sub (x,y)| Mul (x,y)| Div (x,y)| Lt  (x,y)| Eq  (x,y)| And (x,y) | App (x,y) ->freevars x @ freevars y
  | Not x -> freevars x
  | If (x, y, z) -> freevars x @ freevars y @ freevars z
  | Let (s,typ,e1,e2) -> freevars e1 @ (List.filter (fun x -> s <> x) (freevars e2))
  | Id x -> [x]
  | Lam  (s,typ,e1) -> List.filter (fun x -> s <> x) (freevars e1)
  | LetRec(s,typ,e1,e2) ->
    (List.filter (fun x -> s <> x) (freevars e1 @ freevars e2))
 

       
(* Part 3. Type checking *)           
type result = OK of typ
            | Errs of (expr * string) list

let rec lookup (n:string) (env:type_environment) : result =
  match env with
  | [] -> Errs [ (Id n, "Identifier not found") ]
  | (n',Some t) :: rest when n = n' -> OK t
  | _ :: rest -> lookup n rest

let expect_Int_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> [] 
  | OK Bool_type -> [ (e, "expect Int type") ] 
  | Errs errs -> errs 

let expect_Bool_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Bool_type -> [] 
  | OK Int_type -> [ (e, "expect Bool type") ] 
  | Errs errs -> errs 

let rec type_check (e:expr) (env:type_environment) : result =
  match e with
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2)->
     (match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Int_type
      | r1, r2 -> Errs (expect_Int_type r1 e1 @ expect_Int_type r2 e2)
     )
  | Eq (e1, e2) | Lt (e1, e2) ->
     (match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Bool_type
      | r1, r2 -> Errs (expect_Int_type r1 e1 @ expect_Int_type r2 e2)
     )
  
  | And (e1, e2) ->
     (match type_check e1 env, type_check e2 env with
      | OK Bool_type, OK Bool_type -> OK Bool_type
      | r1, r2 -> Errs (expect_Bool_type r1 e1 @ expect_Bool_type r2 e2)
     )
  | Let (n, t, bexpr, body) ->
     (match type_check bexpr env with
      | Errs errs -> Errs errs
      | OK ty -> 
         if ty <> t
         then Errs [ (e, "do not match.") ]
         else type_check body ((n,Some t)::env)
     )
  | Id n -> lookup n env
  | Not n ->  
      (match type_check n env with
      | OK Bool_type -> OK Bool_type
      | r1-> Errs (expect_Bool_type r1 n)
     )
  
  | Lam (n,t,e) ->
      (match type_check e ((n,Some t)::env) with
        |OK var -> OK (Func_type (t,var))
        |r1 -> r1
      )
  | App (e1,e2) ->
    (match type_check e1 env, type_check e2 env with
      | OK (Func_type(i,o)), OK var -> if var = i then OK o else Errs [(e,"do not match")]
      | OK _, var -> Errs [(e1,"expecting function")]
      | Errs err,var  -> Errs (err)
     )

  | LetRec(n, t, bexpr, body)-> 
    (match type_check bexpr ((n,Some t)::env) with
      | Errs errs -> Errs errs
      | OK ty -> 
         if ty <> t
         then Errs [ (e, "do not match.") ]
         else type_check body ((n,Some t)::env)
     )
  | If (e1,e2,e3) ->
      let helper expr = match type_check expr env with
     | Errs err -> err
     | _ -> []
     in
     let r1 = type_check e1 env in let r2 = type_check e2 env in let r3 =type_check e3 env in
     let er1 = helper e1 in let er2 = helper e2 in let er3 = helper e3 in      
     ( match r1 with
      |OK Bool_type -> (match r2,r3 with 
                        |OK v,OK r2 -> if v=r2 then OK v else Errs [ (e, "do not match.") ]
                        |_,_ -> Errs (er2@er3) )
      |OK r1 -> Errs [(e1,"expecting boolen")]
      |Errs _ -> Errs (er1@er2@er3)
     )
 
       
let check e = type_check e [] 

(* 
let rec eval (env: value_environment) (e:expr) : value =
  match e with
  | Val v -> v

  | Add (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _, _ -> raise (Failure "incompatible types, Sub")
     )
  | Mul (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _, _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 / v2)
       | _, _ -> raise (Failure "incompatible types, Div")
     )
  | Lt (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int e1, Int e2 -> Bool (e1 < e2)
       | _, _ -> raise (Failure "incompatible types, Lt")
     )
  | Eq (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> raise (Failure "incompatible types, Eq")
     )
 
  | And (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "Fail in And")
     )
  
  | Not e1 ->
     ( match eval env e1 with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "Fail in Not")
     )
  | Let (n, t, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body
  | Id n -> lookup n env

  | If (e1,e2,e3)->
     
  | App of expr * expr
  
  | Lam (n,t, dexpr) ->
    
  | LetRec of string * typ * expr * expr
  
  

  | _ -> failwith "complete this function" *)

(* some sample expressions *)

let e1 = Add (Val (Int 3), Val (Int 5))
let e2 = Add (Val (Int 3), Val (Bool true))
let e3 = Mul (Val (Bool true), Val (Int 5))
let e4 = Add (e2, e3)

let e5 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )
       
let e6 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )
       
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e8 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "y",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let err_1 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
                 And (Id "x", Val (Bool true))
              )

let err_2 = Let ("x", Int_type, Add (Id "x", Val (Int 4)),
                 And (Id "y", Val (Bool true))
              )

let inc_use = Let ("inc", Func_type (Int_type, Int_type), 
                   Lam ("n", Int_type, Add (Id "n", Val (Int 1))),
                   App (Id "inc", Val (Int 3))
                )

let sumToN : expr =
    LetRec ("sumToN", Func_type (Int_type, Int_type),
            Lam ("n", Int_type,
                 If (Eq (Id "n", Val (Int 0)),
                     Val (Int 0),
                     Add (Id "n",
                          App (Id "sumToN",
                               Sub (Id "n", Val (Int 1))
                              )
                         )
                    )
                ),
            Id "sumToN"
           )

let sumTo3 = App (sumToN, Val (Int 4))
