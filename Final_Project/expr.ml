(* Final project
  Amber Wong
 *)
type value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * value_environment
  | Ref of value ref
  | PairV of value * value
  | ListV of value list

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

  | Pair of expr * expr
  | Fst of expr
  | Snd of expr

  | Nil 
  | Cons of expr * expr
  | Head of expr
  | Tail of expr

and typ = Int_type 
        | Bool_type
        | Func_type of typ * typ
        | Pair_type of typ * typ
        | List_type of typ
        | Unknown_type 
type type_environment = (string * typ option) list 


(* Part 1. Complete unparse *)
let rec changetype (t:typ): string =
  match t with
  | Int_type ->"int"
  | Bool_type ->"bool"
  | Func_type(t1,t2) -> "(" ^ changetype t1 ^ " -> " ^ changetype t2 ^ ")"
  | Pair_type(t1,t2) ->  "("^ changetype t1 ^" * "^  changetype t2^ ")"
  | List_type t ->"(" ^changetype t ^" list)"
  | Unknown_type -> "Unknown_type" 


let helper (f: 'a -> string) (lst: 'a list)  : string= String.concat ";" (List.map f lst)
let rec unparse_helper (e: value) : string =
  match e with
  |  (Int i) -> string_of_int i
  |  (Bool b) -> string_of_bool b
  | (Closure (s,exp,envir)) -> "<fun>"
  | (Ref s) -> "reference"
  | (PairV (v1,v2))  -> "("^unparse_helper v1^","^ unparse_helper v2^")"
  | (ListV lst) -> "["^helper unparse_helper lst^"]"

let rec unparse (e: expr) : string =
  match e with
  | Val x -> unparse_helper x 
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
  | Pair (e1,e2) -> "("^unparse e1^","^ unparse e2^")"
  | Fst e -> "(" ^"fst "^unparse e^")"
  | Snd e -> "(" ^"snd "^unparse e^")"
  | Nil -> "[]"
  | Cons (e1,e2)->  "(" ^unparse e1 ^" :: "^ unparse e2^ ")"
  | Head e ->  "(" ^ "List.hd "^ unparse e^ ")"
  | Tail e ->  "(" ^ "List.tl "^ unparse e^ ")"


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
  | Pair (e1,e2) -> freevars e1 @ freevars e2
  | Fst e ->freevars e
  | Snd e -> freevars e
  | Nil ->[]
  | Cons (e1,e2)->  freevars e1 @ freevars e2
  | Head e -> freevars e
  | Tail e -> freevars e

 
(* Part 3. Type checking *)           
type result = OK of typ
            | Errs of (expr * string) list

let rec lookup (n:string) (env:type_environment) : result =
  match env with
  | [] -> Errs [ (Id n, "Identifier not found") ]
  | (n',Some t) :: rest when n = n' -> OK t
  | _ :: rest -> lookup n rest

let rec lookup_val n env  =
  match env with
  | [] -> None
  | (n', t) :: rest when n = n' -> Some t
  | _ :: rest -> lookup_val n rest

let expect_Int_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> [] 
  | OK Bool_type -> [ (e, "expect Int type") ] 
  | OK Pair_type (x,y) -> [ (e, "expect Int type") ] 
  | OK Func_type (x,y) -> [ (e, "expect Int type") ] 
  | OK List_type x-> [ (e, "expect Int type") ] 
  | OK Unknown_type -> [ (e, "expect Int type") ] 
  | Errs errs ->  errs 

let expect_Bool_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Bool_type -> [] 
  | OK Int_type -> [ (e, "expect Bool type") ] 
  | OK Pair_type (x,y) -> [ (e, "expect Bool type") ] 
  | OK Func_type (x,y) -> [ (e, "expect Int type") ] 
  | OK List_type x -> [ (e, "expect Bool type") ] 
  | OK Unknown_type -> [ (e, "expect Int type") ] 
  | Errs errs ->  errs 


(* let expect_Pair_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Pair_type -> [] 
  | OK Int_type -> [ (e, "expect Pair type") ] 
  | OK Bool_type -> [ (e, "expect Pair type") ] 
  | OK List_type -> [ (e, "expect Pair type") ] 
  | Errs errs -> errs  *)

(* 
let expect_List_type (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK List_type -> [] 
  | OK Int_type -> [ (e, "expect List type") ] 
  | OK Bool_type -> [ (e, "expect List type") ] 
  | OK Pair_type -> [ (e, "expect List type") ] 
  | Errs errs -> errs  *)

let rec check_type_compat t1 t2 = match  t1,t2 with
| Int_type,Int_type | Bool_type, Bool_type -> true
| Func_type(t3,t4),Func_type(t3',t4') | Pair_type (t3,t4),Pair_type(t3',t4') 
    ->  (check_type_compat t3 t3') && (check_type_compat t4 t4')
| List_type Unknown_type,List_type _ -> true
| List_type _,List_type Unknown_type -> true
| List_type t3, List_type t4 -> check_type_compat t3 t4 
| _,_ -> false

let rec type_check (e:expr) (env:type_environment) : result =
  match e with
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Val (PairV _) -> failwith "should not check Pair type"
  | Val (ListV _) -> failwith "should not check List type"
  | Val (Closure (_, _, _))->failwith "should not check List type"
  | Val (Ref _) ->failwith "should not check List type"
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2)->
     (match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Int_type
      | r1, r2 -> Errs (expect_Int_type r1 e1 @ expect_Int_type r2 e2)
     )
  | Eq (e1, e2) | Lt (e1, e2) ->
     (match type_check e1 env, type_check e2 env with
      | OK t1, OK t2 -> if (check_type_compat t1 t2) then OK Bool_type else Errs [(e,"type incompatible.")]
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
  | Id n ->(match ( lookup_val n env) with |Some (Some t) -> OK t |Some (None) ->Errs [] | None -> Errs [(Id n),"error"])
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
    (  let helper expr = match type_check expr env with
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
   )

(* New part *)
  | Pair (e1,e2) -> 
     (match type_check e1 env, type_check e2 env with
      | OK t1_something, OK t2_something -> OK (Pair_type (t1_something,t2_something))
      | r1, r2 -> Errs (expect_Int_type r1 e1 @ expect_Int_type r2 e2)
     )
          
  | Fst e1 ->
  (match type_check e1 env with
      | OK (Pair_type (t1,t2)) -> OK t1
      | r-> Errs [(e,"error")]
     )
   (*   | r1-> Errs (expect_Pair_type r1 e) *)
  | Snd e1 ->
     (match type_check e1 env with
      | OK (Pair_type (t1,t2))-> OK t2
      | r-> Errs [(e,"error")]
     )
      (* | r1-> Errs (expect_Pair_type r1 e) *)
       (* | OK x ->  Errs [(x,"expecting pair type")] *)
  | Nil -> OK (List_type Unknown_type )
  | Cons (e1,e2)->  
    (match type_check e1 env, type_check e2 env with
      | OK t1_something, OK (List_type t2_something)-> if (t1_something = t2_something) || (t2_something =Unknown_type)
      then OK (List_type t1_something) else Errs [(e2,"expecting same type")]
      | OK t1, OK t2-> Errs [(e,"expecting same type")]
      | Errs errs, _ ->Errs errs
      | _,Errs errs -> Errs errs
     )
  | Head e1  ->
     (match type_check e1 env with
      | OK (List_type t) -> OK t
      | OK t1 -> Errs [(e,"expecting List type")]
      | Errs errs -> Errs errs
   (*    | r1-> Errs (expect_List_type r1 e) *)
     )

  | Tail e1 -> 
     (match type_check e1 env with
      | OK (List_type t)-> OK (List_type t)
      | OK t1 -> Errs [(e,"expecting List type")]
      | Errs errs -> Errs errs
     )

       
let check e = type_check e [] 



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
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _, _ -> raise (Failure "incompatible types, Lt")
     )
  | Eq (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | PairV (v1,v2) ,PairV (v1',v2') -> Bool ( (v1 = v1') && (v2 = v2'))
       | ListV v1, ListV v2 -> Bool( v1 = v2)
       | _, _ -> raise (Failure "incompatible types, Eq")
     )
 
  | And (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "incompatible types, And")
     )
  
  | Not e1 ->
     ( match eval env e1 with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "incompatible types, Not" )
     )
  | Let (n, t, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body
  | Id n ->( match (lookup_val n env ) with | Some n-> n |None-> raise (Failure "incompatible types, Not" ))

  | If (e1,e2,e3)->
      (match eval env e1 with
        | Bool true -> eval env e2
        | Bool false -> eval env e3
        | _ -> raise (Failure ("incompatible types, If"))
      )
     
  | App (e1, e2) ->
      let r1 =(eval env e1) in
      (match r1 with
        | Ref r ->
          (match !r with
          | Closure (v1, body, env') -> eval ((v1, eval env e2):: env') body
          | _ -> raise (Failure ("incompatible types, App")))
       | Closure (v1, body, env') -> eval ((v1, eval env e2):: env') body
       | _ -> raise (Failure ("incompatible types, App"))
      )
    (* 
      make r1 first and then compare the eval r2
      ) *)
  
  | Lam (n,t, dexpr) ->
        let temp = (freevars e) in 
        let find fv = match (lookup_val fv env) with |Some v-> (fv,v)|None ->  raise (Failure ("incompatible types, App")) in
        let var=  List.map find temp
        in
        Closure (n, dexpr, var)
    
  | LetRec (n,t,dexpr,body)->
        (match dexpr with
        | Lam (n', t,dexpr) ->
              let recRef = ref (Int 666) in
              let j = Closure (n', dexpr, (n, Ref recRef) :: env) in
              let () = recRef :=  j in
              (eval ((n, j)::env) body)
        | _ -> raise(Failure ("Need function!"))
       )
(*   new part *)
  | Pair (e1,e2) -> 
      ( match eval env e1, eval env e2 with
       |  v1,  v2 -> PairV (v1 ,v2)
     (*   | _, _ -> raise (Failure "incompatible types, Pair") *)
      )
  | Fst e1 -> 
      ( match eval env e1 with
       | PairV (a,b) ->  a
       | _ -> raise (Failure "incompatible types, Fst" )
      )
  | Snd e1 ->
      ( match eval env e1 with
       | PairV (a,b) ->  b
       | _ -> raise (Failure "incompatible types, Snd" )
      )
  | Nil -> ListV []
  | Cons (e1,e2)-> 
      ( match eval env e1, eval env e2 with
       |  b1,  ListV l -> ListV (b1::l)
       | _, _ -> raise (Failure "incompatible types, Cons")
      )

  | Head e1 ->  
      ( match eval env e1 with
       | ListV (v:: _)->  v
       | _ -> raise (Failure "incompatible types, Head" )
      )
  | Tail e1 -> 
      ( match eval env e1 with
       | ListV (_::t)-> ListV t
       | _ -> raise (Failure "incompatible types, Tail" )
      )

 (*  | _ -> failwith "complete this function" *)

let evaluate e = eval [] e
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