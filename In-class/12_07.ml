(* Simple interpreter *)

type value
  = Int of int
  | Bool of bool

type expr = 
  | Val of value
  | Id  of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Lt  of expr * expr
  | Lte of expr * expr
  | Eq  of expr * expr
  | Not of expr
  | And of expr * expr



type environment = (string * int) list

let rec lookup (name: string) (env: environment) : int =
  match env with 
  | [ ] -> raise (Failure ("Name \"" ^ name ^ "\" not found."))
  | (k,v)::rest -> if name = k then v else lookup name rest

let rec eval (e: expr) (env: environment) : value = 
  match e with
  | Val v -> v
  | Id  n -> Int (lookup n env)
  | Add (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "incompatible types, Sub") 
    )
  | Mul (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 / v2)
       | _ -> raise (Failure "incompatible types, Div")
     )
  | Mod (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 mod v2)
       | _ -> raise (Failure "incompatible types, Div")
     )
  | Lt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _ -> raise (Failure "incompatible types, Lt")
     )
  | Lte (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 <= v2)
       | _ -> raise (Failure "incompatible types, Lte")
     )
  | Eq (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool v1, Bool v2 -> Bool (v1 = v2)
       | _ -> raise (Failure "incompatible types, Eq")
     )
  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool v1, Bool v2 -> Bool (v1 && v2)
       | _ -> raise (Failure "incompatible types, And")
     )
  | Not e' ->
     ( match eval e' env with
       | Bool v -> Bool (not v)
       | _ -> raise (Failure "incompatible types, Not")
     )

(* A statement has the expected forms from imperative programming. *)
type stmt =
  | Skip 
  | Assign of string * expr
  | Seq of stmt * stmt
  | ReadNum of string
  | WriteNum of expr
  | WriteStr of string
  | IfThenElse of expr * stmt * stmt
  | While of expr * stmt

  | IfThen of expr * stmt
  | For of string * expr * expr * stmt


type prog = string * string list * stmt

let rec read_number () : int =
  try int_of_string (read_line ()) with
  | Failure _ ->
     print_endline "Please, enter an integer value: ";
     read_number ()

type state = environment

let rec exec (s: stmt) (stt: state) : state =
  match s with
  | Skip -> stt
  | Assign (nm, e) ->
     (match eval e stt with
      | Int i ->  (nm, i) :: stt
      | Bool _ -> failwith "Bools not allowed in assignments"
     )
  | Seq (s1, s2) -> exec s2 (exec s1 stt)
  | ReadNum nm -> (nm, read_number ()) :: stt
  | WriteNum e ->
     (match eval e stt with
      | Int i -> print_int i; print_newline(); stt
      | Bool _ -> failwith "Incompatible types in WriteNum"
     )
  | WriteStr s -> print_endline s; stt
  | IfThenElse (cond, ts, es) ->
     (match eval cond stt with
      | Bool true  -> exec ts stt
      | Bool false -> exec es stt
      | Int _ -> failwith "Incompatible types in IfThenElse"
     )
  | While (cond, body) ->
     (match eval cond stt with
      | Bool true  -> exec (While (cond, body)) (exec body stt)
      | Bool false -> stt
      | Int _ -> failwith "Incompatible types in While"
     )     
  | IfThen (cond, ts) ->
     let translation_to_core = IfThenElse(cond, ts, Skip)
     in exec translation_to_core stt
  | For (i, start, stop, body) ->
     let translation_to_core =
       Seq (Assign (i, start),
       Seq (Assign ("_stop", stop),
            While ( Lte (Id i, Id "_stop"),
                    Seq (body,
                         Assign (i, Add (Id i, Val (Int 1))) )
                      ) ) )
       in exec translation_to_core stt

