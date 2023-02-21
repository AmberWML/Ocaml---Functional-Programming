(* Simple translator for Quiz 4.
   Initial component by Eric Van Wyk.
 *)

(* The type for values in our imperative language. *)
type value
  = Int of int
  | Bool of bool
  | Ref of value ref
(* The type for expressions, which are simi lar to those seen before. *)
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

(* A program has a name, list of variables, and a statement. *)
type prog = string * string list * stmt


(* A translator to OCaml *)

(* All generated OCaml programs have the same first few lines that define
   a `read_number` function.  Those lines are stored in this `header`
   string.
 *)
let header : string =
  "let () =\n" ^
  "\n" ^
  "let rec read_number () : int = \n" ^
  "  try int_of_string (read_line ()) with \n" ^
  "  | Failure _ -> \n" ^
  "      print_endline \"Please, enter an integer value: \"; \n" ^
  "      read_number () \n" ^
  "in \n\n"

(* Your translator will need to write the translation of a program 
   out to a file.  This file does that by writing `contents` into 
   a file named `name`.
 *)
let write_file (name: string) (contents: string) : unit =
  let ch = open_out name
  in Printf.fprintf ch "%s\n" contents;
     close_out ch;
     print_endline ("Wrote to file \"" ^ name ^ "\".")

(* The function below translates expressions (`expr`) into
   OCaml.  You need to fill this in for the other constructors
   of type `expr`.
 *)
let rec translate_expr (e: expr) : string =
  match e with
  | Val (Int i) -> string_of_int i
  | Val (Bool b) -> string_of_bool b
  | Id  s -> "!" ^s
  | Add (x,y) -> "("^translate_expr x ^ " + " ^ translate_expr y ^")"
  | Mul (x,y) -> "("^translate_expr x ^ " * " ^ translate_expr y ^")"
  | Sub (x,y) -> "("^translate_expr x ^ " - " ^ translate_expr y ^")"
  | Div (x,y) -> "("^translate_expr x ^ " / " ^ translate_expr y ^")"
  | Mod (x,y) -> "("^translate_expr x ^ " mod " ^ translate_expr y ^")"
  | Lt (x,y) -> "("^translate_expr x ^ " < " ^ translate_expr y ^")"
  | Lte  (x,y) -> "("^translate_expr x ^ " <= " ^ translate_expr y ^")"
  | Eq  (x,y) -> "("^translate_expr x ^ " = " ^ translate_expr y ^")"
  | Not x -> "<>(" ^ translate_expr x ^")"
  | And (x,y) -> "("^translate_expr x ^ " && " ^ translate_expr y ^")"

  (* | _ -> failwith "Complete translate_expr." *)


(* The function below translates statements (`stmt`) into
   OCaml.  You need to fill this in for the other constructors
   of type `stmt`.
 *)

let rec translate_stmt (s: stmt) : string =
  match s with
  | Skip -> "()"
  | Assign (nm, e) -> nm ^ " := " ^ translate_expr e
  | Seq (st1 , st2) -> translate_stmt st1 ^ ";\n" ^ translate_stmt st2
  | ReadNum x -> x ^ " := read_number ()" 
  | WriteNum e ->  "print_int ("^ translate_expr e ^"); print_newline ()"
  | WriteStr s -> "print_endline " ^ "\"" ^ s ^ "\""
  | IfThenElse (e,st1,st2) -> "(\nif "^ translate_expr e^"\nthen\n(\n"^translate_stmt st1 ^ "\n)\nelse\n(\n" ^translate_stmt st2^")\n)"
  | While (e,st) -> "(\nlet rec loop () =\n(\nif " 
  ^ translate_expr e^ 
  "\nthen\n(\n"^translate_stmt st^";\nloop ()\n)\nelse\n()\n)\nin loop ()\n)"
  | IfThen(e,st)->"(\nif "^ translate_expr e^"\nthen\n(\n"^translate_stmt st ^ "\n)"
  | For(str,ex1,ex2,st)-> str ^ " := " ^ translate_expr ex1 ^";\n_stop := "^translate_expr ex2
                          ^";\n(\nlet rec loop () =\n(\nif (!"^str^" <= !_stop)\n" ^ 
      "then\n(\n"^translate_stmt st^";\ni := (!i + 1);\nloop ()\n)\nelse\n()\n)\nin loop ()\n)"
  | _ -> "print_endline \"Complete translate_stmt.\"" 

      

(* This function translates programs (`prog`) to OCaml and
   write them to a file using the name of the program.
   You do not need to make any changes to this function.
 *)
let translate_prog (p: prog) : unit =
  let (name, vars, s) = p
  in
  let mk_decl n = "let " ^ n ^ " = ref 0 in\n"
  in
  let translation = 
    header ^ 
    String.concat "" (List.map mk_decl (vars @ ["_stop"]) ) ^
    translate_stmt s
  in
  write_file (name ^ ".ml") translation



(* Some sample programs *)

let program_working_now =
  (* x := 2;
   *)
  ("working_now", 
   ["x"],
   Assign ("x", Val (Int 2))
  )


let program_assign =
  (* x := 1;
     y := x + 2;
   *)
  ("assign", 
   ["x"; "y"],
   Seq (Assign ("x", Val (Int 1)),
        Assign ("y", Add (Id  "x", Val (Int 2)))
     )
  )

let program_seq =
  (* write "Enter a value for x:";
     read x;
     y := x + 2;
     z := y * 3;
     write "value of z is:";
     write z
   *) 
  ("seq",
   ["x"; "y"; "z"],
   Seq (WriteStr "Enter a value for x:",
   Seq (ReadNum "x",
   Seq (Assign ("y", Add (Id  "x", Val (Int 2))),
   Seq (Assign ("z", Mul (Id  "y", Val (Int 3))),
   Seq (WriteStr "value of z is:",
        WriteNum (Id  "z")
       ) ) ) ) )
  )

let program_ifthenelse_simple =
  (* y = 10
     if y < 15 then write y else write 100
   *)
  ("ifthen_else_simple",
   ["y"],
   Seq (Assign ("y", Val (Int 10)),
        IfThenElse 
          (Lt (Id  "y", Val (Int 15)),
           WriteNum (Id  "y"),
           WriteNum (Val (Int 100))
          )
     )
  )

let program_sum =
  (* write "Enter a number to sum to:";
     read x;
     i = 0;
     sum = 0;
     while (i < x) {
       write i;
       sum = sum + i;
       i = i + 1
     }
     write sum
   *)
  ("sum",
   ["x"; "i"; "sum"],
   Seq (WriteStr "Enter a number to sum to:",
   Seq (ReadNum "x",
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (While (Lt (Id  "i", Id  "x"),
               Seq (WriteNum (Id  "i"),
	       Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
	            Assign ("i", Add (Id  "i", Val (Int 1)))
	           ) ) ),
        WriteNum (Id  "sum")
   ) ) ) ) )
  )

let program_sum_evens_odds =
  (* write "Enter a number to some sums of:";
     read x;
     i = 0;
     sum_evens = 0;
     sum_odds = 0;
     while (i < x) {
       write i;
       if i mod 2 = 0 then
          sum_evens = sum_evens + i;
       else
          sum_odds = sum_odds + i;
       i = i + 1
     }
     write "sum of evens is:";
     write sum_evens;
     write "sum of odds is:";
     write sum_odds
   *)
  ("sum_evens_odds",
   ["x"; "i"; "sum_evens"; "sum_odds"],
   Seq (WriteStr "Enter a number to some sums of:",
   Seq (ReadNum "x",
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum_evens", Val (Int 0)),
   Seq (Assign ("sum_odds", Val (Int 0)),
   Seq (While (
            Lt (Id  "i", Id  "x"),
      Seq (WriteNum (Id  "i"),
                 Seq (IfThenElse (
                          Eq (Mod (Id  "i", Val (Int 2)), Val (Int 0)),
                          Assign ("sum_evens", Add (Id  "sum_evens", Id  "i")),
                          Assign ("sum_odds", Add (Id  "sum_odds", Id  "i"))
                        ),
          Assign ("i", Add (Id  "i", Val (Int 1)))
          ) ) ),
        Seq (WriteStr "sum of evens is:",
        Seq (WriteNum (Id  "sum_evens"), 
        Seq (WriteStr "sum of odds is:",
             WriteNum (Id  "sum_odds")
       ) ) ) ) ) ) ) ) )
  )

let program_for =
  (* for i = 1 to 5
       write i
   *)
  ("for",
   ["i"],
   For ("i", Val (Int 1), Val (Int 5), WriteNum (Id  "i"))
  )


let program_sum_10 =
  (* sum = 0
     for i = 1 to 10
       sum = sum + i
     write "sum is:";
     write sum
   *)
  ("sum_10",
   ["sum"; "i"],
   Seq (Assign ("sum", Val (Int 0)),
   Seq (For ("i", Val (Int 1), Val (Int 10),
             Assign("sum", Add (Id  "sum", Id  "i"))),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       ) ) )
  )


let program_sample_1 =
  (* write 3
   *)
  ("sample_1", 
   [],
   WriteNum (Val (Int 3))
  )

let program_sample_2 =
  (* x : = 1 + 5;
     write x
   *)
  ("sample_2", 
   ["x"],
   Seq (Assign ("x", Add (Val (Int 1), Val (Int 5))),
        WriteNum (Id "x")
     )
  )

let program_sample_3 =
  (* x := 1;
     y := x + 3;
     write y
   *)
  ("sample_3", 
   ["x"; "y"],
   Seq (Assign ("x", Val (Int 1)),
   Seq (Assign ("y", Add (Id  "x", Val (Int 3))),
        WriteNum (Id "y")
     ) )
  )
  
 let all_programs = [
     program_working_now;
     program_assign; program_seq; 
     program_ifthenelse_simple;
     program_sum; program_sum_evens_odds;
     program_for; program_sum_10;
     program_sample_1; program_sample_2; 
     program_sample_3 
   ]