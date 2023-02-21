(* Amber wong
wang8039
Hw2 *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)

let rec take n l = match l with
				  | [] -> [] 
				  | x::xs -> if n > 0 then x::take (n-1) xs else []
(* val take : int -> 'a list -> 'a list = <fun> *)
let rec drop n l = match l with
				  | [] -> [] 
				  | x::xs -> if n > 0 then drop (n-1) xs else l

(* val drop : 'a -> 'a list -> 'a list = <fun> *)

let length lst = (List.fold_left (fun a _ -> a+1) 0 lst)
(* val length : 'a list -> int = <fun> *)
let rev lst = List.fold_left (fun a b -> b::a) [] lst
(* let rev lst = raise (Failure "This function is not yet implemented!") *)
(* val rev : 'a list -> 'a list = <fun> *)
let rec is_elem_by (f:'a -> 'b -> bool) (elem:'b ) (lst: 'a list):bool = 
					match lst with
				    | [] -> false
				    | x::xs when f x elem ->true
				    | _::xs -> is_elem_by f elem xs
(* val is_elem_by : ('a -> 'b -> bool) -> 'b -> 'a list -> bool = <fun> *)

let is_elem (v:'a) (lst:'a list) :bool = is_elem_by (=) v lst 

(* val is_elem : 'a -> 'a list -> bool = <fun> *)

let dedup lst  = List.fold_right (fun a b -> if is_elem a b then b else a::b) lst []
(* val dedup : 'a list -> 'a list = <fun> *)

let split_by (f:'a -> 'b -> bool) (lst:'b list ) (separator:'a list) : 'b list list= 
	let rec helper elem (l1, l2) = if is_elem_by f elem separator then (l2::l1,[]) else (l1,(elem::l2)) in 
	let (l1,l2) = List.fold_right (helper) lst ([],[])
	in l2::l1
(* val split_by : ('a -> 'b -> bool) -> 'b list -> 'a list -> 'b list list =
  <fun>
  This function works like we use a helper function to see whether the element is in seperator, if it is 
  not a seperator then we let the element :: l2, until we find the first separator element, then we let all the 
  temp thing l2 ::l1 which is empty now, then we make temp empty again, now we can have new element comes in 
  before the next separator. untill the whole lists computed. *)

(* f x y outputs x^16 + y *)
(* let f x y =
	let square x =
		x * x	
	in
	(square (square (square x))) + y


let f x = 
	if x mod 2 = 0 then true
	else false
in
let halver elem (l1,l2) =
	if f elem then (l1, elem::l2)
	else (l2::l1, [])
in
let (l1, l2) = List.fold_right (halver) [1;2;3;4;5;6] ([],[])
in 
l2 :: l1


def f(x):
	.........

def halver(elem, (l1,l2)):
	......

(l1,l2) = List.fold_right .....
return (l2 :: l1)

([[6]],[4])
([[4];[6]],[]) *)

(*
lst = [1,2,3,4,5,6,7,8]
seps = [3]

'a list list

l1 = []
l2 = [1,2]

l1 = [1,2]::[] = [[1,2]] (you would get [1,2] )
l2 = []

l1 = [[1,2]]
l2 = [4]

l2 = [4,5]
l2 = [4,5,7,8]

[[1;2]; [4;5;7;8]]



*)

type word = char list
type line = word list
(* let convert_to_non_blank_lines_of_words =  *)



(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

(* type 'a option = None | Some of 'a *)

type result = OK 
	    | FileNotFound of string
	    | IncorrectNumLines of int 
	    | IncorrectLines of (int * int) list
	    | IncorrectLastStanza


let remove_puc l = match l with 
					|','|'.'|'!'|'?'|';'|':'|'-' -> ' '
					|c -> c

let remove elem = (List.filter (fun x-> not (x = ' ' || x='\t')) elem)<>[]

let breakchartoline (lst:char list) : word list = 
	let temp = List.map remove_puc lst
	in
	let temp2 = List.map Char.lowercase_ascii temp
	in
	List.filter remove (split_by (=) temp2 ['\n'])

let breaklinetoword lst =List.filter (fun x -> x <> []) (split_by (=) lst [' ';',';'.';'!';'?';';';':';'-'])


let convert_to_non_blank_lines_of_words (lst:char list) :word list list = 
	let lines = breakchartoline lst in
		let rec helper i =
			match i with
			| []-> []
			| x::xs -> (List.filter (fun x -> x <> []) (breaklinetoword x )):: helper xs
		in helper lines

let words (x:word):word list = List.filter remove (split_by (=) x [' '; '\t'])


(* let correct_num_lines lst = length lst =24 *)


let ident_line l1 l2 line_num = if l1 = l2 then [] else [line_num](*if(breaklinetoword( List.map Char.lowercase_ascii l1) = 
								breaklinetoword( List.map Char.lowercase_ascii l2 )) then [] else [line_num]*)


(*  ['W'; 'h'; 'e'; 'n'; ' '; 'E'; 'm'; 'm'; 'a'; ' '; 's'; 'c'; 'r'; 'u'; 'n';
  'c'; 'h'; 'e'; 's'; ' '; 'u'; 'p'; ' '; 'h'; 'e'; 'r'; ' '; 'n'; 'o'; 's';
  'e'; ' '; 'a'; 'n'; 'd'; ' '; 'k'; 'n'; 'i'; 't'; 's'; ' '; 'h'; 'e'; 'r';
  ' '; 't'; 'i'; 'n'; 'y'; ' '; 'b'; 'r'; 'o'; 'w'; ','; '\n'; 'W'; 'h'; 'e';
  'n'; ' '; 'E'; 'm'; 'm'; 'a'; ' '; 's'; 'c'; 'r'; 'u'; 'n'; 'c'; 'h'; 'e';
  's'; ' '; 'u'; 'p'; ' '; 'h'; 'e'; 'r'; ' '; 'n'; 'o'; 's'; 'e'; ' '; 'a';
  'n'; 'd'; ' '; 'k'; 'n'; 'i'; 't'; 's'; ' '; 'h'; 'e'; 'r'; ' '; 't'; 'i';
  'n'; 'y'; ' '; 'b'; 'r'; 'o'; 'w'; ','; '\n'; 'M'; 'y'; ' '; 'g'; 'r'; 'a';
  'n'; 'd'; 'd'; 'a'; 'u'; 'g'; 'h'; 't'; 'e'; 'r'; ' '; 's'; 'p'; 'i'; 'n';
  's'; ' '; 'a'; ' '; 'h'; 'a'; 'p'; 'p'; 'y'; ' '; 'w'; 'e'; 'b'; ' '; 'o';
  'f'; ' '; 'h'; 'y'; 'p'; 'h'; 'e'; 'n'; 's'; ' '; 't'; 'h'; 'a'; 't'; ' ';
  'c'; 'o'; 'n'; 'n'; 'e'; 'c'; 't'; '-'; 'h'; 'e'; 'r'; '-'; 'e'; 'y'; 'e';
  's'; '.'; '\n'; 'M'; 'y'; ' '; 'g'; 'r'; 'a'; 'n'; 'd'; 'd'                        ; 'a'; 'u'; 'g';
  'h'; 't'; 'e'; 'r'; ' '; 's'; 'p'; 'i'; 'n'; 's'; ' '; 'a'; ' '; 'h'; 'a';
  'p'; 'p'; 'y'; ' '; 'w'; 'e'; 'b'; ' '; 'o'; 'f'; ' '; 'h'; 'y'; 'p'; 'h';
  'e'; 'n'; 's'; ' '; 't'; 'h'; 'a'; 't'; ' '; 'c'; 'o'; 'n'; 'n'; 'e'; 'c';
  't'; '-'; 'h'; 'e'; 'r'; '-'; 'e'; 'y'; 'e'; 's'; '.'; '\n'; 'C'; 'o'; 'n';
  'n'; 'e'; 'c'; 't'; ' '; 'h'; 'e'; 'r'; ' '; 'u'; 'p'; ','; ' '; 'h'; 'e';
  'r'; ' '; 'b'; 'r'; 'o'; 'w'; ','; ' '; 'h'; 'e'; 'r'; ' '; 'n'; 'o'; 's';
  'e'; ','; ' '; 'a'; ' '; 'w'; 'e'; 'b'; ' '; 'o'; 'f'; ' '; 'E']
                ;; *)




(*let contain_before l1 l2 = let firstfourline  = dedup( List.sort compare (breaklinetoword ( List.map Char.lowercase_ascii l1))) in 
							let lasttwoline = dedup(List.sort compare (breaklinetoword ( List.map Char.lowercase_ascii l2))) in
							 firstfourline = lasttwoline*)

let line index1 index2 = (index1 - 1) * 6 + index2

let comp x y = match x=y, x>y with |true,_ -> 0
									|_,true -> 1
									| _, _ -> -1

let line_5_6 santa index= match santa with 
						|l1::l2::l3::l4::l5::l6::[] ->  if(List.sort comp (l1@l3) =  List.sort comp (l5@l6)) then []
														else [(line index 5, line index 6)]
						|_ -> raise (Failure (""))

let each_santa (santa: word list list) index : (int * int) list = match santa with
							|l1::l2::l3::l4::l5::l6::[]-> (match ident_line l1 l2 (line index 1, line index 2), 
																ident_line l3 l4 (line index 3, line index 4),
																line_5_6 santa index
															with
															|[],[],[] -> []
															|l1, l2, l3 -> (l1 @ l2 @ (if l1 @ l2 = [] then l3 else []))
															) 
							|_ -> raise (Failure (""))
							(*if ident_line l1 l2 then 
								if ident_line l3 l4 then 
									if contain_before (l1@l2@l3@l4) (l5@l6) then 
										last_santa else raise (Failure IncorrectLInes…)
												else ??
											else ??  IncorrectLines (1,2)*)


let last_santa s1 s2 s3 s4 = if (List.sort compare (dedup (List.concat (s1@s2@s3)))) = (List.sort compare (dedup (List.concat (s4))))
							 then OK else IncorrectLastStanza
(* let contain_before2 l1 l2 = let newlines = List.sort compare  (dedup l1) in 
								let lasttwolines = List.sort compare l2 in 
								List.fold_left (fun a b -> a&&(is_elem_by b newlines))  true lasttwoline *)



let overallcheck sa1 sa2 sa3 sa4 =
					  match (each_santa sa1 1), (each_santa sa2 2), (each_santa sa3 3) with
					  |[], [], [] -> last_santa sa1 sa2 sa3 sa4
					  |l1, l2, l3 -> IncorrectLines (l1@l2@l3)
						(*if((first_three_santa sa1)&&(first_three_santa sa2)&&(first_three_santa sa3))
						then (last_santa sa4)
							else IncorrectLines of (int * int) list*)
					



let paradelle filename = match read_file filename  with
						| None -> FileNotFound filename
						|Some lst-> let poemlinelist = breakchartoline lst in
										let linelen = length poemlinelist in 
										if linelen <> 24 then (IncorrectNumLines linelen) else 
										let temp = (List.map words poemlinelist) 
									in overallcheck (take 6 temp) 
													(take 6 (drop 6 temp))
													(take 6 (drop 12 temp))
													(take 6 (drop 18 temp))




(* ALL funtion type:
val remove_puc : char -> char = <fun>
val remove : char list -> bool = <fun>
val breakchartoline : char list -> word list = <fun>
val breaklinetoword : char list -> char list list = <fun>
val convert_to_non_blank_lines_of_words : char list -> word list list = <fun>
val words : word -> word list = <fun>
val ident_line : 'a -> 'a -> 'b -> 'b list = <fun>
val line : int -> int -> int = <fun>
val comp : 'a -> 'a -> int = <fun>
val line_5_6 : 'a list list -> int -> (int * int) list = <fun>
val each_santa : word list list -> int -> (int * int) list = <fun>
val last_santa :
  'a list list -> 'a list list -> 'a list list -> 'a list list -> result =
  <fun>
val overallcheck :
  word list list ->
  word list list -> word list list -> word list list -> result = <fun>
val paradelle : string -> result = <fun>
 *)





