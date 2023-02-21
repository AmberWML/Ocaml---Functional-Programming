(* Amber Wong
I understand that the work to be done on Quiz 1 is to be done on my
own, without any help from my classmates or others.  Discussing the
questions or solutions is an act of academic misconduct and I
understand the penalties as laid out in the syllabus for this. *)
 

let even x = x mod 2 = 0
(* let count_evens lst = 
	let rec inner count lst=
		match lst with
		| []->count
		|x::xs -> if x mod 2 = 0 then inner (count+1) xs else inner count xs
	in 
	inner 0 lst *)

let count_evens lst = List.fold_left (fun a b -> if even b then (a+1) else a) 0 lst
(* There are two ways I can use to write for a count_evens, I choose the List.fold_left  because it is easier
to write and more clear, The way it compute is input the first element from left in lst and test whether it is even or not,
if it is even then count = count+1; if not, count=count; and recursively until the list ends.
The other way is use helper function to write the count_evens and the helper function can be used as a method of 
counting if there is an even then the count plus one until the list ends. 
Like the format wrote below. *)

let count f lst =
	let rec helper many lst = 
	match lst with 
	|[]->many
	|x::xs -> if f x then helper (many+1) xs else helper many xs
in 
helper 0 lst

let count_evens_2 lst = count (fun x-> even x) lst


let makelist n = let rec helper x lst = match x with
| 0-> 0::lst
|_ -> x::helper (x-1) lst
in helper n []

let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list

let is_square n = if n< 0 then false else
	let lst = rev (makelist n) in 
	let rec square list1 n1= 
	match list1 with
	| []-> false
	| hd :: tl -> if n1 = hd*hd then true else (square tl n1)
in square lst n


let subtract_lst lst =
match lst with 
|[]-> 0
|x::[]-> x
|x::xs-> List.fold_left (fun a b -> a-b) x xs

(* The reason why I use List.fold_left is because subtraction is not commutative, so 7 - 3 is 
not the same a 3 - 7. Thus the fact that subtraction is left-associative matters. So use leff hold
is easier to complete the logic, because the subtraction begins from the left side, if I use right side
then the answer might be wrong. If I begin from right it will be like the minus appear on the 3 looks like
-3. the difference is that fold_left is tail recursive whereas 
 fold_right is not. So if you need to use fold_right on a very lengthy list, you may 
 instead want to reverse the list first then use fold_left; the operator will need to take 
 its arguments in the reverse order. 
But we coont use List.fold_left (fun a b -> a-b) hd lst as before, otherwise it will like 
10-10-3 = -3 instead of 7, so we need match with to discuss the occasion when lst is empty and with one element. *)

let suml lst=
	let rec leftsum a lst = 
	  match lst with
	  | [] -> a
	  | x::xs -> leftsum (a+x) xs
in leftsum 0 lst


(* If I use suml [1;2;3;4]  
the trace is:
leftsum (0+1) [2;3;4]
= leftsum ((0+1)+2) [3;4]
= leftsum (((0+1)+2)+3) [4]
= leftsum (((0+1)+2)+3)+4
=10

(((0 + 1) + 2) + 3) + 4 =10
*)



	
