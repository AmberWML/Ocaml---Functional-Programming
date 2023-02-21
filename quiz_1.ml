(* Amber Wong
I understand that the work to be done on Quiz 1 is to be done on my
own, without any help from my classmates or others.  Discussing the
questions or solutions is an act of academic misconduct and I
understand the penalties as laid out in the syllabus for this.
 *)

let even x = x mod 2 = 0
let count_evens lst = 
	let rec inner count lst=
		match lst with
		| []->count
		|x::xs -> if x mod 2 = 0 then inner (count+1) xs else inner count xs
	in 
	inner 0 lst


let count f lst =
	let rec helper many lst = 
	match lst with 
	|[]->many
	|x::xs -> if f x then helper (many+1) xs else helper many xs
in 
helper 0 lst

let count_evens_2 lst = count (fun x-> (x mod 2 =0)) lst

(* let is_square n = if n <0 then false else if n=0 then true else if n=1 then true else 
 *)

let square x n = if x*x = n then true else false 

let is_square n = 
	let rec helper f x b 
	match n with
	| n<0 -> false
	|n=0 -> true
	|_ -> if helper f x b then true else helper f x-1 b
in helper square n n






	

let count_square lst = count (fun x -> ) 