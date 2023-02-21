let rec drop_value to_drop l = 
	match l with
	| [] -> []
	| to_drop:: tl -> drop_value to_drop tl
	| hd::tl -> hd :: drop_value to_drop tl
let first_if_3 (x,y,z) =x
type fraction = int * int

let add_frac (f1:fraction) (f2:fraction) :fraction =
	match f1,f2 with
	| (n1,d1),(n2,d2) -> (n1*d2 +n2*d1,d1*d2)

let rec fib x = match x with
| 0 ->0
|1-> 1
|_ -> fib (x-1)+fib (x-2)




	

