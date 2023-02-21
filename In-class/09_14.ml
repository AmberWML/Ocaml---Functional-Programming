let rec power n y =
	if n = 0 then 1.0 else y *. (power (n-1) y)


(* let cube x = x *. x *. x *)

(* let cube x = power 3 x *)

let cube_v2 = power 3


let rec all xs = match xs with 
| [] -> true
| x::rest -> x && all rest

let rec even1 xs = match xs with 
| [] -> true
| x::rest -> (x mod 2 = 0) && even1 rest

let rec length xs = match xs with 
| [] -> 0
| x::rest -> 1 + length rest

let rec even2ways (xs : int list) :bool = match xs with
| [] -> true
| x::rest -> (even1 xs) && (length xs mod 2 =0)

