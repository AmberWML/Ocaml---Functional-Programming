let rec map f lst = match lst with 
| [] -> []
| x:: xs -> f x :: map f xs

let rec filter f lst = match lst with
| []-> []
|x::xs -> if f x then x::filter f xs
else filter f xs

let removeABC cs = 
	let f c = c <> 'A' && 'B'&&'C'&&'D' && 'E'&&'F'
				&&'G' && 'H'&&'I'&&'G' && 'K'&&'L'
				&&'M' && 'N'&&'O'&&'P' && 'Q'&&'R'
				&&'S' && 'T'&&'U'&&'V' && 'W'&&'X'
				&&'Y' && 'Z'
			in filter f cs
(* 
The type of fold is int and double separately 
fold : ('a -> 'b ->'b) ->'b ->'a list -> 'b

#ocaml implementation of map

we will have same type as before the implementation
 * )