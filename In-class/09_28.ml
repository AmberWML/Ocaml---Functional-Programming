let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (base: 'b) : 'b = 
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)

let rec foldl (f: 'b -> 'a -> 'b) (accum: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> accum
  | x::xs -> foldl f (f accum x) xs

let suml xs = foldl (+) 0 xs

let length lst = foldl (fun b a -> b+1) 0 lst

let max lst = match lst with
    | [] -> failwith "Oh no - the empty list!"
    | x::xs -> foldl (fun b a -> if a > b then a else b) x xs


let rec partition f lst =
	let f elem（pass,fail) = if f elem then (elem :: pass, fail)else(pass,elem::fail)
in
	foldr f ([],[]) lst 


let group_by_3 lst = 
	let accum : 'a list * 'a list lsit = ([],[]) in let f (elems, groups) elem 
	= if length elems =2
	then ([], (elem :: elems) :: groups)
	elem (elem::elems,groups)
in 
foldl f accum lsts