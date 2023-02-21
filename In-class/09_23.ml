let rec find_all_with f lst = match lst with
| []-> []
| x::xs -> let rest = find_all_with f xs in 
			if f x then x::rest else rest 

let rec find_all_by eq elem lst = match lst with
| []-> []
| x::xs when eq x elem -> x:: (find_all_by eq elem xs)
|_:xs -> find_all_by eq elem xs

let find_all x lst = find_all_with (fun elem -> elem =x) lst

let big_nums n lst = find_all_with (fun elem -> elem > n) lst

let big_nums' n lst = find_all_with ((<=) n) lst



let big_strs n lst = find_all_by (fun s n -> String.length s > n) n lst

let find_all_by' eq elem lst = find_all_with(fun x -> eq x elem)

let compose f g = fun x -> f (g x)
