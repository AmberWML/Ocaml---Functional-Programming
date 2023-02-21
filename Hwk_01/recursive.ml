let odd x = if x mod 2 != 0 then true else false

let rec euclid x y = if x = y then x else if x<y then (euclid x (y-x)) else (euclid (x-y) y)

let frac_simplify (x,y) = (x / (euclid x y) , y / (euclid x y) )

let rec min_list lst  = match lst with 
| []-> 0
|x::[]->x
|x :: xs -> let z = min_list xs in 
			if x<z then x else z

let rec drop x lst = match lst with
| []->[]
|first :: xs -> if x = 0 then first::xs else drop (x-1) xs


