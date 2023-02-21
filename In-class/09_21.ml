let rec find_all_by eq elem lst = 
	match lst with 
	|[]-> []
	|x::xs when eq x elem -> x::(find_all_by eq elem xs)
	|_::xs -> find_all_by eq elem xs
	