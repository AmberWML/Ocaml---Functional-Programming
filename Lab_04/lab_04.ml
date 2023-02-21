(*Amber wong
wang8039
Lab04*)

let length lst = List.fold_left (fun a _ -> a + 1) 0 lst

let andf lst = List.fold_left (fun a b -> a && b) true lst

let orf lst = List.fold_left (fun a b -> a || b) false lst

let is_elem elem lst = List.fold_left (fun a b -> a || ( b=elem )) false lst

let rev lst = List.fold_left (fun a b -> b :: a) [] lst

let ascii_sum lst = List.fold_left (+) 0 (List.map Char.code lst)

let lebowski lst = List.fold_right (fun a b -> if a = '.' then [','; ' '; 'd'; 'u'; 'd'; 'e'; '.'] @ b else a :: b) lst [] 

