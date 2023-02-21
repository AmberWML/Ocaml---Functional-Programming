let all_odds lst = List.filter (fun x -> (x mod 2) != 0) lst

let decrement_all lst = List.map (fun x -> (x-1)) lst

let min_fold lst = List.fold_left (fun x y ->if x < y then x else y) (List.hd lst) lst

let sum_prod lst = List.fold_left (fun (x1,x2) y ->(x1+y,x2*y)) (0,1) lst

let partition_left f lst = 
	List.fold_left (fun (x1,x2) y -> if (f y) then ((x1@[y]),x2) else (x1,x2@[y]))
([],[]) lst


let partition_right f lst  = 
	List.fold_right (fun x (y1,y2) -> if (f x) then ((x :: y1), y2) else (y1, (x :: y2 )))
lst ([],[])


let map_as_fold f lst = List.fold_right (fun x y  -> (f x)::y  ) lst []


(* Having built both fold_right and fold_left, it's worthwhile to compare
 and contrast them. The immediately obvious difference is the order 
 in which they combine elements from the list: right to left vs. 
 left to right. When the operator being used to combine elements is 
 associative, that order doesn't doesn't change the final value of the 
 computation.  A second difference is that fold_left is tail recursive whereas 
 fold_right is not. So if you need to use fold_right on a very lengthy list, you may 
 instead want to reverse the list first then use fold_left; the operator will need to take 
 its arguments in the reverse order, too.In fold_left, the list argument is of type 'b list, 
 so the list contains values of type 'b. The return type is 'a, so the accumulator has type 'a. 
 Knowing that, we can figure out that the second argument is the initial value of the accumulator
  (because it has type 'a). And we can figure out that the first argument, the combining operator, 
  takes as its own first argument an accumulator value (because it has type 'a), as its own second 
  argument a list element (because it has type 'b), and returns a new accumulator value.Why I choose 
  List.fold_right is beacuse it is more easy without any reverse probelm need to be deal with.*)