(* (* Amber Wong  *)
wang8039@umn.edu
no other partners this time *) 

(* lab2 discussion *)
let circle_circum_v1 r = 2.0 *. 3.1415 *. r
(* there is one line there and based on the mathmatics background,
I can understand with a glance, this means 2*pi*r = circle circumatance *)
(* Two times pi times the radius. Because this is more clean way to express*)
(* let circle_circum_v1 r = 6.2831852 *. r *)
let circle_circum_v2 r = let pi = 3.1415 in (2.0 *. pi *. r)
(* In this v2 we use a let ... in structure to make the 
expression more advance, anytime we need pi in the future, 
we can use a simple way pi to institute using 3.1415..
the version has one line as well but frame is differnet,
also can be written as many lines but it could be in one line
the tab exists after each variable or other components
let in is like another function nested in the whole expression *)
let rec product xs = match xs with
| [] -> 1
| x::rest -> x * product rest
(* when the list is empty then the product should be 1 because 
 the product is recursion, if []-> 0 then no matter how many elements
 the final product will turn to 0 *)
let square x = x *. x
(* This is simply a helper funtion for the rest functions *)
(* let rec sum_sqrdiffs xs = match xs with
| [] -> 0
|x::[]->0
|x1::(x2::rest)-> (x2-x1)*(x2-x1) + sum_sqrdiffs (x2::rest) *)
let rec sum_sqrdiffs xs = match xs with
|x1::(x2::rest)-> (x2-x1)*(x2-x1) + sum_sqrdiffs (x2::rest)
| _ -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
(* I did not use the @
and for this one I will try the rasing exceptions 
if using exception, we will get the exception at the end no matter how many elements in there*)
let distance (x1,y1) (x2,y2) = let dis = square(x2-.x1) +. square(y2-.y1) in ( sqrt(dis ))

let triangle_perimeter (x1,y1) (x2,y2) (x3,y3) = distance (x1,y1) (x2,y2) +. distance (x1,y1) (x3,y3)  +. distance (x2,y2) (x3,y3)
(* Using *. operator. *)
(* sqrt ((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)) *)
(* I prefer using this one because this one looks easier to understand but the ** is also a new way to express