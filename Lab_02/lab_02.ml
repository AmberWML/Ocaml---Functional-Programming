let circle_circum_v1 r = 2.0 *. 3.1415 *. r

let circle_circum_v2 r = let pi = 3.1415 in (2.0 *. pi *. r)

let rec product xs = match xs with
| [] -> 1
| x::rest -> x * product rest

let square x = x *. x

let rec sum_sqrdiffs xs = match xs with
| [] -> 0
|x::[]->0
|x1::(x2::rest)-> (x2-x1)*(x2-x1) + sum_sqrdiffs (x2::rest)

let distance (x1,y1) (x2,y2) = let dis = square(x2-.x1) +. square(y2-.y1) in ( sqrt(dis ))

let triangle_perimeter (x1,y1) (x2,y2) (x3,y3) = distance (x1,y1) (x2,y2) +. distance (x1,y1) (x3,y3)  +. distance (x2,y2) (x3,y3)