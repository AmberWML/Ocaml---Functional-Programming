(* In class and test in Ocaml 4.08.1 ----Amber *)

let gcd (x: int) (y: int) : int =
  let rec helper i =
    if x mod i = 0  && y mod i = 0
    then i
    else helper (i - 1)
  in helper (min x y)

(* this was an in-class exercise, but no need to turn it in. *)
let is_square (n: int) : bool =
  let rec check i = if i * i = n
                    then true
                    else if  i > (n / 2)
                    then false
                    else check (i + 1)
  in
  n >= 0 && check 0

let max lst = match lst with
    | [] -> failwith "Oh no - empty list"
    | x::xs -> List.fold_left (fun m x' -> if m > x' then m else x') x xs

let max' (lst: int list) : int option  = match lst with
    | [] -> None
    | x::xs -> Some
                 (List.fold_left (fun m x' -> if m > x' then m else x')
                    x xs)

let max'' lst = match max' lst with
    | None -> failwith "Oh no - empty list"
    | Some v -> v


(* Skimming the cream *)
let group_by_3 lst =
  let accum : 'a list * 'a list list = ([], [])
  in
  let f (elems, groups) elem = 
    if List.length elems = 2
    then ([], ( List.rev (elem::elems) :: groups))
    else ( elem::elems, groups )
  in
  match List.fold_left f accum lst with
  | (left_overs, lists_of_3) ->
     List.rev (left_overs :: lists_of_3)


let rec foldl (f: 'b -> 'a -> 'b) (accum: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> accum
  | x::xs -> foldl f (f accum x) xs

let rec rev lst =
  match lst with
  | [] -> []
  | x::xs -> rev xs @ [x]  

(* rev [1;2;3;4]
   rev [2;3;4] @ [1]
   (rev [3;4] @ [2]) @ [1]
   ((rev [4] @ [3]) @ [2]) @ [1])
   (((rev [] @ [4]) @ [3]) @ [2]) @ [1])
   ((([] @ [4]) @ [3]) @ [2]) @ [1])
   ((([4]) @ [3]) @ [2]) @ [1])
   ((([4;3]) @ [2]) @ [1])
   ((([4;3;2]) @ [1])
   ((([4;3;2;1])
   
write append : 'a list -> 'a list -> 'a list
write rev' - and use an accumulator
List.fold_left to write rev' 
*)