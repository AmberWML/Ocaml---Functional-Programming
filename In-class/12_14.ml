type color = R | B
type 'a tree = E
             | T of color * 'a tree * 'a * 'a tree

let rec member (x: 'a) (t: 'a tree) : bool =
  match t with
  | E -> failwith "member on empty red black tree"
  | T (_, t1, y, t2) ->
     if x < y
     then member x t1
     else if x > y
     then member x t2
     else true

let rec insert (x: 'a) (t: 'a tree) : 'a tree =
  let rec ins (t': 'a tree) : 'a tree =
    match t' with
    | E -> T (R, E, x, E)
    | T (c, t1, y, t2) ->
       if x < y
       then balance c (ins t1) y t2
       else if x > y
       then balance c t1 y (ins t2)
       else t'
  in
  match ins t with
  | E -> failwith "cannot happen, ins always returns a T tree"
  | T (_, t1, y, t2) -> T(B, t1, y, t2)
and balance (clr: color) (t1: 'a tree) (v: 'a) (t2: 'a tree) : 'a tree =
  match clr, t1, v, t2 with
  | B, T (R, T (R, a, x, b), y, c), z, d  (* left   tree in 3.5 *)
  | B, T (R, a, x, T (R, b, y, c)), z, d  (* top    tree in 3.5 *)
  | B, a, x, T (R, T (R, b, y, c), z, d)  (* bottom tree in 3.5 *)
  | B, a, x, T (R, b, y, T (R, c, z, d))  (* right  tree in 3.5 *)
   -> T(R, T (B, a, x, b), y, T(B, c, z, d))
  | _, _, _, _ -> T (clr, t1, v, t2)

let fromList (lst: 'a list) : 'a tree =
  let insrt t x = insert x t
  in
  List.fold_left insrt E lst

let rec from a b = 
  assert (a <= b);
  if a = b 
  then [a]
  else a :: from (a+1) b

let rec longest (t: 'a tree) : int =
  match t with
  | E -> 0
  | T (_, t1, _, t2) -> 1 + max (longest t1) (longest t2)