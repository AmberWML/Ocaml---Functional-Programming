let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
          (t: 'a btree) : 'b =
    match t with
    | Empty -> b
    | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)

let sumr (t: int btree) =
  let add3 x y z = x + y + z
  in reduce 0 add3 t

let mapTreeR (f: 'a -> 'b) (t: 'a btree) : 'b btree =
  let g (v: 'a) (t1: 'b btree) (t2: 'b btree) : 'b btree =
    Node (f v, t1, t2)
  in
  reduce Empty g t

let rec is_elem (e: 'a) (t: 'a btree) : bool =
  match t with
  | Empty -> false
  | Node (v, t1, t2) -> e = v || is_elem e t1 || is_elem e t2
        
let is_elemR (e: 'a) (t: 'a btree) : bool =
  let p (v :'a) (b1: bool) (b2: bool) =
    e = v || b1 || b2
  in
  reduce false p t