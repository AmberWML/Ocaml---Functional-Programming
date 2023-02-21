let rec reduce_list  (n: 'b) (c: 'a -> 'b -> 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> n
  | x::xs -> c x (reduce_list n c xs)

let () = assert (reduce_list 0 (+) [1;2;3;4] = 10)

(* The lists we built - in higher.ml *)
type 'a myList
  = Nil
  | Cons of 'a * 'a myList

let rec reduce_myList (n: 'b) (c: 'a -> 'b -> 'b) (lst: 'a myList) : 'b =
  match lst with
  | Nil -> n
  | Cons (x, xs) -> c x (reduce_myList n c xs)

let () = assert (reduce_myList 0 (+)
                   (Cons (1, Cons (2, Cons (3, Cons (4, Nil))))) = 10)

(* Natural numbers *)
type nat
  = Zero
  | Succ of nat

let rec reduce_nat  (z: 'b) (s: 'b -> 'b)   (n: nat) : 'b =
  match n with
  | Zero -> z
  | Succ n' -> s (reduce_nat z s n')

let to_int (n: nat) : int = reduce_nat 0 (fun i -> i + 1) n

let () = assert (to_int (Succ (Succ (Succ Zero))) = 3)

(* Binary trees, from higher.ml *)
type 'a btree
  = Empty
  | Node of 'a * 'a btree * 'a btree

let rec reduce_btree (e: 'b) (n: 'a -> 'b -> 'b -> 'b)  (t: 'a btree) : 'b =
  match t with
  | Empty -> e
  | Node (v, t1, t2) -> n v (reduce_btree e n t1) (reduce_btree e n t2)

let t = Node(5, Node(4, Node(1, Empty, Empty), Empty), Empty)

let () = assert (reduce_btree 0 (fun v b1 b2 -> v + b1 + b2) t = 10)

(* Hwk 03 *)
type 'a btree_Hwk_3
  = Nil
  | Leaf of 'a
  | Fork of 'a btree_Hwk_3 * 'a * 'a btree_Hwk_3

(* A reduce function for this .... *)                                    

(* Some issues with inductive proofs *)

let rec sum (lst: int list) : int =
  match lst with
  | [] -> 0
  | x::xs -> x + sum xs

let () = assert (sum [1;2;3;4] = 10);
         assert (sum [] = 0)

let sumr (lst: int list) : int =
  reduce_list 0 (+) lst

let () = assert (sumr [1;2;3;4] = 10);
         assert (sumr [] = 0)
let rec prod (lst: int list) : int =
  match lst with
  | [] -> 1
  | x::xs -> x * prod xs

let () = assert (prod [2;3;4] = 24);
         assert (prod [] = 1)