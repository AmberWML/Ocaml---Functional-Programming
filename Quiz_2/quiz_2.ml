(* Amber wong
wang8039
2041quiz02 *)


type 'a tree = Leaf of 'a
             | Fork of 'a tree * 'a tree


let t: int tree =  
	Fork(Fork(Leaf 3,Leaf 4), Fork(Leaf 1, Fork(Leaf 6,Leaf 8)))

let t2: int tree  = 
	Fork(Leaf 3,Fork(Leaf 1,Leaf 4))

let rec sum (t:int tree): int =
	match t with
	| Leaf a -> a
	| Fork(t1,t2) -> sum t1 + sum t2

let rec prod (t:int tree): int =
	match t with
	| Leaf a -> a
	| Fork(t1,t2) -> prod t1 * prod t2

(* In your definition of reduce write a complete set of
 type annotations for the function so that the type of 
 reduce is written out explicitly like we have done before.
 *)

let rec reduce f2 (base:'b)  (f: 'b-> 'b -> 'b  ) 
          (t: 'a tree): 'b =
    match t with
    | Leaf a ->  let base = f2 a base in base
    | Fork(t1,t2) -> f (reduce f2 base f t1) (reduce f2 base f t2)
(* sumr using reduce *)
let sumr (t:int tree): int = let f2 a base = a+base in
	let f t1 t2 = t1+t2 in 
	reduce f2 0 f t
(* prodr using reduce *)
let prodr (t:int tree): int = let f2 a base = a*base in
	let f t1 t2 = t1*t2 in reduce f2 1 f t
(* I use the reduce to swtich the int tree to int list first
and then use String operation to map all the ints to string type
finally concatanation all of the strings to get the final result. *)
let string_of_int_tree (t:int tree):string =
	let treelist =
		let f2 a base = a::base in
			let f t1 t2 = t1@t2 in 
	reduce f2 [] f t in
	String.concat "" (List.map string_of_int treelist)


(* below is the way of not changing the type of first element,
but using the recursive helper function *)
(* let rec reducetree (f: 'b -> 'b  -> 'b)
          (t: 'a tree) : 'b =
    match t with
    | Leaf a -> string_of_int a
    | Fork(t1,t2) -> f (reduce f t1) (reduce f t2) *)

(* let tostring x=string_of_int x

let rec map_tree f t=
    match t with
    | Leaf v -> Leaf (f v)
    | Fork(lt,rt) -> Fork(map_tree f lt, map_tree f rt)

let string_of_int_tree t = 
	let tnew = 
	let f x = tostring x in
	map_tree f t in
	let g g1 g2 = g1^g2 in
	reduce g tnew *)
