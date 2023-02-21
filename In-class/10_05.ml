let rec myMap (f: 'a -> 'b) (lst: 'a myList) : 'b myList =
  match lst with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (f hd, myMap f tl)

type 'a btree = Empty
              | Node of 'a * 'a btree * 'a btree

let t = Node (5,
              Node (4,
                    Node (1, Empty, Empty),
                    Empty),
              Node (2, Empty, Empty)
          )

let rec sumTree (t: int btree) : int =
  match t with
  | Empty -> 0
  | Node (v, t1, t2) -> v + sumTree t1 + sumTree t2