(* Amber wong
wang8039
hw3 *)
(* part 3 *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )

let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let strs_tree: string list tree =Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )
let int_tree2 : int tree =
  Node (3, 
        Node (1,Empty, Empty), Empty)

let int_tree3 :int tree = 
  Node(3,
    Node(1,
      Node(2,
        Node(5,Empty,Empty),Empty),Empty),Empty)

let str_tree1 : string tree =
 Node ("cold ",
       Node ("love ",
             Node ("I ", Empty, Empty), Empty),
       Node ("brew!", Empty, Empty)
      )

let ints_tree1: int list tree =
 Node ([10;11],
       Node ([1;2;3;4;5],
             Empty,
             Node ([1;2], Empty, Empty)
            ),
       Node ([],
             Node ([9;8], Empty, Empty),
             Node ([1],Empty,Empty)
            )
      )
let strs_tree1: string list tree =
 Node (["favorite "; "subject! "; "Also "; "math "],
       Node (["is "; "my "],
             Node (["WOW "; "cs "], Empty, Empty), Empty),
       Node (["is "; "one."], Empty, Empty)
      )


let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
          (t: 'a tree) : 'b =
    match t with
    | Empty -> b
    | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)


let size t = let f v t1 t2 = 1+t1+t2 in reduce 0 f t

let sum t = let f v t1 t2 = v+t1+t2 in reduce 0 f t

let product t = let f v t1 t2 = v*t1*t2 in reduce 1 f t

let charcount t = let f v t1 t2 = (String.length v)+t1+t2 in reduce 0 f t

let concat t = let f v t1 t2  = t1 ^ v ^t2 in reduce "" f t

let list_tree_size t = 
  let length lst = List.fold_left (fun a _ -> a + 1) 0 lst in 
  let f v t1 t2 = (length v) +t1+t2 in reduce 0 f t

let list_tree_sum t = 
  let suml lst = List.fold_left (fun a b -> a + b) 0 lst in 
  let f v t1 t2 = (suml v) +t1+t2 in reduce 0 f t

let list_tree_product t = 
  let productl lst = List.fold_left (fun a b -> a * b) 1 lst in 
  let f v t1 t2 = (productl v)*t1*t2 in reduce 1 f t
let list_tree_charcount t = 
  let charl lst = List.fold_left (fun a b -> a + String.length b) 0 lst in 
  let f v t1 t2 = (charl v)+t1+t2 in reduce 0 f t

let list_tree_concat t = 
  let treel lst = List.fold_left (fun a b -> a ^ b) "" lst in 
  let f v t1 t2 =t1^ (treel v) ^t2 in reduce "" f t

let () = 
  print_string "Testing part 3 ... " ;
  try
    
    assert (size str_tree = 4);
    assert (size int_tree2 = 2);
    assert (size int_tree3 = 4);
    assert (size str_tree1 = 4);
    assert (sum int_tree = 10);
    assert (sum int_tree3 = 11);
    assert (sum int_tree2 = 4);
    assert (product int_tree = 24);
    assert (product int_tree2 = 3);
    assert (product int_tree3 = 30);
    assert (charcount str_tree = 20);
    assert (charcount str_tree1 = 17);
    assert (concat str_tree = "I really love OCaml!");
    assert (concat str_tree1 = "I love cold brew!");
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_size strs_tree1 = 10);
    assert (list_tree_size ints_tree1 = 12);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_sum ints_tree1 = 57);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_product ints_tree1 = 1900800);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_charcount strs_tree1 = 48);
    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
    assert (list_tree_concat strs_tree1 = 
               "WOW cs is my favorite subject! Also math is one.");


    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg




