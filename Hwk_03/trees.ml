(* Amber wong
wang8039
hw3 *)

(* A tree type declaration. *)
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


let rec size (t:'a tree) :int=
  match t with
  |Empty -> 0
  |Node(v,t1,t2) -> 1+ size t1 + size t2

let rec sum (t: int tree) : int =
  match t with
  | Empty -> 0
  | Node (v, t1, t2) -> v + sum t1 + sum t2

let rec product (t: int tree) : int =
  match t with
  | Empty -> 1
  | Node (v, t1, t2) -> v * product t1 * product t2

let rec charcount (t: string tree) : int =
  match t with
  | Empty -> 0
  | Node(v,t1,t2) -> String.length v+ charcount t1 + charcount t2

let rec concat (t: string tree) : string =
  match t with
  | Empty -> ""
  | Node(v,t1,t2) -> concat t1^ v  ^ concat t2

(* part 2 *)
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

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )

let rec list_size lst = 
  match lst with
  | [] -> 0
  | hd::tl -> 1+ list_size tl

let rec list_sum lst = 
  match lst with
  | [] -> 0
  | hd::tl -> hd+ list_sum tl

let rec list_product lst = 
  match lst with
  | [] -> 1
  | hd::tl -> hd* list_product tl

let rec list_char lst = 
  match lst with
  | [] -> 0
  | hd::tl -> String.length hd+list_char tl

let rec list_concat lst = 
  match lst with
  | [] -> ""
  | hd::tl -> hd ^ list_concat tl


let rec list_tree_size (t:'a list tree) :int=
  match t with
  |Empty -> 0
  |Node(v,t1,t2) ->  (list_size v) + list_tree_size t1 + list_tree_size t2

let rec list_tree_sum (t: int list tree) : int =
  match t with
  | Empty -> 0
  | Node (v, t1, t2) ->(list_sum v) + list_tree_sum t1 + list_tree_sum t2

let rec list_tree_product (t: int list tree) : int =
  match t with
  | Empty -> 1
  | Node (v, t1, t2) -> (list_product v) * list_tree_product t1 * list_tree_product t2

let rec list_tree_charcount (t: string list tree) : int =
  match t with
  | Empty -> 0
  | Node(v,t1,t2) -> list_char v + list_tree_charcount t1 + list_tree_charcount t2

let rec list_tree_concat (t: string list tree) : string =
  match t with
  | Empty -> ""
  | Node(v,t1,t2) -> list_tree_concat t1^ list_concat v  ^ list_tree_concat t2

(* A sample tree containing ints *)
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

(* tests *)
let () = 
  print_string "Testing part 1 ... " ;
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
    
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg



let () = 
  print_string "Testing part 2 ... " ;
  try
    
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

