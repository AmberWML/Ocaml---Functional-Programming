(* Amber wong
wang8039
hw3 *)

type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

let rec insert_by (f:'a -> 'a -> int) (newa: 'a) (t: 'a btree):  'a btree =
	match t with
	| Nil -> Leaf newa
	| Leaf x -> if f newa x < 0 then Fork( Leaf newa, x ,Nil) else if f newa x = 0 then Leaf x else Fork (Nil, x, Leaf newa)
	| Fork(t1 , xt, t2)-> if (f xt newa) < 0 then Fork( t1, xt , (insert_by f newa t2 )) else if (f xt newa) = 0 then Fork(t1 , xt, t2) else Fork( (insert_by f newa t1 ), xt ,t2)


let from_list (f:'a -> 'a -> int) (lst:  'a list):  'a btree =
	List.fold_left (fun a b -> insert_by f b a) Nil lst

let rec reduce (b: 'b) (f: 'b -> 'a -> 'b -> 'b)
          (t: 'a btree) : 'b =
    match t with
    | Nil -> b
    | Leaf x -> f b x b
    | Fork(t1 ,xt ,t2) -> f (reduce b f t1) xt (reduce b f t2)

let to_list t =  let addlist t1 xt t2 = t1@[xt]@t2
  in reduce [] addlist t


let () = 
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = 
              insert_by compare 4 Nil);
    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   ) 
           );
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));
    assert (insert_by compare 10 (Leaf 5) = Fork (Nil, 5, Leaf 10));
    assert(insert_by compare 10 (Fork (Nil, 2, Leaf 4)) =Fork (Nil, 2, Fork (Nil, 4, Leaf 10)));
    assert (from_list compare [5;100;87;1;25] =Fork (Leaf 1, 5, Fork (Fork (Leaf 25, 87, Nil), 100, Nil)));
    assert (from_list compare [5;100;87;1;6;9;77;61] = Fork (Leaf 1, 5,
 Fork (Fork (Fork (Nil, 6, Fork (Nil, 9, Fork (Leaf 61, 77, Nil))), 87, Nil),
  100, Nil)));
    assert (List.sort compare [5;100;87;1;25] =
              to_list (from_list compare [5;100;87;1;25]));

    assert (List.sort compare [5;100;87;1;6;9;77;61]=
              to_list (from_list compare [5;100;87;1;6;9;77;61]));
    

    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg