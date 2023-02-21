(* No In calss Just some useful code *)




let moves (s:state) : state list =
 let move_person (p,w,g,c) 
   = [ ( other_side p, w, g, c ) ] 
 in
 let move_wolf (p,w,g,c) 
   = if p = w
     then [ ( other_side p, other_side w, g, c ) ]
     else [ ]
 in
 let move_goat (p,w,g,c) 
   = if p = g
     then [ ( other_side p, w, other_side g, c ) ]
     else [ ]
 in
 let move_cabbage (p,w,g,c) 
   = if p = c
     then [ ( other_side p, w, g, other_side c ) ]
     else [ ]
 in
 List.filter ok_state ( move_person s @ move_wolf s @ 
                        move_goat s @ move_cabbage s )
  
let rec is_not_elem set v = not (List.mem v set)


let crossing_v1 () : state list option =
  let rec go_from (state: state) (path: state list) =
    if final state
    then Some path
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> None
      | [a] -> go_from a (path @ [a])
      | [a;b] -> (match go_from a (path @ [a]) with
                  | None -> go_from b (path @ [b])
                  | Some path' -> Some path'
                 )
      | _ -> failwith "No way to move 3 ways"
  in go_from (L,L,L,L) [ (L,L,L,L) ]

 
exception FoundPath of state list

let crossing_v2 () : state list option = 
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> ()
      | [a] -> go_from a (path @ [a])
      | [a;b] -> go_from a (path @ [a]);
                 go_from b (path @ [b])
      | [a;b;c] -> go_from a (path @ [a]);
                   go_from b (path @ [b]);
                   go_from c (path @ [c])
      | _ -> failwith "No way to move 4 ways"
  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p

(* Another that raises exceptions to keep looking for solutions. *)
exception KeepLooking

let crossing_v3 () : state list option =
  let rec go_from (state: state) (path: state list) : state list =
    if final state
    then path
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> raise KeepLooking
      | [a] -> go_from a (path @ [a])
      | [a;b] -> (try go_from a (path @ [a]) with
                 | KeepLooking -> go_from b (path @ [b])
                 )
      | [a;b;c] -> (try go_from a (path @ [a]) with
                    | KeepLooking -> try go_from b (path @ [b]) with
                                     | KeepLooking -> go_from c (path @ [c])
                   )
      | _ -> failwith "No way to move 4 ways"
  in try Some (go_from (L,L,L,L) [ (L,L,L,L) ]) with
     | KeepLooking -> None



let crossing_many_possible_moves () : state list option =
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      let valid_moves = List.filter (is_not_elem path) (moves state)
      in
      let go_func () (m: state) : unit = go_from m (path @ [m])
      in
      List.fold_left go_func () valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p

                         

let crossing_many_possible_moves' () = 
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      let valid_moves = List.filter (is_not_elem path) (moves state)
      in
      let go_func (m: state) : unit = go_from m (path @ [m])
      in
      List.iter go_func valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p

