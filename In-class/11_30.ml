let rec process_solution_exn show s =
  print_endline ("Here is a solution: " ^ show (List.rev s)) ;
  print_endline ("Do you like it ?" ) ;
  match String.sub (read_line ()) 0 1 with
  | "Y" | "y" -> print_endline "Thanks for playing!"; s
  | _ -> raise KeepLooking


let subsetsum_exn (lst: int list) : int list option =
  let rec try_subset partial_subset rest =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest = []
    then
      process_solution_exn (P.show_list string_of_int) partial_subset
    else 
      match rest with
      | [] -> raise KeepLooking
      | x::xs -> try try_subset (x::partial_subset) xs with
                | KeepLooking ->
                   try_subset partial_subset xs
  in try Some (List.rev (try_subset [] lst)) with
       | KeepLooking -> None


                      
let subsetsum_exn_continuation (lst: int list)
      (success: int list -> int list) : int list option =
  let rec try_subset partial_subset rest =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then
      success partial_subset
    else
      match rest with
      | [] -> raise KeepLooking
      | x::xs -> try try_subset (x::partial_subset) xs with
                 | KeepLooking ->
                    try_subset partial_subset xs
  in try Some (List.rev (try_subset [] lst)) with
       | KeepLooking -> None
        
let subsetsum_exn_v1 lst =
  subsetsum_exn_continuation
    lst
    (process_solution_exn (P.show_list string_of_int))
    
let subsetsum_exn_first (lst: int list) : int list option =
  subsetsum_exn_continuation
    lst
    (fun s -> s)

let subsetsum_exn_all (lst: int list) =
  subsetsum_exn_continuation
    lst
    (fun s ->
      P.print_int_list (List.rev s);
      raise KeepLooking
