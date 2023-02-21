
let show_list show lst =
  "[" ^ String.concat "; " (List.map show lst) ^ "]"

let show_lists show lsts =
  String.concat "\n" (List.map (show_list show) lsts)

let print_int_list lst =
  print_endline ((show_list string_of_int) lst)

let print_int_lists lsts =
  print_endline ((show_lists string_of_int) lsts)


module P = PrintLists


let s = [1; 3; -2; 5; -6]
      
let sum lst = List.fold_left (+) 0 lst
exception FoundSet of int list
                    
let subsetsum_exn_on_found (lst: int list) : int list option =
  let rec try_subset partial_subset rest : unit =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then
      raise (FoundSet partial_subset)
    else
      (match rest with
       | [] -> ()
       | x::xs -> try_subset (x::partial_subset) xs;
                  try_subset partial_subset xs
      )
  in try try_subset [] lst; None with
     | FoundSet result -> Some result

exception KeepLooking



let subsetsum_exn (lst: int list) : int list option =
  let rec try_subset  partial_subset rest : int list =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then partial_subset
    else (match rest with
          | [] -> raise KeepLooking
          | x::xs -> (try try_subset (x::partial_subset) xs with
                      | KeepLooking -> 
                         try_subset partial_subset xs
                     )
         )
  in try Some (try_subset [] lst) with
     | KeepLooking -> None


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


                      
(* let subsetsum_exn_continuation (lst: int list)
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
       | KeepLooking -> None *)
        

