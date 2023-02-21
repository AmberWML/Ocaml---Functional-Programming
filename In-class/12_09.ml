(* Suffix: *)

let rec suffixes (lst: 'a list) : 'a list list =
  match lst with
  | [] -> [ [] ]
  | _::xs -> lst :: suffixes xs  

   (* COMPLEX:constant time step *)
    (* suffixes is linear ,DUPLICATUING THE ORIGINAL LIST*)
(*     O(N) O(N)O(N^2) *)