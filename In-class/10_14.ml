(* In class and test in Ocaml 4.08.1 ----Amber *)
let rec rev lst = match lst with
  | [] -> []
  | x::xs -> rev xs @ [x]


let rev_accum lst =
  let rec ra accum ys =
    match ys with
    | [] -> accum
    | x::xs -> ra (x::accum) xs
  in
  ra [] lst
let rev_fold lst =
   List.fold_left (fun accum x -> x::accum) [] lst