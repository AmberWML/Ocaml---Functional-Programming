type shape = Circle of circ_desc
           | Triangle of coord * coord * coord
           | Rect of sqr_desc

let isRect s = match s with
    | Rect _ -> true
    | _ -> false

let area s = match s with
    | Circle (_, r) -> r *. r *. 3.14
    | _ -> failwith "Finish this"

let head lst = match lst with
    | [] -> None
    | x::xs -> Some x

let rec minList lst = match lst with
    | [] -> None
    | x::[] -> Some x
    | x::xs -> match minList xs with
               | None -> failwith "Oops"
               | Some m ->
                  if x < m then Some x
                  else Some m