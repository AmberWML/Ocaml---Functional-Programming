(*
lower * lower <= n <= upper * upper
->
let (lower’,upper’) = sqrt_step n lower upper in 
    lower’ * lower’ <= n <= ’upper * upper’
 *)
let sqrt n =
  let accuracy = 0.0001 in
  let rec sqrt_step lower upper =
    if (upper -. lower) < accuracy
    then (lower, upper)
    else let guess = (lower +. upper) /. 2.0 in
         (if (guess *. guess) > n
          then sqrt_step lower guess
          else sqrt_step guess upper
         )
  in
  if n > 0.0 
  then sqrt_step 1.0 n
  else failwith "Error: positive input required."


let rec euclid m n =
  if m = n then m
  else 
    if m < n 
    then euclid m (n-m) 
    else euclid (m-n) n

let rec euclid' m n = match m, n with 
  | m, n when m = n -> m
  | m, n when m < n -> euclid' m (n-m) 
  | m, n when m > n -> euclid' (m-n) n 
  | _, _ -> failwith "Can't actually get here."
