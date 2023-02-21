let rec even2ways lst = match lst with
| []-> true
| x::[]->false
|x1::x2::rest -> if x1 mod 2 =0 && x2 mod 2 =0 then even2ways rest else false

let is_empty lst = match lst with
| [] ->true
| _ -> false

let head lst = match lst with 
| []-> failwith "oh no there is is_empty list"
| x::_-> x