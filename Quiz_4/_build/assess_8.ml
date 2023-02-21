let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let prod = ref 0 in
let x = ref 0 in
let _stop = ref 0 in
prod := 1;
x := 1;
_stop := 4;
(
let rec loop () =
(
if (!x <= !_stop)
then
(
prod := (!prod * !x);
i := (!i + 1);
loop ()
)
else
()
)
in loop ()
);
print_int (!prod); print_newline ()
