

(* Nov 13 *)

let rec sumToN n = match n with
    | 0 -> 0
    | _ -> n + sumToN (n-1)

let non_negs = from 0
let sumsTo_map = map sumToN non_negs

let () =
  assert (take 10 sumsTo_map = [0;1;3;6;10;15;21;28;36;45])

let rec sumsTo () =
  Cons (0, delay (fun () -> zip (+) nats (sumsTo ()) ))

let () =
  assert (take 10 (sumsTo ()) = [0;1;3;6;10;15;21;28;36;45])

let add_p x y =
  print_endline ("adding " ^ string_of_int x ^ " to " ^
                   string_of_int y ^ ".");
  x + y

let rec sumsTo_p () =
  Cons (0, delay (fun () -> zip add_p nats (sumsTo_p ()) ))

let () =
  assert (take 10 (sumsTo_p ()) = [0;1;3;6;10;15;21;28;36;45])

(*               nats
  sums = 0
         ^ + >   1    
         (=1)
         ^ + >   2
         (=3)
         ^ + >   3
         (=6)
         ...
 *)

(*
let rec sumsTo' =
  Cons (0, delay (fun () -> zip add_p nats sumsTo'))
*)

let sumsTo' =
  let dummy = ref nats
  in
  let sums = Cons (0, delay (fun () -> zip add_p nats (! dummy)))
  in
  let () = dummy := sums
  in
  sums

let () =
  assert (take 10 sumsTo' = [0;1;3;6;10;15;21;28;36;45])

    