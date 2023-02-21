let print (n:int) = print_string "Your number is ";
                    print_int n;
                    print_endline "!"

let delayed_print (n:int) = fun () -> print n                      

type 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

type 'a lazee = 'a hidden ref

let delay (unit_to_x: unit -> 'a) : 'a lazee =
  ref ( Thunk unit_to_x )

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk _ -> failwith "This should not happen."

type 'a stream = Cons of 'a * 'a stream lazee

let rec from n =
  print_endline ("step " ^ string_of_int n);
  Cons (n, delay (fun () -> from (n+1) ) )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
    | Cons (h, _) -> h
let tail (s: 'a stream) : 'a stream = match s with
    | Cons (_, t) -> demand t
                   
let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t)
         )

let rec squares_from (n: int) : int stream =
  Cons ( n*n, delay (fun () -> squares_from (n+1)) )

let rec filter (f: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (h, t) ->
     let rest = delay (fun () -> filter f (demand t)) in
     if f h
     then Cons (h, rest)
     else demand rest

let even n = n mod 2 = 0
let odd n = n mod 2 <> 0

let evens = filter even nats
let odds = filter odd nats

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t) ) )

let odds_via_map = Cons (1, delay (fun () ->  map (fun n -> n + 1) evens ) )

let rec zip (f: 'a -> 'b ->'c) (s1: 'a stream) (s2: 'b stream) :
          'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))
                    
                                              
(* Computing factorials
   nats       = 1   2   3   4    5     6 
                 \   \   \   \    \     \
                  *   *   *   *    *     *
                   \   \   \   \    \     \
   factorials = 1-*-1-*-2-*-6-*-24-*-120-*-720
   We can define factorials recursively.  Each element in the stream
   is the product of then "next" natural number and the previous
   factorial.
 *)

let rec factorials () : int stream =
  Cons (1, delay (fun () -> zip ( * ) nats (factorials ())  ) )
