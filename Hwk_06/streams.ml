(* Constructing lazy values in OCaml *)

(* Lazy datatypes and functions *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = 
  ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


(* Some examples streams from files developed in class. *)
let rec from n =
  Cons ( n, delay ( fun () -> from (n+1) ) )

let ones =
  let rec mk_ones () = Cons (1, delay ( mk_ones ) )
  in mk_ones ()

let nats = from 1


(* Some helpful functions from files developed in class. *)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s :'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t) 
         )

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))


(* Below is a stream of factorials.  It uses, the same definition of
   factorials as we developed in class except that the built in
   multiplication operator is replaced by a function `mul_p` that
   still multiplies its arguments but prints out those arguments as
   well.  *)

let mul_p x y =
  let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
  in x * y

let rec factorials () =
  Cons (1, delay (fun () -> zip mul_p nats (factorials ())))

let facts = factorials ()

let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])



(* Please write your solutions below. *)
(* (part 1) *)


(* For the cubes ,use the cons to make a stream *)
let rec cubes_from (a:int):int stream = 
  Cons(a*a*a,delay (fun () -> cubes_from(a+1)))

(* let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list

let makelist n = let rec helper x lst = match x with
| 0-> 0::lst
|_ -> x::helper (x-1) lst
in helper n []

let is_cube n = 
  let lst = rev (makelist n) in 
  let rec cube list1 n1= 
  match list1 with
  | []-> false
  | hd :: tl -> if n1 = hd*hd*hd then true else (cube tl n1)
in cube lst n

let cubes = filter is_cube nats *)
let cubes_from_map (a:int):int stream =
  (* Cons(a*a*a,delay (fun () -> map (fun a -> (a*a*a)) from a+1) ) *)
  map (fun a -> a*a*a) (from a)


let cubes_from_zip (a:int):int stream =
  (* Cons(a*a*a,delay (fun () -> zip (fun x y -> x * y * y ) (from a+1) (from a+1)) )
  zip (square functions ) from a  from a -> zip again for cube *)
  (* zip (fun x y -> x * y * y ) (from a) (from a) *)
  zip(fun  x y -> x * y) (zip (fun x y -> x * y  ) (from a) (from a)) (from a)


let () =
  assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
  assert (take 3 (cubes_from 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_zip 3) = [27; 64; 125])

(* Using the techniques described in class, define facts' (note the ') that computes the same stream as facts
 but does not repeat these multiplications. *)
let mul_p x y =
  print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                   string_of_int y ^ ".");
  x * y

let facts' =
  let dummy = ref nats
  in
  let muls = Cons (1, delay (fun () -> zip mul_p nats (! dummy)))
  in
  let () = dummy := muls
  in
  muls


let () =
  assert (take 5 facts' = [1; 1; 2; 6; 24])





  (* sift should filter out multiples from a stream *)
let multiple a b = b mod a <> 0
(* use multiple to deal with sift  *)
let sift (n:int) (s:int stream): int stream = filter (multiple n) s
(* match the s and find the final stream *)
let rec sieve (s:int stream):int stream = 
  match s with
  | Cons(x,xs) -> Cons(x,delay (fun ( ) -> sieve( sift x (demand xs))))

let primes = sieve (from 2)


let () =
  assert ( take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] )




