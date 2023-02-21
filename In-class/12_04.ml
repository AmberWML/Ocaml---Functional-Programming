(* No In calss Just some useful code *)

module P = PrintLists


let s = [1; 3; -2; 5; -6]

let sum lst = List.fold_left (+) 0 lst


let rec gen_subsets (lst: 'a list) : 'a list list 
  =  match lst with
  | [] -> [ [] ]
  | x::xs -> (List.map (fun a -> x::a ) (gen_subsets xs)) @
               (gen_subsets xs)

let subsetsum_accum (lst: 'a list) : 'a list list = 
  let rec make_subset partial_subset rest subsets =
    let subsets' = 
      if sum partial_subset = 0 &&
         partial_subset <> [] &&
         rest = []
      then
        partial_subset :: subsets
      else
        subsets
    in
    match rest with
      | [] -> subsets'
      | x::xs -> 
         let subsets'' = make_subset (x::partial_subset) xs subsets'
         in
         make_subset partial_subset xs subsets''
  in make_subset [] lst []


(* Here we see a solution that computes all subsets that 
   sum to 0.  It is also recursive in a non-linear way.
 *)
let rec subsetsum_all (lst: 'a list) : 'a list list =
  List.filter (fun xs -> sum xs = 0) (gen_subsets lst)




(* Using lazy lists.  These are streams that may be finite. *)

(* Lazy datatypes and functions *)
type 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

type 'a lazee = 'a hidden ref

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
               | Nil

let rec from n =
  print_endline ("step " ^ (string_of_int n));
  Cons ( n, delay ( fun () -> from (n+1) ) 
       )



  
let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Nil -> Nil
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then
       Cons (hd, rest)
     else demand rest

let rec filter' (prt: 'a -> unit) (p: 'a -> bool) (s: 'a stream)
         : 'a stream =
  match s with
  | Nil -> Nil
  | Cons (hd, tl) -> 
     prt hd;
     let rest = delay (fun () -> filter' prt p (demand tl)) in
     if p hd 
     then
       Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Nil -> Nil
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec append (s1: 'a stream) (s2: 'a stream) : 'a stream =
  match s1 with
  | Nil -> s2
  | Cons (h, t) -> Cons (h, delay (fun () -> append (demand t) s2))

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Nil -> []
          | Cons (h, t) -> h :: take (n-1) (demand t) 
         )

let rec gen_subsets_lazy (lst: 'a list) : 'a list stream 
  =  match lst with
  | [] -> Cons ([], delay (fun () -> Nil))
  | x::xs -> (append 
                  (map (fun a -> x::a ) (gen_subsets_lazy xs))
                  (gen_subsets_lazy xs)
               )
let rec subsetsum_all_lazy (lst: int list) : int list stream
  =  filter (fun s -> sum s = 0) (gen_subsets_lazy s)


let one_answer = take 10 (subsetsum_all_lazy s)
   
let rec subsetsum_all_lazy' (lst: int list) : int list stream
  =  filter' P.print_int_list (fun s -> sum s = 0) (gen_subsets_lazy s)

    