type 'a bintree = Lf of 'a
                | Nd of 'a bintree * 'a bintree

let rec equal_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | (x::xs), (y::ys) -> if x = y then equal_list xs ys else false
  | _, _ -> false

let rec append l1 l2 = match l1 with
  | [] -> l2
  | (x::xs) -> x :: append xs l2

let rec flatten t = match t with
  | Lf x -> [x]
  | Nd (t1,t2) -> append (flatten t1) (flatten t2)

(* If we evaluate the following function eagerly, it is slow because it 
   must flatten each tree completely before making any comparisons. 
   If we evaluate it lazily, we can avoid some unnecessary computaions. *)
let eqleaves_v1 t1 t2 = equal_list (flatten t1) (flatten t2)




(* This is the fast version that only flattens trees as much as is
   necessary.  This complexity is needed in a language that uses eager
   evaluation.  It is not needed in a lazy language.  *)
let rec eqleaves_v2 t1 t2 = comparestacks [t1] [t2]
and comparestacks f1 f2 = 
  match f1, f2 with
    | [ ], [ ] -> true
    | [ ], a::x -> false
    | a::x, [ ] -> false
    | (Nd (l, r) :: x), y -> comparestacks (l::r::x) y
    | x, (Nd (l, r) :: y) -> comparestacks x (l::r::y)
    | (Lf a)::x, (Lf b)::y when a = b -> comparestacks x y
    | _, _ -> false


(* a few simple sample trees *)
let t1 = Lf (3 * 2)
let t2 = Nd (Lf (3 + 3), Lf 5)
