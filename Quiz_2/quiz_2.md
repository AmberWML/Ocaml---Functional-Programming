# Proof for quiz2

Given
```
let even x = x mod 2 = 0
let rec all_evens (lst: int list) : int list =
  match lst with
  | [] -> []
  | x::xs when even x -> x :: (all_evens xs)
  | _::xs -> all_evens xs
let rec filter (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | x::xs when f x -> x :: filter f xs
  | _::xs -> filter f xs
let all_evens_f lst = filter even lst
```

inductive proof of the claim 
```
all_evens lst = all_evens_f lst 
```
based on the implementations of these functions above

### An OCaml data type is defined by a number of variants, each of which can wrap a tuple of values. Given such a definition, each non-recursive variant is a base-case, and each recursive variant is an inductive case, with an induction hypothesis for each occurence of a recursive type. we prove when lst =[], function hold, assume when lst = xs, function holds and prove when lst = x::xs the function also holds, then proved. 


### Base Case: lst = []

show: all_evens [] = all_evens_f [] 
```
all_evens [] 
= []
  by def of all_evens
= filter (f a -> a mod 2 =0) [] 
  by def of filter
= all_evens_f []
  by def of all_evens_f
```

### Inductive case: lst = x::xs
Given  all_evens xs = all_evens_f xs
Show: all_evens (x::xs) = all_evens_f (x::xs)

Case1: when even x
```
all_evens (x::xs) 
= x :: (all_evens xs)
  by def of all_evens and known x is even
= x :: filter (f a -> a mod 2 =0) xs
  by def of filter
= f x -> x mod 2 =0 :: filter (f a -> a mod 2 =0) xs
  by f and def of filter
= filter (f a -> a mod 2 =0) (x::xs)
  by def of filter
= all_evens_f (x::xs)
```

Case2: when not even x
```
all_evens (x::xs) 
= all_evens xs
  by def of all_evens and known x is not even
= filter (f a -> a mod 2 =0) xs
  by def of filter
= filter (f a -> a mod 2 =0) (_::xs)
= all_evens_f (_::xs) 
  when _ is not even number so f x -> x mod 2 =0 is false
= all_evens_f (x::xs)
```
Hence, proved.

