# Homework 4: Reasoning about Correctness.
by < Amber Wong,wang8039 >


The principle of induction: Prove when the base case holds in the 
statement and then, assume there is n= k, the statement P(k) is true, 
then prove when n = k+1, the statement also holds. In this induction
way, we can know every element in this statement is true. We use this
principle in every problems in this hw.

# problem 1
we have known that
```
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys
```
Now prove
```
prod (l1 @ l2) = prod l1 * prod l2
```
Show Base```prod ([] @ l2) = prod [] * prod l2```
```
prod ([] @ l2) 
= prod l2
  by properties of lists and @
= 1 * prod l2
  by arithmetic
= prod [] * prod l2
  by def of prod
 ```
Inductive case: Show ```prod ((y::ys) @ l2) = prod (y::ys) * prod l2 ```Given ```prod (ys @ l2) = prod ys * prod l2```
principle: ```∀l,P(l) holds if P([ ]) holds and P(l′) =⇒ ∀v, P(v :: l′))```
```
prod ((y::ys) @ l2) 
= prod ( y :: (ys @ l2) )
  by properties of lists and @
= y * prod (ys @ l2)
  by def of prod
= y * (prod ys * prod l2)
  by inductive hypothesis
= (y * prod ys) * prod l2
  by arithmetic
= prod (y::ys) * prod l2
  by def of prod
  ```
Hence ,proved.

# problem 2
Given 
```
let rec sum = function
  | [] -> 0
  | y::ys -> y + sum ys

let rec length = function
  | [] -> 0
  | y::ys -> 1 + length ys

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys
  ```
Using the definitions above, show by induction that

Now prove:
```
sum (inc_all l) = length l + sum l
```
Prove Base case:```sum (inc_all []) = length [] + sum []```
```
sum (inc_all [])
= sum ([])
  by def of inc_all
= 0 + sum ([])
  by arithmetic
= length [] + sum []
  by def of length
```
Inductive case: Show```sum (inc_all (y::ys)) = length (y::ys) + sum (y::ys)```
				given```sum (inc_all ys) = length ys + sum ys```
				principle: ```∀l,P(l) holds if P([ ]) holds and P(l′) =⇒ ∀v, P(v :: l′))```
```
sum (inc_all (y::ys))
= sum ((y+1) :: inc_all ys)
  by def of func inc_all
= (y+1)+sum(inc_all ys)
  by def of func sum
= (y+1)+length ys+sum ys
  by given inductive hypothesis
= (1 + length xs) + (x + sum xs)
  by associativity arithmetic property 
= length (x::xs) + sum (x::xs)
  by def of length and sum
```
Hence ,proved.

# problem 3

Given 
```
let rec map f l = match l with
  | [] -> []
  | y::ys -> f y :: map f ys

let inc x = x + 1

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys
```
Now prove:
```
map inc l = inc_all l
```
Prove Base case:```map inc [] = inc_all []```
```
map inc []
= []
  by def of map
= inc_all []
  by def of inc_all
```
Inductive: prove ```map inc (y::ys) = inc_all (y::ys)``` Given ```map inc ys = inc_all ys```
principle: ```∀l,P(l) holds if P([ ]) holds and P(l′) =⇒ ∀v, P(v :: l′))```
```
map inc (y::ys)
= inc y :: map inc ys
  by def of map
= (y + 1) :: map inc ys
  by def of inc
= (y + 1) :: inc_all ys
  by given inductive hypothesis
= inc_all (y::ys)
  by def of inc_all
```
Hence, proved.

# problem 4

Given 
```
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

let rec to_list (t: 'a tree) : 'a list = match t with
  | Empty -> []
  | Node (v, tl, tr) -> to_list tl @ [v] @ to_list tr

let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys

let rec product (t: int tree) : int =
  match t with
  | Empty -> 1
  | Node (v, t1, t2) -> v * product t1 * product t2
```
Now prove:
```
prod (to_list t) = product t
```
Prove Base case:```prod (to_list Empty) = product Empty```
```
prod (to_list Empty)
= prod []
  by def of to_list
= 1
  by def of prod
= product Empty
  by def of product
```
Inductive: prove ``` prod (to_list Node(v, tl, tr)) = product Node(v, tl, tr)``` 
           Given ```prod (to_list tl) = product tl and prod (to_list tr) = product tr```
principle: ```∀t ∈ ’a btree, P (t) holds if - P(Empty) and -P(t1)andP(t2)=⇒P(Node (t1,v,t2)), P(t) holds if P(Empty) holds if P(t1) and P(t2) hold then P(Node(v,t1,t2)) holds```

```
prod (to_list Node(v, tl, tr))
= prod (to_list tl @ [v] @ to_list tr)
  by def of to_list
= prod (to_list tl) * prod ([v]) * prod (to_list tr)
  we have proved prod (l1 @ l2) = prod l1 * prod l2, so prod (to_list tl @ [v] @ to_list tr) = prod 
  (to_list tl) * prod ([v]) * prod (to_list tr)
= product tl * prod ([v]) * product tr
  by inductive hypothesis
= product tl * v * product tr
  because prod v = prod v::[] = v*1 = v, by def of prod
= v * product tl * product tr
  by associativity arithmetic property 
= product Node(v, tl, tr)
  by def of product
```
Hence, proved.


# problem 5

Given 
```
let rec size (t:'a tree) :int=
  match t with
  |Empty -> 0
  |Node(v,t1,t2) -> 1+ size t1 + size t2

let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
          (t: 'a tree) : 'b =
    match t with
    | Empty -> b
    | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)


let size_r t = let f v t1 t2 = 1+t1+t2 in reduce 0 f t
```
Now prove:
```
size t = size_r t
```
Prove Base case:```size Empty = size_r Empty```
```
size Empty
= 0
  by def of size,from left version
= reduce 0 (fun v t1 t2 -> 1 + t1 + t2 ) Empty
  by def of reduce
= size_r Empty
  by def of size_r
```
Inductive: prove ``` size Node(v, t1, t2) = size_r Node(v, t1, t2)``` 
           Given ```size t1 = size_r t1 and size t2= size_r t2```
principle: ``` ```

```
size Node(v, t1, t2)
= 1 + size t1 + size t2
  by def of size
= 1 + size_r t1 + size_r t2
  by inductive hypothesis
= f:(fun v t1 t2 -> 1 + t1 + t2 ) v (size_r t1) (size_r t2)
   by fun application
= f v (reduce 0 f t1) (reduce 0 f t2)
  by def of size_r
= reduce 0 (fun v t1 t2 -> 1 + t1 + t2) Node(v, t1, t2) 
  by def of reduce
= size_r Node(v, t1, t2)
  by def of size_r
```
Hence, proved.







