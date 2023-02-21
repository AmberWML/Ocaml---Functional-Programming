# Quiz 3, CSci 2041

- **Due at 5:00pm on Wednesday, November 18.**

- **There are no late-day extensions allowed for this quiz.**

- **In case of problems with Git, you may email your work to the TAs
  before the due date.**

- Add your answers to this file by replacing the line 
   ```
   .... remove this line and write your answer here ...
   ```
   and replacing it with the lines containing your answer.

   Please leave the "```" marks that are there now.


# Trees and functions

Consider the following OCaml declarations:
```ocaml
type 'a tree = End of 'a
             | More of 'a * 'a tree * 'a tree

let rec sum (t: int tree) : int = match t with
  | End v -> v
  | More (v, t1, t2) -> v + sum t1 + sum t2

let rec reduce (e: 'a -> 'b) (m: 'a -> 'b -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | End v -> e v
  | More (v, t1, t2) -> m v (reduce e m t1) (reduce e m t2)

let sumr t = reduce (fun a -> a) (fun v l r -> v + l + r) t
```


# A correctness property

Consider the following correctness property over ``t``, an ``int
tree``, that relates the functions
and types given above:
```
sum t = sumr t
```

# A proof

Write your answer to the following 6 parts of a proof of the above
property in the 6 spaces indicated below.


### 1. Principle of induction

What is the principle of induction to be used in this proof?
```
∀t ∈ int tree, P (t) holds if 
- P(End v) holds
and 
-P(t1)andP(t2)=⇒P(More(v, t1, t2)) holds
Translate:
To using the principle of induction, we are going to prove this is true by proving P(t) holds if P(Empty) holds, and assuming P(t1) and P(t2) hold then prove P(Node(v,t1,t2)) holds. Above all, proved.
```


### 2. Base case

What is the base case that needs to be proved?
```
sum (End v)= sumr (End v)
```


### 3. Inductive case

What is the inductive case that needs to be proved?
```
sum More(v, t1, t2)= sumr More(v, t1, t2)
```


### 4. Inductive hypothesis

What is the inductive hypothesis to be used in the proof?
```
sum t1 = sumr t1 and sum t2 = sumr t2
```

### 5. Proof of the base case

Write your proof of the base case below.
```
sum (End v)
= v
  by def of func sum
= (fun a -> a) v
  by func property
= reduce (fun a -> a) (fun v l r -> v + l + r) (End v)
  by def of reduce func
= sumr (End v)
  by def of func sumr 
```


### 6. Proof of the inductive case

Write your proof of the inductive case below.
```
sum More(v, t1, t2)
= v + sum t1 + sum t2
  by def of func sum
= v + sumr t1 + sumr t2
  by inductive hypothesis
= m:(fun v l r -> v + l + r) v  (sumr t1) (sumr t2)
  by using m function application
= m v (reduce (fun a -> a) m t1) (reduce (fun a -> a) m t2) 
  by the def of sumr
= reduce (fun a -> a) (fun v l r -> v + l + r) More(v, t1, t2)
  by def of reduce 
= sumr More(v, t1, t2)
  by def of sumr

Hence,proved.
```
