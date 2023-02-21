type 'a tree = E
             | T of 'a tree * 'a * 'a tree

type 'a set = 'a tree

let rec member (x: 'a) (s: 'a set) : bool =
  match s with
  | E -> false
  | T (t1, x', t2) ->
     if x < x'
     then member x t1
     else if x > x'
     then member x t2
     else true
      
let rec insert (x: 'a) (s: 'a set) : 'a set =
  match s with
  | E -> T (E, x, E)
  | T (t1, x', t2) ->
     if x < x'
     then T (insert x t1, x', t2)
     else if x > x'
     then T (t1, x', insert x t2)
     else s