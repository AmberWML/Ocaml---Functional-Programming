type nat = Zero | Succ of nat

let rec toInt = function
  | Zero -> 0
  | Succ n -> toInt n + 1

let rec add n1 n2 = match n1, n2 with
  | Zero, n -> n
  | Succ m', m -> Succ (add m' m)
                
let rec sumTo (n: int) : int = match n with
  | 0 -> 0
  | n -> n + sumTo (n-1)

let rec sumTo' (n: nat) : nat = match n with
  | Zero -> Zero
  | Succ n' -> add n (sumTo' n')