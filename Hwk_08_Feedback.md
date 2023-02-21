# Hwk_08 Feedback

Run on December 18, 14:27:48 PM.

More tests to come.  Just getting started now.  Tests will be complete by around December 9 if not sooner.

+ Pass: Change into directory "Hwk_08".

## Part 1. Collecting subset sum solutions: `subsetsum_exn_ref_all`

+ Pass: Check that file `search_exceptions.ml` exists.

+ Fail: Check that an OCaml file `search_exceptions.ml` has no syntax  or type errors.

    OCaml file `search_exceptions.ml` has syntax or type errors.



+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [6; -5; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [6; -3; -4; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [6; -3; -5; 2]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [2; -4; 2]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [2; -3; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [2; 6; -5; -4; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [2; 6; -3; -4; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```ocaml
   List.mem [2; 6; -3; -5]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   


  This test was not run because of an earlier failing test.

## Part 2. Continuations in the wolf-goat-cabbage problem.

+ Pass: Check that file `search_wolf.ml` exists.

+ Pass: Check that an OCaml file `search_wolf.ml` has no syntax  or type errors.

    OCaml file `search_wolf.ml` has no syntax or type errors.



+ Fail: 
Check that the result of evaluating
   ```ocaml
   List.mem 
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, L, R, R); (L, L, L, R); (R, R, L, R); (L, R, L, R); (R, R, R, R)]
     (crossing_all ())
   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
        Line 3, characters 6-18:
    3 |      (crossing_all ())) = (true) ;;
              ^^^^^^^^^^^^
    Error: Unbound value crossing_all
    Hint: Did you mean crossing_v1, crossing_v2 or crossing_v3?
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   List.mem 
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, R, R, L); (L, R, L, L); (R, R, L, R); (L, R, L, R); (R, R, R, R)]
     (crossing_all ())
   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
        Line 3, characters 6-18:
    3 |      (crossing_all ())) = (true) ;;
              ^^^^^^^^^^^^
    Error: Unbound value crossing_all
    Hint: Did you mean crossing_v1, crossing_v2 or crossing_v3?
    ```


## Part 3. Evaluating to 24

+ Fail: 
Check that the result of evaluating
   ```ocaml
   rat_simplify (eval (BinOp (Rat (4,1), Add, Rat (3,1) ) ) )
   ```
   matches the pattern `(7, 1)`.

   


   Test failed. The following errors were reported:

   ```
    Line 1, characters 1-13:
    1 | (rat_simplify (eval (BinOp (Rat (4,1), Add, Rat (3,1) ) ) )) = ((7, 1)) ;;
         ^^^^^^^^^^^^
    Error: Unbound value rat_simplify
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (3,1) ) ) )
   ```
   matches the pattern `(4, 3)`.

   


   Test failed. The following errors were reported:

   ```
    Line 1, characters 1-13:
    1 | (rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (3,1) ) ) )) = ((4, 3)) ;;
         ^^^^^^^^^^^^
    Error: Unbound value rat_simplify
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   try rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (0,1) ) ) ) with
    | EvalError DivByZero -> (99,99)
   ```
   matches the pattern `(99,99)`.

   


   Test failed. The following errors were reported:

   ```
      Line 1, characters 5-17:
    1 | (try rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (0,1) ) ) ) with
             ^^^^^^^^^^^^
    Error: Unbound value rat_simplify
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   find_expr [(8,1); (3,1); (8,1); (3,1)]
   ```
   matches the pattern `Some (BinOp (Rat (8, 1), Div, BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))`.

   


   Test failed. The following errors were reported:

   ```
    Line 1, characters 1-10:
    1 | (find_expr [(8,1); (3,1); (8,1); (3,1)]) = (Some (BinOp (Rat (8, 1), Div, BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))) ;;
         ^^^^^^^^^
    Error: Unbound value find_expr
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   match find_expr [(4,1); (2,1); (6,1); (3,1)] with
  | None -> false
  | Some e -> e = BinOp (Rat (4, 1), Add,
                         BinOp (Rat (2, 1), Add, 
                                BinOp (Rat (6, 1), Mul, Rat (3, 1))))
              ||
              e = BinOp (Rat (4, 1), Mul, 
                         BinOp (Rat (2, 1), Mul,
                                BinOp (Rat (6, 1), Sub, Rat (3, 1))))

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                      Line 1, characters 7-16:
    1 | (match find_expr [(4,1); (2,1); (6,1); (3,1)] with
               ^^^^^^^^^
    Error: Unbound value find_expr
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
   find_expr [(2,1); (3,1); (5,1); (7,1)]
   ```
   matches the pattern `None`.

   


   Test failed. The following errors were reported:

   ```
    Line 1, characters 1-10:
    1 | (find_expr [(2,1); (3,1); (5,1); (7,1)]) = (None) ;;
         ^^^^^^^^^
    Error: Unbound value find_expr
    ```


## Part 4. Optional Extra Credit, Evaluating to 24

+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for 8 / (3 - (8 / 3)) *)
      if e = BinOp (Rat (8, 1), Div,
                    BinOp (Rat (3, 1), Sub,
                           BinOp (Rat (8, 1), Div, Rat (3, 1))))
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(8,1); (3,1); (8,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                            Line 3, characters 13-18:
    3 |       if e = BinOp (Rat (8, 1), Div,
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for 4 + (2 + (6 * 3)) *)
      if e = BinOp (Rat (4, 1), Add,
                    BinOp (Rat (2, 1), Add,
                           BinOp (Rat (6, 1), Mul, Rat (3, 1))))
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                            Line 3, characters 13-18:
    3 |       if e = BinOp (Rat (4, 1), Add,
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for 4 * (2 * (6 - 3)) *)
      if e = BinOp (Rat (4, 1), Mul,
                    BinOp (Rat (2, 1), Mul,
                           BinOp (Rat (6, 1), Sub, Rat (3, 1))))
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                            Line 3, characters 13-18:
    3 |       if e = BinOp (Rat (4, 1), Mul,
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check that all solutions evaluate to 24 *)
      if rat_simplify (eval e) <> (24, 1)
      then false
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> true
    | Some _ -> false

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                        Line 3, characters 9-21:
    3 |       if rat_simplify (eval e) <> (24, 1)
                 ^^^^^^^^^^^^
    Error: Unbound value rat_simplify
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for 4 + (2! + (6 * 3)) *)
      if e = BinOp (Rat (4, 1), Add,
                    BinOp (Fac (Rat (2, 1)), Add, 
                           BinOp (Rat (6, 1), Mul, Rat (3, 1))));
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                            Line 3, characters 13-18:
    3 |       if e = BinOp (Rat (4, 1), Add,
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for 4! / (2! / (6 / 3)) *)
      if e = BinOp (Fac (Rat (4, 1)), Div,
                    BinOp (Fac (Rat (2, 1)), Div, 
                           BinOp (Rat (6, 1), Div, Rat (3, 1))))
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                            Line 3, characters 13-18:
    3 |       if e = BinOp (Fac (Rat (4, 1)), Div,
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for ((4 + 2) + (6 * 3)) *)
      if e = BinOp (
                 BinOp (Rat (4, 1), Add, Rat (2, 1)), 
                 Add,
                 BinOp (Rat (6, 1), Mul, Rat (3, 1))
               )
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                                Line 3, characters 13-18:
    3 |       if e = BinOp (
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


+ Fail: 
Check that the result of evaluating
   ```ocaml
    let succ e =
      (* check for ((4 * 2) * (6 - 3)) *)
      if e = BinOp (
                 BinOp (Rat (4, 1), Mul, Rat (2, 1)), 
                 Mul,
                 BinOp (Rat (6, 1), Sub, Rat (3, 1))
               )
      then true
      else raise KeepLooking
    in
    match find_expr_continuation succ 
            [(4,1); (2,1); (6,1); (3,1)] with
    | None -> false
    | Some _ -> true

   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:

   ```
                                Line 3, characters 13-18:
    3 |       if e = BinOp (
                     ^^^^^
    Error: Unbound constructor BinOp
    ```


