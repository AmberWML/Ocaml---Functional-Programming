# Quiz_1 Assessment

Run on October 21, 10:54:59 AM.

*Test results are for the code in this repository as it appeared on October 07 at 17:15 CT.*

+ Pass: Change into directory "Quiz_1".

+ Pass: Check that file "quiz_1.ml" exists.

### 1. ``count_evens``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   count_evens []
   ```
   matches the pattern `0`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   count_evens [11;3;7]
   ```
   matches the pattern `0`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   count_evens [10;3;8]
   ```
   matches the pattern `2`.

   




+  _2_ / _2_ : Pass: count_evens: correct function description.

    

+  _3_ / _3_ : Pass: count_evens: listing at least 2 design options they had to choose from.

    

### 2. `count` and `count_evens_2`

+  _4_ / _4_ : Pass: count: returns 0 when given an empty list (0pts if no function passed to `count` as input)

    

+  _6_ / _6_ : Pass: count: returns 2 when given the list `[1;3;5]` and `(fun x -> x > 2)` (0pts if no function passed to `count` as input)

    

+  _0_ / _3_ : Pass: count: 1pt for each type annotation

    No type annotations

+  _2_ / _2_ : Pass: count: reasonable name for function passed into `count`.

    

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   count_evens_2 []
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   count_evens_2 [10;3;8]
   ```
   matches the pattern `2`.

   




+  _2_ / _2_ : Pass: count_evens_2: calls `count` function

    

+  _1_ / _1_ : Pass: count_evens_2: passes `even` to `count`

    

### 3. `is_square` and `count_squares`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   is_square 9
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   is_square 8
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: is_square: doesn't use rec keyword for is_square

    

+  _3_ / _3_ : Pass: is_square: uses helper function to recurse over n.

    

+  _2_ / _2_ : Pass: is_square: has reasonable indentation (-1pt per "error")

    

+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   count_squares [2;4;5;9]
   ```
   matches the pattern `2`.

   


   Test failed. The following errors were reported:

```
 ;;
Line 1, characters 1-14:
1 | (count_squares [2;4;5;9]) = (2) ;; ;;
     ^^^^^^^^^^^^^
Error: Unbound value count_squares

```


+  _0_ / _1_ : Pass: count_squares: calls `count` with `is_square` as input

    Function not completed

### 4. ``subtract_lst``

+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   subtract_lst []
   ```
   matches the pattern `0`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   subtract_lst [10]
   ```
   matches the pattern `10`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   subtract_lst [10;3]
   ```
   matches the pattern `7`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   subtract_lst [10;3;2]
   ```
   matches the pattern `5`.

   




+  _2_ / _2_ : Pass: subtract_lst: uses call to ``List.fold_left``

    

+  _2_ / _2_ : Pass: subtract_lst: uses first element of list as initial value of accumulator

    

+  _2_ / _2_ : Pass: subtract_lst: if list is empty, prevents call to `List.fold_left` and returns 0

    

+  _2_ / _2_ : Pass: subtract_lst: has reasonable indentation (-1pt per "error")

    

+  _6_ / _6_ : Pass: subtract_lst: fold_left explanation (answer should mention that it'll operate on the list in a left associative way)

    

### 5. ``suml``

+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   suml []
   ```
   matches the pattern `0`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   suml [1;2;4]
   ```
   matches the pattern `7`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   suml [0;2;4;0;8]
   ```
   matches the pattern `14`.

   




+  _1_ / _1_ : Pass: suml: doesn't use rec keyword for suml

    

+  _2_ / _2_ : Pass: suml: calls recursive helper function

    

+  _3_ / _3_ : Pass: suml: for implementation correctly adding up elements from left to right.

    

+  _3_ / _3_ : Pass: suml: has reasonable indentation (-1pt per "error")

    

+  _8_ / _9_ : Pass: suml evaluation: shows correct evaluation order (0 + 1 -> 1 + 2 -> 3 + 4) (3pts each)

    3 is not an element of the test list

+  _3_ / _3_ : Pass: suml evaluation: for other aspects of the trace being mostly correct

    

#### Total score: _94_ / _100_

