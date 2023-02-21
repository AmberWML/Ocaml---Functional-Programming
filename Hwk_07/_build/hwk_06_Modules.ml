open StreamModules
open LazeeModules

module type Hwk_06_Sig = sig
  type 'a stream

  val take: int -> 'a stream -> 'a list

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val facts: int stream
  val facts': int stream
  val primes: int stream
end


module Hwk_06(S: StreamSig) : Hwk_06_Sig = struct
   (* add elements here to complete the functor *)
   (* type 'a stream = Cons of 'a * 'a stream lazee  *)
   type 'a stream = 'a S.t
   let rec from n =
     S.Cons ( n, S.delay ( fun () -> from (n+1) ) )
   let nats = from 1

   let mul_p x y =
   let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
          in x * y
   let rec factorials () =
        S.Cons (1, S.delay (fun () -> S.zip mul_p nats (factorials ())))
   let multiple a b = b mod a <> 0
   let sift (n:int) (s:int stream): int stream = S.filter (multiple n) s
   let rec take (n: int) (s: 'a stream) : 'a list =
      match n with
      | 0 -> []
      | _ -> (match s with
              | S.Cons (h, t) -> h :: take (n-1) (S.demand t) )
   let rec sieve (s:int stream):int stream = 
      match s with
      | S.Cons(x,xs) -> S.Cons(x,S.delay (fun ( ) -> sieve( sift x (S.demand xs))))
   let rec from n =
      S.Cons ( n, S.delay ( fun () -> from (n+1) ) )
   
   let rec cubes_from (a:int):int stream = 
      S.Cons(a*a*a,S.delay (fun () -> cubes_from(a+1)))
   let cubes_from_zip (a:int):int stream =
      (* Cons(a*a*a,delay (fun () -> zip (fun x y -> x * y * y ) (from a+1) (from a+1)) )
      zip (square functions ) from a  from a -> zip again for cube *)
      (* zip (fun x y -> x * y * y ) (from a) (from a) *)
      S.zip(fun  x y -> x * y) (S.zip (fun x y -> x * y  ) (from a) (from a)) (from a)
   let cubes_from_map (a:int):int stream =
      (* Cons(a*a*a,delay (fun () -> map (fun a -> (a*a*a)) from a+1) ) *)
      S.map (fun a -> a*a*a) (from a)
  
   let facts = factorials ()
   let facts' =
      let dummy = ref nats
      in
      let muls = S.Cons (1, S.delay (fun () -> S.zip mul_p nats (! dummy)))
      in
      let () = dummy := muls
      in
      muls
    let primes = sieve (from 2)

end