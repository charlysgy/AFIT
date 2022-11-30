(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = 
  let rec primer x =
    if power x 2 > n then true
    else
      if modulo n x = 0 then false
      else
        primer (x+2)
  in if modulo n 2 = 0 && n <> 2 then false
     else
       primer 3;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  if p <= 0 then
    invalid_arg "P must be positive > 0"
  else
    let rec pseudo_primer = function
        [] -> true
      | a::fin -> let res = mod_power a p p in
                  if res = a then pseudo_primer fin
                  else
                    if modulo a p = res then pseudo_primer fin
                  else false
    in pseudo_primer test_seq;;
