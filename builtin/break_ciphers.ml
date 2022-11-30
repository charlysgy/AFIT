(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem. (n, e)
 *)
let break key =
  let (n, e) = key in
  let rec sqrt s =
    if s*s = n then s
    else if s*s > n then (s-1)
    else sqrt (s+2)
  in
  let rec p_finder modulus =
      if modulo n modulus = 0 then modulus
      else p_finder (modulus - 1)
  in let p = p_finder (sqrt 3) in
     let q = n/p in (p, q);;
