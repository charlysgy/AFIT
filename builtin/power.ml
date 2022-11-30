(** Power function implementations for builtin integers *)

open Builtin;;
open Basic_arithmetics;;

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  if n < 0 then
    invalid_arg("N must be positive or null")
  else
    if n == 0 then 1
    else
      let rec powerer res n =
        if n > 1 then
          powerer (res * x) (n-1)
        else res
      in powerer x n;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n < 0 then
    0
  else
    if n = 0 then 1
    else
      let rec powerer res n =
        if n = 1 then
          res
        else if modulo n 2 = 0 then
          powerer (pow res 2) (n/2)
        else
          res * powerer (pow res 2) ((n-1)/2)
      in powerer x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec mod_power_rec x n m =
    let res = if n = 0 then 1
              else
                let a = mod_power_rec x (n/2) m
                 in
                 if n mod 2 = 0 then modulo (a*a) m
                 else modulo ((modulo (a*a) m ) * x) m
    in
    if x < 0 && n mod 2 = 1 then res + m
    else res
  in modulo (mod_power_rec x n m) m;;


(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if n < 0 then
    invalid_arg "N must be positive > 0"
  else
    if n = 1 then modulo x n
    else
      if x = 0 then 0
      else
        let prime_mod_powerer x n =
          if n = 0 then 1
          else
            let reste = modulo n (p-1) in
            mod_power x reste p
        in prime_mod_powerer x n;;

