(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  if n = [] then [1]
  else
    let rec powerer res n =
      if 0::n >> [0;1] then
        powerer (mult_b res x) (diff_n n [1])
      else res
    in powerer x n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  if n = [] then [1]
  else
    let rec powerer res n =
      print_b n;
      print_newline();
      if n = [1] || n = [] || n = [0] then
        res
      else if mod_b (0::n) [0;0;1] = [] || mod_b (0::n) [0;0;1] = [0] ||  mod_b (0::n) [0;0;1] = [1]
      then powerer (pow res [0;1]) (quot_b n [0;0;1])
      else
        mult_b res  (powerer (pow res [0;1]) (quot_b (diff_n n [1]) [0;0;1]))
    in powerer x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec mod_power_rec x n m =
    let res = if n = [] then [1]
              else
                let a = mod_power_rec x (quot_b n [0;0;1]) m
                in
                 if mod_b n [0;0;1] = [] || mod_b n [0;0;1] = [0] || mod_b n [0;0;1] = [1]
                 then mod_b (mult_b a a) m
                 else mod_b (mult_b (mod_b (mult_b a a) m ) x) m
    in
    if x << [] && mod_b (0::n) [0;0;1] = [1] then add_b res m
    else res
  in mod_b (mod_power_rec x n m) m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if n = [1] then mod_b x n
  else
    if x = [] then []
    else
      let prime_mod_powerer x n =
        if n = [] then [1]
        else
          let reste = mod_b n (diff_n p [1]) in
          mod_power x reste p
      in prime_mod_powerer x n;;
