(** Basic arithmetics with builtin integers *)

open Builtin;;
(* Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b =
  let (q, r) = div a b in
  if r = 0 then
    if a < 0 && b < 0 then -b
    else b
  else gcd b r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec bezouter (a, b) u v u_1 v_1 =
    if a mod b = 0 then (u, v, gcd a b)
    else
      let q = a/b in
      bezouter (b, a mod b) (u_1 - q*u) (v_1 - q*v) u v
  in bezouter (a, b) 0 1 1 0;;
