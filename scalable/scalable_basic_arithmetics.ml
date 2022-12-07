(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let rec gcd_b bA bB =
  let (q, r) = div_b bA bB in
  if r = [] || r = [0] || r = [1] then
    if sign_b bA = (-1) && sign_b bB = (-1) then
      abs_b bB
    else bB
  else gcd_b bB r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let rec bezouter (a, b) u v u_1 v_1 =
    if mod_b a b = [] || mod_b a b = [1] || mod_b a b = [0] then (u, v, gcd_b a b)
    else
      let q = quot_b a b in
      bezouter (b, mod_b a b) (diff_b u_1 (mult_b q u)) (diff_b v_1 (mult_b q v)) u v
  in bezouter (bA, bB) [] [0;1] [0;1] [];;
