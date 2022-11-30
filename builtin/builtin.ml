(** Tweaking OCaml builtin euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign = function 
    x when x < 0 -> (-1)
  | _ -> 1;;

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)
let quot a b =
  if b = 0 then
    invalid_arg "Division by zero error"
  else
    let signe = (sign a * sign b) in
    let a = a * sign a and b = b*sign b in
    let rec quoter q a b =
      if a >= b then
        quoter (q+1) (a-b) b
      else
        if (signe == (-1)) && (a <> 0 && b <> 1) then
          signe * (q+1)
        else
          signe * q
    in quoter 0 a b;;

(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
*)

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    OCAML DEFAULT : For negative numbers euclidean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)
let modulo a b =
  a - b*(quot a b);;

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b =
  (quot a b, modulo a b);;
