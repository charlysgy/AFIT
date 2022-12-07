(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec int_to_bitarray  int =
    if int = 1 then [1]
    else if 2*(int/2) = int then
      0::int_to_bitarray (int/2)
    else
      1::int_to_bitarray  (int/2)
  in if x<0 then 1::(int_to_bitarray (-x))
     else 0::(int_to_bitarray x);;


(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  let rec bitarray_to_int n = function
      [] -> 0
    | e::l when e = 1 -> n+(bitarray_to_int (n*2) l)
    | e::l -> bitarray_to_int (n*2) l
  in match bA with
       [] -> 0
     | h::q -> if h = 0 then
                 bitarray_to_int 1 q
               else
                 -(bitarray_to_int 1 q);;


(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec printer = function
    [] -> ()
    | e::l -> printer l;
              print_int e
  in match bA with
       [] -> ()
     | h::q -> printer q;;


(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let compare_n nA nB =
  if List.length nA < List.length nB then (-1) else
    if List.length nA > List.length nB then 1 else
      let nA = List.rev(nA) and nB = List.rev(nB) in
      let rec compare_rec nA nB = match (nA, nB) with
          ([], []) -> -1
        | ([], _ ) -> -1
        | (_, [] ) ->  1
        | e1::[], e2::[] -> if e1 > e2 then 1
                            else -1
        | e1::l1, e2::l2 -> if e1 > e2 then 1
                            else if e2 > e1 then -1
                            else compare_rec l1 l2
      in compare_rec nA nB;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
  if compare_n nA nB = 1 then true
  else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
  if compare_n nA nB = -1 then true
  else false

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  if nA = nB then true
  else nA >>! nB;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
  if nA = nB then true
  else nA <<! nB;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)

let compare_b bA bB = match (bA,bB) with
  |([],e2::bB) -> if e2 = 1 then 1 else (-1)
  |(e1::bA,[]) -> if e1 = 1 then (-1) else 1
  |(e1::bA,e2::bB) when e1 != e2 -> if e1 < e2 then 1 else (-1)
  |(e1::bA,e2::bB) when e1 = 1 && e2 = 1 -> if compare_n bA bB = 1
                                            then (-1)
                                            else
                                              if compare_n bA bB= (-1)
                                              then 1
                                              else 0
  |(e1::bA,e2::bB) when e1 = 0 && e2 = 0 -> compare_n bA bB
  |_ -> 0;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  if compare_b bA bB = 1 then true
  else false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
  if compare_b bA bB = -1 then true
  else false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
  if bA = bB then true
  else bA >> bB;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  if bA = bB then true
  else bA << bB;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
    [] -> 1
  | e::l when e = 1 -> -1
  | _ -> 1

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
    [] -> []
  | h::q -> if h = 1 then
              0::q
            else h::q;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a =
  if a < 2 then 0
  else 1;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a =
  if a = 1 || a = 3 then 1
  else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a =
  (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
 *)

let remove_0 bA =
  let bA = List.rev bA
  in let rec remover = function
       |[] -> []
       |e::l when e = 1 -> e::l
       |e::l -> remover l
     in List.rev (remover bA);;
  
let add_n nA nB =
  let rec adder retenu = function
      ([], []) -> if retenu = 1 then [1] else []
    | ([], l ) -> if retenu = 1 then adder 0 ([1], l) else l
    | (l, [] ) -> if retenu = 1 then adder 0 (l, [1]) else l
    | (e1::l1, e2::l2) -> if e1 = 1 && e2 = 1 && retenu = 1 then 1::adder 1 (l1, l2)
                          else if e1 = 1 && e2 = 1 then 0::adder 1 (l1, l2)
                          else if (e1 = 1 || e2 = 1) && retenu = 1 then 0::adder 1 (l1, l2)
                          else if (e1 = 1 || e2 = 1) then 1::adder 0 (l1, l2)
                          else 0::adder 0 (l1, l2)
  in adder 0 (nA, nB);;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec suber retenu = function
      ([], []) -> []
    | (l, [])  -> suber retenu (l, [0])
    | ([], l) -> []
    | (e1::[],e2::[]) -> if e1 =1 && e2 = 0 && retenu = 0 then e1::[] else []
    | (e1::l1, e2::l2) -> match (e1, e2, retenu) with
                            (1, 1, 1) | (0, 0, 1) | (0, 1, 0) -> 1::suber 1 (l1, l2)
                            | (1, 1, 0) -> 0::suber 0 (l1, l2)
                            | (1, 0, 1) -> 0::suber 0 (l1, l2)
                            | (1, 0, 0) -> 1::suber 0 (l1, l2)
                            | (0, 0, 0) -> 0::suber 0 (l1, l2)
                            | _ -> 0::suber 1 (l1, l2)
  in suber 0 (nA, nB);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = remove_0(match (bA, bB) with
    ([], []) -> []
  | (e1::l1, e2::l2) when e1 = 0 && e2 = 0 -> 0::add_n l1 l2
  | (e1::l1, e2::l2) when e1 = 1 && e2 = 1 -> 1::add_n l1 l2
  | (e1::l1, e2::l2) when e1 = 1 && e2 = 0 -> if compare_n l1 l2 = 1 then 1::diff_n l1 l2
                                            else if compare_n l1 l2 = (-1) then 0::diff_n l1 l2
                                            else []
  |(e1::l1,e2::l2) when e1 = 0 && e2 = 1 -> if compare_n l1 l2 = 1 then 0::diff_n l1 l2
                                            else if compare_n l1 l2 = (-1) then 1::diff_n l1 l2
                                            else []
  |(l,[]) -> l
  |([],l) -> l
  |_ -> []);;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)

let diff_b bA bB = remove_0(match (bA,bB) with
  |([],[]) -> []
  |([],e2::l2) when e2 = 0 -> 1::l2
  |([],e2::l2) when e2 = 1 -> 0::l2
  |(e1::l1,[]) -> e1::l1
  |(e1::l1,e2::l2) -> begin
      match (e1, e2) with
        (0, 0) -> if compare_n l1 l2 = 1 then 0::diff_n l1 l2
                  else if compare_n l1 l2 = 0 then []
                  else 1::diff_n l1 l2
      | (1, 1) -> add_b (e1::l1) (0::l2)
      | (1, 0) -> 1::add_n l1 l2
      | (0, 1) -> 0::add_n l1 l2
      | _ -> []
    end
  |_ -> []);;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
    if d = 0 then bA
    else match bA with
           [] -> []
          | h::q -> match d with
                           0 -> h::q
                         | n -> shift (h::(0::q)) (n-1)

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let rec multiplier res bitarray = function
      [] -> res
    | e::q -> if e = 1 then
                multiplier (add_n res bitarray) (0::bitarray) q
              else multiplier res (0::bitarray) q
  in match (bA, bB) with
       ([], []) | ([], _) | (_, []) -> []
       | e1::l1, e2::l2 -> if e1 = 0 && e2 = 0 || e1 = 1 && e2 = 1 then
                             0::multiplier [] l1 l2
                           else
                             1::multiplier [] l1 l2;;

let rec mult_n nA nB =
  match (nA,nB) with
  |(l1,[]) -> []
  |(l1,e::l2) when e = 1 -> add_n l1 (mult_n (0::l1) l2)
  |(l1,e::l2) when e = 0 -> mult_n (0::l1) l2
  |_ -> [];;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let signe = (sign_b bA) * (sign_b bB) in
  let bA = abs_b bA and bB = abs_b bB in
  let rec quoter q a b =
    if a >>= b then
      quoter (add_n q [1]) (diff_n a b) b
    else if signe == (-1) && ( a <> [] && (b <> [0; 1] || b <> [1])) then
      if signe = (-1) then
        1::(add_n q [1])
      else
        0::(add_n q [1])
    else
      if signe = (-1) then
        1::q
      else
        0::q
  in quoter [] bA bB;;

let quot_b bA bB =
  let signe = (sign_b bA)*(sign_b bB) in
  let bA = abs_b bA and bB = abs_b bB in
  let rec quoter q a b =
    if a >>= b then
      quoter (add_b q [0;1]) (diff_b a b) b
    else
      if signe = (-1) && (a <> []  && b <> [0; 1]) then
          mult_b (from_int signe) (add_b q [0;1])
      else
        mult_b (from_int signe) q
  in quoter [0] bA bB;;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  if bA = (mult_b bB (quot_b bA bB)) then []
  else
    diff_b bA (mult_b bB (quot_b bA bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB);;
