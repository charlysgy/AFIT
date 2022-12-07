(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n = 
  if 0::n <<= [0;0;1] then [[0;0;1]]
  else
    let rec lister x =
      if x >> 0::n then []
      else
        x::lister (add_b x [0;0;1])
    in [0;0;1]::lister [0;1;1];;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  if 0::n <<= [0;0;1] then [[0;0;1]]
  else let init_liste = init_eratosthenes n in
       let rec filter n = function
           [] -> []
         | x::fin -> if mod_b x n = [] then
                       filter n fin
                     else
                       x::filter n fin
       in let rec siever = function
              [] -> []
            | prime::fin -> prime::(siever (filter prime fin))
          in siever init_liste;;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = 
  let oc = open_out file in
    let rec aux = function
        [] -> close_out oc
      | e::l -> List.iter (Printf.fprintf oc "%d\n") e;
                aux l
    in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = 
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> let rec creater in_c=
                     match input_line_opt in_c with
                     | Some line -> (int_of_string line)::(creater in_c)
                     | None -> []
                   in
                   (creater in_c)::_create_list in_c
    | None -> []
  in
  _create_list in_c

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = 
  let in_c = open_in file in 
  create_list in_c;;

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime = 
  let rec finding (x, y) = function
      [] -> []
    | h::t -> if isprime y then
                if 0::y >> 0::limit then [(x, y)]
                else
                  (x, y)::finding (h, mult_b [0;0;1] (add_n h [1])) t
              else
                finding (h, mult_b [0;0;1] (add_n h [1])) t
  in finding ([], []) (eratosthenes limit);;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  
  let rec finding (x, y) = function
      [] -> []
    | h::t -> if isprime y then
                if 0::y > 0::limit then [(x, y)]
                else
                  (x, y)::finding (h, add_n h [0;1]) t
              else
                finding (h, add_n h [0;1]) t
  in finding ([], []) (eratosthenes  limit);;
