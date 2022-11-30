(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  if n <= 2 then [2]
  else
    let rec lister x =
      if x > n then []
      else
        x::lister (x+2)
    in 2::lister 3;;


(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  if n <= 2 then [2]
  else let init_liste = init_eratosthenes n in
       let rec filter n = function
           [] -> []
         | x::fin -> if modulo x n = 0 then
                       filter n fin
                     else
                       x::filter n fin
       in let rec siever = function
              [] -> []
            | prime::fin -> prime::(siever (filter prime fin))
          in siever init_liste;;


(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
    let rec aux = function
        [] -> close_out oc
      | e::l -> Printf.fprintf oc "%d\n" e;
                aux l
    in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
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

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let in_c = open_in file in 
  create_list in_c;;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec finding (x, y) = function
      [] -> []
    | h::t -> if isprime y then
                if y > limit then [(x, y)]
                else
                  (x, y)::finding (h, 2*h+1) t
              else
                finding (h, 2*h+1) t
  in finding (0, 0) (eratosthenes limit);;


(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec finding (x, y) = function
      [] -> []
    | h::t -> if isprime y then
                if y > limit then [(x, y)]
                else
                  (x, y)::finding (h, h+2) t
              else
                finding (h, h+2) t
  in finding (0, 0) (eratosthenes limit);;
