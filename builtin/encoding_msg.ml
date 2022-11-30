(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
(* Get the length of str;
   for each character in str :
   get its ascii code
   determine his binary encoding by recursive substraction by successive power of 2 *)
let encode str bits =
  let size = String.length str in
  let rec encoder i list =
    if i < 0 then
      list
    else
      let char = Char.code (str.[i]) in
      let rec chartobit c n =
        if n < 0 then list
        else
          if quot c (power 2 n) = 0 then
            0::(chartobit c (n-1))
          else
            1::(chartobit (c-(pow 2 n)) (n-1))
      in encoder (i-1) (chartobit char (bits-1))
  in let binaryMsg = encoder (size-1) [] in
     let rec integer x = function
      [] -> 0
    | e::l -> (power 2 x) * e + integer (x-1) l
  in integer (List.length binaryMsg -1) binaryMsg;;


(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec binarer n list = match n with
      0 -> list
    | _ -> binarer (n/2) ((n mod 2)::list)
  in
  let binaryMsg = binarer msg []
  in
  let rec integer x = function
      [] -> 0
    | e::l -> (power 2 x) * e + (integer (x+1) l)
  in
  let rec bits_to_string msg char list = match list with
      [] when List.length char = bits ->
       msg ^ String.make 1 (char_of_int (integer 0 char))
    | [] -> msg
    | h::q when List.length char = bits ->
       bits_to_string (msg ^ String.make 1 (char_of_int (integer 0 char))) [] (h::q)
    | h::q -> bits_to_string msg (h::char) q
  in bits_to_string "" [] binaryMsg;;

