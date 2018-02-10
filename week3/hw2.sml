(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2
	     
(* put your solutions for problem 1 here *)

fun all_except_option (s : string, xs : string list) =
    case xs of
	[] => NONE
      | x :: xs' => if same_string(s, x)
		   then SOME(xs)
		   else all_except_option(s, x::xs)
(*		      
fun all_except_option(str, []) = NONE
  | all_except_option(str, x::xs) =
    case (same_string(str,x), all_except_option(str, xs))
      of (true, _) => SOME(xs)
       | (false, NONE) => NONE
       | (false, SOME(y)) => SOME(x::y)	
*)

fun get_substitutions1 (strls : (string list) list, s : string) =
    strls
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)
(*	      
fun card_color (c) =
    case c of
	suit c => c
*)
