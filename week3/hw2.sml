(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2
	     
(* put your solutions for problem 1 here *)
		      
fun all_except_option(s, xs) =
    case xs
     of [] => NONE
      | x::xs  =>
	case (same_string(s,x), all_except_option(s, xs))
	 of (true, _) => SOME(xs)
	  | (false, NONE) => NONE
	  | (false, SOME(y)) => SOME(x::y)	

fun get_substitutions1 (strls : (string list) list, s : string) =
    case strls
     of [] => []
      | stl :: strls =>
	case all_except_option(s, stl)
	 of NONE => get_substitutions1(strls, s)
	  | SOME(x) => x @ get_substitutions1(strls, s)

fun get_substitutions2 (strls : (string list) list, s : string) =
    let
	fun sub_helper(lst, acc) =
	    case lst
	     of [] => acc
	      | x :: xs =>
		case all_except_option(s, x)
		 of NONE => sub_helper(xs, acc)
		  | SOME(y) => sub_helper(xs, acc @ y)
    in
	sub_helper(strls, [])
    end

fun similar_names (xss : string list list,
		   full_name : {first:string,
				middle:string,
				last:string}) =
    full_name
    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)

fun card_color (card) =
    case card of
	(Diamonds, _) => Red
     |  (Hearts, _) => Red
     | (_, _) => Black
		 
fun card_value (card) =
    case card of
	(_, Ace) => 11
      | (_, Num x) => x
      | _  => 10
		  
fun remove_card (cs, c, exp) =
    case cs
     of [] => raise exp
      | x :: cs => case x = c
		    of true => cs
		    | false => x :: remove_card(cs, c, exp) 
	
fun all_same_color (cs) =
    case cs of
	[] => true
      | x :: [] => true
      | x :: y :: xs => card_color(x) = card_color(y)
			andalso all_same_color(y :: xs) 

fun sum_cards (cs) =
    let fun f (cs, acc) =
	    case cs of
		[] => acc
	     |  i :: cs' => f(cs', card_value(i) + acc)
    in
	f(cs,0)
    end

fun score (cs : card list, goal : int) =
    let
	val sum = sum_cards(cs)		     
    in
	if all_same_color(cs)
	then sum div 2 
	else if sum > goal
	then (sum + goal) * 3
	else goal - sum	    
    end
	
	
