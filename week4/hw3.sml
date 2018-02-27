(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (xs) =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs
				     
val tl1 = ["Dod", "snacks", "Sod", "foo", "Bar"]
val tl2 = ["Dod", "Sod", "foo", "Bar"]
fun longest_string1 (xs) =
    foldl (fn (s, x) => if String.size s > String.size x then s else x) "" xs 

fun longest_string2 (xs) =
    foldl (fn (s, x) => if String.size s >= String.size x then s else x) "" xs

fun longest_string_helper f xs =
    foldl (fn(x, y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3  =
    longest_string_helper (fn(x, y) => x > y) 

val longest_string4  =
    longest_string_helper (fn(x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val tl3 = tl1 @ ["BLAAAH"]

val rev_string  = implode o rev o explode

fun first_answer f xs =
    case xs
     of [] => raise NoAnswer
      | x::xs' => case f(x)
		   of NONE => first_answer f xs'
		    | SOME v => v

				    
fun all_answers f xss =
    let fun answer_helper (xs, acc) =
	    case xs
	     of [] => SOME acc
	      | x :: xs' => case f x
			     of NONE => NONE
			      | SOME(y) => answer_helper(xs', acc @ y)
    in
	answer_helper(xss, []) 
    end

(*
(* fun g provided for use in part 2 of hw *)
fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
*)

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn y => String.size y)

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x=s then 1 else 0) p

fun check_pat p =
    let
	fun get_vars (p) =
	    case p
	     of Variable x => [x]
	      | TupleP ps => List.foldl (fn (p', acc) => acc @ get_vars(p')) [] ps
	      | (_) => []
	fun has_duplicates (xs) =
	    case xs
	     of [] => false
	      | x :: xs' => List.exists (fn y => x=y) xs' orelse has_duplicates xs'
    in
	(not o has_duplicates o get_vars) p
    end
(*
fun match vp =
    case vp
     of (_, Wildcard) => SOME []
      | (v, Variable s) => [(s, v)] 
      | _ => NONE
*)
fun check_pat3 (p) = 
    let fun list_vars (Variable x) = [x]
	  | list_vars (TupleP ps) = List.foldl (fn (p', acc) => acc @ list_vars(p')) [] ps
	  | list_vars (_) = []
	fun has_repeats ([]) = false
	  | has_repeats (x::xs) = List.exists (fn x' => x = x') xs orelse has_repeats xs
    in
	(not o has_repeats o list_vars) p
    end	
