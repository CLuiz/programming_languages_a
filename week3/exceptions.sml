fun hd xs =
    case xs of
	[] => raise List.Empty
     |  x :: _ => x

exception MyUndesireableCondition
exception MyOtherException of int * int

fun mydiv (x, y) =
	  if y = 0
	  then raise MyUndesireableCondition
	  else x div y

fun maxlist (xs, ex) = (* int list * exn -> int *)
    case xs of
	[] => raise ex
      | x :: [] => x
      | x :: xs' => Int.max(x, maxlist(xs', ex))
				      
val w = maxlist([3, 4, 5], MyUndesireableCondition)
val x = maxlist([3, 4, 5], MyUndesireableCondition)
 	handle MyUndesireableCondition => 42

(* val y = maxlist([], MyUndesireableCondition) *)

val z = maxlist([], MyUndesireableCondition)
	       handle MyUndesireableCondition => 42
