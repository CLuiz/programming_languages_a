fun sorted3_tuple (x, y, z) = z >= y andalso y >= x

val t1 = sorted3_tuple (7, 9, 11)

(* new way: currying *)
val sorted3 = fn x => fn y => fn z => z >=y andalso y >= x
(*fun sorted3 x = fn y => fn z => ...*)
val t2 = ((sorted3 7) 9) 11

(* can leave out parens when currying *)
val t3 = sorted3 7 9 11

fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11

fun fold f acc xs = (* means fun fold f = fn acc => fn xs => *)
    case xs
     of [] => acc
      | x :: xs' => fold f (f(acc, x)) xs'

(* a call to curried fold will improve this call next *)
fun sum xs = fold (fn (x, y) => x + y) 0 xs
		  

			 
