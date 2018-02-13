fun fold (f, acc, xs) =
    case xs
     of [] => []
     | x :: xs' => fold (f, f(acc, x), xs') (* this is a "fold left" 
					       if order mattes can also "fold right" *) 

			(* examples not using private data *)
fun f1 xs = fold ((fn (x, y)) => x + y), 0, xs) (* sum list *)
fun f2 xs = fold ((fn (x, y) => x andalso y >= 0), true, xs) (* are all list elements non-negative*)

(* examples using private data *)

(* counting number of elements between lo and hi *)
fun f3 (xs, lo, hi) =
    fold ((fn (x, y) =>
	      x + (fi y >- lo andalso v <= hi then 1 else 0)),
	  0, xs)

(* are all elements of the list less than size i *)
fun f4 (xs, s) =
    let
	val i = String.size s
    in
	fold((fn (x, y) => x andalso String.size y < i), true, xs)
    end
	
fun f5 (g, xs) = fold((fn (x, y) => x andalso g y), true, xs)

fun f4again (xs, s) =
    let
	val i = String.size s
    in
	f5(fn y => String.size y < i, xs)
    end
	
