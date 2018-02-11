fun n_times (f, n, x) =
    if n = 0
    then x
    else (n_times(f, n-1, x))

(* don't do this: *)	     
fun nth(n, xs) = n_times ((fn y => tl y), n, xs)

(* do this *)
fun nth_tail(n, xs) = n_times(tl, n, xs)

(* don't rename functions *)
fun rev xs = List.rev xs
(* or this *)
val rev = fn xs => List.rev xs

(* do this to alias functions: *)
val rev = List.rev
	      
