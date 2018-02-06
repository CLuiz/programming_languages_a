funxx count (from : int, to : int) =
    if from=to
    then to :: []
    else from :: count(from+1, to);

fun countup_from1 (x : int) =
    let
	fun count (from : int) =
	    if from=x
	    then x :: []
	    else from :: count(from+1)
    in
	count (1, x)
    end
	
		      
