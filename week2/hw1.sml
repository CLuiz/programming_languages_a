fun is_older (dt1 : int*int*int, dt2 : int*int*int) =
    if (#1 dt1) < (#1 dt2)
    then true
    else if (#1 dt1) = (#1 dt2) andalso (#2 dt1) < (#2 dt2)
    then true
    else if (#1 dt1) = (#1 dt2) andalso	(#2 dt1) = (#2 dt2) andalso (#3 dt1) < (#3 dt2)
    then true
    else false;

fun number_in_month (dts : (int*int*int) list, m : int) =
    if null dts
    then 0
    else
	if #2 (hd dts) = m
	then 1 + number_in_month (tl dts, m)
	else number_in_month (tl dts, m);

fun number_in_months (dts : (int*int*int) list, mts : int list) =
    if null mts
    then 0
    else number_in_month(dts, (hd mts)) +
	 number_in_months(dts, (tl mts))

fun dates_in_month (dts : (int*int*int) list, m : int) =
    if null dts
    then []
    else if (#2 (hd dts)) = m
    then (hd dts) :: dates_in_month((tl dts), m)
    else dates_in_month((tl dts), m)
	     
fun dates_in_months (dts : (int*int*int) list, mts : int list) =
    if null mts
    then []
    else dates_in_month(dts, (hd mts)) :: dates_in_months(dts, (tl mts))
	    
fun get_nth (sts : string list, n : int) =
    let
	val i = n
    in
	if i > 1
	then get_nth((tl sts), i-1)
	else hd sts
    end
	
	    
	    
			 
