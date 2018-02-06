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
    else dates_in_month(dts, (hd mts)) @ dates_in_months(dts, (tl mts))
	    
fun get_nth (sts : string list, n : int) =
    let
	val i = n
    in
	if i > 1
	then get_nth((tl sts), i-1)
	else hd sts
    end

fun date_to_string (dt : int*int*int) =
    let
	val month_map = ["January ",  "February ", "March ", "April ",
			 "May ", "June ", "July ", "August ",
			 "September ", "October ", "November "]
    in
	get_nth(month_map, (#2 dt)) ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)
    end

fun number_before_reaching_sum (sum : int, ints : int list) =
    let
	fun total (ints : int list, cum_sum: int, idx: int) =
	    if (hd ints) + cum_sum >= sum
	    then idx
	    else total((tl ints), cum_sum + (hd ints), idx + 1)
    in
	total(ints, 0, 0)
    end

fun what_month(day : int) =
    let
	val days_per_month = [31, 28, 31, 30,
			      31, 30, 31, 31,
			      31, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_per_month) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
	     else what_month(day1) :: month_range(day1 + 1, day2)
		 
fun oldest (dts : (int*int*int) list) =
    if null dts
    then NONE
    else
	let
	    val ans = oldest(tl dts)
	in
	    if isSome ans andalso is_older(valOf(ans), (hd dts))
	    then ans
	    else SOME (hd dts)
	end


			    
			     
	    
	    
			 
