print_endline (string_of_int (7 * (1 + 2 + 3)));;
"CS" ^ string_of_int 3110 |> print_endline;;

print_int (42 * 10);;
3.14 /. 2.0 |> string_of_float |> print_endline;;

let rec pow_helper x n acc = if n = 0 then acc else pow_helper x (n - 1) (x *. acc);;

let pow x n = pow_helper x n 1.;;

pow 4.2 7 |> string_of_float |> print_endline;;

42 = 42 |> string_of_bool |> print_endline;;

"hi" = "hi" |> string_of_bool |> print_endline;; 

"hi" == "hi" |> string_of_bool |> print_endline;;

assert (2210 <> 3310);;

(if 2 > 1 then 42 else 7) |> string_of_int |> print_endline;;

let double = fun x -> x + x;;

let print_int_endline n = string_of_int n |> print_endline;;

assert (double 10 = 20);;
assert (7 |> double = 14);;
assert ((-4 |> double) = -8);;
assert (double 0 = 0)

let cube_float x = x *. x *. x;;

let close x y = abs_float (x -. y) < 0.00001;;

assert (cube_float 3. |> close 27.);;
assert (cube_float 10. |> close 1000.);;
assert (cube_float (-7.) |> close (-343.));;
assert (cube_float (-7.4) |> close (-405.224));;

let sign_int n = if n = 0 then 0 else if n > 0 then 1 else -1;;

assert (sign_int 0 = 0);;
assert (sign_int (-213) = -1);;
assert (sign_int 32 = 1);;

let rms x y = sqrt ((x *. x +. y *. y)/. 2.);;

assert (rms 10. 3. |> close 7.38241153012)

let date m d =
  if (m = "Jan" || m = "Mar" || m = "May" || m = "July" || m = "Sept" || m = "Nov") && d >= 1 && d <= 30
    then true
  else if (m = "April" || m = "Jun" || m = "Aug" || m = "Oct" || m = "Dec") && d >= 1 && d <= 31
    then true
  else if (m = "Feb") && d >= 1 && d <= 28
    then true
  else false;;

let rec fib n = if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2);;

fib 1 |> print_int_endline;;
fib 2 |> print_int_endline;;
fib 3 |> print_int_endline;;
fib 5 |> print_int_endline;;
fib 10 |> print_int_endline;;

let rec fib_fast_helper n p1 p2 = if n = 3 then (p1 + p2) else fib_fast_helper (n - 1) p2 (p1 + p2);;

let fib_fast n = if n = 1 || n = 2 then 1 else fib_fast_helper n 1 1;;

fib_fast 1 |> print_int_endline;;
fib_fast 2 |> print_int_endline;;
fib_fast 3 |> print_int_endline;;
fib_fast 5 |> print_int_endline;;
fib_fast 10 |> print_int_endline;;
fib_fast 100 |> print_int_endline;;
fib_fast 500 |> print_int_endline;;

let rec first_negative_from_index f n = if f n < 0 then n else first_negative_from_index f (n + 1);;
first_negative_from_index fib_fast 1 |> print_int_endline;;

let divide ~numerator ~denominator = numerator /. denominator;;
divide ~numerator:10. ~denominator:3. |> string_of_float |> print_endline;;

let ( +/. ) x y = ((x +. y) /. 2.);;
print_endline (string_of_float (1. +/. 2.));;
print_endline (string_of_float (0. +/. 0.));;
