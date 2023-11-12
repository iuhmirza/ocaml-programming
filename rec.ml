(** [fact n] is the factorial of [n] *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)
let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)
