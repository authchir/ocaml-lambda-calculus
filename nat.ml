type t = int
let zero = 0
let succ n = n + 1
let add = (+)
let of_int =
  let rec impl acc n = if n <= 0 then acc else impl (succ acc) (n - 1) in
  impl zero
let to_int n = n
