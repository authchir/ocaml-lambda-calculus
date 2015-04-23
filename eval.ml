open Util

let rec by_value1 t = match t with
  | Syntax.App (Syntax.Abs t1, v2) when Syntax.is_value v2 ->
    Some
      (Syntax.shift (-1) Nat.zero
        (Syntax.subst Nat.zero
          (Syntax.shift 1 Nat.zero v2) t1))
  | Syntax.App (v1, t2) when Syntax.is_value v1 ->
    Option.map (fun t2' -> Syntax.App (v1, t2')) (by_value1 t2)
  | Syntax.App (t1, t2) ->
    Option.map (fun t1' -> Syntax.App (t1', t2)) (by_value1 t1)
  | _ -> None

let rec by_value t = Option.case t by_value (by_value1 t)
