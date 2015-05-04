open Core.Std
open Lambda

let rec by_value1 t = match t with
  | Term.If (t1, t2, t3) -> (match t1 with
    | Term.True -> Some t2
    | Term.False -> Some t3
    | _ -> Option.map ~f:(fun t1' -> Term.If (t1', t2, t3)) (by_value1 t1))
  | Term.App (Term.Abs (_, t1), v2) when Term.is_value v2 ->
    Some
      (Term.shift (-1) Nat.zero
        (Term.subst Nat.zero
          (Term.shift 1 Nat.zero v2) t1))
  | Term.App (v1, t2) when Term.is_value v1 ->
    Option.map ~f:(fun t2' -> Term.App (v1, t2')) (by_value1 t2)
  | Term.App (t1, t2) ->
    Option.map ~f:(fun t1' -> Term.App (t1', t2)) (by_value1 t1)
  | _ -> None

let rec by_value t = Option.value_map (by_value1 t) ~default:t ~f:by_value
