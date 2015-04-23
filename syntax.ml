open Core.Std

type context = (string * Nat.t) list

let empty_context = []
let add_binding ctxt str =
  let ctxt = List.Assoc.map ctxt ~f:Nat.succ in
  List.Assoc.add ctxt str Nat.zero

let name_to_index = List.Assoc.find_exn

type t =
  | Var of Nat.t
  | Abs of t
  | App of t * t

let rec to_string t = match t with
  | Var k -> string_of_int (Nat.to_int k)
  | Abs t -> "(\\. " ^ to_string t ^ ")"
  | App (t1, t2) -> "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"

let is_value t = match t with
  | Abs _ -> true
  | _ -> false

let rec shift d c t = match t with
  | Var k -> Var (if k < c then k else (Nat.of_int (Nat.to_int k + d)))
  | Abs t -> Abs (shift d (Nat.succ c) t)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)

let rec subst j s t = match t with
  | Var k -> if k = j then s else Var k
  | Abs t -> Abs (subst (Nat.succ j) (shift 1 Nat.zero s) t)
  | App (t1, t2) -> App (subst j s t1, subst j s t2)
