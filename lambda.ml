open Core.Std

module Syntax = struct
  type context = (string * Nat.t) list

  let empty_context = []
  let add_binding ctxt str =
    let ctxt = List.Assoc.map ctxt ~f:Nat.succ in
    List.Assoc.add ctxt str Nat.zero

  let name_to_index = List.Assoc.find_exn
end

module rec Type : sig
  type t =
    | Fun of t * t
    | Bool
  val to_string : t -> string
  val of_term : t list -> Term.t -> t option
end = struct
  type t =
    | Fun of t * t
    | Bool

  let rec to_string t = match t with
    | Fun (t1, t2) -> "(" ^ to_string t1 ^ " -> " ^ to_string t2 ^ ")"
    | Bool -> "Bool"

  let rec of_term ctxt t = match t with
    | Term.True -> Some Bool
    | Term.False -> Some Bool
    | Term.If (t1, t2, t3) ->
      let open Option in
      (of_term ctxt t1) >>= (function Bool ->
        (of_term ctxt t2) >>= (fun ty1 ->
          (of_term ctxt t3) >>= (fun ty2 ->
            if ty1 = ty2 then Some ty1 else None
          )
        )
      | _ -> None)
    | Term.Var n -> List.nth ctxt (Nat.to_int n)
    | Term.Abs (ty, t) ->
      let open Option in
      (of_term (ty :: ctxt) t) >>= (fun ty' -> Some (Fun (ty,ty')))
    | Term.App (t1, t2) ->
      let open Option in
      (of_term ctxt t1) >>= (function Fun (ty1, ty2) ->
        (of_term ctxt t2) >>= (fun ty1' ->
          if ty1 = ty1' then Some ty2 else None
        )
      | _ -> None)
end
and Term : sig
  type t =
    | True
    | False
    | If of t * t * t
    | Var of Nat.t
    | Abs of Type.t * t
    | App of t * t
  val to_string : t -> string
  val is_value : t -> bool
  val shift : int -> Nat.t -> t -> t
  val subst : Nat.t -> t -> t -> t
end = struct
  type t =
    | True
    | False
    | If of t * t * t
    | Var of Nat.t
    | Abs of Type.t * t
    | App of t * t

  let rec to_string t = match t with
    | True -> "True"
    | False -> "False"
    | If (t1, t2, t3) -> "if " ^ to_string t1 ^ " then " ^ to_string t2 ^ " else " ^ to_string t3
    | Var k -> string_of_int (Nat.to_int k)
    | Abs (ty, t) -> "(\\_ : " ^ Type.to_string ty ^ ". " ^ to_string t ^ ")"
    | App (t1, t2) -> "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"

  let is_value t = match t with
    | True -> true
    | False -> true
    | Abs _ -> true
    | _ -> false

  let rec shift d c t = match t with
    | True -> True
    | False -> False
    | If (t1, t2, t3) -> If (shift d c t1, shift d c t2, shift d c t3)
    | Var k -> Var (if k < c then k else (Nat.of_int (Nat.to_int k + d)))
    | Abs (ty, t) -> Abs (ty, shift d (Nat.succ c) t)
    | App (t1, t2) -> App (shift d c t1, shift d c t2)

  let rec subst j s t = match t with
    | True -> True
    | False -> False
    | If (t1, t2, t3) -> If (subst j s t1, subst j s t2, subst j s t3)
    | Var k -> if k = j then s else Var k
    | Abs (ty, t) -> Abs (ty, subst (Nat.succ j) (shift 1 Nat.zero s) t)
    | App (t1, t2) -> App (subst j s t1, subst j s t2)
end
