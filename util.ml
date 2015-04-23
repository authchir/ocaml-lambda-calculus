module Option = struct
  let map f o = match o with
    | None -> None
    | Some x -> Some (f x)

  let case n f o = match o with
    | None -> n
    | Some x -> f x
end
