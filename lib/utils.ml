let unwrap opt =
  match opt with
  | Some v -> v
  | None -> raise (Invalid_argument "Failed to unrwap")

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []
