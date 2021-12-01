let unwrap opt =
  match opt with
  | Some v -> v
  | None -> raise (Invalid_argument "Failed to unrwap")
