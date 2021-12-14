open Core

let binary_to_int chars =
  let rec sum chars i =
    match chars with
    | [] -> 0
    | head :: rest -> (
        match head with
        | '0' -> sum rest (i - 1)
        | '1' -> Int.pow 2 i + sum rest (i - 1)
        | _ -> failwith "unexpected bit")
  in

  sum chars (List.length chars - 1)

let rec flip_bits = function
  | [] -> []
  | head :: rest ->
      (match head with
      | '0' -> '1'
      | '1' -> '0'
      | _ -> failwith "unexpected bit")
      :: flip_bits rest

let common_mask l =
  let counts = Array.create ~len:(List.length (List.hd_exn l)) 0 in

  l
  |> List.iter ~f:(fun curr ->
         List.iteri
           ~f:(fun i char ->
             counts.(i) <-
               (counts.(i)
               +
               match char with
               | '0' -> -1
               | '1' -> 1
               | _ -> failwith "unexpected bit"))
           curr);

  counts
  |> Array.map ~f:(function n when n < 0 -> '0' | _ -> '1')
  |> Array.to_list

let part1 l =
  let mask = common_mask l in

  (mask |> binary_to_int) * (mask |> flip_bits |> binary_to_int)

let part2 l =
  let matching_mask map =
    List.foldi (List.hd_exn l) ~init:l ~f:(fun i acc _ ->
        match acc with
        | [ _ ] -> acc
        | _ ->
            let mask = common_mask acc |> map in
            List.filter acc ~f:(fun ele ->
                Char.equal (List.nth_exn ele i) (List.nth_exn mask i)))
  in

  (List.hd_exn (matching_mask ident) |> binary_to_int)
  * (List.hd_exn (matching_mask flip_bits) |> binary_to_int)

let run () =
  let input =
    In_channel.read_lines "inputs/day03.txt" |> List.map ~f:String.to_list
  in
  Printf.printf "Day 3:\n  Part 1: %d\n  Part 2: %d\n" (part1 input)
    (part2 input)
