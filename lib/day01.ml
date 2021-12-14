open Core

let rec part1 prev l =
  match l with
  | [] -> 0
  | head :: rest -> Bool.to_int (head > prev) + part1 head rest

let rec part2 prev l =
  match l with
  | h1 :: h2 :: h3 :: rest ->
      Bool.to_int (h3 > prev) + part2 h1 (h2 :: h3 :: rest)
  | _ -> 0

let run () =
  let input =
    In_channel.read_lines "inputs/day01.txt" |> List.map ~f:Int.of_string
  in
  Printf.printf "Day 1:\n  Part 1: %d\n  Part 2: %d\n"
    (part1 (List.hd_exn input) input)
    (part2 (List.nth_exn input 2) input)
