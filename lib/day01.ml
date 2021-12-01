open Core
open Utils

let rec part1 prev l =
  match l with
  | [] -> 0
  | head :: rest -> Bool.to_int (head > prev) + part1 head rest

let rec part2 prev l =
  match l with
  | h1 :: h2 :: h3 :: rest ->
      let sum = h1 + h2 + h3 in
      Bool.to_int (sum > prev) + part2 sum (h2 :: h3 :: rest)
  | _ -> 0

let run () =
  let input =
    In_channel.read_lines "inputs/day01.txt" |> List.map ~f:Int.of_string
  in
  Printf.printf "Day 1:\n  Part 1: %d\n  Part 2: %d\n"
    (part1 (unwrap (List.hd input)) input)
    (part2 (unwrap (List.hd input)) input)
