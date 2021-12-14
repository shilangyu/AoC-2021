open Core

let part1 l =
  let res =
    List.fold_left l ~init:(0, 0) ~f:(fun acc curr ->
        match String.split curr ~on:' ' with
        | [ cmd; amount ] -> (
            let amount = Int.of_string amount in
            match cmd with
            | "forward" -> (fst acc + amount, snd acc)
            | "down" -> (fst acc, snd acc + amount)
            | "up" -> (fst acc, snd acc - amount)
            | _ -> failwith "unrecognized command")
        | _ -> failwith "unrecognized format")
  in
  fst res * snd res

let part2 l =
  let res =
    List.fold_left l ~init:(0, 0, 0) ~f:(fun acc curr ->
        match String.split curr ~on:' ' with
        | [ cmd; amount ] -> (
            let amount = Int.of_string amount in
            match cmd with
            | "forward" ->
                (fst3 acc + amount, snd3 acc + (trd3 acc * amount), trd3 acc)
            | "down" -> (fst3 acc, snd3 acc, trd3 acc + amount)
            | "up" -> (fst3 acc, snd3 acc, trd3 acc - amount)
            | _ -> failwith "unrecognized command")
        | _ -> failwith "unrecognized format")
  in
  fst3 res * snd3 res

let run () =
  let input = In_channel.read_lines "inputs/day02.txt" in
  Printf.printf "Day 2:\n  Part 1: %d\n  Part 2: %d\n" (part1 input)
    (part2 input)
