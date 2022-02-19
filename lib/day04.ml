open Core

type input_data = { draws : int list; boards : int list list list }

let part1 data = List.length data.draws

let part2 data = List.length data.boards

let run () =
  let parse_board =
    List.map ~f:(fun s ->
        String.split s ~on:' '
        |> List.filter ~f:(Fn.non String.is_empty)
        |> List.map ~f:Int.of_string)
  in

  let input =
    match In_channel.read_lines "inputs/day04.txt" with
    | draws :: _ :: boards ->
        {
          draws = String.split draws ~on:',' |> List.map ~f:Int.of_string;
          boards =
            List.chunks_of boards ~length:6
            |> List.map ~f:List.drop_last_exn
            |> List.map ~f:parse_board;
        }
    | _ -> failwith "Incorrect input data"
  in

  Printf.printf "Day 4:\n  Part 1: %d\n  Part 2: %d\n" (part1 input)
    (part2 input)
