open Core
open Advent
open Intcode

let () =
  let mem = read_lines "./inputs/d05/input.txt" |> List.hd_exn |> Intcode.mem_of_string in
  let pt1_state = Intcode.create_state (Array.copy mem) |> Intcode.add_input 1 in
  let res1 =
    match (Intcode.full_cycle pt1_state).last_output with
    | Some x -> x
    | None -> 0
  in
  let pt2_state = Intcode.create_state (Array.copy mem) |> Intcode.add_input 5 in
  let res2 =
    match (Intcode.full_cycle pt2_state).last_output with
    | Some x -> x
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
