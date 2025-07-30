open Core
open Advent
open Intcode

let () =
  let mem = read_lines "./inputs/d09/input.txt" |> List.hd_exn |> Intcode.mem_of_string in
  let res1_state =
    Intcode.create_state mem |> Intcode.add_input 1 |> Intcode.full_cycle
  in
  let res1 =
    match res1_state.last_output with
    | Some x -> x
    | None -> 0
  in
  let res2_state =
    Intcode.create_state mem |> Intcode.add_input 2 |> Intcode.full_cycle
  in
  let res2 =
    match res2_state.last_output with
    | Some x -> x
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
