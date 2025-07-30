open Core
open Advent
open Intcode

let () =
  let mem =
    read_lines "./inputs/d05/input.txt"
    |> List.hd_exn
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:int_of_string
    |> List.to_array
  in
  let pt1_state = Intcode.add_input (Intcode.create_state (Array.copy mem)) 1 in
  let res1 =
    match (Intcode.full_cycle pt1_state).last_output with
    | Some x -> x
    | None -> 0
  in
  let pt2_state = Intcode.add_input (Intcode.create_state (Array.copy mem)) 5 in
  let res2 =
    match (Intcode.full_cycle pt2_state).last_output with
    | Some x -> x
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
