open Core
open Advent
open Intcode

let () =
  let mem =
    read_lines "./inputs/d02/input.txt"
    |> List.hd_exn
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:int_of_string
    |> List.to_array
  in
  let pt1_mem = Intcode.initialize mem 12 2 in
  let res =
    match Intcode.run_cycle pt1_mem with
    | Halted x -> x.(0)
    | _ -> 0
  in
  let res2 =
    match Intcode.find_inputs ~mem ~goal:19690720 with
    | Some (noun, verb) -> (100 * noun) + verb
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
