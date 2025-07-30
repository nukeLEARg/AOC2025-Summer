open Core
open Advent
open Intcode

let () =
  let mem = read_lines "./inputs/d02/input.txt" |> List.hd_exn |> Intcode.mem_of_string in
  let pt1_mem = Intcode.initialize mem 12 2 in
  let pt1_state = Intcode.create_state pt1_mem in
  let res = (Intcode.full_cycle pt1_state).memory.(0) in
  let pt2_state = Intcode.create_state mem in
  let res2 =
    match Intcode.find_inputs 19690720 pt2_state with
    | Some (noun, verb) -> (100 * noun) + verb
    | None -> failwith "No inputs found"
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
