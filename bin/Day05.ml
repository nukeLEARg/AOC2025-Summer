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
  Intcode.set_input 1;
  let _ = Intcode.run_cycle (Array.copy mem) in
  Intcode.set_input 5;
  let _ = Intcode.run_cycle (Array.copy mem) in
  Printf.printf "\n"
;;
