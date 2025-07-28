open Core
open Advent
open Intcode

let all_amp_settings = permutations [ 0; 1; 2; 3; 4 ]

let run_amp_chain (phases : int list) (omem : int array) : int * int list =
  let phase_len = List.length phases in
  let rec aux (i : int) (pout : int) : int * int list =
    if i >= phase_len
    then pout, phases
    else (
      let phase = List.nth_exn phases i in
      Intcode.set_input_l [ phase; pout ];
      match Intcode.run_cycle (Array.copy omem) with
      | Halted (_, nout) -> aux (i + 1) nout
      | _ -> raise (Invalid_argument "Bad result from Intcode comp"))
  in
  aux 0 0
;;

let () =
  let mem =
    read_lines "./inputs/d07/input.txt"
    |> List.hd_exn
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:int_of_string
    |> List.to_array
  in
  let pt1_results = List.map all_amp_settings ~f:(fun perm -> run_amp_chain perm mem) in
  let res1, res1_phases =
    List.fold ~init:(0, []) pt1_results ~f:(fun (aout, aperm) (out, perm) ->
      if out > aout then out, perm else aout, aperm)
  in
  print_int_list res1_phases;
  let res2 = 1 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
