open Core
open Advent
open Intcode

let all_amp_settings_pt1 = permutations [ 0; 1; 2; 3; 4 ]
let all_amp_settings_pt2 = permutations [ 5; 6; 7; 8; 9 ]

let run_amp_chain (phases : int list) (omem : int array) : int * int list =
  let phase_len = List.length phases in
  let rec aux (i : int) (pout : int) : int * int list =
    if i >= phase_len
    then pout, phases
    else (
      let phase = List.nth_exn phases i in
      let state = Intcode.create_state omem in
      let state = Intcode.add_input phase state in
      let state = Intcode.add_input pout state in
      let state = Intcode.full_cycle state in
      match state.last_output with
      | Some nout -> aux (i + 1) nout
      | None -> failwith "No output from Intcode")
  in
  aux 0 0
;;

let run_feedback_chain (phases : int list) (omem : int array) : int =
  let states =
    Array.init 5 ~f:(fun i ->
      let state = Intcode.create_state omem in
      let phase = List.nth_exn phases i in
      Intcode.add_input phase state)
  in
  let last_output_from_e = ref 0 in
  let current_signal = ref 0 in
  let rec run_loop () =
    let any_running = ref false in
    for amp = 0 to 4 do
      let state = states.(amp) in
      if not state.halted
      then (
        any_running := true;
        states.(amp) <- Intcode.add_input !current_signal state;
        match Intcode.run_until_output_or_halt states.(amp) with
        | Some output ->
          current_signal := output;
          if amp = 4 then last_output_from_e := output
        | None -> ())
    done;
    if !any_running then run_loop ()
  in
  run_loop ();
  !last_output_from_e
;;

let () =
  let mem = read_lines "./inputs/d07/input.txt" |> List.hd_exn |> Intcode.mem_of_string in
  let pt1_results =
    List.map all_amp_settings_pt1 ~f:(fun perm -> run_amp_chain perm mem)
  in
  let res1, res1_phases =
    List.fold ~init:(0, []) pt1_results ~f:(fun (aout, aperm) (out, perm) ->
      if out > aout then out, perm else aout, aperm)
  in
  print_int_list res1_phases;
  let pt2_results =
    List.map all_amp_settings_pt2 ~f:(fun perm -> run_feedback_chain perm mem, perm)
  in
  let res2, res2_phases =
    List.fold ~init:(0, []) pt2_results ~f:(fun (aout, aperm) (out, perm) ->
      if out > aout then out, perm else aout, aperm)
  in
  print_int_list res2_phases;
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
