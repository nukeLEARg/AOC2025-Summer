open Core
open Advent

let op1 (mem : int array) (inst_pt : int) : unit =
  let res = mem.(mem.(inst_pt + 1)) + mem.(mem.(inst_pt + 2)) in
  mem.(mem.(inst_pt + 3)) <- res
;;

let op2 (mem : int array) (inst_pt : int) : unit =
  let res = mem.(mem.(inst_pt + 1)) * mem.(mem.(inst_pt + 2)) in
  mem.(mem.(inst_pt + 3)) <- res
;;

let run_cycle (mem : int array) : int array option =
  let rec aux (inst_pt : int) : int array option =
    if inst_pt + 3 >= Array.length mem
    then None
    else (
      match mem.(inst_pt) with
      | 1 ->
        op1 mem inst_pt;
        aux (inst_pt + 4)
      | 2 ->
        op2 mem inst_pt;
        aux (inst_pt + 4)
      | 99 -> Some mem
      | _ -> None)
  in
  aux 0
;;

let restor_grav (mem : int array) : unit =
  mem.(1) <- 12;
  mem.(2) <- 2
;;

let restore_mem (mem : int array) (noun : int) (verb : int) : unit =
  mem.(1) <- noun;
  mem.(2) <- verb
;;

let cycler (omem : int array) (goal : int) =
  let result = ref None in
  for x = 0 to 9999 do
    let noun = x / 100 in
    let verb = x mod 100 in
    let nmem = Array.copy omem in
    restore_mem nmem noun verb;
    let rmem = run_cycle nmem in
    match rmem with
    | Some rmem -> if rmem.(0) = goal then result := Some x
    | None -> ()
  done;
  !result
;;

let () =
  let mem =
    read_lines "./inputs/d02/input.txt"
    |> List.hd_exn
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:int_of_string
    |> List.to_array
  in
  let pt1_mem = Array.copy mem in
  restor_grav pt1_mem;
  let res =
    match run_cycle pt1_mem with
    | Some x -> x.(0)
    | None -> 0
  in
  let res2 =
    match cycler mem 19690720 with
    | Some x -> x
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
