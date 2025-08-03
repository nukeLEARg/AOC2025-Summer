open Core
open Advent
open Intcode
module Cordmap = Map.Make (Pos)

type dir =
  | Up
  | Down
  | Left
  | Right

type bot =
  { pos : int * int
  ; dir : dir
  }

let match_dir : dir -> int * int = function
  | Up -> 0, -1
  | Down -> 0, 1
  | Left -> -1, 0
  | Right -> 1, 0
;;

let read_color (bot : bot) (map : bool Cordmap.t) : bool =
  match Map.find map bot.pos with
  | None -> false
  | Some b -> b
;;

let move_bot (bot : bot) (o : bool) : bot =
  let new_dir =
    match bot.dir, o with
    | Up, true -> Right
    | Up, false -> Left
    | Down, true -> Left
    | Down, false -> Right
    | Left, true -> Up
    | Left, false -> Down
    | Right, true -> Down
    | Right, false -> Up
  in
  let x, y = match_dir new_dir in
  { pos = fst bot.pos + x, snd bot.pos + y; dir = new_dir }
;;

let paint (bot : bot) (map : bool Cordmap.t) (o : bool) : bool Cordmap.t =
  Map.set map ~key:bot.pos ~data:o
;;

let rec cycle_bot bot map comp_state input_condition : bool Cordmap.t =
  match Intcode.run_to_pause_state comp_state with
  | PausedOutput output when input_condition ->
    let new_map = paint bot map (int_to_bool output) in
    cycle_bot bot new_map comp_state (not input_condition)
  | PausedOutput output ->
    let new_bot = move_bot bot (int_to_bool output) in
    cycle_bot new_bot map comp_state (not input_condition)
  | PausedInput ->
    cycle_bot
      bot
      map
      (Intcode.add_input (read_color bot map |> bool_to_int) comp_state)
      input_condition
  | PausedHalt -> map
;;

let () =
  let mem = read_lines "./inputs/d11/bot.txt" |> List.hd_exn |> Intcode.mem_of_string in
  let res1 =
    cycle_bot { pos = 0, 0; dir = Up } Cordmap.empty (Intcode.create_state mem) true
    |> Map.length
  in
  let r2map = Cordmap.empty |> Map.set ~key:(0, 0) ~data:true in
  let res2map =
    cycle_bot { pos = 0, 0; dir = Up } r2map (Intcode.create_state mem) true
  in
  Printf.printf "\nPart 1: %i\nPart 2: \n" res1;
  print_map res2map
;;
