open Core
open Advent

let dupeHash (nums : int list) : int =
  let pFreqs = Hashtbl.create (module Int) in
  Hashtbl.set pFreqs ~key:0 ~data:1;
  let rec aux (onums : int list) (nums : int list) (freq : int) : int =
    if List.is_empty nums
    then aux onums onums freq
    else (
      let nFreq = freq + List.hd_exn nums in
      if Hashtbl.mem pFreqs nFreq
      then nFreq
      else (
        Hashtbl.set pFreqs ~key:nFreq ~data:1;
        aux onums (List.tl_exn nums) nFreq))
  in
  aux nums nums 0
;;

let () =
  let lines = read_lines "./inputs/2018/d01/input.txt" |> List.map ~f:int_of_string in
  let res = List.fold lines ~init:0 ~f:( + ) in
  let res2 = dupeHash lines in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
