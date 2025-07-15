open Core
open Advent

let count_letters (line : string) : int * int =
  let counts = Hashtbl.create (module Char) in
  String.iter line ~f:(fun c ->
    Hashtbl.update counts c ~f:(function
      | None -> 1
      | Some c -> c + 1));
  Hashtbl.fold counts ~init:(0, 0) ~f:(fun ~key:_ ~data:count (two, three) ->
    if count = 3 then two, 1 else if count = 2 then 1, three else two, three)
;;

let count_diffs (s1 : string) (s2 : string) : int =
  let l1 = String.to_list s1 in
  let l2 = String.to_list s2 in
  List.fold2_exn l1 l2 ~init:0 ~f:(fun diff c1 c2 ->
    if Char.equal c1 c2 then diff else diff + 1)
;;

let find_one_off (str : string) (lines : string list) : (string * string) option =
  let rec aux (sublines : string list) : (string * string) option =
    if List.is_empty sublines
    then None
    else (
      let nline = List.hd_exn sublines in
      if count_diffs str nline = 1 then Some (str, nline) else aux (List.tl_exn sublines))
  in
  aux lines
;;

let find_pair (lines : string list) : string * string =
  let searched =
    List.mapi lines ~f:(fun i s -> find_one_off s (List.drop lines (i + 1)))
  in
  let res =
    List.find searched ~f:(fun opt ->
      match opt with
      | None -> false
      | Some _ -> true)
  in
  match res with
  | None -> "", ""
  | Some ps ->
    (match ps with
     | None -> "", ""
     | Some s -> s)
;;

let remove_diff ((s1, s2) : string * string) : string =
  let l1 = String.to_list s1 in
  let l2 = String.to_list s2 in
  let mapped =
    List.map2_exn l1 l2 ~f:(fun c1 c2 -> if Char.equal c1 c2 then c1 else '-')
  in
  List.filter mapped ~f:(fun c -> not (Char.equal c '-')) |> String.of_char_list
;;

let () =
  let lines = read_lines "./inputs/2018/d02/bot.txt" in
  let counted = List.map lines ~f:count_letters in
  let two, three =
    List.fold counted ~init:(0, 0) ~f:(fun (two, three) (bTwo, bThree) ->
      two + bTwo, three + bThree)
  in
  let res = two * three in
  let res2 = find_pair lines |> remove_diff in
  Printf.printf "\nPart 1: %i\nPart 2: %s\n" res res2
;;
