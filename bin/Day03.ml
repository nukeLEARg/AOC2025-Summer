open Core
open Advent

type dir =
  | Up
  | Down
  | Left
  | Right

module Cordmap = Map.Make (Pos)

let interp (paths : string list) : (dir * int) list =
  List.map paths ~f:(fun s ->
    let dir =
      match s.[0] with
      | 'U' -> Up
      | 'D' -> Down
      | 'L' -> Left
      | 'R' -> Right
      | _ -> raise (Invalid_argument "bad string")
    in
    let len = String.sub s 1 (String.length s - 1) |> int_of_string in
    dir, len)
;;

let map_one_run
      (pos : int * int)
      ((dir, len) : dir * int)
      (curlen : int)
      (map : int Cordmap.t ref)
  : (int * int) * int
  =
  let v =
    match dir with
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0
  in
  let rec aux (nlen : int) (npos : int * int) (curlen : int) : (int * int) * int =
    if nlen = 0
    then npos, curlen
    else (
      let pos = add_tuples npos v in
      if not (Map.mem !map pos) then map := Map.add_exn !map ~key:pos ~data:curlen;
      aux (nlen - 1) pos (curlen + 1))
  in
  printf "";
  aux len pos curlen
;;

let map_wire (paths : (dir * int) list) (map : int Cordmap.t ref) : unit =
  let rec aux (pos : int * int) (paths : (dir * int) list) (curlen : int) =
    let npos, curlen = map_one_run pos (List.hd_exn paths) curlen map in
    if List.length paths = 1
    then ()
    else (
      let npaths = List.tl_exn paths in
      aux npos npaths curlen)
  in
  aux (0, 0) paths 1
;;

let () =
  let lines = read_lines "./inputs/d03/input.txt" in
  let wire1_map : int Cordmap.t ref = ref Cordmap.empty in
  let wire2_map : int Cordmap.t ref = ref Cordmap.empty in
  let wire1 = String.split_on_chars (List.nth_exn lines 0) ~on:[ ',' ] |> interp in
  let wire2 = String.split_on_chars (List.nth_exn lines 1) ~on:[ ',' ] |> interp in
  map_wire wire1 wire1_map;
  map_wire wire2 wire2_map;
  let crossings =
    Map.merge !wire1_map !wire2_map ~f:(fun ~key:_ -> function
      | `Both (w1, w2) -> Some (w1, w2)
      | _ -> None)
  in
  let res =
    Map.fold crossings ~init:Int.max_value ~f:(fun ~key:k ~data:_ acc ->
      let dist = man_dist k (0, 0) in
      if dist < acc then dist else acc)
  in
  let res2 =
    Map.fold crossings ~init:Int.max_value ~f:(fun ~key:_ ~data:(d1, d2) acc ->
      let dist = d1 + d2 in
      if dist < acc then dist else acc)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
