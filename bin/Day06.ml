open Core
open Advent
module Stringmap = Map.Make (String)

let build_map (orbit_pairs : (string * string) list) (map : string Stringmap.t ref) : unit
  =
  List.iter orbit_pairs ~f:(fun (parent, child) ->
    map := Map.add_exn !map ~key:child ~data:parent)
;;

let count_orbits (map : string Stringmap.t ref) (obj : string) : int =
  let rec aux (o : string) : int =
    match Map.find !map o with
    | None -> 0
    | Some parent -> 1 + aux parent
  in
  aux obj
;;

let rec find_full_path (orbit_map : string Stringmap.t ref) (start : string) : string list
  =
  match Map.find !orbit_map start with
  | None -> [ start ]
  | Some parent -> start :: find_full_path orbit_map parent
;;

let find_path_split (path1 : string list) (path2 : string list)
  : string list * string list
  =
  let rec aux (p1 : string list) (p2 : string list) (prev_parent : string)
    : string list * string list
    =
    match p1, p2 with
    | h1 :: t1, h2 :: t2 when String.equal h1 h2 -> aux t1 t2 h1
    | _, _ -> prev_parent :: List.rev p1, prev_parent :: List.rev p2
  in
  aux (List.rev path1) (List.rev path2) "COM"
;;

let count_transfers (orbit_map : string Stringmap.t ref) (you : string) (santa : string)
  : int
  =
  let you_to_com = find_full_path orbit_map you in
  let santa_to_com = find_full_path orbit_map santa in
  let you_to_split, santa_to_split = find_path_split you_to_com santa_to_com in
  List.length you_to_split - 1 + (List.length santa_to_split - 1)
;;

let () =
  let orbit_pairs =
    read_lines "./inputs/d06/input.txt"
    |> List.map ~f:(fun s ->
      match String.split ~on:')' s with
      | [ parent; child ] -> parent, child
      | _ -> raise (Invalid_argument "bad string"))
  in
  let orbit_map : string Stringmap.t ref = ref Stringmap.empty in
  build_map orbit_pairs orbit_map;
  let object_list = Map.keys !orbit_map in
  let res =
    List.fold object_list ~init:0 ~f:(fun acc obj -> acc + count_orbits orbit_map obj)
  in
  let you_orbit =
    match Map.find !orbit_map "YOU" with
    | Some x -> x
    | None -> raise (Invalid_argument "no YOU")
  in
  let santa_orbit =
    match Map.find !orbit_map "SAN" with
    | Some x -> x
    | None -> raise (Invalid_argument "no SAN")
  in
  let transfers = count_transfers orbit_map you_orbit santa_orbit in
  let res2 = transfers in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
