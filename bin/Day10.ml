open Core
open Advent
module Cordmap = Map.Make (Pos)

let parse_map (lines : string list) : (int * int) list =
  List.foldi lines ~init:[] ~f:(fun y acc line ->
    String.foldi line ~init:acc ~f:(fun x acc char ->
      if Char.equal char '#' then (x, y) :: acc else acc))
  |> List.rev
;;

let group_asteroids_by_dir (sx, sy) (asteroids : (int * int) list) =
  let grouped =
    List.fold asteroids ~init:Cordmap.empty ~f:(fun acc (ax, ay) ->
      if not (ax = sx && ay = sy)
      then (
        let dx = ax - sx in
        let dy = ay - sy in
        let reduced = reduce_vector (dx, dy) in
        let distance2 = (dx * dx) + (dy * dy) in
        Map.update acc reduced ~f:(function
          | None -> [ ax, ay, distance2 ]
          | Some existing -> (ax, ay, distance2) :: existing))
      else acc)
  in
  Map.mapi grouped ~f:(fun ~key:_ ~data:asteroids ->
    List.sort asteroids ~compare:(fun (_, _, d1) (_, _, d2) -> Int.compare d1 d2)
    |> List.map ~f:(fun (x, y, _) -> x, y))
;;

let find_best_location (asteroids : (int * int) list) : (int * int) * int =
  List.fold
    asteroids
    ~init:((0, 0), 0)
    ~f:(fun ((best_x, best_y), best_count) pos ->
      let count = group_asteroids_by_dir pos asteroids |> Map.length in
      if count > best_count then pos, count else (best_x, best_y), best_count)
;;

let find_laser_angle (x, y) : float =
  let angle_rads = Float.atan2 (Float.of_int x) (Float.of_int (-y)) in
  let angle_norm =
    if Float.(angle_rads < 0.0) then angle_rads +. (2.0 *. Float.pi) else angle_rads
  in
  angle_norm
;;

let find_vaporized_order (sx, sy) (asteroids : (int * int) list) : (int * int) list =
  let asteroids_grouped_sorted : (int * int) list ref list =
    group_asteroids_by_dir (sx, sy) asteroids
    |> Map.to_alist
    |> List.map ~f:(fun ((dx, dy), asteasteroid_list) ->
      let angle = find_laser_angle (dx, dy) in
      angle, ref asteasteroid_list)
    |> List.sort ~compare:(fun (a1, _) (a2, _) -> Float.compare a1 a2)
    |> List.map ~f:(fun (_, asteroids) -> asteroids)
  in
  let vaporized = ref [] in
  let rec aux () =
    let laser_hit = ref false in
    List.iter asteroids_grouped_sorted ~f:(fun asteroids ->
      match !asteroids with
      | [] -> ()
      | (x, y) :: rest ->
        vaporized := (x, y) :: !vaporized;
        asteroids := rest;
        laser_hit := true);
    if !laser_hit then aux ()
  in
  aux ();
  List.rev !vaporized
;;

let () =
  let asteroids = read_lines "./inputs/d10/input.txt" |> parse_map in
  let station_pos, res1 = find_best_location asteroids in
  let vaporized = find_vaporized_order station_pos asteroids in
  let v200x, v200y = List.nth_exn vaporized (200 - 1) in
  let res2 = (v200x * 100) + v200y in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res1 res2
;;
