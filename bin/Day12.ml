open Core
open Advent

type vec3 =
  { x : int
  ; y : int
  ; z : int
  }

type moon =
  { pos : vec3
  ; vel : vec3
  }

let calculate_gravity (moons : moon array) (moon_idx : int) : vec3 =
  Array.foldi moons ~init:{ x = 0; y = 0; z = 0 } ~f:(fun i acc other_moon ->
    if i = moon_idx
    then acc
    else (
      let current_moon = moons.(moon_idx) in
      { x = acc.x + -Int.compare current_moon.pos.x other_moon.pos.x
      ; y = acc.y + -Int.compare current_moon.pos.y other_moon.pos.y
      ; z = acc.z + -Int.compare current_moon.pos.z other_moon.pos.z
      }))
;;

let apply_velocity (moon : moon) : moon =
  { moon with
    pos =
      { x = moon.pos.x + moon.vel.x
      ; y = moon.pos.y + moon.vel.y
      ; z = moon.pos.z + moon.vel.z
      }
  }
;;

let calculate_energy (moon : moon) : int =
  let pot = abs moon.pos.x + abs moon.pos.y + abs moon.pos.z in
  let kin = abs moon.vel.x + abs moon.vel.y + abs moon.vel.z in
  pot * kin
;;

let parse_moon (line : string) : moon =
  Scanf.sscanf line "<x=%d, y=%d, z=%d>" (fun x y z ->
    { pos = { x; y; z }; vel = { x = 0; y = 0; z = 0 } })
;;

let cycle_moons (moons : moon list) : moon list =
  let moons_array = Array.of_list moons in
  let velocity_deltas =
    Array.mapi moons_array ~f:(fun i _ -> calculate_gravity moons_array i)
  in
  Array.mapi moons_array ~f:(fun i moon ->
    let new_vel =
      { x = moon.vel.x + velocity_deltas.(i).x
      ; y = moon.vel.y + velocity_deltas.(i).y
      ; z = moon.vel.z + velocity_deltas.(i).z
      }
    in
    apply_velocity { moon with vel = new_vel })
  |> Array.to_list
;;

let total_energy (moons : moon list) : int =
  List.fold moons ~init:0 ~f:(fun acc moon -> acc + calculate_energy moon)
;;

let cycle_one_axis (axis_state : (int * int) list) : (int * int) list =
  let positions = List.map axis_state ~f:fst in
  let velocities = List.map axis_state ~f:snd in
  let new_velocities =
    List.mapi velocities ~f:(fun i vel ->
      let pos = List.nth_exn positions i in
      List.foldi positions ~init:vel ~f:(fun j acc other_pos ->
        if i = j then acc else acc + -Int.compare pos other_pos))
  in
  List.map2_exn positions new_velocities ~f:(fun pos vel -> pos + vel, vel)
;;

let find_sub_cycle_length (initial_state : (int * int) list) : int =
  let rec aux current_state step =
    if
      step > 0
      && List.equal
           (fun (p1, v1) (p2, v2) -> p1 = p2 && v1 = v2)
           current_state
           initial_state
    then step
    else aux (cycle_one_axis current_state) (step + 1)
  in
  aux initial_state 0
;;

let extract_axis (moons : moon list) (axis : [ `X | `Y | `Z ]) : (int * int) list =
  List.map moons ~f:(fun moon ->
    match axis with
    | `X -> moon.pos.x, moon.vel.x
    | `Y -> moon.pos.y, moon.vel.y
    | `Z -> moon.pos.z, moon.vel.z)
;;

let find_cycle_length (initial_moons : moon list) : int =
  let resx = find_sub_cycle_length (extract_axis initial_moons `X) in
  let resy = find_sub_cycle_length (extract_axis initial_moons `Y) in
  let resz = find_sub_cycle_length (extract_axis initial_moons `Z) in
  List.fold [ resx; resy; resz ] ~init:1 ~f:lcm
;;

let () =
  let moons = read_lines "./inputs/d12/bot.txt" |> List.map ~f:parse_moon in
  let p1_moons = Fn.apply_n_times ~n:1000 cycle_moons moons in
  let res1 = total_energy p1_moons in
  let res2 = find_cycle_length moons in
  Printf.printf "Part 1: %i\nPart 2: %i\n" res1 res2
;;
