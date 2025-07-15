open Core
open Advent

let fuelCalc (mass : int) : int = (mass / 3) - 2

let recFuel (mass : int) : int =
  let rec aux (mass : int) (fuel : int) : int =
    let nFuel = fuelCalc mass in
    if nFuel < 1 then fuel else aux nFuel (fuel + nFuel)
  in
  aux mass 0
;;

let () =
  let masses = read_lines "./inputs/d01/bot.txt" |> List.map ~f:int_of_string in
  let res = List.fold masses ~init:0 ~f:(fun acc mass -> acc + fuelCalc mass) in
  let res2 = List.fold masses ~init:0 ~f:(fun acc mass -> acc + recFuel mass) in
  (* let res2 = Parmap.parmap recFuel (Parmap.L masses) |> List.fold ~init:0 ~f:( + ) in *)
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
