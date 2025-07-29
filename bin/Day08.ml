open Core
open Advent

let imagewidth = 25
let imageheight = 6
let imagesize = imagewidth * imageheight

let print_image (image : string) : unit =
  let rec aux (i : int) : unit =
    if i < String.length image
    then (
      Printf.printf "%c" (String.get image i);
      if (i + 1) mod imagewidth = 0 then Printf.printf "\n";
      aux (i + 1))
    else Printf.printf "\n"
  in
  aux 0
;;

let () =
  let line = read_lines "./inputs/d08/input.txt" |> List.hd_exn in
  let num_layers = String.length line / imagesize in
  let layers =
    List.init num_layers ~f:(fun i -> String.sub line ~pos:(i * imagesize) ~len:imagesize)
  in
  let counts =
    List.fold layers ~init:[] ~f:(fun acc layer ->
      let count_0 = String.count layer ~f:(Char.equal '0') in
      let count_1 = String.count layer ~f:(Char.equal '1') in
      let count_2 = String.count layer ~f:(Char.equal '2') in
      (count_0, count_1, count_2) :: acc)
    |> List.rev
  in
  let res, _ =
    List.fold counts ~init:(0, Int.max_value) ~f:(fun acc (count_0, count_1, count_2) ->
      let _, zero = acc in
      if count_0 < zero then count_1 * count_2, count_0 else acc)
  in
  let image =
    List.init imagesize ~f:(fun i ->
      let pixel =
        List.find_map layers ~f:(fun layer ->
          let c = String.get layer i in
          Option.some_if (not (Char.equal c '2')) c)
      in
      match pixel with
      | Some '0' -> ' '
      | Some '1' -> '#'
      | _ -> '!')
    |> String.of_char_list
  in
  Printf.printf "\nPart 1: %i\nPart 2:\n" res;
  print_image image
;;
