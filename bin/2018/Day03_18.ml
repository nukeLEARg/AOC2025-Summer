open Core
open Advent
module Cordmap = Map.Make (Pos)

type plot =
  { id : int
  ; pos : Pos.t
  ; x_size : int
  ; y_size : int
  }

let parse_plot (line : string) : plot =
  let split = String.split_on_chars line ~on:[ '@'; ':' ] |> List.map ~f:String.strip in
  let id =
    String.sub ~pos:1 ~len:(String.length (List.hd_exn split) - 1) (List.hd_exn split)
    |> int_of_string
  in
  let x, y =
    match String.split (List.nth_exn split 1) ~on:',' with
    | [ x_str; y_str ] -> int_of_string x_str, int_of_string y_str
    | _ -> failwith "Invalid position format"
  in
  let x_size, y_size =
    match String.split (List.nth_exn split 2) ~on:'x' with
    | [ w; h ] -> int_of_string w, int_of_string h
    | _ -> failwith "Invalid size format"
  in
  { id; pos = x, y; x_size; y_size }
;;

let plot_to_positions (plot : plot) : Pos.t list =
  let rec aux (x : int) (y : int) (acc : (int * int) list) =
    if y >= plot.y_size
    then acc
    else if x >= plot.x_size
    then aux 0 (y + 1) acc
    else aux (x + 1) y (add_tuples plot.pos (x, y) :: acc)
  in
  aux 0 0 []
;;

let make_plot_map (plots : plot list) : int list Cordmap.t =
  List.fold plots ~init:Cordmap.empty ~f:(fun acc plot ->
    List.fold (plot_to_positions plot) ~init:acc ~f:(fun acc pos ->
      Map.update acc pos ~f:(function
        | None -> [ plot.id ]
        | Some existing -> plot.id :: existing)))
;;

let find_non_overlapping_claims_v2 (plot_map : int list Cordmap.t) : int option =
  let all_claims_set =
    Map.fold
      plot_map
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data:claim_list acc ->
        List.fold claim_list ~init:acc ~f:(fun acc claim_id -> Set.add acc claim_id))
  in
  Set.filter all_claims_set ~f:(fun claim_id ->
    not
      (Map.existsi plot_map ~f:(fun ~key:_ ~data:claim_list ->
         List.mem claim_list claim_id ~equal:Int.equal && List.length claim_list > 1)))
  |> Set.choose
;;

let () =
  let plots = read_lines "./inputs/2018/d03/input.txt" |> List.map ~f:parse_plot in
  let plot_map = make_plot_map plots in
  let res =
    Map.fold plot_map ~init:0 ~f:(fun ~key:_ ~data:plots acc ->
      if List.length plots > 1 then acc + 1 else acc)
  in
  let res2 =
    match find_non_overlapping_claims_v2 plot_map with
    | Some id -> id
    | None -> 0
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
