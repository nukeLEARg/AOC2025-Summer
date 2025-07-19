open Core
open Advent
open Bigarray
open Iter

type dir =
  | Up
  | Down
  | Left
  | Right

let dim = 50000
let cent = dim / 2
let make_layout = Array2.init Bigarray.int8_unsigned c_layout dim dim (fun _y _x -> 0)
let crossings : (int * int) option list ref = ref []

let print_pair (p : dir * int) : unit =
  let dir, len = p in
  let d =
    match dir with
    | Up -> 'U'
    | Down -> 'D'
    | Left -> 'L'
    | Right -> 'R'
  in
  printf "%c%i " d len
;;

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
      (map : (int, int8_unsigned_elt, c_layout) Array2.t)
      (sym : int)
  : int * int
  =
  let v =
    match dir with
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0
  in
  let rec aux (nlen : int) (npos : int * int) : int * int =
    if nlen = 0
    then npos
    else (
      let x, y = add_tuples npos v in
      if map.{x, y} = 0 || map.{x, y} = sym
      then map.{x, y} <- sym
      else (
        crossings := Some (x, y) :: !crossings;
        map.{x, y} <- 3);
      aux (nlen - 1) (x, y))
  in
  printf "";
  aux len pos
;;

let map_wire
      (paths : (dir * int) list)
      (map : (int, int8_unsigned_elt, c_layout) Array2.t)
      (sym : int)
  : unit
  =
  let rec aux (pos : int * int) (paths : (dir * int) list) =
    let npos = map_one_run pos (List.hd_exn paths) map sym in
    if List.length paths = 1
    then ()
    else (
      let npaths = List.tl_exn paths in
      aux npos npaths)
  in
  aux (cent, cent) paths
;;

let () =
  let lines = read_lines "./inputs/d03/bot.txt" in
  let wire1 = String.split_on_chars (List.nth_exn lines 0) ~on:[ ',' ] |> interp in
  let wire2 = String.split_on_chars (List.nth_exn lines 1) ~on:[ ',' ] |> interp in
  let map = make_layout in
  map_wire wire1 map 1;
  map_wire wire2 map 2;
  let cross1 =
    List.filter_map !crossings ~f:(fun pos ->
      match pos with
      | None -> None
      | Some pos -> Some (man_dist (cent, cent) pos))
  in
  print_int_list cross1;
  let res =
    List.fold cross1 ~init:(List.hd_exn cross1) ~f:(fun acc x ->
      if x < acc then x else acc)
  in
  (* let res =
    Iter.fold_left
      (fun acc x -> if x = 3 then acc + 1 else acc)
      0
      (genarray_of_array2 map)
  in *)
  (* Iter.iteri
    (fun i x -> if i.(0) = 0 then printf "\n%i" x else printf "%i" x)
    (genarray_of_array2 map); *)
  (* Iter.iteri
    (fun i w ->
       let x = i.(0) in
       let y = i.(1) in
       if w = 3 then printf "\ndist:%i;%i,%i\n" (man_dist (25000, 25000) (x, y)) x y)
    (genarray_of_array2 map); *)
  let res2 = 1 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
