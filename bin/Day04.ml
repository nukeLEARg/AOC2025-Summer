open Core
open Advent

let check_decrease strpw =
  let len = String.length strpw in
  let rec aux i =
    if i >= len - 1
    then true
    else if int_of_char strpw.[i] > int_of_char strpw.[i + 1]
    then false
    else aux (i + 1)
  in
  aux 0
;;

let check_repeating strpw =
  let len = String.length strpw in
  let rec aux i =
    if i >= len - 1
    then false
    else if Char.equal strpw.[i] strpw.[i + 1]
    then true
    else aux (i + 1)
  in
  aux 0
;;

let check_repeating_strict strpw =
  let len = String.length strpw in
  let rec aux i current_count found_double =
    if i >= len
    then found_double || current_count = 2
    else if i > 0 && Char.equal strpw.[i] strpw.[i - 1]
    then aux (i + 1) (current_count + 1) found_double
    else (
      let new_found = found_double || current_count = 2 in
      aux (i + 1) 1 new_found)
  in
  if len = 0 then false else aux 1 1 false
;;

let is_valid_password pw =
  let strpw = Int.to_string pw in
  check_repeating strpw && check_decrease strpw
;;

let is_valid_password_pt2 pw =
  let strpw = Int.to_string pw in
  check_repeating_strict strpw && check_decrease strpw
;;

let count_valid_passwords min max =
  let rec aux pw count1 count2 =
    if pw > max
    then count1, count2
    else
      aux
        (pw + 1)
        (if is_valid_password pw then count1 + 1 else count1)
        (if is_valid_password_pt2 pw then count2 + 1 else count2)
  in
  aux min 0 0
;;

let () =
  let lines =
    read_lines "./inputs/d04/bot.txt" |> List.hd_exn |> String.split_on_chars ~on:[ '-' ]
  in
  let min = List.nth_exn lines 0 |> int_of_string in
  let max = List.nth_exn lines 1 |> int_of_string in
  let res, res2 = count_valid_passwords min max in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
