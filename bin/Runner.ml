open Core

let time_execution f =
  let start_time = Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  let elapsed = Time_ns.diff end_time start_time in
  printf "Execution time: %s" (Time_ns.Span.to_string elapsed);
  result
;;

let time_execution_t f =
  let start_time = Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  let elapsed = Time_ns.diff end_time start_time in
  printf "\nTotal Execution time: %s\n" (Time_ns.Span.to_string elapsed);
  result
;;

let dayrunner (d : int) =
  for i = 1 to d do
    let _ = printf "\nExecuting Day %02d%!" i in
    match
      time_execution (fun () ->
        Core_unix.system (sprintf "./_build/default/bin/Day%02d.exe" i))
    with
    | Ok () -> printf "\n"
    | err -> printf "\nCommand failed: %s\n" (Core_unix.Exit_or_signal.to_string_hum err)
  done
;;

let () =
  printf "Enter the number of days to execute: %!";
  let input = In_channel.input_line_exn In_channel.stdin in
  match Int.of_string input with
  | num_days ->
    let _ = printf "Starting Execution of %d Days\n%!" num_days in
    time_execution_t (fun () -> dayrunner num_days)
  | exception _ -> printf "Error: Please enter a valid integer\n"
;;
