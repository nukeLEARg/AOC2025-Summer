open Core

module Intcode = struct
  type memory = int array

  type opcode =
    | Add
    | Multiply
    | Input
    | Output
    | JumpTrue
    | JumpFalse
    | LessThan
    | Equal
    | Halt
    | Invalid

  type status =
    | Running of int
    | Halted of memory
    | Error of string

  type mode =
    | Position
    | Immediate

  let decode_op = function
    | 1 -> Add
    | 2 -> Multiply
    | 3 -> Input
    | 4 -> Output
    | 5 -> JumpTrue
    | 6 -> JumpFalse
    | 7 -> LessThan
    | 8 -> Equal
    | 99 -> Halt
    | _ -> Invalid
  ;;

  let decode_instr (instr : int) : opcode * mode list =
    let op = decode_op (instr mod 100) in
    let params_ct =
      match op with
      | Add | Multiply | LessThan | Equal -> 3
      | JumpFalse | JumpTrue -> 2
      | Input | Output -> 1
      | Halt | Invalid -> 0
    in
    let rec aux n remaining acc =
      if n >= params_ct
      then List.rev acc
      else (
        let mode =
          match remaining / Int.pow 10 n mod 10 with
          | 1 -> Immediate
          | _ -> Position
        in
        aux (n + 1) remaining (mode :: acc))
    in
    op, aux 0 (instr / 100) []
  ;;

  let read mem mode addr =
    match mode with
    | Position -> mem.(addr)
    | Immediate -> addr
  ;;

  let input = ref 0

  let get_input x =
    Printf.printf "\nGetting Input %i" x;
    x
  ;;

  let set_input x = input := x

  let send_output (out : int) (inst_pt : int) : unit =
    if not (out = 0) then Printf.printf "\nOutput:%i @%i " out inst_pt
  ;;

  let execute_op mem inst_pt =
    let op, modes = decode_instr mem.(inst_pt) in
    match op with
    | Add ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      mem.(dest) <- val1 + val2;
      Running (inst_pt + 4)
    | Multiply ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      mem.(dest) <- val1 * val2;
      Running (inst_pt + 4)
    | Input ->
      let dest = mem.(inst_pt + 1) in
      mem.(dest) <- get_input !input;
      Running (inst_pt + 2)
    | Output ->
      let param1 = mem.(inst_pt + 1) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      send_output val1 inst_pt;
      Running (inst_pt + 2)
    | JumpTrue ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      if val1 = 0 then Running (inst_pt + 3) else Running val2
    | JumpFalse ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      if val1 = 0 then Running val2 else Running (inst_pt + 3)
    | LessThan ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      if val1 < val2 then mem.(dest) <- 1 else mem.(dest) <- 0;
      Running (inst_pt + 4)
    | Equal ->
      let param1 = mem.(inst_pt + 1) in
      let param2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      let val1 = read mem (List.nth_exn modes 0) param1 in
      let val2 = read mem (List.nth_exn modes 1) param2 in
      if val1 = val2 then mem.(dest) <- 1 else mem.(dest) <- 0;
      Running (inst_pt + 4)
    | Halt -> Halted mem
    | Invalid -> Error "Invalid opcode"
  ;;

  let run_cycle (mem : memory) : status =
    let rec aux inst_pt =
      if inst_pt >= Array.length mem
      then Error "Instruction pointer out of bounds"
      else (
        match execute_op mem inst_pt with
        | Running next_pt ->
          if next_pt < 0 || next_pt >= Array.length mem
          then Error "Instruction pointer jumped out of bounds"
          else aux next_pt
        | Halted mem -> Halted mem
        | Error e -> Error e)
    in
    aux 0
  ;;

  let initialize mem noun verb =
    let mem' = Array.copy mem in
    mem'.(1) <- noun;
    mem'.(2) <- verb;
    mem'
  ;;

  let find_inputs ~mem ~goal =
    let result = ref None in
    for x = 0 to 9999 do
      let noun = x / 100 in
      let verb = x mod 100 in
      let nmem = initialize mem noun verb in
      match run_cycle nmem with
      | Halted rmem when rmem.(0) = goal -> result := Some (noun, verb)
      | _ -> ()
    done;
    !result
  ;;
end
