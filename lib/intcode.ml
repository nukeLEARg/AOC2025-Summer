module Intcode = struct
  type memory = int array

  type opcode =
    | Add
    | Multiply
    | Halt
    | Invalid

  type status =
    | Running of int
    | Halted of memory
    | Error of string

  let decode_op = function
    | 1 -> Add
    | 2 -> Multiply
    | 99 -> Halt
    | _ -> Invalid
  ;;

  let execute_op mem inst_pt = function
    | Add ->
      let idx1 = mem.(inst_pt + 1) in
      let idx2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      mem.(dest) <- mem.(idx1) + mem.(idx2);
      Running (inst_pt + 4)
    | Multiply ->
      let idx1 = mem.(inst_pt + 1) in
      let idx2 = mem.(inst_pt + 2) in
      let dest = mem.(inst_pt + 3) in
      mem.(dest) <- mem.(idx1) * mem.(idx2);
      Running (inst_pt + 4)
    | Halt -> Halted mem
    | Invalid -> Error "Invalid opcode"
  ;;

  let run_cycle (mem : memory) : status =
    let rec aux inst_pt =
      if inst_pt >= Array.length mem
      then Error "Instruction pointer out of bounds"
      else (
        match decode_op mem.(inst_pt) with
        | op ->
          (match execute_op mem inst_pt op with
           | Running next_pt -> aux next_pt
           | Halted mem -> Halted mem
           | Error e -> Error e))
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
