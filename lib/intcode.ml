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

  type mode =
    | Position
    | Immediate

  type computer_state =
    { memory : memory
    ; mutable inst_pt : int
    ; mutable input_queue : int list
    ; mutable halted : bool
    ; mutable last_output : int option
    }

  type step_result =
    | StepOutput of int
    | StepHalt
    | StepNeedInput
    | StepContinue
    | StepError

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

  let read (mem : memory) (mode : mode) (addr : int) =
    match mode with
    | Position -> mem.(addr)
    | Immediate -> addr
  ;;

  let create_state mem =
    { memory = Array.copy mem
    ; inst_pt = 0
    ; input_queue = []
    ; halted = false
    ; last_output = None
    }
  ;;

  let add_input (state : computer_state) (input_val : int) =
    state.input_queue <- state.input_queue @ [ input_val ];
    state
  ;;

  let send_output (out : int) : unit =
    if not (out = 0) then Printf.printf "\nOutput: %i" out
  ;;

  let execute_step (state : computer_state) : step_result =
    if state.halted
    then StepHalt
    else if state.inst_pt >= Array.length state.memory
    then (
      state.halted <- true;
      StepHalt)
    else (
      let op, modes = decode_instr state.memory.(state.inst_pt) in
      match op with
      | Add ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.memory.(dest) <- val1 + val2;
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Multiply ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.memory.(dest) <- val1 * val2;
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Input ->
        (match state.input_queue with
         | [] -> StepNeedInput
         | hd :: tl ->
           let dest = state.memory.(state.inst_pt + 1) in
           state.memory.(dest) <- hd;
           state.input_queue <- tl;
           state.inst_pt <- state.inst_pt + 2;
           StepContinue)
      | Output ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        state.inst_pt <- state.inst_pt + 2;
        state.last_output <- Some val1;
        send_output val1;
        StepOutput val1
      | JumpTrue ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.inst_pt <- (if val1 = 0 then state.inst_pt + 3 else val2);
        StepContinue
      | JumpFalse ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.inst_pt <- (if val1 = 0 then val2 else state.inst_pt + 3);
        StepContinue
      | LessThan ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.memory.(dest) <- (if val1 < val2 then 1 else 0);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Equal ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state.memory (List.nth_exn modes 0) param1 in
        let val2 = read state.memory (List.nth_exn modes 1) param2 in
        state.memory.(dest) <- (if val1 = val2 then 1 else 0);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Halt ->
        state.halted <- true;
        StepHalt
      | Invalid -> StepError)
  ;;

  let full_cycle (state : computer_state) : computer_state =
    let rec aux () : computer_state =
      if state.inst_pt >= Array.length state.memory
      then failwith "Instruction pointer out of bounds"
      else (
        match execute_step state with
        | StepContinue | StepOutput _ -> aux ()
        | StepHalt -> state
        | StepError -> failwith "An error occurred during execution"
        | StepNeedInput -> failwith "Computer needs input but none provided")
    in
    aux ()
  ;;

  let verify_cycle (state : computer_state) : memory option =
    let rec aux () : memory option =
      if state.inst_pt >= Array.length state.memory
      then failwith "Instruction pointer out of bounds"
      else (
        match execute_step state with
        | StepContinue | StepOutput _ -> aux ()
        | StepHalt -> Some state.memory
        | StepError -> None
        | StepNeedInput -> failwith "Computer needs input but none provided")
    in
    aux ()
  ;;

  let run_until_output_or_halt (state : computer_state) : int option =
    let rec aux () : int option =
      match execute_step state with
      | StepOutput output -> Some output
      | StepHalt -> None
      | StepNeedInput -> failwith "Computer needs input but none provided"
      | StepError -> failwith "An error occurred during execution"
      | StepContinue -> aux ()
    in
    aux ()
  ;;

  let initialize (mem : memory) (noun : int) (verb : int) : int array =
    let mem' = Array.copy mem in
    mem'.(1) <- noun;
    mem'.(2) <- verb;
    mem'
  ;;

  let find_inputs (goal : int) (state : computer_state) : (int * int) option =
    let result = ref None in
    for x = 0 to 9999 do
      let noun = x / 100 in
      let verb = x mod 100 in
      let test_mem = initialize state.memory noun verb in
      let test_state = create_state test_mem in
      match verify_cycle test_state with
      | Some rmem when rmem.(0) = goal -> result := Some (noun, verb)
      | _ -> ()
    done;
    !result
  ;;
end
