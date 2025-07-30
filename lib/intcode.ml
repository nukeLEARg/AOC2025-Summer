open Core
open Core.Poly

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
    | AdjustRelativeBase
    | Halt
    | Invalid

  type mode =
    | Position
    | Immediate
    | Relative

  type computer_state =
    { mutable memory : memory
    ; mutable inst_pt : int
    ; mutable input_queue : int list
    ; mutable halted : bool
    ; mutable last_output : int option
    ; mutable relative_base : int
    ; mutable output_buffer : int list
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
    | 9 -> AdjustRelativeBase
    | 99 -> Halt
    | _ -> Invalid
  ;;

  let decode_instr (instr : int) : opcode * mode list =
    let op = decode_op (instr mod 100) in
    let params_ct =
      match op with
      | Add | Multiply | LessThan | Equal -> 3
      | JumpFalse | JumpTrue -> 2
      | Input | Output | AdjustRelativeBase -> 1
      | Halt | Invalid -> 0
    in
    let rec aux n remaining acc =
      if n >= params_ct
      then List.rev acc
      else (
        let mode =
          match remaining / Int.pow 10 n mod 10 with
          | 1 -> Immediate
          | 2 -> Relative
          | 0 -> Position
          | _ -> failwith "Unknown mode"
        in
        aux (n + 1) remaining (mode :: acc))
    in
    op, aux 0 (instr / 100) []
  ;;

  let mem_expander (state : computer_state) (addr : int) : unit =
    if addr >= Array.length state.memory
    then (
      let new_size = max (addr + 1) (Array.length state.memory * 2) in
      let new_memory = Array.create ~len:new_size 0 in
      Array.blit
        ~src:state.memory
        ~dst:new_memory
        ~src_pos:0
        ~dst_pos:0
        ~len:(Array.length state.memory);
      state.memory <- new_memory)
  ;;

  let read (state : computer_state) (mode : mode) (addr : int) : int =
    if mode = Immediate
    then addr
    else (
      let rel = state.relative_base in
      let effective_addr =
        match mode with
        | Position -> addr
        | Immediate -> failwith "Cannot read in immediate mode"
        | Relative -> addr + rel
      in
      if effective_addr < 0 then failwith "Negative memory address";
      mem_expander state effective_addr;
      state.memory.(effective_addr))
  ;;

  let write (state : computer_state) (mode : mode) (addr : int) (value : int) : unit =
    let rel = state.relative_base in
    let effective_addr =
      match mode with
      | Position -> addr
      | Immediate -> failwith "Cannot write in immediate mode"
      | Relative -> addr + rel
    in
    if effective_addr < 0 then failwith "Negative memory address";
    mem_expander state effective_addr;
    state.memory.(effective_addr) <- value
  ;;

  let create_state (mem : memory) : computer_state =
    { memory = Array.copy mem
    ; inst_pt = 0
    ; input_queue = []
    ; halted = false
    ; last_output = None
    ; relative_base = 0
    ; output_buffer = []
    }
  ;;

  let add_input (input_val : int) (state : computer_state) =
    state.input_queue <- state.input_queue @ [ input_val ];
    state
  ;;

  let send_output_to_buffer (out : int) (state : computer_state) : unit =
    state.output_buffer <- state.output_buffer @ [ out ]
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
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        write state (List.nth_exn modes 2) dest (val1 + val2);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Multiply ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        write state (List.nth_exn modes 2) dest (val1 * val2);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Input ->
        (match state.input_queue with
         | [] -> StepNeedInput
         | hd :: tl ->
           let dest = state.memory.(state.inst_pt + 1) in
           write state (List.nth_exn modes 0) dest hd;
           state.input_queue <- tl;
           state.inst_pt <- state.inst_pt + 2;
           StepContinue)
      | Output ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        state.inst_pt <- state.inst_pt + 2;
        state.last_output <- Some val1;
        send_output_to_buffer val1 state;
        StepOutput val1
      | JumpTrue ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        state.inst_pt <- (if val1 = 0 then state.inst_pt + 3 else val2);
        StepContinue
      | JumpFalse ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        state.inst_pt <- (if val1 = 0 then val2 else state.inst_pt + 3);
        StepContinue
      | LessThan ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        write state (List.nth_exn modes 2) dest (if val1 < val2 then 1 else 0);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | Equal ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let param2 = state.memory.(state.inst_pt + 2) in
        let dest = state.memory.(state.inst_pt + 3) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        let val2 = read state (List.nth_exn modes 1) param2 in
        write state (List.nth_exn modes 2) dest (if val1 = val2 then 1 else 0);
        state.inst_pt <- state.inst_pt + 4;
        StepContinue
      | AdjustRelativeBase ->
        let param1 = state.memory.(state.inst_pt + 1) in
        let val1 = read state (List.nth_exn modes 0) param1 in
        state.relative_base <- state.relative_base + val1;
        state.inst_pt <- state.inst_pt + 2;
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

  let mem_of_string (input : string) : int array =
    String.split_on_chars input ~on:[ ',' ] |> List.map ~f:int_of_string |> List.to_array
  ;;
end
