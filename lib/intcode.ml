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
    | StepError of string

  type paused_state =
    | PausedOutput of int
    | PausedInput
    | PausedHalt

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

  let get_params (state : computer_state) (modes : mode list) (count : int)
    : int array * int array
    =
    let params = Array.init count ~f:(fun i -> state.memory.(state.inst_pt + 1 + i)) in
    let values =
      Array.init count ~f:(fun i -> read state (List.nth_exn modes i) params.(i))
    in
    params, values
  ;;

  let exec_add (state : computer_state) (modes : mode list) : step_result =
    let params, values = get_params state modes 3 in
    write state (List.nth_exn modes 2) params.(2) (values.(0) + values.(1));
    state.inst_pt <- state.inst_pt + 4;
    StepContinue
  ;;

  let exec_multiply (state : computer_state) (modes : mode list) : step_result =
    let params, values = get_params state modes 3 in
    write state (List.nth_exn modes 2) params.(2) (values.(0) * values.(1));
    state.inst_pt <- state.inst_pt + 4;
    StepContinue
  ;;

  let exec_input (state : computer_state) (modes : mode list) : step_result =
    match state.input_queue with
    | [] -> StepNeedInput
    | hd :: tl ->
      let dest = state.memory.(state.inst_pt + 1) in
      write state (List.nth_exn modes 0) dest hd;
      state.input_queue <- tl;
      state.inst_pt <- state.inst_pt + 2;
      StepContinue
  ;;

  let exec_output (state : computer_state) (modes : mode list) : step_result =
    let _, values = get_params state modes 1 in
    state.inst_pt <- state.inst_pt + 2;
    state.last_output <- Some values.(0);
    send_output_to_buffer values.(0) state;
    StepOutput values.(0)
  ;;

  let exec_jump_true (state : computer_state) (modes : mode list) : step_result =
    let _, values = get_params state modes 2 in
    state.inst_pt <- (if values.(0) = 0 then state.inst_pt + 3 else values.(1));
    StepContinue
  ;;

  let exec_jump_false (state : computer_state) (modes : mode list) : step_result =
    let _, values = get_params state modes 2 in
    state.inst_pt <- (if values.(0) = 0 then values.(1) else state.inst_pt + 3);
    StepContinue
  ;;

  let exec_less_than (state : computer_state) (modes : mode list) : step_result =
    let params, values = get_params state modes 3 in
    write
      state
      (List.nth_exn modes 2)
      params.(2)
      (if values.(0) < values.(1) then 1 else 0);
    state.inst_pt <- state.inst_pt + 4;
    StepContinue
  ;;

  let exec_equal (state : computer_state) (modes : mode list) : step_result =
    let params, values = get_params state modes 3 in
    write
      state
      (List.nth_exn modes 2)
      params.(2)
      (if values.(0) = values.(1) then 1 else 0);
    state.inst_pt <- state.inst_pt + 4;
    StepContinue
  ;;

  let exec_adj_rbase (state : computer_state) (modes : mode list) : step_result =
    let _, values = get_params state modes 1 in
    state.relative_base <- state.relative_base + values.(0);
    state.inst_pt <- state.inst_pt + 2;
    StepContinue
  ;;

  let exec_halt (state : computer_state) : step_result =
    state.halted <- true;
    StepHalt
  ;;

  let execute_step (state : computer_state) : step_result =
    let op, modes = decode_instr state.memory.(state.inst_pt) in
    match op with
    | Add -> exec_add state modes
    | Multiply -> exec_multiply state modes
    | Input -> exec_input state modes
    | Output -> exec_output state modes
    | JumpTrue -> exec_jump_true state modes
    | JumpFalse -> exec_jump_false state modes
    | LessThan -> exec_less_than state modes
    | Equal -> exec_equal state modes
    | AdjustRelativeBase -> exec_adj_rbase state modes
    | Halt -> exec_halt state
    | Invalid -> StepError "Invalid opcode encountered"
  ;;

  let full_cycle (state : computer_state) : computer_state =
    let rec aux () : computer_state =
      match execute_step state with
      | StepContinue | StepOutput _ -> aux ()
      | StepHalt -> state
      | StepError e -> failwith e
      | StepNeedInput -> failwith "Computer needs input but none provided"
    in
    aux ()
  ;;

  let verify_cycle (state : computer_state) : memory option =
    let rec aux () : memory option =
      match execute_step state with
      | StepContinue | StepOutput _ -> aux ()
      | StepHalt -> Some state.memory
      | StepError _ -> None
      | StepNeedInput -> failwith "Computer needs input but none provided"
    in
    aux ()
  ;;

  let run_to_pause_state (state : computer_state) : paused_state =
    let rec aux () : paused_state =
      match execute_step state with
      | StepOutput output -> PausedOutput output
      | StepHalt -> PausedHalt
      | StepNeedInput -> PausedInput
      | StepError e -> failwith e
      | StepContinue -> aux ()
    in
    aux ()
  ;;

  let run_until_output_match
        (state : computer_state)
        ?(input : int option = None)
        ?(f : int -> bool = fun _ -> true)
        ()
    : paused_state
    =
    let _ =
      match input with
      | Some x -> state.input_queue <- x :: state.input_queue
      | None -> ()
    in
    let rec aux () : paused_state =
      match execute_step state with
      | StepOutput output when f output -> PausedOutput output
      | StepHalt -> PausedHalt
      | StepError e -> failwith e
      | StepContinue | StepNeedInput | StepOutput _ -> aux ()
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
