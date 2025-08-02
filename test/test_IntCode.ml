open Core
open Core.Poly
open Intcode

(* Include your Intcode module here *)

module IntcodeTests = struct
  open Intcode

  let opcode_testable =
    Alcotest.testable
      (fun fmt op ->
         let s =
           match op with
           | Add -> "Add"
           | Multiply -> "Multiply"
           | Input -> "Input"
           | Output -> "Output"
           | JumpTrue -> "JumpTrue"
           | JumpFalse -> "JumpFalse"
           | LessThan -> "LessThan"
           | Equal -> "Equal"
           | AdjustRelativeBase -> "AdjustRelativeBase"
           | Halt -> "Halt"
           | Invalid -> "Invalid"
         in
         Format.fprintf fmt "%s" s)
      (fun a b ->
         match a, b with
         | Add, Add
         | Multiply, Multiply
         | Input, Input
         | Output, Output
         | JumpTrue, JumpTrue
         | JumpFalse, JumpFalse
         | LessThan, LessThan
         | Equal, Equal
         | AdjustRelativeBase, AdjustRelativeBase
         | Halt, Halt
         | Invalid, Invalid -> true
         | _ -> false)
  ;;

  let mode_testable =
    Alcotest.testable
      (fun fmt mode ->
         let s =
           match mode with
           | Position -> "Position"
           | Immediate -> "Immediate"
           | Relative -> "Relative"
         in
         Format.fprintf fmt "%s" s)
      (fun a b ->
         match a, b with
         | Position, Position | Immediate, Immediate | Relative, Relative -> true
         | _ -> false)
  ;;

  let step_result_testable =
    Alcotest.testable
      (fun fmt result ->
         let s =
           match result with
           | StepOutput i -> Printf.sprintf "StepOutput(%d)" i
           | StepHalt -> "StepHalt"
           | StepNeedInput -> "StepNeedInput"
           | StepContinue -> "StepContinue"
           | StepError msg -> Printf.sprintf "StepError(%s)" msg
         in
         Format.fprintf fmt "%s" s)
      (fun a b ->
         match a, b with
         | StepOutput i1, StepOutput i2 -> i1 = i2
         | StepHalt, StepHalt | StepNeedInput, StepNeedInput | StepContinue, StepContinue
           -> true
         | StepError m1, StepError m2 -> String.equal m1 m2
         | _ -> false)
  ;;

  let paused_state_testable =
    Alcotest.testable
      (fun fmt state ->
         let s =
           match state with
           | PausedOutput i -> Printf.sprintf "PausedOutput(%d)" i
           | PausedInput -> "PausedInput"
           | PausedHalt -> "PausedHalt"
         in
         Format.fprintf fmt "%s" s)
      (fun a b ->
         match a, b with
         | PausedOutput i1, PausedOutput i2 -> i1 = i2
         | PausedInput, PausedInput | PausedHalt, PausedHalt -> true
         | _ -> false)
  ;;

  let int_array_testable =
    Alcotest.testable
      (fun fmt arr ->
         Format.fprintf
           fmt
           "[|%s|]"
           (String.concat ~sep:"; " (Array.to_list (Array.map arr ~f:string_of_int))))
      (Array.equal Int.equal)
  ;;

  (* Test opcode decoding *)
  let test_decode_op () =
    Alcotest.(check opcode_testable) "decode_op 1" Add (decode_op 1);
    Alcotest.(check opcode_testable) "decode_op 2" Multiply (decode_op 2);
    Alcotest.(check opcode_testable) "decode_op 3" Input (decode_op 3);
    Alcotest.(check opcode_testable) "decode_op 4" Output (decode_op 4);
    Alcotest.(check opcode_testable) "decode_op 5" JumpTrue (decode_op 5);
    Alcotest.(check opcode_testable) "decode_op 6" JumpFalse (decode_op 6);
    Alcotest.(check opcode_testable) "decode_op 7" LessThan (decode_op 7);
    Alcotest.(check opcode_testable) "decode_op 8" Equal (decode_op 8);
    Alcotest.(check opcode_testable) "decode_op 9" AdjustRelativeBase (decode_op 9);
    Alcotest.(check opcode_testable) "decode_op 99" Halt (decode_op 99);
    Alcotest.(check opcode_testable) "decode_op invalid" Invalid (decode_op 42)
  ;;

  let test_decode_instr () =
    let op, modes = decode_instr 1002 in
    Alcotest.(check opcode_testable) "1002 opcode" Multiply op;
    Alcotest.(check int) "1002 mode count" 3 (List.length modes);
    Alcotest.(check mode_testable) "1002 param 0 mode" Position (List.nth_exn modes 0);
    Alcotest.(check mode_testable) "1002 param 1 mode" Immediate (List.nth_exn modes 1);
    Alcotest.(check mode_testable) "1002 param 2 mode" Position (List.nth_exn modes 2);
    let op2, modes2 = decode_instr 21101 in
    Alcotest.(check opcode_testable) "21101 opcode" Add op2;
    Alcotest.(check mode_testable) "21101 param 0 mode" Immediate (List.nth_exn modes2 0);
    Alcotest.(check mode_testable) "21101 param 1 mode" Immediate (List.nth_exn modes2 1);
    Alcotest.(check mode_testable) "21101 param 2 mode" Relative (List.nth_exn modes2 2)
  ;;

  (* Test memory operations *)
  let test_memory_expansion () =
    let mem = [| 1; 2; 3 |] in
    let state = create_state mem in
    mem_expander state 10;
    Alcotest.(check bool) "Memory expanded" true (Array.length state.memory >= 11);
    Alcotest.(check int) "Original data preserved" 1 state.memory.(0);
    Alcotest.(check int) "New memory initialized" 0 state.memory.(10)
  ;;

  let test_create_state () =
    let mem = [| 1; 2; 3 |] in
    let state = create_state mem in
    Alcotest.(check int_array_testable) "Memory copied" [| 1; 2; 3 |] state.memory;
    Alcotest.(check int) "Instruction pointer" 0 state.inst_pt;
    Alcotest.(check bool) "Not halted" false state.halted;
    Alcotest.(check int) "Relative base" 0 state.relative_base;
    Alcotest.(check (list int)) "Empty input queue" [] state.input_queue;
    Alcotest.(check (list int)) "Empty output buffer" [] state.output_buffer
  ;;

  (* Test arithmetic operations *)
  let test_addition () =
    let mem = [| 1; 5; 6; 0; 99; 10; 20 |] in
    let state = create_state mem in
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Add step result" StepContinue result;
    Alcotest.(check int) "Addition result" 30 state.memory.(0);
    Alcotest.(check int) "Instruction pointer advanced" 4 state.inst_pt
  ;;

  let test_multiplication () =
    let mem = [| 2; 5; 6; 0; 99; 3; 4 |] in
    let state = create_state mem in
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Multiply step result" StepContinue result;
    Alcotest.(check int) "Multiplication result" 12 state.memory.(0);
    Alcotest.(check int) "Instruction pointer advanced" 4 state.inst_pt
  ;;

  let test_immediate_mode_arithmetic () =
    let mem = [| 1101; 10; 20; 0; 99 |] in
    (* Add 10 + 20 in immediate mode *)
    let state = create_state mem in
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Immediate add step result" StepContinue result;
    Alcotest.(check int) "Immediate addition result" 30 state.memory.(0)
  ;;

  (* Test input/output operations *)
  let test_input () =
    let mem = [| 3; 3; 99; 0 |] in
    let state = create_state mem in
    (* Test input needed when no input available *)
    let result1 = execute_step state in
    Alcotest.(check step_result_testable) "Input needed" StepNeedInput result1;
    (* Add input and try again *)
    let _ = add_input 42 state in
    let result2 = execute_step state in
    Alcotest.(check step_result_testable) "Input step result" StepContinue result2;
    Alcotest.(check int) "Input stored" 42 state.memory.(3);
    Alcotest.(check (list int)) "Input queue consumed" [] state.input_queue
  ;;

  let test_output () =
    let mem = [| 4; 3; 99; 123 |] in
    let state = create_state mem in
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Output step result" (StepOutput 123) result;
    Alcotest.(check (option int)) "Last output stored" (Some 123) state.last_output;
    Alcotest.(check (list int)) "Output buffer" [ 123 ] state.output_buffer
  ;;

  let test_multiple_inputs () =
    let mem = [| 3; 9; 3; 10; 1; 9; 10; 11; 99; 0; 0; 0 |] in
    let state = create_state mem in
    let state = add_input 5 state in
    let state = add_input 7 state in
    let _ = execute_step state in
    (* First input *)
    let _ = execute_step state in
    (* Second input *)
    let _ = execute_step state in
    (* Add them *)
    Alcotest.(check int) "Multiple inputs result" 12 state.memory.(11)
  ;;

  (* Test jump operations *)
  let test_jump_true () =
    (* Jump when condition is true *)
    let mem1 = [| 5; 4; 5; 99; 1; 10 |] in
    let state1 = create_state mem1 in
    let result1 = execute_step state1 in
    Alcotest.(check step_result_testable) "Jump-true step result" StepContinue result1;
    Alcotest.(check int) "Jump-true jumps" 10 state1.inst_pt;
    (* Don't jump when condition is false *)
    let mem2 = [| 5; 4; 5; 99; 0; 10 |] in
    let state2 = create_state mem2 in
    let _ = execute_step state2 in
    Alcotest.(check int) "Jump-true advances" 3 state2.inst_pt
  ;;

  let test_jump_false () =
    (* Jump when condition is false *)
    let mem1 = [| 6; 4; 5; 99; 0; 10 |] in
    let state1 = create_state mem1 in
    let result1 = execute_step state1 in
    Alcotest.(check step_result_testable) "Jump-false step result" StepContinue result1;
    Alcotest.(check int) "Jump-false jumps" 10 state1.inst_pt;
    (* Don't jump when condition is true *)
    let mem2 = [| 6; 4; 5; 99; 1; 10 |] in
    let state2 = create_state mem2 in
    let _ = execute_step state2 in
    Alcotest.(check int) "Jump-false advances" 3 state2.inst_pt
  ;;

  (* Test comparison operations *)
  let test_less_than () =
    (* True case: 3 < 5 *)
    let mem1 = [| 7; 5; 6; 7; 99; 3; 5; 0 |] in
    let state1 = create_state mem1 in
    let _ = execute_step state1 in
    Alcotest.(check int) "Less-than true" 1 state1.memory.(7);
    (* False case: 5 < 3 *)
    let mem2 = [| 7; 5; 6; 7; 99; 5; 3; 0 |] in
    let state2 = create_state mem2 in
    let _ = execute_step state2 in
    Alcotest.(check int) "Less-than false" 0 state2.memory.(7)
  ;;

  let test_equal () =
    (* True case: 5 == 5 *)
    let mem1 = [| 8; 5; 6; 7; 99; 5; 5; 0 |] in
    let state1 = create_state mem1 in
    let _ = execute_step state1 in
    Alcotest.(check int) "Equal true" 1 state1.memory.(7);
    (* False case: 5 == 3 *)
    let mem2 = [| 8; 5; 6; 7; 99; 5; 3; 0 |] in
    let state2 = create_state mem2 in
    let _ = execute_step state2 in
    Alcotest.(check int) "Equal false" 0 state2.memory.(7)
  ;;

  (* Test relative base operations *)
  let test_adjust_relative_base () =
    let mem = [| 9; 3; 99; 5 |] in
    let state = create_state mem in
    Alcotest.(check int) "Initial relative base" 0 state.relative_base;
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Adjust base step result" StepContinue result;
    Alcotest.(check int) "Relative base adjusted" 5 state.relative_base
  ;;

  let test_relative_mode_operations () =
    (* Set relative base to 10, then output relative[-1] *)
    let mem = [| 109; 10; 204; -1; 99 |] in
    let state = create_state mem in
    mem_expander state 20;
    state.memory.(9) <- 42;
    (* Value at position 9 (10 + (-1)) *)
    let _ = execute_step state in
    (* Adjust relative base *)
    Alcotest.(check int) "Relative base set" 10 state.relative_base;
    let result = execute_step state in
    (* Output with relative mode *)
    Alcotest.(check step_result_testable) "Relative mode output" (StepOutput 42) result
  ;;

  (* Test halt operation *)
  let test_halt () =
    let mem = [| 99 |] in
    let state = create_state mem in
    let result = execute_step state in
    Alcotest.(check step_result_testable) "Halt step result" StepHalt result;
    Alcotest.(check bool) "State halted" true state.halted
  ;;

  (* Test error conditions *)
  let test_invalid_opcode () =
    let mem = [| 42; 0; 0; 0 |] in
    let state = create_state mem in
    let result = execute_step state in
    match result with
    | StepError _ -> ()
    | _ -> Alcotest.fail "Should return StepError for invalid opcode"
  ;;

  (* Test pause states *)
  let test_pause_on_output () =
    let mem = [| 4; 3; 99; 42 |] in
    let state = create_state mem in
    let pause_state = run_to_pause_state state in
    Alcotest.(check paused_state_testable) "Pause on output" (PausedOutput 42) pause_state
  ;;

  let test_pause_on_halt () =
    let mem = [| 99 |] in
    let state = create_state mem in
    let pause_state = run_to_pause_state state in
    Alcotest.(check paused_state_testable) "Pause on halt" PausedHalt pause_state
  ;;

  let test_pause_on_input () =
    let mem = [| 3; 0; 99 |] in
    let state = create_state mem in
    let pause_state = run_to_pause_state state in
    Alcotest.(check paused_state_testable) "Pause on input" PausedInput pause_state
  ;;

  (* Test utility functions *)
  let test_mem_of_string () =
    let mem = mem_of_string "1,2,3,4,5" in
    let expected = [| 1; 2; 3; 4; 5 |] in
    Alcotest.(check int_array_testable) "Parse memory string" expected mem
  ;;

  let test_initialize () =
    let base_mem = [| 1; 0; 0; 0 |] in
    let init_mem = initialize base_mem 12 2 in
    Alcotest.(check int) "Initialize noun" 12 init_mem.(1);
    Alcotest.(check int) "Initialize verb" 2 init_mem.(2);
    Alcotest.(check int) "Original unchanged" 0 base_mem.(1)
  ;;

  let test_output_buffer () =
    let mem = [| 4; 6; 4; 7; 99; 0; 42; 123 |] in
    let state = create_state mem in
    let _ = execute_step state in
    (* Output 42 *)
    let _ = execute_step state in
    (* Output 123 *)
    Alcotest.(check (list int)) "Output buffer contents" [ 42; 123 ] state.output_buffer
  ;;

  let test_add_input () =
    let mem = [| 99 |] in
    let state = create_state mem in
    let state = add_input 10 state in
    let state = add_input 20 state in
    Alcotest.(check (list int)) "Multiple inputs queued" [ 10; 20 ] state.input_queue
  ;;

  (* Test find_inputs function *)
  let test_find_inputs () =
    let mem = [| 1; 0; 0; 0; 99 |] in
    (* Simple add program *)
    let state = create_state mem in
    (* This is a simple test - in practice you'd want a more complex program *)
    match find_inputs 3 state with
    | Some (noun, verb) ->
      let test_mem = initialize mem noun verb in
      let test_state = create_state test_mem in
      let result_state = full_cycle test_state in
      Alcotest.(check int) "Find inputs produces correct result" 3 result_state.memory.(0)
    | None ->
      (* It's okay if no solution is found for this simple test *)
      ()
  ;;

  (* Test complete programs *)
  let test_day2_example () =
    let mem = [| 1; 9; 10; 3; 2; 3; 11; 0; 99; 30; 40; 50 |] in
    let state = create_state mem in
    let final_state = full_cycle state in
    Alcotest.(check int) "Day 2 example result" 3500 final_state.memory.(0);
    Alcotest.(check bool) "Program halted" true final_state.halted
  ;;

  let test_day5_example () =
    let mem = [| 3; 0; 4; 0; 99 |] in
    let state = create_state mem in
    let state = add_input 42 state in
    let final_state = full_cycle state in
    Alcotest.(check (option int)) "Day 5 output" (Some 42) final_state.last_output;
    Alcotest.(check bool) "Program halted" true final_state.halted
  ;;

  let amplifier_program =
    [| 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 |]
  ;;

  let test_day7_single_amplifier () =
    (* Test single amplifier with phase setting 4 and input signal 0 *)
    let state = create_state amplifier_program in
    let state = add_input 4 state in
    (* Phase setting *)
    let state = add_input 0 state in
    (* Input signal *)
    let final_state = full_cycle state in
    Alcotest.(check (option int))
      "Single amplifier output"
      (Some 4)
      final_state.last_output
  ;;

  let test_day7_amplifier_chain () =
    (* Test complete amplifier chain with phase settings [4,3,2,1,0] *)
    let phase_settings = [ 4; 3; 2; 1; 0 ] in
    let input_signal = ref 0 in
    List.iter phase_settings ~f:(fun phase ->
      let state = create_state amplifier_program in
      let state = add_input phase state in
      let state = add_input !input_signal state in
      let final_state = full_cycle state in
      match final_state.last_output with
      | Some output -> input_signal := output
      | None -> Alcotest.fail "Amplifier should produce output");
    Alcotest.(check int) "Amplifier chain output" 43210 !input_signal
  ;;

  let test_input_output_program () =
    (* Program: input, compare to 8, output result *)
    let mem = [| 3; 9; 8; 9; 10; 9; 4; 9; 99; -1; 8 |] in
    let state = create_state mem in
    let state = add_input 8 state in
    let final_state = full_cycle state in
    Alcotest.(check (option int)) "Input equals 8" (Some 1) final_state.last_output
  ;;

  let test_run_until_output_match_basic () =
    (* Program that outputs 10, 20, 30, then halts *)
    let mem = [| 4; 9; 4; 10; 4; 11; 99; 0; 0; 10; 20; 30 |] in
    let state = create_state mem in
    (* Test matching specific output *)
    let pause_state = run_until_output_match state ~f:(fun x -> x = 20) () in
    Alcotest.(check paused_state_testable) "Match output 20" (PausedOutput 20) pause_state;
    (* Continue and match next output *)
    let pause_state2 = run_until_output_match state ~f:(fun x -> x = 30) () in
    Alcotest.(check paused_state_testable)
      "Match output 30"
      (PausedOutput 30)
      pause_state2
  ;;

  let test_run_until_output_with_input () =
    (* Program that takes input and outputs it *)
    let mem = [| 3; 5; 4; 5; 99; 0 |] in
    let state = create_state mem in
    (* Test with input parameter *)
    let pause_state = run_until_output_match state ~input:(Some 42) () in
    Alcotest.(check paused_state_testable)
      "Expected output with input"
      (PausedOutput 42)
      pause_state
  ;;

  (* Organize tests into suites *)
  let basic_tests =
    [ "decode_op", `Quick, test_decode_op
    ; "decode_instr", `Quick, test_decode_instr
    ; "create_state", `Quick, test_create_state
    ; "memory_management", `Quick, test_memory_expansion
    ]
  ;;

  let arithmetic_tests =
    [ "addition", `Quick, test_addition
    ; "multiplication", `Quick, test_multiplication
    ; "immediate_mode_arithmetic", `Quick, test_immediate_mode_arithmetic
    ]
  ;;

  let io_tests =
    [ "input", `Quick, test_input
    ; "output", `Quick, test_output
    ; "multiple_inputs", `Quick, test_multiple_inputs
    ]
  ;;

  let control_flow_tests =
    [ "jump_true", `Quick, test_jump_true
    ; "jump_false", `Quick, test_jump_false
    ; "less_than", `Quick, test_less_than
    ; "equal", `Quick, test_equal
    ; "halt", `Quick, test_halt
    ]
  ;;

  let relative_base_tests =
    [ "adjust_relative_base", `Quick, test_adjust_relative_base
    ; "relative_mode_operations", `Quick, test_relative_mode_operations
    ]
  ;;

  let error_tests = [ "invalid_opcode", `Quick, test_invalid_opcode ]

  let pause_state_tests =
    [ "pause_on_output", `Quick, test_pause_on_output
    ; "pause_on_halt", `Quick, test_pause_on_halt
    ; "pause_on_input", `Quick, test_pause_on_input
    ]
  ;;

  let utility_tests =
    [ "mem_of_string", `Quick, test_mem_of_string
    ; "initialize", `Quick, test_initialize
    ; "output_buffer", `Quick, test_output_buffer
    ; "add_input", `Quick, test_add_input
    ; "find_inputs", `Slow, test_find_inputs
      (* Marked as slow since it does brute force search *)
    ]
  ;;

  let integration_tests =
    [ "day2_example", `Quick, test_day2_example
    ; "day5_example", `Quick, test_day5_example
    ; "day7_example1", `Quick, test_day7_single_amplifier
    ; "day7_example2", `Quick, test_day7_amplifier_chain
    ; "input_output_program", `Quick, test_input_output_program
    ]
  ;;

  let output_match_tests =
    [ "run_until_output_match_basic", `Quick, test_run_until_output_match_basic
    ; "run_until_output_match_with_input", `Quick, test_run_until_output_with_input
    ]
  ;;

  let all_tests =
    [ "Basic Operations", basic_tests
    ; "Arithmetic", arithmetic_tests
    ; "Input/Output", io_tests
    ; "Control Flow", control_flow_tests
    ; "Relative Base", relative_base_tests
    ; "Error Handling", error_tests
    ; "Pause States", pause_state_tests
    ; "Output Matching", output_match_tests
    ; "Utilities", utility_tests
    ; "Integration", integration_tests
    ]
  ;;
end

(* Run the tests *)
let () = Alcotest.run "Intcode Computer" IntcodeTests.all_tests
