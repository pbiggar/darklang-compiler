(*
   ExecuteProcess.ml - Generate process replacement runtime support.
*)
open HeapAllocation
open ARM64Operands

(*
   Copy the managed command to a NUL-terminated native buffer.
   Reserve two bounded managed string blocks.
   pipe2(stdout), pipe2(stderr)
   clone(SIGCHLD, 0, 0, 0, 0)
*)
let generateLinuxCliExecuteHelper () =
  let syscall number =
    [ Symbolic.MOVZ (Symbolic.X8, number, 0); Symbolic.SVC 0 ]
  in
  let zero reg = Symbolic.MOVZ (reg, 0, 0) in
  let pairFd slot shift =
    [
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, slot);
      Symbolic.LSR_imm (Symbolic.X0, Symbolic.X0, shift);
    ]
  in
  let closeFd slot shift = pairFd slot shift @ syscall 57 in
  let setNonblocking slot =
    pairFd slot 0
    @ [
        Symbolic.AND_imm (Symbolic.X0, Symbolic.X0, 0xffffffffL);
        Symbolic.MOVZ (Symbolic.X1, 4, 0);
        Symbolic.MOVZ (Symbolic.X2, 2048, 0);
      ]
    @ syscall 25
  in
  let readPipe slot buffer lengthReg nextLabel =
    pairFd slot 0
    @ [
        Symbolic.AND_imm (Symbolic.X0, Symbolic.X0, 0xffffffffL);
        Symbolic.ADD_imm (Symbolic.X1, buffer, 16);
        Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, lengthReg);
      ]
    @ loadImmediate Symbolic.X2 1048576L
    @ [ Symbolic.SUB_reg (Symbolic.X2, Symbolic.X2, lengthReg) ]
    @ syscall 63
    @ [
        Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.LE, nextLabel);
        Symbolic.ADD_reg (lengthReg, lengthReg, Symbolic.X0);
      ]
  in
  let finalizeString buffer lengthReg =
    [
      Symbolic.MOVZ (Symbolic.X10, 1, 0);
      Symbolic.STR (Symbolic.X10, buffer, 0);
      Symbolic.STR (lengthReg, buffer, 8);
    ]
  in
  [
    Symbolic.Label "__dark_cli_execute";
    Symbolic.STP_pre (Symbolic.X29, Symbolic.X30, Symbolic.SP, -16);
    Symbolic.MOV_reg (Symbolic.X29, Symbolic.SP);
    Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -16);
    Symbolic.STP_pre (Symbolic.X21, Symbolic.X22, Symbolic.SP, -16);
    Symbolic.STP_pre (Symbolic.X23, Symbolic.X30, Symbolic.SP, -16);
    Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 96);
    Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28);
    Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X0, 16);
    zero Symbolic.X12;
    Symbolic.Label "__dark_cli_command_copy";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_cli_command_copied");
    Symbolic.LDRB (Symbolic.X13, Symbolic.X11, Symbolic.X12);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X12);
    Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_cli_command_copy";
    Symbolic.Label "__dark_cli_command_copied";
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X10);
    zero Symbolic.X15;
    Symbolic.STRB_reg (Symbolic.X15, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 8);
    Symbolic.LSR_imm (Symbolic.X10, Symbolic.X10, 3);
    Symbolic.LSL_imm (Symbolic.X10, Symbolic.X10, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X10);
    Symbolic.MOV_reg (Symbolic.X20, Symbolic.X28);
  ]
  @ loadImmediate Symbolic.X10 1048592L
  @ [
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X10);
      Symbolic.MOV_reg (Symbolic.X21, Symbolic.X28);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X10);
      zero Symbolic.X22;
      zero Symbolic.X23;
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 0);
      zero Symbolic.X1;
    ]
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_cli_spawn_error");
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 8);
      zero Symbolic.X1;
    ]
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_cli_spawn_error_close_stdout");
      Symbolic.MOVZ (Symbolic.X0, 17, 0);
      zero Symbolic.X1;
      zero Symbolic.X2;
      zero Symbolic.X3;
      zero Symbolic.X4;
    ]
  @ syscall 220
  @ [
      Symbolic.CBZ (Symbolic.X0, "__dark_cli_child");
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_cli_spawn_error_close_all");
      Symbolic.STR (Symbolic.X0, Symbolic.SP, 32);
      zero Symbolic.X9;
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 16);
    ]
  @ closeFd 0 32 @ closeFd 8 32 @ setNonblocking 0 @ setNonblocking 8
  @ [ Symbolic.Label "__dark_cli_drain_wait" ]
  @ readPipe 0 Symbolic.X20 Symbolic.X22 "__dark_cli_read_stderr"
  @ [ Symbolic.Label "__dark_cli_read_stderr" ]
  @ readPipe 8 Symbolic.X21 Symbolic.X23 "__dark_cli_wait"
  @ [
      Symbolic.Label "__dark_cli_wait";
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 32);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 16);
      Symbolic.MOVZ (Symbolic.X2, 1, 0);
      zero Symbolic.X3;
    ]
  @ syscall 260
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_cli_drain_wait");
      Symbolic.CBNZ (Symbolic.X0, "__dark_cli_finished");
      zero Symbolic.X9;
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 40);
    ]
  @ loadImmediate Symbolic.X9 1000000L
  @ [
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 48);
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 40);
      zero Symbolic.X1;
    ]
  @ syscall 101
  @ [
      Symbolic.B_label "__dark_cli_drain_wait";
      Symbolic.Label "__dark_cli_finished";
    ]
  @ readPipe 0 Symbolic.X20 Symbolic.X22 "__dark_cli_final_stderr"
  @ [ Symbolic.Label "__dark_cli_final_stderr" ]
  @ readPipe 8 Symbolic.X21 Symbolic.X23 "__dark_cli_final_close"
  @ [ Symbolic.Label "__dark_cli_final_close" ]
  @ closeFd 0 0 @ closeFd 8 0
  @ [
      Symbolic.LDR (Symbolic.X10, Symbolic.SP, 16);
      Symbolic.AND_imm (Symbolic.X11, Symbolic.X10, 0x7fL);
      Symbolic.CBNZ (Symbolic.X11, "__dark_cli_signaled");
      Symbolic.LSR_imm (Symbolic.X12, Symbolic.X10, 8);
      Symbolic.B_label "__dark_cli_build_result";
      Symbolic.Label "__dark_cli_signaled";
      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X11, 128);
      Symbolic.B_label "__dark_cli_build_result";
      Symbolic.Label "__dark_cli_spawn_error_close_all";
    ]
  @ closeFd 8 0 @ closeFd 8 32
  @ [ Symbolic.Label "__dark_cli_spawn_error_close_stdout" ]
  @ closeFd 0 0 @ closeFd 0 32
  @ [
      Symbolic.Label "__dark_cli_spawn_error";
      Symbolic.MOVZ (Symbolic.X12, 127, 0);
      Symbolic.Label "__dark_cli_build_result";
    ]
  @ finalizeString Symbolic.X20 Symbolic.X22
  @ finalizeString Symbolic.X21 Symbolic.X23
  @ [
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X28);
      Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 32);
    ]
  @ [
      Symbolic.STR (Symbolic.X12, Symbolic.X0, 0);
      Symbolic.STR (Symbolic.X20, Symbolic.X0, 8);
      Symbolic.STR (Symbolic.X21, Symbolic.X0, 16);
      Symbolic.MOVZ (Symbolic.X10, 1, 0);
      Symbolic.STR (Symbolic.X10, Symbolic.X0, 24);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 96);
      Symbolic.LDP_post (Symbolic.X23, Symbolic.X30, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X21, Symbolic.X22, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X19, Symbolic.X20, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X29, Symbolic.X30, Symbolic.SP, 16);
      Symbolic.RET;
      Symbolic.Label "__dark_cli_child";
    ]
  @ pairFd 0 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 1, 0); zero Symbolic.X2 ]
  @ syscall 24 @ pairFd 8 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 2, 0); zero Symbolic.X2 ]
  @ syscall 24 @ closeFd 0 0 @ closeFd 0 32 @ closeFd 8 0 @ closeFd 8 32
  @ loadStringLiteralPointer Symbolic.X0 "/bin/bash"
  @ [
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.X0, 16);
      Symbolic.MOV_reg (Symbolic.X14, Symbolic.X29);
      Symbolic.Label "__dark_cli_find_root_for_exec";
      Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
      Symbolic.CBZ (Symbolic.X13, "__dark_cli_exec_root_found");
      Symbolic.MOV_reg (Symbolic.X14, Symbolic.X13);
      Symbolic.B_label "__dark_cli_find_root_for_exec";
      Symbolic.Label "__dark_cli_exec_root_found";
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 24);
      Symbolic.Label "__dark_cli_find_envp_for_exec";
      Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 8);
      Symbolic.CBNZ (Symbolic.X13, "__dark_cli_find_envp_for_exec");
      Symbolic.STR (Symbolic.X14, Symbolic.SP, 88);
      Symbolic.Label "__dark_cli_find_shell";
      Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
      Symbolic.CBZ (Symbolic.X13, "__dark_cli_shell_found");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 0);
      Symbolic.CMP_imm (Symbolic.X10, 83);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 1);
      Symbolic.CMP_imm (Symbolic.X10, 72);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 2);
      Symbolic.CMP_imm (Symbolic.X10, 69);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 3);
      Symbolic.CMP_imm (Symbolic.X10, 76);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 4);
      Symbolic.CMP_imm (Symbolic.X10, 76);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.LDRB_imm (Symbolic.X10, Symbolic.X13, 5);
      Symbolic.CMP_imm (Symbolic.X10, 61);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_cli_next_env");
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.X13, 6);
      Symbolic.B_label "__dark_cli_shell_found";
      Symbolic.Label "__dark_cli_next_env";
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 8);
      Symbolic.B_label "__dark_cli_find_shell";
      Symbolic.Label "__dark_cli_shell_found";
      Symbolic.STR (Symbolic.X0, Symbolic.SP, 56);
    ]
  @ loadStringLiteralPointer Symbolic.X9 "-c"
  @ [
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 16);
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X19, Symbolic.SP, 72);
      zero Symbolic.X9;
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 80);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 56);
      Symbolic.LDR (Symbolic.X2, Symbolic.SP, 88);
    ]
  @ syscall 221
  @ [ Symbolic.MOVZ (Symbolic.X0, 127, 0) ]
  @ syscall 93
