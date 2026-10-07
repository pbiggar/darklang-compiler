(*
   RunProcess.ml - Generate process execution with captured output.
*)
open ARM64Operands

(*
   Linux AArch64 argv runner. The request contains packed NUL-separated argv,
   an optional cwd/environment overlay or a second argv for a pipeline.
   Executables are resolved portably before this boundary, so the child can
   call execve directly without a shell or utility process.
   ENOENT is recoverable policy input for PATH lookup. Remember the
   scratch-allocation boundary so a failed candidate attempt can return
   empty strings without permanently consuming its two 1 MiB buffers.
   Copy packed argv and add the terminating NUL required by execve.
   Build argv pointers from separators in the copied buffer.
   Pipeline mode carries a second packed argv. Build its independent
   native buffer and pointer vector before either child is created.
   Locate the inherited envp from _start's root frame.
   Prepend packed environment overrides to inherited envp. libc getenv
   observes the first matching entry, so an override wins even when the
   inherited vector also contains that name.
   Count inherited entries to reserve the complete pointer vector.
   Build pointers for each packed override.
   Copy cwd to a native NUL-terminated buffer.
   Reserve managed output buffers.
   stdout and stderr pipes
   A close-on-exec pipe reports child setup/exec errno to the parent.
   The producer and consumer share this pipe only in pipeline mode. It
   is created unconditionally so every child/error path has one stable
   descriptor layout.
   Timeout mode decrements one millisecond per wait probe.
   Apply cwd only for runIn mode.
   Linux syscalls return -errno. Preserve errno before write changes X0.
*)
let generateLinuxCliRunProcessHelper () =
  let syscall number =
    [ Symbolic.MOVZ (Symbolic.X8, number, 0); Symbolic.SVC 0 ]
  in
  let zero reg = Symbolic.MOVZ (reg, 0, 0) in
  let pairFd slot shift =
    [
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, slot);
      Symbolic.LSR_imm (Symbolic.X0, Symbolic.X0, shift);
      Symbolic.AND_imm (Symbolic.X0, Symbolic.X0, 0xffffffffL);
    ]
  in
  let closeFd slot shift = pairFd slot shift @ syscall 57 in
  let setNonblocking slot =
    pairFd slot 0
    @ [
        Symbolic.MOVZ (Symbolic.X1, 4, 0); Symbolic.MOVZ (Symbolic.X2, 2048, 0);
      ]
    @ syscall 25
  in
  let readPipe slot buffer lengthReg nextLabel =
    pairFd slot 0
    @ [
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
      Symbolic.STR (lengthReg, buffer, 8);
      Symbolic.MOVZ (Symbolic.X10, 1, 0);
      Symbolic.STR (Symbolic.X10, buffer, 0);
    ]
  in
  [
    Symbolic.Label "__dark_cli_run_process";
    Symbolic.STP_pre (Symbolic.X29, Symbolic.X30, Symbolic.SP, -16);
    Symbolic.MOV_reg (Symbolic.X29, Symbolic.SP);
    Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -16);
    Symbolic.STP_pre (Symbolic.X21, Symbolic.X22, Symbolic.SP, -16);
    Symbolic.STP_pre (Symbolic.X23, Symbolic.X24, Symbolic.SP, -16);
    Symbolic.STP_pre (Symbolic.X25, Symbolic.X26, Symbolic.SP, -16);
    Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 128);
    Symbolic.STR (Symbolic.X28, Symbolic.SP, 96);
    Symbolic.MOV_reg (Symbolic.X25, Symbolic.X0);
    Symbolic.LDR (Symbolic.X0, Symbolic.X25, 8);
    Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
    Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X0, 16);
    zero Symbolic.X12;
    Symbolic.Label "__dark_run_argv_copy";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_argv_copied");
    Symbolic.LDRB (Symbolic.X13, Symbolic.X11, Symbolic.X12);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X12);
    Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_argv_copy";
    Symbolic.Label "__dark_run_argv_copied";
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X10);
    zero Symbolic.X15;
    Symbolic.STRB_reg (Symbolic.X15, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X10, 8);
    Symbolic.LSR_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11);
    Symbolic.MOV_reg (Symbolic.X24, Symbolic.X28);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X10, 2);
    Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11);
    zero Symbolic.X12;
    zero Symbolic.X13;
    Symbolic.MOV_reg (Symbolic.X14, Symbolic.X24);
    Symbolic.STR (Symbolic.X19, Symbolic.X14, 0);
    Symbolic.Label "__dark_run_argv_scan";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_argv_done");
    Symbolic.LDRB (Symbolic.X15, Symbolic.X19, Symbolic.X12);
    Symbolic.CBNZ (Symbolic.X15, "__dark_run_argv_next");
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X24, Symbolic.X13);
    Symbolic.ADD_imm (Symbolic.X15, Symbolic.X12, 1);
    Symbolic.ADD_reg (Symbolic.X15, Symbolic.X19, Symbolic.X15);
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.Label "__dark_run_argv_next";
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_argv_scan";
    Symbolic.Label "__dark_run_argv_done";
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X24, Symbolic.X13);
    zero Symbolic.X15;
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
    Symbolic.CMP_imm (Symbolic.X9, 4);
    Symbolic.B_cond_label (Symbolic.NE, "__dark_run_pipeline_argv_done");
    Symbolic.LDR (Symbolic.X0, Symbolic.X25, 40);
    Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
    Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X0, 16);
    zero Symbolic.X12;
    Symbolic.Label "__dark_run_pipeline_argv_copy";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_pipeline_argv_copied");
    Symbolic.LDRB (Symbolic.X13, Symbolic.X11, Symbolic.X12);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X12);
    Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_pipeline_argv_copy";
    Symbolic.Label "__dark_run_pipeline_argv_copied";
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X10);
    zero Symbolic.X15;
    Symbolic.STRB_reg (Symbolic.X15, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X10, 8);
    Symbolic.LSR_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11);
    Symbolic.MOV_reg (Symbolic.X11, Symbolic.X28);
    Symbolic.STR (Symbolic.X11, Symbolic.SP, 104);
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X10, 2);
    Symbolic.LSL_imm (Symbolic.X13, Symbolic.X13, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X13);
    zero Symbolic.X12;
    zero Symbolic.X13;
    Symbolic.STR (Symbolic.X19, Symbolic.X11, 0);
    Symbolic.Label "__dark_run_pipeline_argv_scan";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_pipeline_argv_terminated");
    Symbolic.LDRB (Symbolic.X15, Symbolic.X19, Symbolic.X12);
    Symbolic.CBNZ (Symbolic.X15, "__dark_run_pipeline_argv_next");
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X11, Symbolic.X13);
    Symbolic.ADD_imm (Symbolic.X15, Symbolic.X12, 1);
    Symbolic.ADD_reg (Symbolic.X15, Symbolic.X19, Symbolic.X15);
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.Label "__dark_run_pipeline_argv_next";
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_pipeline_argv_scan";
    Symbolic.Label "__dark_run_pipeline_argv_terminated";
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X11, Symbolic.X13);
    zero Symbolic.X15;
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.Label "__dark_run_pipeline_argv_done";
    Symbolic.MOV_reg (Symbolic.X14, Symbolic.X29);
    Symbolic.Label "__dark_run_find_root";
    Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
    Symbolic.CBZ (Symbolic.X13, "__dark_run_root_found");
    Symbolic.MOV_reg (Symbolic.X14, Symbolic.X13);
    Symbolic.B_label "__dark_run_find_root";
    Symbolic.Label "__dark_run_root_found";
    Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 24);
    Symbolic.Label "__dark_run_find_envp";
    Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
    Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 8);
    Symbolic.CBNZ (Symbolic.X13, "__dark_run_find_envp");
    Symbolic.MOV_reg (Symbolic.X26, Symbolic.X14);
    Symbolic.STR (Symbolic.X26, Symbolic.SP, 88);
    Symbolic.LDR (Symbolic.X0, Symbolic.X25, 24);
    Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
    Symbolic.CBZ (Symbolic.X10, "__dark_run_environment_done");
    Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X0, 16);
    zero Symbolic.X12;
    Symbolic.Label "__dark_run_environment_copy";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_environment_copied");
    Symbolic.LDRB (Symbolic.X13, Symbolic.X11, Symbolic.X12);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X12);
    Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_environment_copy";
    Symbolic.Label "__dark_run_environment_copied";
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X10);
    zero Symbolic.X15;
    Symbolic.STRB_reg (Symbolic.X15, Symbolic.X14);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X10, 8);
    Symbolic.LSR_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11);
    Symbolic.LDR (Symbolic.X11, Symbolic.SP, 88);
    zero Symbolic.X12;
    Symbolic.Label "__dark_run_environment_count";
    Symbolic.LDR (Symbolic.X13, Symbolic.X11, 0);
    Symbolic.CBZ (Symbolic.X13, "__dark_run_environment_counted");
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
    Symbolic.B_label "__dark_run_environment_count";
    Symbolic.Label "__dark_run_environment_counted";
    Symbolic.MOV_reg (Symbolic.X26, Symbolic.X28);
    Symbolic.ADD_reg (Symbolic.X11, Symbolic.X10, Symbolic.X12);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 2);
    Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11);
    zero Symbolic.X12;
    zero Symbolic.X13;
    Symbolic.STR (Symbolic.X19, Symbolic.X26, 0);
    Symbolic.Label "__dark_run_environment_scan";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label
      (Symbolic.GE, "__dark_run_environment_append_inherited");
    Symbolic.LDRB (Symbolic.X15, Symbolic.X19, Symbolic.X12);
    Symbolic.CBNZ (Symbolic.X15, "__dark_run_environment_next");
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X26, Symbolic.X13);
    Symbolic.ADD_imm (Symbolic.X15, Symbolic.X12, 1);
    Symbolic.ADD_reg (Symbolic.X15, Symbolic.X19, Symbolic.X15);
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.Label "__dark_run_environment_next";
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_environment_scan";
    Symbolic.Label "__dark_run_environment_append_inherited";
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.LDR (Symbolic.X11, Symbolic.SP, 88);
    Symbolic.Label "__dark_run_environment_append_next";
    Symbolic.LDR (Symbolic.X15, Symbolic.X11, 0);
    Symbolic.ADD_reg (Symbolic.X14, Symbolic.X26, Symbolic.X13);
    Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
    Symbolic.CBZ (Symbolic.X15, "__dark_run_environment_done");
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
    Symbolic.B_label "__dark_run_environment_append_next";
    Symbolic.Label "__dark_run_environment_done";
    Symbolic.LDR (Symbolic.X19, Symbolic.X24, 0);
    Symbolic.LDR (Symbolic.X0, Symbolic.X25, 16);
    Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
    Symbolic.MOV_reg (Symbolic.X11, Symbolic.X28);
    Symbolic.STR (Symbolic.X11, Symbolic.SP, 72);
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X0, 16);
    zero Symbolic.X12;
    Symbolic.Label "__dark_run_cwd_copy";
    Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.B_cond_label (Symbolic.GE, "__dark_run_cwd_done");
    Symbolic.LDRB (Symbolic.X14, Symbolic.X13, Symbolic.X12);
    Symbolic.ADD_reg (Symbolic.X15, Symbolic.X11, Symbolic.X12);
    Symbolic.STRB_reg (Symbolic.X14, Symbolic.X15);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label "__dark_run_cwd_copy";
    Symbolic.Label "__dark_run_cwd_done";
    Symbolic.ADD_reg (Symbolic.X15, Symbolic.X11, Symbolic.X10);
    zero Symbolic.X14;
    Symbolic.STRB_reg (Symbolic.X14, Symbolic.X15);
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
      zero Symbolic.X9;
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 56);
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 80);
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 0);
      zero Symbolic.X1;
    ]
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_spawn_error");
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 8);
      zero Symbolic.X1;
    ]
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_spawn_error_close_stdout");
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 16);
    ]
  @ loadImmediate Symbolic.X1 524288L
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_spawn_error_close_output");
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 112);
      zero Symbolic.X1;
    ]
  @ syscall 59
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_spawn_error_close_errno");
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 4);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_clone_consumer");
      Symbolic.MOVZ (Symbolic.X0, 17, 0);
      zero Symbolic.X1;
      zero Symbolic.X2;
      zero Symbolic.X3;
      zero Symbolic.X4;
    ]
  @ syscall 220
  @ [
      Symbolic.CBZ (Symbolic.X0, "__dark_run_pipeline_producer");
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label
        (Symbolic.LT, "__dark_run_spawn_error_close_pipeline");
      Symbolic.STR (Symbolic.X0, Symbolic.SP, 120);
      Symbolic.Label "__dark_run_clone_consumer";
      Symbolic.MOVZ (Symbolic.X0, 17, 0);
      zero Symbolic.X1;
      zero Symbolic.X2;
      zero Symbolic.X3;
      zero Symbolic.X4;
    ]
  @ syscall 220
  @ [
      Symbolic.CBZ (Symbolic.X0, "__dark_run_child");
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_spawn_error_close_all");
      Symbolic.STR (Symbolic.X0, Symbolic.SP, 32);
      zero Symbolic.X9;
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 24);
    ]
  @ closeFd 0 32 @ closeFd 8 32 @ closeFd 16 32 @ closeFd 112 0 @ closeFd 112 32
  @ setNonblocking 0 @ setNonblocking 8
  @ [ Symbolic.Label "__dark_run_drain_wait" ]
  @ readPipe 0 Symbolic.X20 Symbolic.X22 "__dark_run_read_stderr"
  @ [ Symbolic.Label "__dark_run_read_stderr" ]
  @ readPipe 8 Symbolic.X21 Symbolic.X23 "__dark_run_wait"
  @ [
      Symbolic.Label "__dark_run_wait";
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 32);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 24);
      Symbolic.MOVZ (Symbolic.X2, 1, 0);
      zero Symbolic.X3;
    ]
  @ syscall 260
  @ [
      Symbolic.CBNZ (Symbolic.X0, "__dark_run_finished");
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 3);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_sleep");
      Symbolic.LDR (Symbolic.X10, Symbolic.SP, 56);
      Symbolic.LDR (Symbolic.X11, Symbolic.X25, 32);
      Symbolic.CMP_reg (Symbolic.X10, Symbolic.X11);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_timeout_next");
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 32);
      Symbolic.MOVZ (Symbolic.X1, 9, 0);
    ]
  @ syscall 129
  @ [
      Symbolic.MOVZ (Symbolic.X9, 1, 0);
      Symbolic.STR (Symbolic.X9, Symbolic.SP, 64);
      Symbolic.B_label "__dark_run_blocking_wait";
      Symbolic.Label "__dark_run_timeout_next";
      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
      Symbolic.STR (Symbolic.X10, Symbolic.SP, 56);
      Symbolic.Label "__dark_run_sleep";
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
      Symbolic.B_label "__dark_run_drain_wait";
      Symbolic.Label "__dark_run_blocking_wait";
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 32);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 24);
      zero Symbolic.X2;
      zero Symbolic.X3;
    ]
  @ syscall 260
  @ [
      Symbolic.Label "__dark_run_finished";
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 4);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_all_children_finished");
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 120);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 80);
      zero Symbolic.X2;
      zero Symbolic.X3;
    ]
  @ syscall 260
  @ [ Symbolic.Label "__dark_run_all_children_finished" ]
  @ readPipe 0 Symbolic.X20 Symbolic.X22 "__dark_run_final_stderr"
  @ [ Symbolic.Label "__dark_run_final_stderr" ]
  @ readPipe 8 Symbolic.X21 Symbolic.X23 "__dark_run_final_close"
  @ [ Symbolic.Label "__dark_run_final_close" ]
  @ closeFd 0 0 @ closeFd 8 0 @ pairFd 16 0
  @ [
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 80);
      Symbolic.MOVZ (Symbolic.X2, 8, 0);
    ]
  @ syscall 63 @ closeFd 16 0
  @ [
      Symbolic.LDR (Symbolic.X10, Symbolic.SP, 24);
      Symbolic.AND_imm (Symbolic.X11, Symbolic.X10, 0x7fL);
      Symbolic.CBNZ (Symbolic.X11, "__dark_run_signaled");
      Symbolic.LSR_imm (Symbolic.X12, Symbolic.X10, 8);
      Symbolic.B_label "__dark_run_build_result";
      Symbolic.Label "__dark_run_signaled";
      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X11, 128);
      Symbolic.B_label "__dark_run_build_result";
      Symbolic.Label "__dark_run_spawn_error_close_all";
    ]
  @ closeFd 112 0 @ closeFd 112 32
  @ [ Symbolic.Label "__dark_run_spawn_error_close_pipeline" ]
  @ [ Symbolic.Label "__dark_run_spawn_error_close_errno" ]
  @ closeFd 16 0 @ closeFd 16 32
  @ [ Symbolic.Label "__dark_run_spawn_error_close_output" ]
  @ closeFd 8 0 @ closeFd 8 32
  @ [ Symbolic.Label "__dark_run_spawn_error_close_stdout" ]
  @ closeFd 0 0 @ closeFd 0 32
  @ [
      Symbolic.Label "__dark_run_spawn_error";
      Symbolic.MOVZ (Symbolic.X12, 127, 0);
      Symbolic.Label "__dark_run_build_result";
      Symbolic.LDR (Symbolic.X9, Symbolic.SP, 80);
      Symbolic.CMP_imm (Symbolic.X9, 2);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_keep_capture_buffers");
      Symbolic.LDR (Symbolic.X28, Symbolic.SP, 96);
      Symbolic.MOV_reg (Symbolic.X20, Symbolic.X28);
      Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 16);
      Symbolic.MOV_reg (Symbolic.X21, Symbolic.X28);
      Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 16);
      zero Symbolic.X22;
      zero Symbolic.X23;
      Symbolic.Label "__dark_run_keep_capture_buffers";
    ]
  @ finalizeString Symbolic.X20 Symbolic.X22
  @ finalizeString Symbolic.X21 Symbolic.X23
  @ [
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X28);
      Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 48);
      Symbolic.LDR (Symbolic.X9, Symbolic.SP, 80);
      Symbolic.STR (Symbolic.X9, Symbolic.X0, 0);
      Symbolic.STR (Symbolic.X12, Symbolic.X0, 8);
      Symbolic.STR (Symbolic.X20, Symbolic.X0, 16);
      Symbolic.STR (Symbolic.X21, Symbolic.X0, 24);
      Symbolic.LDR (Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X9, Symbolic.X0, 32);
      Symbolic.MOVZ (Symbolic.X9, 1, 0);
      Symbolic.STR (Symbolic.X9, Symbolic.X0, 40);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 128);
      Symbolic.LDP_post (Symbolic.X25, Symbolic.X26, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X23, Symbolic.X24, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X21, Symbolic.X22, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X19, Symbolic.X20, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X29, Symbolic.X30, Symbolic.SP, 16);
      Symbolic.RET;
      Symbolic.Label "__dark_run_child";
    ]
  @ pairFd 0 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 1, 0); zero Symbolic.X2 ]
  @ syscall 24 @ pairFd 8 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 2, 0); zero Symbolic.X2 ]
  @ syscall 24
  @ [
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 4);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_child_input_ready");
    ]
  @ pairFd 112 0
  @ [ zero Symbolic.X1; zero Symbolic.X2 ]
  @ syscall 24
  @ [ Symbolic.Label "__dark_run_child_input_ready" ]
  @ closeFd 0 0 @ closeFd 0 32 @ closeFd 8 0 @ closeFd 8 32 @ closeFd 16 0
  @ closeFd 112 0 @ closeFd 112 32
  @ [
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 1);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_child_exec");
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 72);
    ]
  @ syscall 49
  @ [
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, "__dark_run_child_fail");
      Symbolic.Label "__dark_run_child_exec";
      Symbolic.LDR (Symbolic.X9, Symbolic.X25, 0);
      Symbolic.CMP_imm (Symbolic.X9, 4);
      Symbolic.B_cond_label (Symbolic.NE, "__dark_run_child_first_argv");
      Symbolic.LDR (Symbolic.X1, Symbolic.SP, 104);
      Symbolic.LDR (Symbolic.X0, Symbolic.X1, 0);
      Symbolic.B_label "__dark_run_child_argv_ready";
      Symbolic.Label "__dark_run_child_first_argv";
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X19);
      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X24);
      Symbolic.Label "__dark_run_child_argv_ready";
      Symbolic.MOV_reg (Symbolic.X2, Symbolic.X26);
    ]
  @ syscall 221
  @ [
      Symbolic.Label "__dark_run_child_fail";
      zero Symbolic.X10;
      Symbolic.SUB_reg (Symbolic.X10, Symbolic.X10, Symbolic.X0);
      Symbolic.STR (Symbolic.X10, Symbolic.SP, 80);
    ]
  @ pairFd 16 32
  @ [
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 80);
      Symbolic.MOVZ (Symbolic.X2, 8, 0);
    ]
  @ syscall 64
  @ [ Symbolic.MOVZ (Symbolic.X0, 127, 0) ]
  @ syscall 93
  @ [ Symbolic.Label "__dark_run_pipeline_producer" ]
  @ pairFd 112 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 1, 0); zero Symbolic.X2 ]
  @ syscall 24 @ pairFd 8 32
  @ [ Symbolic.MOVZ (Symbolic.X1, 2, 0); zero Symbolic.X2 ]
  @ syscall 24 @ closeFd 0 0 @ closeFd 0 32 @ closeFd 8 0 @ closeFd 8 32
  @ closeFd 16 0 @ closeFd 112 0 @ closeFd 112 32
  @ [
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X19);
      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X24);
      Symbolic.MOV_reg (Symbolic.X2, Symbolic.X26);
    ]
  @ syscall 221
  @ [ Symbolic.B_label "__dark_run_child_fail" ]
