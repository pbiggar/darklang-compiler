// ProcessLifecycle.fs - Generate process lifecycle and argument-vector runtime helpers.

module ARM64ProcessLifecycle

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

/// Generate heap initialization code for _start function
/// Uses mmap to allocate 512MB of heap space and initializes X27/X28
///
/// Memory layout:
///   X27 -> [free list heads: 256 bytes (32 entries × 8 bytes)]
///   X28 -> [heap allocation area: mapped heap after free list heads]
///
/// Free list heads are indexed by (totalSize / 8), where totalSize includes
/// the 8-byte ref count. Size class 0 and 1 are unused (too small).
/// Size class 2 = 16 bytes, class 3 = 24 bytes, etc.
///
/// X27 is the base for free list heads (constant after init)
/// X28 is the bump pointer for new allocations
let generateHeapInit (target: ARM64.TargetConfig) : ARM64Symbolic.Instr list =
    let freeListSize = 256
    let os = ARM64.targetOS target
    let syscalls = ARM64.targetSyscalls target
    let mmapFlags =
        match os with
        | Platform.MacOS -> 0x1002us  // MAP_PRIVATE | MAP_ANON
        | Platform.Linux -> 0x22us    // MAP_PRIVATE | MAP_ANONYMOUS
    let heapSizeForMmap = loadImmediate ARM64Symbolic.X1 heapMmapSizeBytes
    [
        // mmap(NULL, 512MB, PROT_READ|PROT_WRITE, flags, -1, 0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)              // addr = NULL
    ]
    @ heapSizeForMmap
    @ [
        ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 3us, 0)              // PROT_READ | PROT_WRITE
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, mmapFlags, 0)        // flags
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)              // X4 = 0
        ARM64Symbolic.MVN (ARM64Symbolic.X4, ARM64Symbolic.X4)             // X4 = ~0 = -1 (fd)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)              // offset = 0
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Mmap, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
        // Check for mmap failure (returns -1 on error)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0us, 0)             // X15 = 0
        ARM64Symbolic.MVN (ARM64Symbolic.X15, ARM64Symbolic.X15)           // X15 = -1
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X15)        // Compare X0 with -1
        ARM64Symbolic.B_cond (ARM64Symbolic.NE, 3)                 // Skip exit if not error (+3 instructions)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)              // exit code = 1
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
        // X0 now contains mmap result (valid address)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X27, ARM64Symbolic.X0)        // X27 = free list heads base
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X27, uint16 freeListSize)  // X28 = heap start
        // No need to zero free list - MAP_ANONYMOUS provides zeroed pages
    ]

/// Linux AArch64 shell runner. Generated binaries remain libc-free, and both
/// redirected streams are made nonblocking and drained on every wait probe.
/// Return argv[index + 1] as a boxed Option<String>. Native argv entries are
/// zero-terminated bytes, so present values are copied into managed Dark strings.
/// The root _start frame terminates the normal frame-pointer chain. Its initial
/// stack layout keeps argc at +16, argv[0] at +24, and the first positional
/// argument at +32, so no register is reserved
/// for CLI state between calls.
let internal generateCliArgvHelper (ctx: CodeGenContext) (label: string) : ARM64Symbolic.Instr list =
    let missingLabel = $"{label}_missing"
    let lengthLabel = $"{label}_length"
    let lengthDoneLabel = $"{label}_length_done"
    let copyLabel = $"{label}_copy"
    let copyDoneLabel = $"{label}_copy_done"
    let boxLabel = $"{label}_box"
    [ ARM64Symbolic.Label label
      ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
      ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, missingLabel)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X29)
      ARM64Symbolic.Label $"{label}_find_root"
      ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X1, 0s)
      ARM64Symbolic.CBZ (ARM64Symbolic.X2, $"{label}_root_found")
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X2)
      ARM64Symbolic.B_label $"{label}_find_root"
      ARM64Symbolic.Label $"{label}_root_found"
      ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X1, 16s)
      ARM64Symbolic.SUB_imm (ARM64Symbolic.X2, ARM64Symbolic.X2, 1us)
      ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X2)
      ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, missingLabel)
      ARM64Symbolic.LSL_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 3)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X2)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 32us)
      ARM64Symbolic.LDR (ARM64Symbolic.X3, ARM64Symbolic.X1, 0s)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X5, ARM64Symbolic.X3)
      ARM64Symbolic.Label lengthLabel
      ARM64Symbolic.LDRB_imm (ARM64Symbolic.X6, ARM64Symbolic.X5, 0)
      ARM64Symbolic.CBZ (ARM64Symbolic.X6, lengthDoneLabel)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 1us)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.X5, 1us)
      ARM64Symbolic.B_label lengthLabel
      ARM64Symbolic.Label lengthDoneLabel
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X7, ARM64Symbolic.X28)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 1us, 0)
      ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X7, 0s)
      ARM64Symbolic.STR (ARM64Symbolic.X4, ARM64Symbolic.X7, 8s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X4, 7us)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 3us, 0)
      ARM64Symbolic.LSR_reg (ARM64Symbolic.X9, ARM64Symbolic.X9, ARM64Symbolic.X10)
      ARM64Symbolic.LSL_reg (ARM64Symbolic.X9, ARM64Symbolic.X9, ARM64Symbolic.X10)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X9, 16us)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X10)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X5, ARM64Symbolic.X3)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X7, 16us)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X4)
      ARM64Symbolic.Label copyLabel
      ARM64Symbolic.CBZ (ARM64Symbolic.X2, copyDoneLabel)
      ARM64Symbolic.LDRB_imm (ARM64Symbolic.X1, ARM64Symbolic.X5, 0)
      ARM64Symbolic.STRB_reg (ARM64Symbolic.X1, ARM64Symbolic.X10)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.X5, 1us)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
      ARM64Symbolic.SUB_imm (ARM64Symbolic.X2, ARM64Symbolic.X2, 1us)
      ARM64Symbolic.B_label copyLabel
      ARM64Symbolic.Label copyDoneLabel ]
    @ generateLeakCounterInc ctx
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
        ARM64Symbolic.B_label boxLabel
        ARM64Symbolic.Label missingLabel
        ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 1us, 0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X7, 0us, 0)
        ARM64Symbolic.Label boxLabel
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X28)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
        ARM64Symbolic.STR (ARM64Symbolic.X6, ARM64Symbolic.X0, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X7, ARM64Symbolic.X0, 8s)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 1us, 0)
        ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X0, 16s) ]
    @ generateLeakCounterInc ctx
    @ [ ARM64Symbolic.RET ]

/// Start a shell-language command and retain its pid and descriptors in the
/// fixed process table rooted at X25.
let internal generateLinuxCliSpawnProcessHelper () : ARM64Symbolic.Instr list =
    let syscall number =
        [ ARM64Symbolic.MOVZ (ARM64Symbolic.X8, number, 0)
          ARM64Symbolic.SVC 0us ]
    let zero reg = ARM64Symbolic.MOVZ (reg, 0us, 0)
    let pairFd slot shift =
        [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, slot)
          ARM64Symbolic.LSR_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, shift)
          ARM64Symbolic.AND_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 0xffffffffUL) ]
    let closeFd slot shift = pairFd slot shift @ syscall 57us
    let setNonblocking slot =
        pairFd slot 0
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 4us, 0)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2048us, 0) ]
        @ syscall 25us
    [ ARM64Symbolic.Label "__dark_cli_spawn_process"
      ARM64Symbolic.STP_pre (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X29, ARM64Symbolic.SP)
      ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, -16s)
      ARM64Symbolic.STP_pre (ARM64Symbolic.X21, ARM64Symbolic.X22, ARM64Symbolic.SP, -16s)
      ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 80us)
      ARM64Symbolic.CBNZ (ARM64Symbolic.X25, "__dark_spawn_table_ready")
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X25, ARM64Symbolic.X28)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 4096us, 0)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X9)
      ARM64Symbolic.Label "__dark_spawn_table_ready"
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X28)
      ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X0, 16us)
      zero ARM64Symbolic.X12
      ARM64Symbolic.Label "__dark_spawn_command_copy"
      ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X10)
      ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, "__dark_spawn_command_copied")
      ARM64Symbolic.LDRB (ARM64Symbolic.X13, ARM64Symbolic.X11, ARM64Symbolic.X12)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X19, ARM64Symbolic.X12)
      ARM64Symbolic.STRB_reg (ARM64Symbolic.X13, ARM64Symbolic.X14)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
      ARM64Symbolic.B_label "__dark_spawn_command_copy"
      ARM64Symbolic.Label "__dark_spawn_command_copied"
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X19, ARM64Symbolic.X10)
      zero ARM64Symbolic.X13
      ARM64Symbolic.STRB_reg (ARM64Symbolic.X13, ARM64Symbolic.X14)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 8us)
      ARM64Symbolic.LSR_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 3)
      ARM64Symbolic.LSL_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 3)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X10)
      // Recover inherited envp from the root frame.
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X29)
      ARM64Symbolic.Label "__dark_spawn_find_root"
      ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X14, 0s)
      ARM64Symbolic.CBZ (ARM64Symbolic.X13, "__dark_spawn_root_found")
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X13)
      ARM64Symbolic.B_label "__dark_spawn_find_root"
      ARM64Symbolic.Label "__dark_spawn_root_found"
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 24us)
      ARM64Symbolic.Label "__dark_spawn_find_envp"
      ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X14, 0s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 8us)
      ARM64Symbolic.CBNZ (ARM64Symbolic.X13, "__dark_spawn_find_envp")
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X22, ARM64Symbolic.X14)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.SP, 0us)
      zero ARM64Symbolic.X1 ]
    @ syscall 59us
    @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_spawn_failed")
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.SP, 8us)
        zero ARM64Symbolic.X1 ]
    @ syscall 59us
    @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_spawn_failed_close_stdin")
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.SP, 16us)
        zero ARM64Symbolic.X1 ]
    @ syscall 59us
    @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_spawn_failed_close_output")
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 17us, 0)
        zero ARM64Symbolic.X1; zero ARM64Symbolic.X2; zero ARM64Symbolic.X3; zero ARM64Symbolic.X4 ]
    @ syscall 220us
    @ [ ARM64Symbolic.CBZ (ARM64Symbolic.X0, "__dark_spawn_child")
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_spawn_failed_close_all")
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X21, ARM64Symbolic.X0) ]
    @ closeFd 0s 0 @ closeFd 8s 32 @ closeFd 16s 32
    @ setNonblocking 8s @ setNonblocking 16s
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
        ARM64Symbolic.Label "__dark_spawn_find_slot"
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 63us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, "__dark_spawn_no_slot")
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X10, ARM64Symbolic.X0, 6)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X10, ARM64Symbolic.X25, ARM64Symbolic.X10)
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X10, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X9, "__dark_spawn_slot_found")
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
        ARM64Symbolic.B_label "__dark_spawn_find_slot"
        ARM64Symbolic.Label "__dark_spawn_slot_found"
        ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 1us, 0)
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X10, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X21, ARM64Symbolic.X10, 8s)
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 0s)
        ARM64Symbolic.LSR_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 32)
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X10, 16s)
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 8s)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 0xffffffffUL)
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X10, 24s)
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 0xffffffffUL)
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X10, 32s)
        // Keep raw accumulated output outside the managed object graph. Each
        // communicate call copies only its newly-read suffix into immutable
        // managed strings; terminate copies the complete buffers.
        ARM64Symbolic.STR (ARM64Symbolic.X28, ARM64Symbolic.X10, 48s)
        zero ARM64Symbolic.X9
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X28, 0s) ]
    @ loadImmediate ARM64Symbolic.X9 1048584L
    @ [ ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X9)
        ARM64Symbolic.STR (ARM64Symbolic.X28, ARM64Symbolic.X10, 56s)
        zero ARM64Symbolic.X9
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X28, 0s) ]
    @ loadImmediate ARM64Symbolic.X9 1048584L
    @ [ ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X9)
        ARM64Symbolic.B_label "__dark_spawn_return"
        ARM64Symbolic.Label "__dark_spawn_no_slot"
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X21)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 9us, 0) ]
    @ syscall 129us
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.MVN (ARM64Symbolic.X0, ARM64Symbolic.X0)
        ARM64Symbolic.B_label "__dark_spawn_return"
        ARM64Symbolic.Label "__dark_spawn_failed_close_all" ]
    @ closeFd 16s 0 @ closeFd 16s 32
    @ [ ARM64Symbolic.Label "__dark_spawn_failed_close_output" ]
    @ closeFd 8s 0 @ closeFd 8s 32
    @ [ ARM64Symbolic.Label "__dark_spawn_failed_close_stdin" ]
    @ closeFd 0s 0 @ closeFd 0s 32
    @ [ ARM64Symbolic.Label "__dark_spawn_failed"
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.MVN (ARM64Symbolic.X0, ARM64Symbolic.X0)
        ARM64Symbolic.Label "__dark_spawn_return"
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 80us)
        ARM64Symbolic.LDP_post (ARM64Symbolic.X21, ARM64Symbolic.X22, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.LDP_post (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.RET
        ARM64Symbolic.Label "__dark_spawn_child" ]
    @ pairFd 0s 0
    @ [ zero ARM64Symbolic.X1; zero ARM64Symbolic.X2 ]
    @ syscall 24us
    @ pairFd 8s 32
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 1us, 0); zero ARM64Symbolic.X2 ]
    @ syscall 24us
    @ pairFd 16s 32
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 2us, 0); zero ARM64Symbolic.X2 ]
    @ syscall 24us
    @ closeFd 0s 0 @ closeFd 0s 32 @ closeFd 8s 0 @ closeFd 8s 32 @ closeFd 16s 0 @ closeFd 16s 32
    @ loadStringLiteralPointer ARM64Symbolic.X20 "/bin/bash"
    @ loadStringLiteralPointer ARM64Symbolic.X9 "-c"
    @ [ ARM64Symbolic.ADD_imm (ARM64Symbolic.X20, ARM64Symbolic.X20, 16us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 16us)
        ARM64Symbolic.STR (ARM64Symbolic.X20, ARM64Symbolic.SP, 32s)
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 40s)
        ARM64Symbolic.STR (ARM64Symbolic.X19, ARM64Symbolic.SP, 48s)
        zero ARM64Symbolic.X9
        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 56s)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X20)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 32us)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X22) ]
    @ syscall 221us
    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 127us, 0) ]
    @ syscall 93us

/// Communicate with and terminate children tracked by the X25 process table.
let internal generateLinuxCliProcessLifecycleHelpers (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    let syscall number =
        [ ARM64Symbolic.MOVZ (ARM64Symbolic.X8, number, 0)
          ARM64Symbolic.SVC 0us ]
    let zero reg = ARM64Symbolic.MOVZ (reg, 0us, 0)
    let finalizeString buffer lengthReg =
        [ ARM64Symbolic.STR (lengthReg, buffer, 8s)
          ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 1us, 0)
          ARM64Symbolic.STR (ARM64Symbolic.X10, buffer, 0s)
        ]
    let epilogue =
        [ ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 64us)
          ARM64Symbolic.LDP_post (ARM64Symbolic.X23, ARM64Symbolic.X24, ARM64Symbolic.SP, 16s)
          ARM64Symbolic.LDP_post (ARM64Symbolic.X21, ARM64Symbolic.X22, ARM64Symbolic.SP, 16s)
          ARM64Symbolic.LDP_post (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, 16s)
          ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
          ARM64Symbolic.RET ]
    let invalidOutcome label =
        loadStringLiteralPointer ARM64Symbolic.X8 ""
        @ loadStringLiteralPointer ARM64Symbolic.X9 "Process not found"
        @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X28)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 32us)
            zero ARM64Symbolic.X10
            ARM64Symbolic.MVN (ARM64Symbolic.X10, ARM64Symbolic.X10)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s)
            ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.X0, 8s)
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X0, 16s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 1us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X0, 24s) ]
        @ generateLeakCounterInc ctx
        @ [ ARM64Symbolic.B_label label ]
    let communicate =
        [ ARM64Symbolic.Label "__dark_cli_process_io"
          ARM64Symbolic.STP_pre (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.MOV_reg (ARM64Symbolic.X29, ARM64Symbolic.SP)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X21, ARM64Symbolic.X22, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X23, ARM64Symbolic.X24, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 64us)
          ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X0)
          ARM64Symbolic.MOV_reg (ARM64Symbolic.X20, ARM64Symbolic.X1)
          ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.SP, 24s)
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X19, 1us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_process_io_invalid")
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X19, 63us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, "__dark_process_io_invalid")
          ARM64Symbolic.LSL_imm (ARM64Symbolic.X9, ARM64Symbolic.X19, 6)
          ARM64Symbolic.ADD_reg (ARM64Symbolic.X21, ARM64Symbolic.X25, ARM64Symbolic.X9)
          ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
          ARM64Symbolic.CBZ (ARM64Symbolic.X9, "__dark_process_io_invalid")
          ARM64Symbolic.LDR (ARM64Symbolic.X22, ARM64Symbolic.X21, 48s)
          ARM64Symbolic.LDR (ARM64Symbolic.X23, ARM64Symbolic.X21, 56s)
          ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X22, 0s)
          ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 0s)
          ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X23, 0s)
          ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 8s)
          ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 24s)
          ARM64Symbolic.CBZ (ARM64Symbolic.X9, "__dark_process_io_suffix_ready")
          zero ARM64Symbolic.X9
          ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 0s)
          ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 8s)
          ARM64Symbolic.Label "__dark_process_io_suffix_ready"
          zero ARM64Symbolic.X24
          // Write input plus the interpreter's WriteLine newline.
          ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X20, 8s)
          ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.SP, 16s)
          ARM64Symbolic.CBZ (ARM64Symbolic.X2, "__dark_process_io_read_stdout")
          ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 16s)
          ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X20, 16us) ]
        @ syscall 64us
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 10us, 0)
            ARM64Symbolic.STRB (ARM64Symbolic.X9, ARM64Symbolic.SP, 56)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 16s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 56us)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0) ]
        @ syscall 64us
        @ [ ARM64Symbolic.Label "__dark_process_io_read_stdout"
            ARM64Symbolic.LDR (ARM64Symbolic.X19, ARM64Symbolic.X22, 0s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X22, 8us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X19) ]
        @ loadImmediate ARM64Symbolic.X2 1048576L
        @ [ ARM64Symbolic.SUB_reg (ARM64Symbolic.X2, ARM64Symbolic.X2, ARM64Symbolic.X19)
            ARM64Symbolic.CBZ (ARM64Symbolic.X2, "__dark_process_io_read_stderr")
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 24s) ]
        @ syscall 63us
        @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, "__dark_process_io_read_stderr")
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X19, ARM64Symbolic.X19, ARM64Symbolic.X0)
            ARM64Symbolic.STR (ARM64Symbolic.X19, ARM64Symbolic.X22, 0s)
            ARM64Symbolic.B_label "__dark_process_io_read_stdout"
            ARM64Symbolic.Label "__dark_process_io_read_stderr"
            ARM64Symbolic.LDR (ARM64Symbolic.X20, ARM64Symbolic.X23, 0s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X23, 8us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X20) ]
        @ loadImmediate ARM64Symbolic.X2 1048576L
        @ [ ARM64Symbolic.SUB_reg (ARM64Symbolic.X2, ARM64Symbolic.X2, ARM64Symbolic.X20)
            ARM64Symbolic.CBZ (ARM64Symbolic.X2, "__dark_process_io_status")
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 32s) ]
        @ syscall 63us
        @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, "__dark_process_io_status")
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X20, ARM64Symbolic.X20, ARM64Symbolic.X0)
            ARM64Symbolic.STR (ARM64Symbolic.X20, ARM64Symbolic.X23, 0s)
            ARM64Symbolic.B_label "__dark_process_io_read_stderr"
            ARM64Symbolic.Label "__dark_process_io_status"
            ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 2us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, "__dark_process_io_stored_status")
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 8s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 32us)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
            zero ARM64Symbolic.X3
            // wait4 writes a 32-bit status. Clear the full stack word so the
            // 64-bit decode below cannot observe stale upper bytes.
            zero ARM64Symbolic.X9
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 32s) ]
        @ syscall 260us
        @ [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_process_io_read_stdout")
            ARM64Symbolic.CBNZ (ARM64Symbolic.X0, "__dark_process_io_finished")
            ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X9, "__dark_process_io_running")
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X24, ARM64Symbolic.X24, 1us)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X24, 100us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, "__dark_process_io_running")
            zero ARM64Symbolic.X9
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 40s) ]
        @ loadImmediate ARM64Symbolic.X9 100000000L
        @ [ ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.SP, 40us)
            zero ARM64Symbolic.X1 ]
        @ syscall 101us
        @ [ ARM64Symbolic.B_label "__dark_process_io_read_stdout"
            ARM64Symbolic.Label "__dark_process_io_finished"
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.AND_imm (ARM64Symbolic.X11, ARM64Symbolic.X10, 0x7fUL)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X11, "__dark_process_io_signaled")
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X24, ARM64Symbolic.X10, 8)
            ARM64Symbolic.B_label "__dark_process_io_store_status"
            ARM64Symbolic.Label "__dark_process_io_signaled"
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X24, ARM64Symbolic.X11, 128us)
            ARM64Symbolic.Label "__dark_process_io_store_status"
            ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 2us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
            ARM64Symbolic.STR (ARM64Symbolic.X24, ARM64Symbolic.X21, 40s)
            ARM64Symbolic.B_label "__dark_process_io_build"
            ARM64Symbolic.Label "__dark_process_io_stored_status"
            ARM64Symbolic.LDR (ARM64Symbolic.X24, ARM64Symbolic.X21, 40s)
            ARM64Symbolic.B_label "__dark_process_io_build"
            ARM64Symbolic.Label "__dark_process_io_running"
            zero ARM64Symbolic.X24
            ARM64Symbolic.Label "__dark_process_io_build"
            // Copy the newly-read suffix (or the full accumulation for the
            // terminate caller) into immutable managed strings.
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X28) ]
        @ loadImmediate ARM64Symbolic.X9 1048592L
        @ [ ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X9)
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X22, 0s)
            ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.SUB_reg (ARM64Symbolic.X10, ARM64Symbolic.X10, ARM64Symbolic.X11)
            zero ARM64Symbolic.X12
            ARM64Symbolic.Label "__dark_process_io_copy_stdout"
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X10)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, "__dark_process_io_stdout_copied")
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X22, 8us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X11)
            ARM64Symbolic.LDRB (ARM64Symbolic.X14, ARM64Symbolic.X13, ARM64Symbolic.X12)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X19, 16us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X12)
            ARM64Symbolic.STRB_reg (ARM64Symbolic.X14, ARM64Symbolic.X15)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
            ARM64Symbolic.B_label "__dark_process_io_copy_stdout"
            ARM64Symbolic.Label "__dark_process_io_stdout_copied"
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X20, ARM64Symbolic.X28)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.SP, 0s) ]
        @ loadImmediate ARM64Symbolic.X9 1048592L
        @ [ ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X9)
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X23, 0s)
            ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 8s)
            ARM64Symbolic.SUB_reg (ARM64Symbolic.X10, ARM64Symbolic.X10, ARM64Symbolic.X11)
            zero ARM64Symbolic.X12
            ARM64Symbolic.Label "__dark_process_io_copy_stderr"
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X10)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, "__dark_process_io_stderr_copied")
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X23, 8us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X11)
            ARM64Symbolic.LDRB (ARM64Symbolic.X14, ARM64Symbolic.X13, ARM64Symbolic.X12)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X20, 16us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X12)
            ARM64Symbolic.STRB_reg (ARM64Symbolic.X14, ARM64Symbolic.X15)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
            ARM64Symbolic.B_label "__dark_process_io_copy_stderr"
            ARM64Symbolic.Label "__dark_process_io_stderr_copied"
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.SP, 8s) ]
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 0s) ]
        @ finalizeString ARM64Symbolic.X19 ARM64Symbolic.X10
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 8s) ]
        @ finalizeString ARM64Symbolic.X20 ARM64Symbolic.X10
        @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X28)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 32us)
            ARM64Symbolic.STR (ARM64Symbolic.X24, ARM64Symbolic.X0, 0s)
            ARM64Symbolic.STR (ARM64Symbolic.X19, ARM64Symbolic.X0, 8s)
            ARM64Symbolic.STR (ARM64Symbolic.X20, ARM64Symbolic.X0, 16s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 1us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X0, 24s) ]
        @ generateLeakCounterInc ctx
        @ generateLeakCounterInc ctx
        @ generateLeakCounterInc ctx
        @ [ ARM64Symbolic.B_label "__dark_process_io_return"
            ARM64Symbolic.Label "__dark_process_io_invalid" ]
        @ invalidOutcome "__dark_process_io_return"
        @ [ ARM64Symbolic.Label "__dark_process_io_return" ]
        @ epilogue
    let terminate =
        [ ARM64Symbolic.Label "__dark_cli_terminate_process"
          ARM64Symbolic.STP_pre (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.MOV_reg (ARM64Symbolic.X29, ARM64Symbolic.SP)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X21, ARM64Symbolic.X22, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.STP_pre (ARM64Symbolic.X23, ARM64Symbolic.X24, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 64us)
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 1us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, "__dark_terminate_invalid")
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 63us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, "__dark_terminate_invalid")
          ARM64Symbolic.LSL_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 6)
          ARM64Symbolic.ADD_reg (ARM64Symbolic.X21, ARM64Symbolic.X25, ARM64Symbolic.X9)
          ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
          ARM64Symbolic.CBZ (ARM64Symbolic.X9, "__dark_terminate_invalid")
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 2us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, "__dark_terminate_collect")
          ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 8s)
          ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 15us, 0) ]
        @ syscall 129us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 8s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 32us)
            zero ARM64Symbolic.X2
            zero ARM64Symbolic.X3
            // wait4 writes only the low 32 bits of the status slot.
            zero ARM64Symbolic.X9
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 32s) ]
        @ syscall 260us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.AND_imm (ARM64Symbolic.X11, ARM64Symbolic.X10, 0x7fUL)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X11, "__dark_terminate_signaled")
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X9, ARM64Symbolic.X10, 8)
            ARM64Symbolic.B_label "__dark_terminate_store"
            ARM64Symbolic.Label "__dark_terminate_signaled"
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X11, 128us)
            ARM64Symbolic.Label "__dark_terminate_store"
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X21, 40s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 2us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
            ARM64Symbolic.Label "__dark_terminate_collect"
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X0, ARM64Symbolic.X9, 6) // overwritten below; keep X0 defined
            zero ARM64Symbolic.X1 ]
        @ [ // Call communicate with empty input to drain and box the outcome.
            // Recover handle from slot address rather than the allocation counter.
            ARM64Symbolic.SUB_reg (ARM64Symbolic.X0, ARM64Symbolic.X21, ARM64Symbolic.X25)
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 6) ]
        @ loadStringLiteralPointer ARM64Symbolic.X1 ""
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
            ARM64Symbolic.BL "__dark_cli_process_io"
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X0)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 16s) ]
        @ syscall 57us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 24s) ]
        @ syscall 57us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, 32s) ]
        @ syscall 57us
        @ [ zero ARM64Symbolic.X9
            ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X21, 0s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X19)
            ARM64Symbolic.B_label "__dark_terminate_return"
            ARM64Symbolic.Label "__dark_terminate_invalid" ]
        @ invalidOutcome "__dark_terminate_return"
        @ [ ARM64Symbolic.Label "__dark_terminate_return" ]
        @ epilogue
    let cleanup =
        [ ARM64Symbolic.Label "__dark_cli_cleanup_processes"
          ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
          ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
          ARM64Symbolic.CBZ (ARM64Symbolic.X25, "__dark_cleanup_process_done")
          ARM64Symbolic.MOVZ (ARM64Symbolic.X19, 1us, 0)
          ARM64Symbolic.Label "__dark_cleanup_process_next"
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X19, 63us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, "__dark_cleanup_process_done")
          ARM64Symbolic.LSL_imm (ARM64Symbolic.X9, ARM64Symbolic.X19, 6)
          ARM64Symbolic.ADD_reg (ARM64Symbolic.X9, ARM64Symbolic.X25, ARM64Symbolic.X9)
          ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
          ARM64Symbolic.CBZ (ARM64Symbolic.X10, "__dark_cleanup_process_advance")
          ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X9, 8s)
          ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 9us, 0) ]
        @ syscall 129us
        @ [ ARM64Symbolic.LSL_imm (ARM64Symbolic.X9, ARM64Symbolic.X19, 6)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X9, ARM64Symbolic.X25, ARM64Symbolic.X9)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X9, 8s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
            zero ARM64Symbolic.X2
            zero ARM64Symbolic.X3 ]
        @ syscall 260us
        @ [ ARM64Symbolic.LSL_imm (ARM64Symbolic.X9, ARM64Symbolic.X19, 6)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X9, ARM64Symbolic.X25, ARM64Symbolic.X9)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X9, 16s) ]
        @ syscall 57us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X9, 24s) ]
        @ syscall 57us
        @ [ ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X9, 32s) ]
        @ syscall 57us
        @ [ zero ARM64Symbolic.X10
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
            ARM64Symbolic.Label "__dark_cleanup_process_advance"
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X19, ARM64Symbolic.X19, 1us)
            ARM64Symbolic.B_label "__dark_cleanup_process_next"
            ARM64Symbolic.Label "__dark_cleanup_process_done"
            ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X19, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.RET ]
    communicate @ terminate @ cleanup
