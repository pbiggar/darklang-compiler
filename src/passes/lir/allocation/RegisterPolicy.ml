(* RegisterPolicy.ml - Select allocatable and preserved registers for the target. *)
[@@@warning "-4"]
(*
   Register Definitions
   Caller-saved registers (X1-X7) - preferred for allocation
   Note: X8 is excluded because StringConcat uses it as a scratch register for byte copying
   Note: X9-X10 are excluded because they are used as compiler scratch registers
*)
let callerSavedRegs = [LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7]
(*
   Callee-saved registers - used when caller-saved exhausted
   These must be saved/restored in function prologue/epilogue
   Note: X27 reserved for free list base (ARM64) / unused (x86_64)
   On x86_64, X22→R14 and X23→R15 are reserved for heap/free list pointers
   x86_64: X22 (R14) = heap ptr, X23 (R15) = free list — not allocatable
   X24-X26 have no x86_64 equivalents
   ARM64: X27/X28 reserved, X19-X26 allocatable.
*)
let calleeSavedRegsFor = function
 | Platform.X86_64 -> [LIR.X19;LIR.X20;LIR.X21]
 | Platform.ARM64 -> [LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26]
(*
   Check if an instruction is a non-tail call (requires SaveRegs/RestoreRegs)
*)
let isNonTailCall = function
 | LIR.MappedAlloc _ | LIR.MappedFree _ | LIR.Call _ | LIR.IndirectCall _ | LIR.ClosureCall _ | LIR.Sleep _ | LIR.CliNative _ -> true
 | _ -> false
(*
   Check if a function has any non-tail calls
   If it does, we prefer callee-saved registers to avoid per-call save/restore overhead
*)
let hasNonTailCalls blocks = Array.exists (fun (block : LIR.basicBlock) -> List.exists isNonTailCall block.LIR.instrs) blocks
(*
   Get the optimal register allocation order based on calling pattern
   - Functions with non-tail calls: prefer callee-saved (save once in prologue/epilogue)
   - Leaf functions / tail-call-only: prefer caller-saved (no prologue/epilogue overhead)
*)
let getAllocatableRegs arch blocks =
 let calleeSaved = calleeSavedRegsFor arch in
 if hasNonTailCalls blocks then calleeSaved @ callerSavedRegs else callerSavedRegs @ calleeSaved
