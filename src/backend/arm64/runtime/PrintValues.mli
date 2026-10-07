val generatePrintInt64NoExit : ARM64.targetConfig -> ARM64.instr list
val generatePrintUInt64NoExit : ARM64.targetConfig -> ARM64.instr list
val generatePrintInt64ToStderrNoExit : ARM64.targetConfig -> ARM64.instr list
val generatePrintBoolNoExit : ARM64.targetConfig -> ARM64.instr list
val generatePrintInt64NoNewline : ARM64.targetConfig -> ARM64.instr list
val generatePrintUInt64NoNewline : ARM64.targetConfig -> ARM64.instr list
val generatePrintBoolNoNewline : ARM64.targetConfig -> ARM64.instr list
val generatePrintFloatNoNewline : ARM64.targetConfig -> ARM64.instr list
val generatePrintStringNoNewline : ARM64.targetConfig -> ARM64.instr list
val generatePrintChars : ARM64.targetConfig -> int list -> ARM64.instr list

val generatePrintCharsToStderr :
  ARM64.targetConfig -> int list -> ARM64.instr list

val generatePrintBlob : ARM64.targetConfig -> ARM64.instr list
val generateWriteSyscall : ARM64.targetConfig -> ARM64.instr list
