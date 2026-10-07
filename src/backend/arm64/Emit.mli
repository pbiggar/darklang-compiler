(* Emit.mli - ARM64 Emission (Encoding + Binary Generation)
   Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
   and produces a platform-specific binary in a single pass. *)
type emitResult={machineCode:ARM64.machineCode array;binary:bytes}
val emitBinary : Backend_Arm64_CodeGen.generatedProgram -> Platform.os -> bool -> (Symbolic.instr list -> (unit -> ARM64_Encoding.preparedChunk) -> ARM64_Encoding.preparedChunk) option -> (Symbolic.instr list list -> (unit -> ARM64_Encoding.preparedChunk) -> ARM64_Encoding.preparedChunk) option -> (string -> float -> unit) option -> emitResult
