(* Conservatively summarize x64 caller-register writes. *)
type writes = ARM64CalleeClobbers.writes

val all : writes

val summariesWithKnown :
  writes FunctionIdMap.t -> LIR.functionDef list -> writes FunctionIdMap.t

val callWritesForSaves : writes FunctionIdMap.t -> LIR.basicBlock -> writes list
val pruneFunction : writes FunctionIdMap.t -> LIR.functionDef -> LIR.functionDef
