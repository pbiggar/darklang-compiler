(* Conservatively summarize ARM64 writes across direct calls. *)
type writes = {ints:int64;floats:int64}
val intBit : LIR.physReg -> int64
val floatBit : LIR.physFPReg -> int64
val ofInts : LIR.physReg list -> int64
val ofFloats : LIR.physFPReg list -> int64
val containsInt : LIR.physReg -> writes -> bool
val containsFloat : LIR.physFPReg -> writes -> bool
val all : writes
val summariesWithKnown : writes FunctionIdMap.t -> LIR.functionDef list -> writes FunctionIdMap.t
val summaries : LIR.functionDef list -> writes FunctionIdMap.t
val callWritesForSaves : writes FunctionIdMap.t -> LIR.basicBlock -> writes list
val refineWithCache : (LIR.functionDef -> writes FunctionIdMap.t -> (unit -> LIR.functionDef) -> LIR.functionDef) option -> writes FunctionIdMap.t option -> LIR.functionDef list -> LIR.functionDef list
val refine : LIR.functionDef list -> LIR.functionDef list
