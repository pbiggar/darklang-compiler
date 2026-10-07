val prettyPrintCanonicalBufferKind : MemoryModel.canonicalBufferKind -> string
val formatANF : ANF.program -> string
val formatANFFunction : string FunctionIdMap.t -> ANF.functionDef -> string
val formatANFDump : string option -> bool -> ANF.program -> string
