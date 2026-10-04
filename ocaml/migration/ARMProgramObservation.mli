(* Complete program chunks, helper/cache arguments, callbacks and emitted binaries. *)
val observe : string -> Yojson.Basic.t
val observeEmit : string -> Yojson.Basic.t
val metadata : Dark_compiler.ARM64CodeGenTypes.arm64ProgramMetadata -> Yojson.Basic.t
val helperKey : Dark_compiler.Backend_Arm64_CodeGen.helperCacheKey -> Yojson.Basic.t
val chunk : Dark_compiler.Backend_Arm64_CodeGen.generatedChunk -> Yojson.Basic.t
