val observe : string -> Yojson.Basic.t
val stringPool : Dark_compiler.LiteralPool.stringPool -> Yojson.Basic.t
val floatPool : Dark_compiler.LiteralPool.floatPool -> Yojson.Basic.t
val prepared : Dark_compiler.ARM64_Encoding.preparedChunk -> Yojson.Basic.t
