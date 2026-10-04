val padString : string -> int -> bytes
val uint32ToBytes : int32 -> bytes
val uint64ToBytes : int64 -> bytes
val serializeMachHeader : Binary.machHeader -> bytes
val serializeSection64 : Binary.section64 -> bytes
val serializeSegmentCommand64 : Binary.segmentCommand64 -> bytes
val serializeMainCommand : Binary.mainCommand -> bytes
val serializeDylinkerCommand : Binary.dylinkerCommand -> bytes
val serializeDylibCommand : Binary.dylibCommand -> bytes
val serializeUuidCommand : Binary.uuidCommand -> bytes
val serializeBuildVersionCommand : Binary.buildVersionCommand -> bytes
val serializeSymtabCommand : Binary.symtabCommand -> bytes
val serializeDysymtabCommand : Binary.dysymtabCommand -> bytes
val calculateCommandsSize : Binary.machOBinary -> int32
val serializeMachO : Binary.machOBinary -> bytes
val createFloatData : LiteralPool.floatPool -> bytes
val createStringData : LiteralPool.stringPool -> bytes * int StringOrder.Map.t
val createExecutableWithPools : int32 array -> LiteralPool.stringPool -> LiteralPool.floatPool -> bool -> bytes
val createExecutableWithStrings : int32 array -> LiteralPool.stringPool -> bytes
val createExecutable : int32 array -> bytes
val createExecutableWithCoverage : int32 array -> LiteralPool.stringPool -> LiteralPool.floatPool -> int -> bool -> bytes
val writeToFile : string -> bytes -> (unit,string) result
