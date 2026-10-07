val uint16ToBytes : int -> bytes
val uint32ToBytes : int32 -> bytes
val uint64ToBytes : int64 -> bytes
val serializeElf64Header : ELF.elf64Header -> bytes
val serializeElf64ProgramHeader : ELF.elf64ProgramHeader -> bytes
val serializeElf : ELF.elfBinary -> bytes
val createFloatData : LiteralPool.floatPool -> bytes
val createStringData : LiteralPool.stringPool -> bytes
val createExecutableWithPools : int32 array -> LiteralPool.stringPool -> LiteralPool.floatPool -> bool -> bytes
val createExecutableWithStrings : int32 array -> LiteralPool.stringPool -> bytes
val createExecutable : int32 array -> bytes
val createExecutableWithCoverage : int32 array -> LiteralPool.stringPool -> LiteralPool.floatPool -> int -> bool -> bytes
val writeToFile : string -> bytes -> (unit,string) result
