val encodeReg : ARM64.reg -> int32
val encodeFReg : ARM64.fReg -> int32
val encodeWord : ARM64.instr -> ARM64.machineCode
val encode : ARM64.instr -> ARM64.machineCode list
type preparedChunk = {machineCodeTemplate:ARM64.machineCode array;relocations:(int*Symbolic.instr) array;codeLabels:(string*int) array;poolLabelRefs:Symbolic.labelRef array}
val computeLabelPositions : ARM64.instr list -> int StringOrder.Map.t
val computeSymbolicLabelPositions : Symbolic.instr list -> int StringOrder.Map.t
val computeSymbolicLayout : Symbolic.instr list -> int * int StringOrder.Map.t
val getSymbolicCodeSize : Symbolic.instr list -> int
val encodeWithLabels : ARM64.instr -> int -> int StringOrder.Map.t -> int StringOrder.Map.t -> int StringOrder.Map.t -> int StringOrder.Map.t -> ARM64.machineCode
val prepareSymbolicChunk : Symbolic.instr list -> preparedChunk
val combinePreparedChunks : preparedChunk list -> preparedChunk
val getCodeSize : ARM64.instr list -> int
val getFloatPoolSize : LiteralPool.floatPool -> int
val getStringPoolSize : LiteralPool.stringPool -> int
val computeLeakCounterLabel : Platform.os -> int -> int -> int -> int -> int StringOrder.Map.t
val encodePreparedChunksWithPools : preparedChunk list -> LiteralPool.stringPool -> LiteralPool.floatPool -> Platform.os -> bool -> ARM64.machineCode array
val encodeSymbolicWithPools : Symbolic.instr list -> LiteralPool.stringPool -> LiteralPool.floatPool -> Platform.os -> bool -> ARM64.machineCode array
val encodeAllWithPools : ARM64.instr list -> LiteralPool.stringPool -> LiteralPool.floatPool -> Platform.os -> bool -> ARM64.machineCode array
