type funcCtx={functionName:string;stackSize:int;usedCalleeSaved:LIR.physReg list;enableLeakCheck:bool;recordRegistry:LIR.recordRegistry;sumShapeRegistry:MemoryModel.rcSumShapeRegistry;functionNames:string FunctionIdMap.t}
val functionName : funcCtx -> AST.functionId -> string
val rcSumShapeRegistryFromVariantRegistry : LIR.variantRegistry -> MemoryModel.rcSumShapeRegistry
val genLeakCounterInc : funcCtx -> X86_64.instr list
val genLeakCounterDec : funcCtx -> X86_64.instr list
val genLeakCheckReport : unit -> X86_64.instr list
