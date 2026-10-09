(* Build a minimal ARM64 context for native runtime execution checks. *)
open Dark_compiler

let context source target enabled : ARM64CodeGenTypes.codeGenContext =
  {
    ARM64CodeGenTypes.target;
    options =
      {
        ARM64CodeGenTypes.defaultOptions with
        ARM64CodeGenTypes.enableLeakCheck = enabled;
      };
    sumShapeRegistry = StringOrder.Map.empty;
    recordRegistry = StringOrder.Map.empty;
    rawSlotInitRetainTargets = None;
    closurePayloadSizes = StringOrder.Map.empty;
    closureCaptureTypes = StringOrder.Map.empty;
    functionNames = FunctionIdMap.empty;
    functionName = source;
    instructionSite = source;
    stackSize = 0;
    usedCalleeSaved = [];
    usedCalleeSavedF = [];
    heapOverflowLabel = source;
    recordLirOpExpansion = None;
  }
