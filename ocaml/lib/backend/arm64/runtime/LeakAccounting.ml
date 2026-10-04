(*
   LeakAccounting.fs - Generate allocation accounting and leak reports.
*)
open ARM64CodeGenTypes
open HeapAllocation

let generateLeakCounterInc (ctx: codeGenContext) =
    if ctx.ARM64CodeGenTypes.options.ARM64CodeGenTypes.enableLeakCheck then
        let labelRef = dataLabel leakCounterLabel in
        [
            Symbolic.ADRP (Symbolic.X17, labelRef);
            Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
            Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
            Symbolic.ADD_imm (Symbolic.X16, Symbolic.X16, 1);
            Symbolic.STR (Symbolic.X16, Symbolic.X17, 0);
        ]
    else
        []

let generateLeakCounterDec (ctx: codeGenContext) =
    if ctx.ARM64CodeGenTypes.options.ARM64CodeGenTypes.enableLeakCheck then
        let labelRef = dataLabel leakCounterLabel in
        [
            Symbolic.ADRP (Symbolic.X17, labelRef);
            Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
            Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
            Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
            Symbolic.STR (Symbolic.X16, Symbolic.X17, 0);
        ]
    else
        []

let generateLeakCounterIncIfResultError (ctx: codeGenContext) (resultReg: Symbolic.reg) =
    let leakInc = generateLeakCounterInc ctx in
    if leakInc=[] then
        []
    else
        [
            Symbolic.LDR (Symbolic.X15, resultReg, 0);
            Symbolic.CBZ_offset (Symbolic.X15, List.length leakInc + 1);
        ] @ leakInc

let generateLeakCheckReport (ctx: codeGenContext) =
    if ctx.ARM64CodeGenTypes.options.ARM64CodeGenTypes.enableLeakCheck then
        let prefix = PrintValues.generatePrintCharsToStderr ctx.ARM64CodeGenTypes.target [108; 101; 97; 107; 115; 58; 32] |> runtimeInstrs in
        let printCount = PrintValues.generatePrintInt64ToStderrNoExit ctx.ARM64CodeGenTypes.target |> runtimeInstrs in
        let skipOffset = List.length prefix + 1 + List.length printCount + 1 in
        let labelRef = dataLabel leakCounterLabel in
        [
            Symbolic.ADRP (Symbolic.X17, labelRef);
            Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
            Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
            Symbolic.CBZ_offset (Symbolic.X16, skipOffset);
        ]
        @ prefix
        @ [Symbolic.MOV_reg (Symbolic.X0, Symbolic.X16)]
        @ printCount
    else
        []
