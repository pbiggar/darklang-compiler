// Printing.fs - Emit arm64 instructions for printing operations.

module ARM64EmitPrinting

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64Operands
open ARM64InstructionContext

let internal emitPrintBool (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print booleans as "true" or "false" (no exit)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintBoolNoExit ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintBoolNoExit ctx.Target))

let internal emitPrintChars (ctx: CodeGenContext) (chars: byte list) : Result<ARM64Symbolic.Instr list, string> =
    // Print literal characters (for tuple/list delimiters like "(", ", ", ")")
    Ok (runtimeInstrs (ARM64PrintValues.generatePrintChars ctx.Target chars))

let internal emitPrintBlob (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Render the in-process Blob without exposing its payload or identity.
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X19 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintBlob ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintBlob ctx.Target))

let internal emitPrintInt64NoNewline (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print integer without newline (for tuple elements)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target))

let internal emitPrintUInt64NoNewline (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print unsigned integer without newline (for tuple elements)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target))

let internal emitPrintBoolNoNewline (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print boolean without newline (for tuple elements)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target))

let internal emitPrintFloatNoNewline (ctx: CodeGenContext) (freg: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    // Print float without newline (for tuple/list elements)
    lirFRegToARM64FReg freg
    |> Result.map (fun fregARM64 ->
        if fregARM64 <> ARM64Symbolic.D0 then
            [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D0, fregARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target))

let internal emitPrintHeapStringNoNewline (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print heap string without newline (for tuple/list elements)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        // Dynamic buffer layout: [refcount:8][length:8][data:N].
        let loadInstrs = [ARM64Symbolic.LDR (ARM64Symbolic.X10, regARM64, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, regARM64, 16us)]
        let loadAndPrint = loadInstrs @ runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
        if regARM64 <> ARM64Symbolic.X9 then
            loadAndPrint
        else
            // Need to save the original address first
            let saveReg = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, regARM64)]
            let loadFromSaved = [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X11, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X11, 16us)]
            saveReg @ loadFromSaved @ runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target))

let internal emitPrintList (ctx: CodeGenContext) (listPtr: LIR.Reg) (elemType: AST.SemanticType) : Result<ARM64Symbolic.Instr list, string> =
    // Print list as [elem1, elem2, ...]
    // List layout: Nil = 0, Cons = [tag=1, head, tail]
    // Uses X19 for list pointer (callee-saved), X20 for first flag
    lirRegToARM64Reg listPtr
    |> Result.map (fun listReg -> generatePrintListInstrs ctx listReg elemType true)

let internal emitPrintSum (ctx: CodeGenContext) (convertInstr: CodeGenContext -> LIR.Instr -> Result<ARM64Symbolic.Instr list, string>) (sumPtr: LIR.Reg) (variants: (string * int * AST.SemanticType option) list) (transparentInt64: bool) : Result<ARM64Symbolic.Instr list, string> =
    // Print sum type: variant name + optional payload + newline
    // Sum layout depends on whether ANY variant has a payload:
    // - If any payload: [tag, payload] on heap
    // - If all nullary: just the tag value (integer)
    lirRegToARM64Reg sumPtr
    |> Result.map (fun sumReg ->
        let syscalls = ARM64.targetSyscalls ctx.Target

        // Check if any variant has a payload
        let hasAnyPayload = variants |> List.exists (fun (_, _, payload) -> Option.isSome payload)

        // Helper: generate code to print a string literal
        let printLiteral (s: string) =
            let bytes = System.Text.Encoding.UTF8.GetBytes(s)
            if bytes.Length = 0 then []
            else
                let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)] @
                (bytes |> Array.toList |> List.mapi (fun i b ->
                    [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 b, 0); ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, i)]
                ) |> List.concat) @
                [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                 ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                 ARM64Symbolic.MOVZ (ARM64Symbolic.X2, uint16 bytes.Length, 0);
                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                 ARM64Symbolic.SVC syscalls.SvcImmediate;
                 ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)]

        // Setup depends on representation
        let setup =
            if transparentInt64 then
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, sumReg)]
            elif hasAnyPayload then
                // Heap-allocated: X19 = sum pointer, load tag from [X19, 0] into X20
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, sumReg); ARM64Symbolic.LDR (ARM64Symbolic.X20, ARM64Symbolic.X19, 0s)]
            else
                // All nullary: X19 = sum pointer (for consistency), X20 = tag (the value itself)
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, sumReg); ARM64Symbolic.MOV_reg (ARM64Symbolic.X20, sumReg)]

        // Generate code for each variant: compare tag, branch, print name, optionally print payload
        // Structure: for each variant, generate:
        //   CMP X20, #tag
        //   B.NE next_variant
        //   <print variant name>
        //   <if payload: print "(", print payload, print ")">
        //   B end
        // next_variant:
        //   ... (repeat)
        // end:
        //   <print "\n">

        // Pre-calculate code blocks for each variant
        let variantBlocks =
            variants |> List.map (fun (variantName, _tag, payloadType) ->
                let printName = printLiteral variantName
                let printPayload =
                    match payloadType with
                    | None -> []
                    | Some pType ->
                        let printOpen = printLiteral "("
                        let loadPayload =
                            if transparentInt64 then [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X19)]
                            else [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 8s)]
                        let printPayloadValue =
                            match pType with
                            | AST.TInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
                            | AST.TUInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target)
                            | AST.TBool -> runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target)
                            | AST.TFloat64 ->
                                [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target)
                            | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                                [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 16us)] @
                                runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
                            | AST.TList elemType ->
                                match ListDisplay.getDisplayStringFunc elemType with
                                | Some funcName ->
                                    let callToDisplay = [ARM64Symbolic.BL funcName]
                                    let saveDisplayString = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X21, ARM64Symbolic.X0)]
                                    let printString =
                                        [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 16us)] @
                                        runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
                                    let releaseDisplayString =
                                        match convertInstr ctx (LIR.RefCountDecString (LIR.Reg (LIR.Physical LIR.X21))) with
                                        | Ok instrs -> instrs
                                        | Error e -> Crash.crash e
                                    callToDisplay @ saveDisplayString @ printString @ releaseDisplayString
                                | None ->
                                    Crash.crash $"Unsupported list element type in sum variant: {elemType}"
                            | t -> Crash.crash $"Unsupported payload type in sum variant: {t}"
                        let printClose = printLiteral ")"
                        printOpen @ loadPayload @ printPayloadValue @ printClose
                (printName, printPayload))

        // Calculate end label offset from each variant block
        // We'll build the code and calculate offsets manually

        let printNewline = printLiteral "\n"

        // Build variant blocks with branching
        // For each variant: CMP(1) + B.NE(1) + name + payload + B(1) to end
        let blockLengths =
            variants
            |> List.mapi (fun i (_, _tag, _) ->
                let (printName, printPayload) = variantBlocks.[i]
                2 + List.length printName + List.length printPayload + 1)  // CMP + B.NE + name + payload + B

        let totalVariantCodeLen = List.sum blockLengths

        let variantCode =
            variants
            |> List.mapi (fun i (_, tag, _) -> i, tag)
            |> List.mapFold (fun currentPos (i, tag) ->
                let (printName, printPayload) = variantBlocks.[i]
                let blockLen = 2 + List.length printName + List.length printPayload + 1
                // B.NE is at position 1, next block CMP is at position blockLen
                // So offset = blockLen - 1 (forward jump from B.NE to next CMP)
                let nextBlockOffset = blockLen - 1
                let endFromHere = totalVariantCodeLen - currentPos - blockLen + 1  // Jump to after all variant blocks

                let cmpInstr = ARM64Symbolic.CMP_imm (ARM64Symbolic.X20, uint16 tag)
                let branchNeInstr = ARM64Symbolic.B_cond (ARM64Symbolic.NE, nextBlockOffset)  // Skip this variant's code
                let branchEndInstr = ARM64Symbolic.B endFromHere  // Jump to end (after all variant code)

                [cmpInstr; branchNeInstr] @ printName @ printPayload @ [branchEndInstr],
                currentPos + blockLen)
                0
            |> fst
            |> List.concat

        if transparentInt64 then
            match variantBlocks with
            | [(printName, printPayload)] -> setup @ printName @ printPayload @ printNewline
            | _ -> Crash.crash "Transparent Int64 sum must have exactly one case"
        else
            setup @ variantCode @ printNewline)

let internal emitPrintRecord (ctx: CodeGenContext) (recordPtr: LIR.Reg) (typeName: string) (fields: (string * AST.SemanticType) list) : Result<ARM64Symbolic.Instr list, string> =
    // Print record: TypeName { field1 = val1, field2 = val2, ... }\n
    // Record layout: [field0, field1, field2, ...] on heap (each 8 bytes)
    lirRegToARM64Reg recordPtr
    |> Result.map (fun recordReg ->
        let syscalls = ARM64.targetSyscalls ctx.Target

        // Helper: generate code to print a string literal
        let printLiteral (s: string) =
            let bytes = System.Text.Encoding.UTF8.GetBytes(s)
            if bytes.Length = 0 then []
            else
                let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)] @
                (bytes |> Array.toList |> List.mapi (fun i b ->
                    [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 b, 0); ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, i)]
                ) |> List.concat) @
                [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                 ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                 ARM64Symbolic.MOVZ (ARM64Symbolic.X2, uint16 bytes.Length, 0);
                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                 ARM64Symbolic.SVC syscalls.SvcImmediate;
                 ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)]

        // Save record pointer in callee-saved register X19
        let setup = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, recordReg)]

        // Print type name and opening brace
        let printHeader = printLiteral (typeName + " { ")

        // Print each field: "fieldName = value" with ", " separator between fields
        let printFields =
            fields
            |> List.mapi (fun i (fieldName, fieldType) ->
                let printFieldName = printLiteral (fieldName + " = ")
                let offset = int16 (i * 8)  // Each field is 8 bytes
                let loadField = [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, offset)]
                let printValue =
                    match fieldType with
                    | AST.TInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
                    | AST.TUInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target)
                    | AST.TBool -> runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target)
                    | AST.TFloat64 ->
                        [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target)
                    | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                        // String is a pointer: load length, compute data ptr, print
                        [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 16us)] @
                        runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
                    | t -> Crash.crash $"Unsupported field type in record: {t}"
                let separator =
                    if i < List.length fields - 1 then printLiteral ", "
                    else []
                printFieldName @ loadField @ printValue @ separator)
            |> List.concat

        // Print closing brace and newline
        let printFooter = printLiteral " }\n"

        setup @ printHeader @ printFields @ printFooter)

let internal emitPrintInt64 (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Value to print should be in X0 (no exit)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            // Move to X0 if not already there
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintInt64NoExit ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintInt64NoExit ctx.Target))

let internal emitPrintUInt64 (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Value to print should be in X0 (no exit)
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> ARM64Symbolic.X0 then
            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoExit ctx.Target)
        else
            runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoExit ctx.Target))

let internal emitPrintFloat (ctx: CodeGenContext) (freg: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    // Print float value from FP register
    // Value should be in D0 for generatePrintFloat
    lirFRegToARM64FReg freg
    |> Result.map (fun fregARM64 ->
        if fregARM64 <> ARM64Symbolic.D0 then
            // Move to D0 if not already there
            [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D0, fregARM64)] @ runtimeInstrs (ARM64PrintAndExit.generatePrintFloat ctx.Target)
        else
            runtimeInstrs (ARM64PrintAndExit.generatePrintFloat ctx.Target))

let internal emitPrintString (ctx: CodeGenContext) (value: string) : Result<ARM64Symbolic.Instr list, string> =
    // To print a string, we need:
    // 1. ADRP + ADD to load string address into X0
    // 2. Call ARM64PrintAndExit.generatePrintString which handles write syscall
    let len = utf8Len value
    let labelRef = stringDataLabel value
    Ok ([
        ARM64Symbolic.ADRP (ARM64Symbolic.X0, labelRef)  // Load page address of string
        ARM64Symbolic.ADD_label (ARM64Symbolic.X0, ARM64Symbolic.X0, labelRef)  // Add page offset
    ] @ runtimeInstrs (ARM64PrintAndExit.generatePrintString ctx.Target len))

let internal emitPrintHeapString (ctx: CodeGenContext) (reg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Print a dynamic string with [refcount:8][length:8][data:N].
    // Note: The syscall clobbers X0, X1, X2, X8. If the input register is one
    // of these, we save it to X9 before and restore after so subsequent code
    // can still use it.
    // 1. Save input to X9
    // 2. Load length from [X9] into X2
    // 3. Compute data pointer (X9 + 8) into X1
    // 4. Set X0 = 1 (stdout)
    // 5. write syscall
    // 6. Write the result-rendering newline
    // 7. Restore input register if it was clobbered
    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        let isClobbered = regARM64 = ARM64Symbolic.X0 || regARM64 = ARM64Symbolic.X1 || regARM64 = ARM64Symbolic.X2 || regARM64 = ARM64Symbolic.X8
        let restoreInstrs = if isClobbered then [ARM64Symbolic.MOV_reg (regARM64, ARM64Symbolic.X9)] else []
        [
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, regARM64)           // X9 = input (save in case regARM64 is X0/X1/X2)
            ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X9, 8s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X9, 16us)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)                // X0 = stdout fd
        ]
        @ runtimeInstrs (ARM64PrintValues.generateWriteSyscall ctx.Target)
        @ [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 10us, 0)
            ARM64Symbolic.STRB (ARM64Symbolic.X10, ARM64Symbolic.SP, 0)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0) ]
        @ runtimeInstrs (ARM64PrintValues.generateWriteSyscall ctx.Target)
        @ [ ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us) ]
        @ restoreInstrs)
