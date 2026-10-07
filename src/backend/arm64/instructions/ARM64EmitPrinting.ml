(*
   Printing.fs - Emit arm64 instructions for printing operations.
*)
[@@@warning "-4"]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low
let mapFold action state values=
 let rec go state=function [] -> [],state | value::rest -> let value,next=action state value in let rest,state=go next rest in value::rest,state in go state values
let utf8Bytes = Bytes.of_string



open ARM64CodeGenTypes
open HeapAllocation
open ARM64Operands
open ARM64InstructionContext

(*
   Print booleans as "true" or "false" (no exit)
*)
let emitPrintBool (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then
            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintBoolNoExit ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintBoolNoExit ctx.target))

(*
   Print literal characters (for tuple/list delimiters like "(", ", ", ")")
*)
let emitPrintChars (ctx: codeGenContext) (chars: int list) =

    Ok (runtimeInstrs (PrintValues.generatePrintChars ctx.target chars))

(*
   Render the in-process Blob without exposing its payload or identity.
*)
let emitPrintBlob (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X19 then
            [Symbolic.MOV_reg (Symbolic.X19, regARM64)] @ runtimeInstrs (PrintValues.generatePrintBlob ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintBlob ctx.target))

(*
   Print integer without newline (for tuple elements)
*)
let emitPrintInt64NoNewline (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then
            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.target))

(*
   Print unsigned integer without newline (for tuple elements)
*)
let emitPrintUInt64NoNewline (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then
            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.target))

(*
   Print boolean without newline (for tuple elements)
*)
let emitPrintBoolNoNewline (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then
            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.target))

(*
   Print float without newline (for tuple/list elements)
*)
let emitPrintFloatNoNewline (ctx: codeGenContext) (freg: LIR.fReg) =

    lirFRegToARM64FReg freg
    |> Result.map (fun fregARM64 ->
        if fregARM64 <> Symbolic.D0 then
            [Symbolic.FMOV_reg (Symbolic.D0, fregARM64)] @ runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.target))

(*
   Print heap string without newline (for tuple/list elements)
   Dynamic buffer layout: [refcount:8][length:8][data:N].
   Need to save the original address first
*)
let emitPrintHeapStringNoNewline (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->

        let loadInstrs = [Symbolic.LDR (Symbolic.X10, regARM64, 8); Symbolic.ADD_imm (Symbolic.X9, regARM64, 16)]
        in
        let loadAndPrint = loadInstrs @ runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.target)
        in
        if regARM64 <> Symbolic.X9 then
            loadAndPrint
        else

            let saveReg = [Symbolic.MOV_reg (Symbolic.X11, regARM64)]
            in
            let loadFromSaved = [Symbolic.LDR (Symbolic.X10, Symbolic.X11, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X11, 16)]
            in
            saveReg @ loadFromSaved @ runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.target))

(*
   Print list as [elem1, elem2, ...]
   List layout: Nil = 0, Cons = [tag=1, head, tail]
   Uses X19 for list pointer (callee-saved), X20 for first flag
*)
let emitPrintList (ctx: codeGenContext) (listPtr: LIR.reg) (elemType: AST.semanticType) =



    lirRegToARM64Reg listPtr
    |> Result.map (fun listReg -> generatePrintListInstrs ctx listReg elemType true)

(*
   Print sum type: variant name + optional payload + newline
   Sum layout depends on whether ANY variant has a payload:
   - If any payload: [tag, payload] on heap
   - If all nullary: just the tag value (integer)
   Check if any variant has a payload
   Helper: generate code to print a string literal
   Setup depends on representation
   Heap-allocated: X19 = sum pointer, load tag from [X19, 0] into X20
   All nullary: X19 = sum pointer (for consistency), X20 = tag (the value itself)
   Generate code for each variant: compare tag, branch, print name, optionally print payload
   Structure: for each variant, generate:
   CMP X20, #tag
   B.NE next_variant
   <print variant name>
   <if payload: print "(", print payload, print ")">
   B end
   next_variant:
   ... (repeat)
   end:
   <print "\n">
   Pre-calculate code blocks for each variant
   Calculate end label offset from each variant block
   We'll build the code and calculate offsets manually
   Build variant blocks with branching
   For each variant: CMP(1) + B.NE(1) + name + payload + B(1) to end
   CMP + B.NE + name + payload + B
   B.NE is at position 1, next block CMP is at position blockLen
   So offset = blockLen - 1 (forward jump from B.NE to next CMP)
   Jump to after all variant blocks
   Skip this variant's code
   Jump to end (after all variant code)
*)
let emitPrintSum (ctx: codeGenContext) (convertInstr: codeGenContext -> LIR.instr -> (Symbolic.instr list,string) result) (sumPtr: LIR.reg) (variants: (string * int * AST.semanticType option) list) (transparentPayload: bool) =




    lirRegToARM64Reg sumPtr
    |> Result.map (fun sumReg ->
        let syscalls = ARM64.targetSyscalls ctx.target


        in
        let hasAnyPayload = variants |> List.exists (fun (_, _, payload) -> Option.is_some payload)
        in
        let nullableStringTags =
            if transparentPayload || List.length variants <> 2 then None
            else
                let nullaryTag =
                    variants |> List.find_map (fun (_, tag, payload) -> if payload = None then Some tag else None)
                in
                let stringTag =
                    variants |> List.find_map (fun (_, tag, payload) -> if payload = Some AST.TString then Some tag else None)
                in
                match nullaryTag, stringTag with
                | Some nullary, Some present -> Some (nullary, present)
                | _ -> None


        in
        let printLiteral (s: string) =
            let bytes = utf8Bytes s
            in
            if (Bytes.length bytes) = 0 then []
            else
                let alignedSize = max 16 ((add (Bytes.length bytes) 15) land (lnot 15))
                in
                [Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, (alignedSize land 65535))] @
                (bytes |> Bytes.to_seq |> List.of_seq |> List.map Char.code |> List.mapi (fun i b ->
                    [Symbolic.MOVZ (Symbolic.X0, b, 0); Symbolic.STRB (Symbolic.X0, Symbolic.SP, i)]
                ) |> List.concat) @
                [Symbolic.MOVZ (Symbolic.X0, 1, 0);
                 Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                 Symbolic.MOVZ (Symbolic.X2, ((Bytes.length bytes) land 65535), 0);
                 Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                 Symbolic.SVC syscalls.ARM64.svcImmediate;
                 Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, (alignedSize land 65535))]


        in
        let setup =
            if transparentPayload then
                [Symbolic.MOV_reg (Symbolic.X19, sumReg)]
            else if Option.is_some nullableStringTags then
                (match nullableStringTags with
                | Some (nullaryTag, presentTag) ->
                    [ Symbolic.MOV_reg (Symbolic.X19, sumReg);
                      Symbolic.CMP_imm (Symbolic.X19, 0);
                      Symbolic.MOVZ (Symbolic.X20, (nullaryTag land 65535), 0);
                      Symbolic.MOVZ (Symbolic.X21, (presentTag land 65535), 0);
                      Symbolic.CSEL (Symbolic.X20, Symbolic.X20, Symbolic.X21, Symbolic.EQ) ]
                | None -> Crash.crash "Nullable String tags disappeared after classification")
            else if hasAnyPayload then

                [Symbolic.MOV_reg (Symbolic.X19, sumReg); Symbolic.LDR (Symbolic.X20, Symbolic.X19, 0)]
            else

                [Symbolic.MOV_reg (Symbolic.X19, sumReg); Symbolic.MOV_reg (Symbolic.X20, sumReg)]














        in
        let variantBlocks =
            variants |> List.map (fun (variantName, _tag, payloadType) ->
                let printName = printLiteral variantName
                in
                let printPayload =
                    match payloadType with
                    | None -> []
                    | Some pType ->
                        let printOpen = printLiteral "("
                        in
                        let loadPayload =
                            if transparentPayload || Option.is_some nullableStringTags then [Symbolic.MOV_reg (Symbolic.X0, Symbolic.X19)]
                            else [Symbolic.LDR (Symbolic.X0, Symbolic.X19, 8)]
                        in
                        let printPayloadValue =
                            match pType with
                            | AST.TInt64 -> runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.target)
                            | AST.TUInt64 -> runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.target)
                            | AST.TBool -> runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.target)
                            | AST.TFloat64 ->
                                [Symbolic.FMOV_from_gp (Symbolic.D0, Symbolic.X0)] @ runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.target)
                            | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                                [Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X0, 16)] @
                                runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.target)
                            | AST.TList elemType ->
                                (match ListDisplay.getDisplayStringFunc elemType with
                                | Some funcName ->
                                    let callToDisplay = [Symbolic.BL funcName]
                                    in
                                    let saveDisplayString = [Symbolic.MOV_reg (Symbolic.X21, Symbolic.X0)]
                                    in
                                    let printString =
                                        [Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X0, 16)] @
                                        runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.target)
                                    in
                                    let releaseDisplayString =
                                        match convertInstr ctx (LIR.RefCountDecString (LIR.Reg (LIR.Physical LIR.X21))) with
                                        | Ok instrs -> instrs
                                        | Error e -> Crash.crash e
                                    in
                                    callToDisplay @ saveDisplayString @ printString @ releaseDisplayString
                                | None ->
                                    Crash.crash ("Unsupported list element type in sum variant: " ^ HostStructuralFormat.semanticType elemType))
                            | t -> Crash.crash ("Unsupported payload type in sum variant: " ^ HostStructuralFormat.semanticType t)
                        in
                        let printClose = printLiteral ")"
                        in
                        printOpen @ loadPayload @ printPayloadValue @ printClose
                in
                (printName, printPayload))




        in
        let printNewline = printLiteral "\n"



        in
        let blockLengths =
            variants
            |> List.mapi (fun i (_, _tag, _) ->
                let (printName, printPayload) = (List.nth variantBlocks i)
                in
                add (add (add 2 (List.length printName)) (List.length printPayload)) 1)

        in
        let totalVariantCodeLen = List.fold_left add 0 blockLengths

        in
        let variantCode =
            variants
            |> List.mapi (fun i (_, tag, _) -> i, tag)
            |> mapFold (fun currentPos (i, tag) ->
                let (printName, printPayload) = (List.nth variantBlocks i)
                in
                let blockLen = add (add (add 2 (List.length printName)) (List.length printPayload)) 1


                in
                let nextBlockOffset = sub blockLen 1
                in
                let endFromHere = add (sub (sub totalVariantCodeLen currentPos) blockLen) 1

                in
                let cmpInstr = Symbolic.CMP_imm (Symbolic.X20, (tag land 65535))
                in
                let branchNeInstr = Symbolic.B_cond (Symbolic.NE, nextBlockOffset)
                in
                let branchEndInstr = Symbolic.B endFromHere

                in
                [cmpInstr; branchNeInstr] @ printName @ printPayload @ [branchEndInstr],
                add currentPos blockLen)
                0
            |> fst
            |> List.concat

        in
        if transparentPayload then
            (match variantBlocks with
            | [(printName, printPayload)] -> setup @ printName @ printPayload @ printNewline
            | _ -> Crash.crash "Transparent sum must have exactly one case")
        else
            setup @ variantCode @ printNewline)

(*
   Print record: TypeName { field1 = val1, field2 = val2, ... }\n
   Record layout: [field0, field1, field2, ...] on heap (each 8 bytes)
   Helper: generate code to print a string literal
   Save record pointer in callee-saved register X19
   Print type name and opening brace
   Print each field: "fieldName = value" with ", " separator between fields
   Each field is 8 bytes
   String is a pointer: load length, compute data ptr, print
   Print closing brace and newline
*)
let emitPrintRecord (ctx: codeGenContext) (recordPtr: LIR.reg) (typeName: string) (fields: (string * AST.semanticType) list) =


    lirRegToARM64Reg recordPtr
    |> Result.map (fun recordReg ->
        let syscalls = ARM64.targetSyscalls ctx.target


        in
        let printLiteral (s: string) =
            let bytes = utf8Bytes s
            in
            if (Bytes.length bytes) = 0 then []
            else
                let alignedSize = max 16 ((add (Bytes.length bytes) 15) land (lnot 15))
                in
                [Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, (alignedSize land 65535))] @
                (bytes |> Bytes.to_seq |> List.of_seq |> List.map Char.code |> List.mapi (fun i b ->
                    [Symbolic.MOVZ (Symbolic.X0, b, 0); Symbolic.STRB (Symbolic.X0, Symbolic.SP, i)]
                ) |> List.concat) @
                [Symbolic.MOVZ (Symbolic.X0, 1, 0);
                 Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                 Symbolic.MOVZ (Symbolic.X2, ((Bytes.length bytes) land 65535), 0);
                 Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                 Symbolic.SVC syscalls.ARM64.svcImmediate;
                 Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, (alignedSize land 65535))]


        in
        let setup = [Symbolic.MOV_reg (Symbolic.X19, recordReg)]


        in
        let printHeader = printLiteral (typeName ^ " { ")


        in
        let printFields =
            fields
            |> List.mapi (fun i (fieldName, fieldType) ->
                let printFieldName = printLiteral (fieldName ^ " = ")
                in
                let offset = int16 (mul i 8)
                in
                let loadField = [Symbolic.LDR (Symbolic.X0, Symbolic.X19, offset)]
                in
                let printValue =
                    match fieldType with
                    | AST.TInt64 -> runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.target)
                    | AST.TUInt64 -> runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.target)
                    | AST.TBool -> runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.target)
                    | AST.TFloat64 ->
                        [Symbolic.FMOV_from_gp (Symbolic.D0, Symbolic.X0)] @ runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.target)
                    | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->

                        [Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X0, 16)] @
                        runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.target)
                    | t -> Crash.crash ("Unsupported field type in record: " ^ HostStructuralFormat.semanticType t)
                in
                let separator =
                    if i < sub (List.length fields) 1 then printLiteral ", "
                    else []
                in
                printFieldName @ loadField @ printValue @ separator)
            |> List.concat


        in
        let printFooter = printLiteral " }\n"

        in
        setup @ printHeader @ printFields @ printFooter)

(*
   Value to print should be in X0 (no exit)
   Move to X0 if not already there
*)
let emitPrintInt64 (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then

            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintInt64NoExit ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintInt64NoExit ctx.target))

(*
   Value to print should be in X0 (no exit)
*)
let emitPrintUInt64 (ctx: codeGenContext) (reg: LIR.reg) =

    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        if regARM64 <> Symbolic.X0 then
            [Symbolic.MOV_reg (Symbolic.X0, regARM64)] @ runtimeInstrs (PrintValues.generatePrintUInt64NoExit ctx.target)
        else
            runtimeInstrs (PrintValues.generatePrintUInt64NoExit ctx.target))

(*
   Print float value from FP register
   Value should be in D0 for generatePrintFloat
   Move to D0 if not already there
*)
let emitPrintFloat (ctx: codeGenContext) (freg: LIR.fReg) =


    lirFRegToARM64FReg freg
    |> Result.map (fun fregARM64 ->
        if fregARM64 <> Symbolic.D0 then

            [Symbolic.FMOV_reg (Symbolic.D0, fregARM64)] @ runtimeInstrs (PrintAndExit.generatePrintFloat ctx.target)
        else
            runtimeInstrs (PrintAndExit.generatePrintFloat ctx.target))

(*
   To print a string, we need:
   1. ADRP + ADD to load string address into X0
   2. Call ARM64PrintAndExit.generatePrintString which handles write syscall
   Load page address of string
   Add page offset
*)
let emitPrintString (ctx: codeGenContext) (value: string) =



    let len = utf8Len value
    in
    let labelRef = stringDataLabel value
    in
    Ok ([
        Symbolic.ADRP (Symbolic.X0, labelRef);
        Symbolic.ADD_label (Symbolic.X0, Symbolic.X0, labelRef)
    ] @ runtimeInstrs (PrintAndExit.generatePrintString ctx.target len))

(*
   Print a dynamic string with [refcount:8][length:8][data:N].
   Note: The syscall clobbers X0, X1, X2, X8. If the input register is one
   of these, we save it to X9 before and restore after so subsequent code
   can still use it.
   1. Save input to X9
   2. Load length from [X9] into X2
   3. Compute data pointer (X9 + 8) into X1
   4. Set X0 = 1 (stdout)
   5. write syscall
   6. Write the result-rendering newline
   7. Restore input register if it was clobbered
   X9 = input (save in case regARM64 is X0/X1/X2)
   X0 = stdout fd
*)
let emitPrintHeapString (ctx: codeGenContext) (reg: LIR.reg) =











    lirRegToARM64Reg reg
    |> Result.map (fun regARM64 ->
        let isClobbered = regARM64 = Symbolic.X0 || regARM64 = Symbolic.X1 || regARM64 = Symbolic.X2 || regARM64 = Symbolic.X8
        in
        let restoreInstrs = if isClobbered then [Symbolic.MOV_reg (regARM64, Symbolic.X9)] else []
        in
        [
            Symbolic.MOV_reg (Symbolic.X9, regARM64);
            Symbolic.LDR (Symbolic.X2, Symbolic.X9, 8);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X9, 16);
            Symbolic.MOVZ (Symbolic.X0, 1, 0)
        ]
        @ runtimeInstrs (PrintValues.generateWriteSyscall ctx.target)
        @ [ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
            Symbolic.MOVZ (Symbolic.X10, 10, 0);
            Symbolic.STRB (Symbolic.X10, Symbolic.SP, 0);
            Symbolic.MOVZ (Symbolic.X0, 1, 0);
            Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
            Symbolic.MOVZ (Symbolic.X2, 1, 0) ]
        @ runtimeInstrs (PrintValues.generateWriteSyscall ctx.target)
        @ [ Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16) ]
        @ restoreInstrs)
