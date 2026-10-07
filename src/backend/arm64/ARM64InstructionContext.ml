(*
   ARM64InstructionContext.ml - Shared operand context for arm64 instruction-family lowering.
*)
[@@@warning "-4"]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let neg a=Int32.to_int (Int32.neg (Int32.of_int a))
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low



open ARM64CodeGenTypes
open HeapAllocation

(*
   Generate element print code based on type (uses X0 for value)
   Need to move from X0 to D0 for float
   X0 has string address, load len/data and print
   Print tuple inside list: (elem1, elem2, ...)
   Use X21 for tuple ptr (callee-saved), keep X19 for list ptr
   Print "("
   Print ", " helper
   Generate code for each tuple element (load from X21)
   Print ")"
   For other types (nested lists, etc.), print as integer for now
   Print "[" - 9 instructions
   fd = stdout
   buffer
   len = 1
   Setup: X19 = list pointer, X20 = 1 (first element flag)
   Print ", " - used inside loop when not first element
   Loop structure:
   loop_start:
   CBZ X19, loop_end           // if list == nil, exit
   CBNZ X20, skip_comma        // if first, skip comma
   <print ", ">
   skip_comma:
   MOV X20, 0                  // first = false
   LDR X0, [X19, #8]           // X0 = head
   <print element>
   LDR X19, [X19, #16]         // X19 = tail
   B loop_start
   loop_end:
   <print "]">
   Calculate branch offsets
   loopBodyLen = instructions after CBZ = CBNZ(1) + comma(11) + skipComma(2) + element(N) + loopEnd(2)
   CBZ skips to loop_end (after B), which is at index loopBodyLen+1 (since CBZ is at index 0)
   CBNZ skips commaLen instructions to reach skipComma
   B is at index loopBodyLen, jump back to CBZ at index 0
*)
let generatePrintListInstrs (ctx: codeGenContext) (listReg: Symbolic.reg) (elemType: AST.semanticType) (includeNewline: bool) =
    let syscalls = ARM64.targetSyscalls ctx.ARM64CodeGenTypes.target


    in
    let elemPrintCode =
        match elemType with
        | AST.TInt64 -> runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.ARM64CodeGenTypes.target)
        | AST.TUInt64 -> runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.ARM64CodeGenTypes.target)
        | AST.TBool -> runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.ARM64CodeGenTypes.target)
        | AST.TFloat64 ->

            [Symbolic.FMOV_from_gp (Symbolic.D0, Symbolic.X0)] @ runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.ARM64CodeGenTypes.target)
        | AST.TString | AST.TChar ->

            [Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X0, 16)] @
            runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.ARM64CodeGenTypes.target)
        | AST.TTuple elemTypes ->


            let moveTupleToX21 = [Symbolic.MOV_reg (Symbolic.X21, Symbolic.X0)]


            in
            let printOpenParen = [
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                Symbolic.MOVZ (Symbolic.X0, 40, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
                Symbolic.MOVZ (Symbolic.X0, 1, 0);
                Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                Symbolic.MOVZ (Symbolic.X2, 1, 0);
                Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                Symbolic.SVC syscalls.ARM64.svcImmediate;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            ]


            in
            let printTupleCommaSpace = [
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                Symbolic.MOVZ (Symbolic.X0, 44, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
                Symbolic.MOVZ (Symbolic.X0, 32, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 1);
                Symbolic.MOVZ (Symbolic.X0, 1, 0);
                Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                Symbolic.MOVZ (Symbolic.X2, 2, 0);
                Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                Symbolic.SVC syscalls.ARM64.svcImmediate;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            ]


            in
            let tupleElemInstrs =
                elemTypes
                |> List.mapi (fun i eType ->
                    let loadElem = [Symbolic.LDR (Symbolic.X0, Symbolic.X21, int16 (mul i 8))]
                    in
                    let printElem =
                        match eType with
                        | AST.TInt64 -> runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.ARM64CodeGenTypes.target)
                        | AST.TUInt64 -> runtimeInstrs (PrintValues.generatePrintUInt64NoNewline ctx.ARM64CodeGenTypes.target)
                        | AST.TBool -> runtimeInstrs (PrintValues.generatePrintBoolNoNewline ctx.ARM64CodeGenTypes.target)
                        | AST.TFloat64 ->
                            [Symbolic.FMOV_from_gp (Symbolic.D0, Symbolic.X0)] @ runtimeInstrs (PrintValues.generatePrintFloatNoNewline ctx.ARM64CodeGenTypes.target)
                        | AST.TString | AST.TChar ->
                            [Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8); Symbolic.ADD_imm (Symbolic.X9, Symbolic.X0, 16)] @
                            runtimeInstrs (PrintValues.generatePrintStringNoNewline ctx.ARM64CodeGenTypes.target)
                        | _ -> runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.ARM64CodeGenTypes.target)
                    in
                    let comma = if i < sub (List.length elemTypes) 1 then printTupleCommaSpace else []
                    in
                    loadElem @ printElem @ comma
                )
                |> List.concat


            in
            let printCloseParen = [
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                Symbolic.MOVZ (Symbolic.X0, 41, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
                Symbolic.MOVZ (Symbolic.X0, 1, 0);
                Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                Symbolic.MOVZ (Symbolic.X2, 1, 0);
                Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                Symbolic.SVC syscalls.ARM64.svcImmediate;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            ]

            in
            moveTupleToX21 @ printOpenParen @ tupleElemInstrs @ printCloseParen
        | _ ->

            runtimeInstrs (PrintValues.generatePrintInt64NoNewline ctx.ARM64CodeGenTypes.target)

    in
    let elemPrintLen = List.length elemPrintCode


    in
    let printOpenBracket = [
        Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
        Symbolic.MOVZ (Symbolic.X0, 91, 0);
        Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
        Symbolic.MOVZ (Symbolic.X0, 1, 0);
        Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
        Symbolic.MOVZ (Symbolic.X2, 1, 0);
        Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        Symbolic.SVC syscalls.ARM64.svcImmediate;
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
    ]


    in
    let setup = [Symbolic.MOV_reg (Symbolic.X19, listReg); Symbolic.MOVZ (Symbolic.X20, 1, 0)]


    in
    let printCommaSpace = [
        Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
        Symbolic.MOVZ (Symbolic.X0, 44, 0);
        Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
        Symbolic.MOVZ (Symbolic.X0, 32, 0);
        Symbolic.STRB (Symbolic.X0, Symbolic.SP, 1);
        Symbolic.MOVZ (Symbolic.X0, 1, 0);
        Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
        Symbolic.MOVZ (Symbolic.X2, 2, 0);
        Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        Symbolic.SVC syscalls.ARM64.svcImmediate;
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
    ]
    in
    let commaLen = List.length printCommaSpace

    in
    let loopBodyLen = add (add (add (add 1 commaLen) 2) elemPrintLen) 2

    in
    let cbzOffset = add loopBodyLen 1

    in
    let skipCommaOffset = commaLen

    in
    let loopStart = [Symbolic.CBZ_offset (Symbolic.X19, cbzOffset); Symbolic.CBNZ_offset (Symbolic.X20, skipCommaOffset)]
    in
    let skipComma = [Symbolic.MOVZ (Symbolic.X20, 0, 0); Symbolic.LDR (Symbolic.X0, Symbolic.X19, 8)]

    in
    let loopEnd = [Symbolic.LDR (Symbolic.X19, Symbolic.X19, 16); Symbolic.B (neg loopBodyLen)]
    in
    let loopCode = loopStart @ printCommaSpace @ skipComma @ elemPrintCode @ loopEnd

    in
    let printCloseBracket =
        if includeNewline then
            [
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                Symbolic.MOVZ (Symbolic.X0, 93, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
                Symbolic.MOVZ (Symbolic.X0, 10, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 1);
                Symbolic.MOVZ (Symbolic.X0, 1, 0);
                Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                Symbolic.MOVZ (Symbolic.X2, 2, 0);
                Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                Symbolic.SVC syscalls.ARM64.svcImmediate;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            ]
        else
            [
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                Symbolic.MOVZ (Symbolic.X0, 93, 0);
                Symbolic.STRB (Symbolic.X0, Symbolic.SP, 0);
                Symbolic.MOVZ (Symbolic.X0, 1, 0);
                Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                Symbolic.MOVZ (Symbolic.X2, 1, 0);
                Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
                Symbolic.SVC syscalls.ARM64.svcImmediate;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            ]

    in
    setup @ printOpenBracket @ loopCode @ printCloseBracket
