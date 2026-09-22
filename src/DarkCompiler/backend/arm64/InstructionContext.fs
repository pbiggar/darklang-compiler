// InstructionContext.fs - Shared operand context for arm64 instruction-family lowering.

module ARM64InstructionContext

open ARM64CodeGenTypes
open ARM64HeapAllocation

let generatePrintListInstrs (ctx: CodeGenContext) (listReg: ARM64Symbolic.Reg) (elemType: AST.SemanticType) (includeNewline: bool) : ARM64Symbolic.Instr list =
    let syscalls = ARM64.targetSyscalls ctx.Target

    // Generate element print code based on type (uses X0 for value)
    let elemPrintCode =
        match elemType with
        | AST.TInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
        | AST.TUInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target)
        | AST.TBool -> runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target)
        | AST.TFloat64 ->
            // Need to move from X0 to D0 for float
            [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target)
        | AST.TString | AST.TChar ->
            // X0 has string address, load len/data and print
            [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 16us)] @
            runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
        | AST.TTuple elemTypes ->
            // Print tuple inside list: (elem1, elem2, ...)
            // Use X21 for tuple ptr (callee-saved), keep X19 for list ptr
            let moveTupleToX21 = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X21, ARM64Symbolic.X0)]

            // Print "("
            let printOpenParen = [
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '('), 0)
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                ARM64Symbolic.SVC syscalls.SvcImmediate
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ]

            // Print ", " helper
            let printTupleCommaSpace = [
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ','), 0)
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ' '), 0)
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0)
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                ARM64Symbolic.SVC syscalls.SvcImmediate
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ]

            // Generate code for each tuple element (load from X21)
            let tupleElemInstrs =
                elemTypes
                |> List.mapi (fun i eType ->
                    let loadElem = [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, int16 (i * 8))]
                    let printElem =
                        match eType with
                        | AST.TInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
                        | AST.TUInt64 -> runtimeInstrs (ARM64PrintValues.generatePrintUInt64NoNewline ctx.Target)
                        | AST.TBool -> runtimeInstrs (ARM64PrintValues.generatePrintBoolNoNewline ctx.Target)
                        | AST.TFloat64 ->
                            [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (ARM64PrintValues.generatePrintFloatNoNewline ctx.Target)
                        | AST.TString | AST.TChar ->
                            [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 8s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 16us)] @
                            runtimeInstrs (ARM64PrintValues.generatePrintStringNoNewline ctx.Target)
                        | _ -> runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)
                    let comma = if i < List.length elemTypes - 1 then printTupleCommaSpace else []
                    loadElem @ printElem @ comma
                )
                |> List.concat

            // Print ")"
            let printCloseParen = [
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ')'), 0)
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                ARM64Symbolic.SVC syscalls.SvcImmediate
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ]

            moveTupleToX21 @ printOpenParen @ tupleElemInstrs @ printCloseParen
        | _ ->
            // For other types (nested lists, etc.), print as integer for now
            runtimeInstrs (ARM64PrintValues.generatePrintInt64NoNewline ctx.Target)

    let elemPrintLen = List.length elemPrintCode

    // Print "[" - 9 instructions
    let printOpenBracket = [
        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '['), 0);
        ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);          // fd = stdout
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);    // buffer
        ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0);         // len = 1
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
        ARM64Symbolic.SVC syscalls.SvcImmediate;
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
    ]

    // Setup: X19 = list pointer, X20 = 1 (first element flag)
    let setup = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, listReg); ARM64Symbolic.MOVZ (ARM64Symbolic.X20, 1us, 0)]

    // Print ", " - used inside loop when not first element
    let printCommaSpace = [
        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ','), 0);
        ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ' '), 0);
        ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
        ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0);
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
        ARM64Symbolic.SVC syscalls.SvcImmediate;
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
    ]
    let commaLen = List.length printCommaSpace

    // Loop structure:
    // loop_start:
    //   CBZ X19, loop_end           // if list == nil, exit
    //   CBNZ X20, skip_comma        // if first, skip comma
    //   <print ", ">
    // skip_comma:
    //   MOV X20, 0                  // first = false
    //   LDR X0, [X19, #8]           // X0 = head
    //   <print element>
    //   LDR X19, [X19, #16]         // X19 = tail
    //   B loop_start
    // loop_end:
    //   <print "]">

    // Calculate branch offsets
    // loopBodyLen = instructions after CBZ = CBNZ(1) + comma(11) + skipComma(2) + element(N) + loopEnd(2)
    let loopBodyLen = 1 + commaLen + 2 + elemPrintLen + 2
    // CBZ skips to loop_end (after B), which is at index loopBodyLen+1 (since CBZ is at index 0)
    let cbzOffset = loopBodyLen + 1
    // CBNZ skips commaLen instructions to reach skipComma
    let skipCommaOffset = commaLen

    let loopStart = [ARM64Symbolic.CBZ_offset (ARM64Symbolic.X19, cbzOffset); ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X20, skipCommaOffset)]
    let skipComma = [ARM64Symbolic.MOVZ (ARM64Symbolic.X20, 0us, 0); ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 8s)]
    // B is at index loopBodyLen, jump back to CBZ at index 0
    let loopEnd = [ARM64Symbolic.LDR (ARM64Symbolic.X19, ARM64Symbolic.X19, 16s); ARM64Symbolic.B (-loopBodyLen)]
    let loopCode = loopStart @ printCommaSpace @ skipComma @ elemPrintCode @ loopEnd

    let printCloseBracket =
        if includeNewline then
            [
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ']'), 0);
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '\n'), 0);
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0);
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                ARM64Symbolic.SVC syscalls.SvcImmediate;
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ]
        else
            [
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ']'), 0);
                ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0);
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                ARM64Symbolic.SVC syscalls.SvcImmediate;
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ]

    setup @ printOpenBracket @ loopCode @ printCloseBracket
