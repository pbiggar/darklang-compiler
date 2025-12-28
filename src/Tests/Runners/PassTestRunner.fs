// PassTestRunner.fs - Test runner for compiler pass tests
//
// Loads pass test files (e.g., MIR→LIR tests), runs the compiler pass,
// and compares the output with expected results.
//
// NOTE: Temporarily disabled - MIR/LIR structure changed to CFG, needs rewrite
// Pass tests will be re-enabled after CFG-based test infrastructure is built

module TestDSL.PassTestRunner

open System.IO
open TestDSL.Common
open TestDSL.ANFParser
open TestDSL.MIRParser
open TestDSL.LIRParser
open TestDSL.ARM64Parser
open ANF
open MIR
open LIR
open ARM64
open ANF_to_MIR
open MIR_to_LIR
open CodeGen

/// Result of running a pass test
type PassTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

/// Pretty-print MIR operand
let prettyPrintMIROperand = function
    | MIR.IntConst n -> string n
    | MIR.BoolConst b -> if b then "true" else "false"
    | MIR.FloatRef idx -> $"float[{idx}]"
    | MIR.StringRef idx -> $"str[{idx}]"
    | MIR.Register (MIR.VReg n) -> $"v{n}"
    | MIR.FuncAddr name -> $"&{name}"

/// Pretty-print MIR operator
let prettyPrintMIROp = function
    | MIR.Add -> "+"
    | MIR.Sub -> "-"
    | MIR.Mul -> "*"
    | MIR.Div -> "/"
    | MIR.Mod -> "%"
    | MIR.Shl -> "<<"
    | MIR.Shr -> ">>"
    | MIR.BitAnd -> "&"
    | MIR.BitOr -> "|"
    | MIR.BitXor -> "^"
    | MIR.Eq -> "=="
    | MIR.Neq -> "!="
    | MIR.Lt -> "<"
    | MIR.Gt -> ">"
    | MIR.Lte -> "<="
    | MIR.Gte -> ">="
    | MIR.And -> "&&"
    | MIR.Or -> "||"

/// Pretty-print MIR instruction
/// NOTE: Disabled - MIR structure changed to CFG
let prettyPrintMIRInstr (instr: MIR.Instr) : string =
    "<MIR pretty-print disabled - CFG structure>"

/// Pretty-print MIR program
/// NOTE: Disabled - MIR structure changed to CFG
let prettyPrintMIR (program: MIR.Program) : string =
    "<MIR pretty-print disabled - CFG structure>"

/// Pretty-print LIR physical register
let prettyPrintLIRPhysReg = function
    | LIR.X0 -> "X0" | LIR.X1 -> "X1" | LIR.X2 -> "X2" | LIR.X3 -> "X3"
    | LIR.X4 -> "X4" | LIR.X5 -> "X5" | LIR.X6 -> "X6" | LIR.X7 -> "X7"
    | LIR.X8 -> "X8" | LIR.X9 -> "X9" | LIR.X10 -> "X10" | LIR.X11 -> "X11"
    | LIR.X12 -> "X12" | LIR.X13 -> "X13" | LIR.X14 -> "X14" | LIR.X15 -> "X15"
    | LIR.X19 -> "X19" | LIR.X20 -> "X20" | LIR.X21 -> "X21" | LIR.X22 -> "X22"
    | LIR.X23 -> "X23" | LIR.X24 -> "X24" | LIR.X25 -> "X25" | LIR.X26 -> "X26"
    | LIR.X27 -> "X27"
    | LIR.X29 -> "X29" | LIR.X30 -> "X30" | LIR.SP -> "SP"

/// Pretty-print LIR register
let prettyPrintLIRReg = function
    | LIR.Physical pr -> prettyPrintLIRPhysReg pr
    | LIR.Virtual n -> $"v{n}"

/// Pretty-print LIR FP physical register
let prettyPrintLIRPhysFPReg = function
    | LIR.D0 -> "D0" | LIR.D1 -> "D1" | LIR.D2 -> "D2" | LIR.D3 -> "D3"
    | LIR.D4 -> "D4" | LIR.D5 -> "D5" | LIR.D6 -> "D6" | LIR.D7 -> "D7"
    | LIR.D8 -> "D8" | LIR.D9 -> "D9" | LIR.D10 -> "D10" | LIR.D11 -> "D11"
    | LIR.D12 -> "D12" | LIR.D13 -> "D13" | LIR.D14 -> "D14" | LIR.D15 -> "D15"

/// Pretty-print LIR FP register
let prettyPrintLIRFReg = function
    | LIR.FPhysical pr -> prettyPrintLIRPhysFPReg pr
    | LIR.FVirtual n -> $"fv{n}"

/// Pretty-print LIR operand
let prettyPrintLIROperand = function
    | LIR.Imm n -> $"Imm {n}"
    | LIR.FloatImm f -> $"FloatImm {f}"
    | LIR.Operand.Reg reg -> $"Reg {prettyPrintLIRReg reg}"
    | LIR.StackSlot n -> $"Stack {n}"
    | LIR.StringRef idx -> $"str[{idx}]"
    | LIR.FloatRef idx -> $"float[{idx}]"
    | LIR.FuncAddr name -> $"&{name}"

/// Pretty-print LIR instruction
let prettyPrintLIRInstr (instr: LIR.Instr) : string =
    match instr with
    | LIR.Mov (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mov({prettyPrintLIROperand src})"
    | LIR.Store (offset, src) ->
        $"Store(Stack {offset}, {prettyPrintLIRReg src})"
    | LIR.Add (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Add({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Sub (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sub({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Mul (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Mul({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Sdiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sdiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        $"{prettyPrintLIRReg dest} <- Msub({prettyPrintLIRReg mulLeft}, {prettyPrintLIRReg mulRight}, {prettyPrintLIRReg sub})"
    | LIR.Cmp (left, right) ->
        $"Cmp({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Cset (dest, cond) ->
        $"{prettyPrintLIRReg dest} <- Cset({cond})"
    | LIR.And (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- And({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Orr (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Orr({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Eor (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Eor({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Lsl (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsl({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Lsr (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsr({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Mvn (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mvn({prettyPrintLIRReg src})"
    | LIR.Call (dest, funcName, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- Call({funcName}, [{argStr}])"
    | LIR.IndirectCall (dest, func, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- IndirectCall({prettyPrintLIRReg func}, [{argStr}])"
    | LIR.ClosureAlloc (dest, funcName, captures) ->
        let capsStr = captures |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- ClosureAlloc({funcName}, [{capsStr}])"
    | LIR.ClosureCall (dest, closure, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- ClosureCall({prettyPrintLIRReg closure}, [{argStr}])"
    | LIR.PrintInt reg ->
        $"PrintInt({prettyPrintLIRReg reg})"
    | LIR.PrintBool reg ->
        $"PrintBool({prettyPrintLIRReg reg})"
    | LIR.PrintFloat freg ->
        $"PrintFloat({prettyPrintLIRFReg freg})"
    | LIR.PrintString (idx, len) ->
        $"PrintString(str[{idx}], len={len})"
    | LIR.PrintChars chars ->
        let s = chars |> List.map (fun b -> char b) |> System.String.Concat
        $"PrintChars(\"{s}\")"
    | LIR.PrintIntNoNewline reg ->
        $"PrintIntNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintBoolNoNewline reg ->
        $"PrintBoolNoNewline({prettyPrintLIRReg reg})"
    | LIR.SaveRegs ->
        "SaveRegs"
    | LIR.RestoreRegs ->
        "RestoreRegs"
    | LIR.ArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> sprintf "%A <- %s" dest (prettyPrintLIROperand src))
        sprintf "ArgMoves(%s)" (String.concat ", " moveStrs)
    | LIR.FArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> sprintf "%A <- %s" dest (prettyPrintLIRFReg src))
        sprintf "FArgMoves(%s)" (String.concat ", " moveStrs)
    // FP instructions
    | LIR.FMov (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FMov({prettyPrintLIRFReg src})"
    | LIR.FLoad (dest, idx) ->
        $"{prettyPrintLIRFReg dest} <- FLoad(float[{idx}])"
    | LIR.FAdd (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FAdd({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FSub (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FSub({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FMul (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FMul({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FDiv (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FDiv({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FNeg (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FNeg({prettyPrintLIRFReg src})"
    | LIR.FAbs (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FAbs({prettyPrintLIRFReg src})"
    | LIR.FSqrt (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FSqrt({prettyPrintLIRFReg src})"
    | LIR.FCmp (left, right) ->
        $"FCmp({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.IntToFloat (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- IntToFloat({prettyPrintLIRReg src})"
    | LIR.FloatToInt (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FloatToInt({prettyPrintLIRFReg src})"
    // Heap operations
    | LIR.HeapAlloc (dest, sizeBytes) ->
        $"{prettyPrintLIRReg dest} <- HeapAlloc({sizeBytes})"
    | LIR.HeapStore (addr, offset, src) ->
        $"HeapStore({prettyPrintLIRReg addr}, {offset}, {prettyPrintLIROperand src})"
    | LIR.HeapLoad (dest, addr, offset) ->
        $"{prettyPrintLIRReg dest} <- HeapLoad({prettyPrintLIRReg addr}, {offset})"
    // Reference counting operations
    | LIR.RefCountInc (addr, payloadSize) ->
        $"RefCountInc({prettyPrintLIRReg addr}, {payloadSize})"
    | LIR.RefCountDec (addr, payloadSize) ->
        $"RefCountDec({prettyPrintLIRReg addr}, {payloadSize})"
    // String operations
    | LIR.StringConcat (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- StringConcat({prettyPrintLIROperand left}, {prettyPrintLIROperand right})"
    | LIR.PrintHeapString reg ->
        $"PrintHeapString({prettyPrintLIRReg reg})"
    | LIR.LoadFuncAddr (dest, funcName) ->
        $"{prettyPrintLIRReg dest} <- LoadFuncAddr({funcName})"
    | LIR.FileReadText (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileReadText({prettyPrintLIROperand path})"
    | LIR.FileExists (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileExists({prettyPrintLIROperand path})"
    | LIR.FileWriteText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileWriteText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    | LIR.FileAppendText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileAppendText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    // Raw memory operations
    | LIR.RawAlloc (dest, numBytes) ->
        $"{prettyPrintLIRReg dest} <- RawAlloc({prettyPrintLIRReg numBytes})"
    | LIR.RawFree ptr ->
        $"RawFree({prettyPrintLIRReg ptr})"
    | LIR.RawGet (dest, ptr, byteOffset) ->
        $"{prettyPrintLIRReg dest} <- RawGet({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset})"
    | LIR.RawSet (ptr, byteOffset, value) ->
        $"RawSet({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value})"
    // String intrinsics
    | LIR.StringHash (dest, str) ->
        $"{prettyPrintLIRReg dest} <- StringHash({prettyPrintLIROperand str})"
    | LIR.StringEq (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- StringEq({prettyPrintLIROperand left}, {prettyPrintLIROperand right})"
    | LIR.RefCountIncString str ->
        $"RefCountIncString({prettyPrintLIROperand str})"
    | LIR.RefCountDecString str ->
        $"RefCountDecString({prettyPrintLIROperand str})"
    | LIR.Exit -> "Exit"

/// Pretty-print LIR terminator
let prettyPrintLIRTerminator (term: LIR.Terminator) : string =
    match term with
    | LIR.Ret -> "Ret"
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        $"Branch({prettyPrintLIRReg cond}, {trueLabel}, {falseLabel})"
    | LIR.Jump label -> $"Jump({label})"

/// Pretty-print LIR program (flat format for single-block CFGs)
let prettyPrintLIR (program: LIR.Program) : string =
    let (LIR.Program (funcs, _, _)) = program
    // For simple test cases, we expect a single function with single block
    match funcs with
    | [func] ->
        let entry = func.CFG.Entry
        match Map.tryFind entry func.CFG.Blocks with
        | Some block ->
            let instrLines = block.Instrs |> List.map prettyPrintLIRInstr
            let termLine = prettyPrintLIRTerminator block.Terminator
            String.concat "\n" (instrLines @ [termLine])
        | None -> "<entry block not found>"
    | _ -> "<multiple functions not supported in pretty-print>"

/// Load MIR→LIR test from file
let loadMIR2LIRTest (path: string) : Result<MIR.Program * LIR.Program, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-MIR" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseMIR inputText with
            | Error e -> Error $"Failed to parse INPUT-MIR: {e}"
            | Ok mirProgram ->
                match getRequiredSection "OUTPUT-LIR" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseLIR outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-LIR: {e}"
                    | Ok lirProgram -> Ok (mirProgram, lirProgram)

/// Run MIR→LIR test
let runMIR2LIRTest (input: MIR.Program) (expected: LIR.Program) : PassTestResult =
    match MIR_to_LIR.toLIR input with
    | Error err ->
        { Success = false
          Message = $"LIR conversion error: {err}"
          Expected = Some (prettyPrintLIR expected)
          Actual = None }
    | Ok actual ->
        if actual = expected then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "Output mismatch"
              Expected = Some (prettyPrintLIR expected)
              Actual = Some (prettyPrintLIR actual) }

/// Pretty-print ANF atom
let prettyPrintANFAtom = function
    | ANF.UnitLiteral -> "()"
    | ANF.IntLiteral n -> string n
    | ANF.BoolLiteral b -> if b then "true" else "false"
    | ANF.StringLiteral s -> $"\"{s}\""
    | ANF.FloatLiteral f -> string f
    | ANF.Var (ANF.TempId n) -> $"t{n}"
    | ANF.FuncRef name -> $"&{name}"

/// Pretty-print ANF binary operator
let prettyPrintANFOp = function
    | ANF.Add -> "+"
    | ANF.Sub -> "-"
    | ANF.Mul -> "*"
    | ANF.Div -> "/"
    | ANF.Mod -> "%"
    | ANF.Shl -> "<<"
    | ANF.Shr -> ">>"
    | ANF.BitAnd -> "&"
    | ANF.BitOr -> "|"
    | ANF.BitXor -> "^"
    | ANF.Eq -> "=="
    | ANF.Neq -> "!="
    | ANF.Lt -> "<"
    | ANF.Gt -> ">"
    | ANF.Lte -> "<="
    | ANF.Gte -> ">="
    | ANF.And -> "&&"
    | ANF.Or -> "||"

/// Pretty-print ANF unary operator
let prettyPrintANFUnaryOp = function
    | ANF.Neg -> "-"
    | ANF.Not -> "!"

/// Pretty-print ANF complex expression
let prettyPrintANFCExpr = function
    | ANF.Atom atom -> prettyPrintANFAtom atom
    | ANF.Prim (op, left, right) ->
        $"{prettyPrintANFAtom left} {prettyPrintANFOp op} {prettyPrintANFAtom right}"
    | ANF.UnaryPrim (op, operand) ->
        $"{prettyPrintANFUnaryOp op}{prettyPrintANFAtom operand}"
    | ANF.Call (funcName, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"{funcName}({argStr})"
    | ANF.IndirectCall (func, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"IndirectCall({prettyPrintANFAtom func}, [{argStr}])"
    | ANF.ClosureAlloc (funcName, captures) ->
        let capsStr = captures |> List.map prettyPrintANFAtom |> String.concat ", "
        $"ClosureAlloc({funcName}, [{capsStr}])"
    | ANF.ClosureCall (closure, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"ClosureCall({prettyPrintANFAtom closure}, [{argStr}])"
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        $"if {prettyPrintANFAtom cond} then {prettyPrintANFAtom thenAtom} else {prettyPrintANFAtom elseAtom}"
    | ANF.TupleAlloc elems ->
        let elemsStr = elems |> List.map prettyPrintANFAtom |> String.concat ", "
        $"({elemsStr})"
    | ANF.TupleGet (tupleAtom, index) ->
        $"{prettyPrintANFAtom tupleAtom}.{index}"
    | ANF.RefCountInc (atom, payloadSize) ->
        $"rc_inc({prettyPrintANFAtom atom}, size={payloadSize})"
    | ANF.RefCountDec (atom, payloadSize) ->
        $"rc_dec({prettyPrintANFAtom atom}, size={payloadSize})"
    | ANF.StringConcat (left, right) ->
        $"{prettyPrintANFAtom left} ++ {prettyPrintANFAtom right}"
    | ANF.Print (atom, valueType) ->
        $"print({prettyPrintANFAtom atom}, type={valueType})"
    | ANF.FileReadText path ->
        $"FileReadText({prettyPrintANFAtom path})"
    | ANF.FileExists path ->
        $"FileExists({prettyPrintANFAtom path})"
    | ANF.FileWriteText (path, content) ->
        $"FileWriteText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    | ANF.FileAppendText (path, content) ->
        $"FileAppendText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    // Raw memory operations
    | ANF.RawAlloc numBytes ->
        $"RawAlloc({prettyPrintANFAtom numBytes})"
    | ANF.RawFree ptr ->
        $"RawFree({prettyPrintANFAtom ptr})"
    | ANF.RawGet (ptr, byteOffset) ->
        $"RawGet({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
    | ANF.RawSet (ptr, byteOffset, value) ->
        $"RawSet({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
    // Float intrinsics
    | ANF.FloatSqrt atom ->
        $"FloatSqrt({prettyPrintANFAtom atom})"
    | ANF.FloatAbs atom ->
        $"FloatAbs({prettyPrintANFAtom atom})"
    | ANF.FloatNeg atom ->
        $"FloatNeg({prettyPrintANFAtom atom})"
    | ANF.IntToFloat atom ->
        $"IntToFloat({prettyPrintANFAtom atom})"
    | ANF.FloatToInt atom ->
        $"FloatToInt({prettyPrintANFAtom atom})"
    // String intrinsics
    | ANF.StringHash str ->
        $"StringHash({prettyPrintANFAtom str})"
    | ANF.StringEq (left, right) ->
        $"StringEq({prettyPrintANFAtom left}, {prettyPrintANFAtom right})"
    | ANF.RefCountIncString str ->
        $"RefCountIncString({prettyPrintANFAtom str})"
    | ANF.RefCountDecString str ->
        $"RefCountDecString({prettyPrintANFAtom str})"

/// Pretty-print ANF expression (recursive)
let rec prettyPrintANFExpr = function
    | ANF.Return atom -> $"return {prettyPrintANFAtom atom}"
    | ANF.Let (ANF.TempId n, cexpr, body) ->
        let cexprStr = prettyPrintANFCExpr cexpr
        let bodyStr = prettyPrintANFExpr body
        $"let t{n} = {cexprStr}\n{bodyStr}"
    | ANF.If (cond, thenBranch, elseBranch) ->
        let condStr = prettyPrintANFAtom cond
        let thenStr = prettyPrintANFExpr thenBranch
        let elseStr = prettyPrintANFExpr elseBranch
        $"if {condStr} then\n  {thenStr}\nelse\n  {elseStr}"

/// Pretty-print ANF program
let prettyPrintANF (ANF.Program (functions, mainExpr)) : string =
    let funcStrs =
        functions
        |> List.map (fun func ->
            $"Function {func.Name}:\n{prettyPrintANFExpr func.Body}")
        |> String.concat "\n\n"

    let mainStr = prettyPrintANFExpr mainExpr

    if List.isEmpty functions then
        mainStr
    else
        funcStrs + "\n\nMain:\n" + mainStr

/// Load ANF→MIR test from file
let loadANF2MIRTest (path: string) : Result<ANF.Program * MIR.Program, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-ANF" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseANF inputText with
            | Error e -> Error $"Failed to parse INPUT-ANF: {e}"
            | Ok anfProgram ->
                match getRequiredSection "OUTPUT-MIR" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseMIR outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-MIR: {e}"
                    | Ok mirProgram -> Ok (anfProgram, mirProgram)

/// Run ANF→MIR test
let runANF2MIRTest (input: ANF.Program) (expected: MIR.Program) : PassTestResult =
    // Pass empty TypeMap and TypeReg since payload sizes are stored in instructions
    // Use TInt64 as default for pass tests (E2E tests use actual program type)
    let emptyTypeMap : ANF.TypeMap = Map.empty
    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
    match ANF_to_MIR.toMIR input MIR.initialRegGen emptyTypeMap emptyTypeReg AST.TInt64 with
    | Error err ->
        { Success = false
          Message = $"MIR conversion error: {err}"
          Expected = Some (prettyPrintMIR expected)
          Actual = None }
    | Ok (actual, _) ->
        if actual = expected then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "Output mismatch"
              Expected = Some (prettyPrintMIR expected)
              Actual = Some (prettyPrintMIR actual) }

/// Pretty-print ARM64 register
let prettyPrintARM64Reg = function
    | ARM64.X0 -> "X0" | ARM64.X1 -> "X1" | ARM64.X2 -> "X2" | ARM64.X3 -> "X3"
    | ARM64.X4 -> "X4" | ARM64.X5 -> "X5" | ARM64.X6 -> "X6" | ARM64.X7 -> "X7"
    | ARM64.X8 -> "X8" | ARM64.X9 -> "X9" | ARM64.X10 -> "X10" | ARM64.X11 -> "X11"
    | ARM64.X12 -> "X12" | ARM64.X13 -> "X13" | ARM64.X14 -> "X14" | ARM64.X15 -> "X15"
    | ARM64.X16 -> "X16"
    | ARM64.X19 -> "X19" | ARM64.X20 -> "X20" | ARM64.X21 -> "X21" | ARM64.X22 -> "X22"
    | ARM64.X23 -> "X23" | ARM64.X24 -> "X24" | ARM64.X25 -> "X25" | ARM64.X26 -> "X26"
    | ARM64.X27 -> "X27" | ARM64.X28 -> "X28"
    | ARM64.X29 -> "X29" | ARM64.X30 -> "X30" | ARM64.SP -> "SP"

/// Pretty-print ARM64 instruction
let prettyPrintARM64Instr = function
    | ARM64.MOVZ (dest, imm, shift) ->
        $"MOVZ({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.MOVK (dest, imm, shift) ->
        $"MOVK({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.ADD_imm (dest, src, imm) ->
        $"ADD_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.ADD_reg (dest, src1, src2) ->
        $"ADD_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SUB_imm (dest, src, imm) ->
        $"SUB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.SUB_reg (dest, src1, src2) ->
        $"SUB_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MUL (dest, src1, src2) ->
        $"MUL({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SDIV (dest, src1, src2) ->
        $"SDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.UDIV (dest, src1, src2) ->
        $"UDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MSUB (dest, src1, src2, src3) ->
        $"MSUB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64.MOV_reg (dest, src) ->
        $"MOV_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.STRB (src, addr, offset) ->
        $"STRB({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDRB (dest, baseAddr, index) ->
        $"LDRB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {prettyPrintARM64Reg index})"
    | ARM64.LDRB_imm (dest, baseAddr, offset) ->
        $"LDRB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {offset})"
    | ARM64.STRB_reg (src, addr) ->
        $"STRB_reg({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr})"
    | ARM64.CMP_imm (src, imm) ->
        $"CMP_imm({prettyPrintARM64Reg src}, {imm})"
    | ARM64.CMP_reg (src1, src2) ->
        $"CMP_reg({prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.CSET (dest, cond) ->
        $"CSET({prettyPrintARM64Reg dest}, {cond})"
    | ARM64.AND_reg (dest, src1, src2) ->
        $"AND_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.ORR_reg (dest, src1, src2) ->
        $"ORR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.EOR_reg (dest, src1, src2) ->
        $"EOR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.LSL_reg (dest, src, shift) ->
        $"LSL_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64.LSR_reg (dest, src, shift) ->
        $"LSR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64.MVN (dest, src) ->
        $"MVN({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.CBZ (reg, label) ->
        $"CBZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBZ_offset (reg, offset) ->
        $"CBZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.CBNZ (reg, label) ->
        $"CBNZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBNZ_offset (reg, offset) ->
        $"CBNZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.TBNZ (reg, bit, offset) ->
        $"TBNZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64.B offset ->
        $"B({offset})"
    | ARM64.B_cond (cond, offset) ->
        $"B_cond({cond}, {offset})"
    | ARM64.B_label label ->
        $"B_label({label})"
    | ARM64.NEG (dest, src) ->
        $"NEG({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.STP (reg1, reg2, addr, offset) ->
        $"STP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDP (reg1, reg2, addr, offset) ->
        $"LDP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STR (src, addr, offset) ->
        $"STR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDR (dest, addr, offset) ->
        $"LDR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STUR (src, addr, offset) ->
        $"STUR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDUR (dest, addr, offset) ->
        $"LDUR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.BL label ->
        $"BL({label})"
    | ARM64.BLR reg ->
        $"BLR({prettyPrintARM64Reg reg})"
    | ARM64.RET -> "RET"
    | ARM64.SVC imm -> $"SVC({imm})"
    | ARM64.Label label -> $"Label({label})"
    | ARM64.ADRP (dest, label) ->
        $"ADRP({prettyPrintARM64Reg dest}, {label})"
    | ARM64.ADD_label (dest, src, label) ->
        $"ADD_label({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {label})"
    | ARM64.ADR (dest, label) ->
        $"ADR({prettyPrintARM64Reg dest}, {label})"
    // Floating-point instructions
    | ARM64.LDR_fp (dest, addr, offset) ->
        $"LDR_fp({dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STR_fp (src, addr, offset) ->
        $"STR_fp({src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.FADD (dest, src1, src2) ->
        $"FADD({dest}, {src1}, {src2})"
    | ARM64.FSUB (dest, src1, src2) ->
        $"FSUB({dest}, {src1}, {src2})"
    | ARM64.FMUL (dest, src1, src2) ->
        $"FMUL({dest}, {src1}, {src2})"
    | ARM64.FDIV (dest, src1, src2) ->
        $"FDIV({dest}, {src1}, {src2})"
    | ARM64.FNEG (dest, src) ->
        $"FNEG({dest}, {src})"
    | ARM64.FABS (dest, src) ->
        $"FABS({dest}, {src})"
    | ARM64.FCMP (src1, src2) ->
        $"FCMP({src1}, {src2})"
    | ARM64.FMOV_reg (dest, src) ->
        $"FMOV_reg({dest}, {src})"
    | ARM64.FMOV_to_gp (dest, src) ->
        $"FMOV_to_gp({prettyPrintARM64Reg dest}, {src})"
    | ARM64.FSQRT (dest, src) ->
        $"FSQRT({dest}, {src})"
    | ARM64.SCVTF (dest, src) ->
        $"SCVTF({dest}, {prettyPrintARM64Reg src})"
    | ARM64.FCVTZS (dest, src) ->
        $"FCVTZS({prettyPrintARM64Reg dest}, {src})"

/// Pretty-print ARM64 program (filtering out Label pseudo-instructions)
let prettyPrintARM64 (instrs: ARM64.Instr list) : string =
    instrs
    |> List.filter (function | ARM64.Label _ -> false | _ -> true)
    |> List.map prettyPrintARM64Instr
    |> String.concat "\n"

/// Load LIR→ARM64 test from file
let loadLIR2ARM64Test (path: string) : Result<LIR.Program * ARM64.Instr list, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-LIR" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseLIR inputText with
            | Error e -> Error $"Failed to parse INPUT-LIR: {e}"
            | Ok lirProgram ->
                match getRequiredSection "OUTPUT-ARM64" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseARM64 outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-ARM64: {e}"
                    | Ok arm64Instrs -> Ok (lirProgram, arm64Instrs)

/// Run LIR→ARM64 test
let runLIR2ARM64Test (input: LIR.Program) (expected: ARM64.Instr list) : PassTestResult =
    match CodeGen.generateARM64 input with
    | Error err ->
        { Success = false
          Message = $"Code generation failed: {err}"
          Expected = Some (prettyPrintARM64 expected)
          Actual = None }
    | Ok actualRaw ->
        // Filter out Label pseudo-instructions for comparison
        let actual = actualRaw |> List.filter (function | ARM64.Label _ -> false | _ -> true)
        if actual = expected then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "Output mismatch"
              Expected = Some (prettyPrintARM64 expected)
              Actual = Some (prettyPrintARM64 actualRaw) }
