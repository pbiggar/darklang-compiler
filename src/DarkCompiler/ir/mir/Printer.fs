// Printer.fs - Format MIR graphs and scoped or summarized dumps.

module MIRPrinter

open MemoryModel
open ANF
open MIR
open LIR
open IRPrinting
open ANFPrinter

/// Pretty-print MIR operand
let private prettyPrintMIROperand = function
    | MIR.Int64Const n -> string n
    | MIR.BoolConst b -> if b then "true" else "false"
    | MIR.FloatSymbol value -> $"float[{value}]"
    | MIR.StringSymbol value -> $"str[{escapeStringContent value}]"
    | MIR.Register (MIR.VReg n) -> $"v{n}"
    | MIR.FuncAddr name -> $"&{name}"

/// Pretty-print MIR operator
let private prettyPrintMIROp = function
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

/// Pretty-print MIR unary operator
let private prettyPrintMIRUnaryOp = function
    | MIR.Neg -> "-"
    | MIR.Not -> "!"
    | MIR.BitNot -> "~~~"

let private prettyPrintMIRRcKind = function
    | MIR.GenericHeap -> "generic"
    | MIR.StreamHeap -> "stream"
    | MIR.TaggedList -> "list"
    | MIR.DictHeap -> "dict"
    | MIR.ClosureHeap -> "closure"

/// Pretty-print MIR virtual register
let private prettyPrintMIRVReg (MIR.VReg n) : string =
    $"v{n}"

/// Pretty-print MIR label
let private prettyPrintMIRLabel (MIR.Label name) : string =
    name

/// Pretty-print MIR instruction
let private prettyPrintMIRInstr (instr: MIR.Instr) : string =
    match instr with
    | MIR.Mov (dest, src, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- {prettyPrintMIROperand src}"
        appendTypeSuffix valueType baseText
    | MIR.BinOp (dest, op, left, right, operandType) ->
        $"{prettyPrintMIRVReg dest} <- {prettyPrintMIROperand left} {prettyPrintMIROp op} {prettyPrintMIROperand right} : {operandType}"
    | MIR.UnaryOp (dest, op, src) ->
        $"{prettyPrintMIRVReg dest} <- {prettyPrintMIRUnaryOp op}{prettyPrintMIROperand src}"
    | MIR.Call (dest, funcName, args, _, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- Call({funcName}, [{argStr}])"
    | MIR.CanonicalBufferEq (dest, kind, left, right) ->
        $"{prettyPrintMIRVReg dest} <- CanonicalBufferEq[{prettyPrintCanonicalBufferKind kind}]({prettyPrintMIROperand left}, {prettyPrintMIROperand right})"
    | MIR.TailCall (funcName, args, _, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"TailCall({funcName}, [{argStr}])"
    | MIR.IndirectCall (dest, func, args, _, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- IndirectCall({prettyPrintMIROperand func}, [{argStr}])"
    | MIR.IndirectTailCall (func, args, _, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"IndirectTailCall({prettyPrintMIROperand func}, [{argStr}])"
    | MIR.ClosureAlloc (dest, funcName, captures) ->
        let capsStr = captures |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- ClosureAlloc({funcName}, [{capsStr}])"
    | MIR.ClosureCall (dest, closure, args, _, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- ClosureCall({prettyPrintMIROperand closure}, [{argStr}])"
    | MIR.ClosureTailCall (closure, args, _) ->
        let argStr = args |> commaSeparated prettyPrintMIROperand
        $"ClosureTailCall({prettyPrintMIROperand closure}, [{argStr}])"
    | MIR.HeapAlloc (dest, sizeBytes) ->
        $"{prettyPrintMIRVReg dest} <- HeapAlloc({sizeBytes})"
    | MIR.HeapStore (addr, offset, src, valueType) ->
        let baseText = $"HeapStore({prettyPrintMIRVReg addr}, {offset}, {prettyPrintMIROperand src})"
        appendTypeSuffix valueType baseText
    | MIR.HeapLoad (dest, addr, offset, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- HeapLoad({prettyPrintMIRVReg addr}, {offset})"
        appendTypeSuffix valueType baseText
    | MIR.StringConcat (dest, first, second, remaining) ->
        let operands = first :: second :: remaining |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- StringConcat({operands})"
    | MIR.RefCountInc (addr, payloadSize, kind, _) ->
        $"RefCountInc({prettyPrintMIRVReg addr}, size={payloadSize}, kind={prettyPrintMIRRcKind kind})"
    | MIR.RefCountDec (addr, payloadSize, kind, _) ->
        $"RefCountDec({prettyPrintMIRVReg addr}, size={payloadSize}, kind={prettyPrintMIRRcKind kind})"
    | MIR.Print (src, valueType) ->
        $"Print({prettyPrintMIROperand src}, type={valueType})"
    | MIR.StdoutWrite (_, src, appendNewline) ->
        $"StdoutWrite({prettyPrintMIROperand src}, newline={appendNewline})"
    | MIR.StdinReadLine dest ->
        $"{prettyPrintMIRVReg dest} <- StdinReadLine()"
    | MIR.RuntimeError message ->
        $"RuntimeError(\"{escapeStringContent message}\")"
    | MIR.RuntimeErrorString message ->
        $"RuntimeErrorString({prettyPrintMIROperand message})"
    | MIR.FileReadText (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileReadText({prettyPrintMIROperand path})"
    | MIR.FileExists (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileExists({prettyPrintMIROperand path})"
    | MIR.FileWriteText (dest, path, content) ->
        $"{prettyPrintMIRVReg dest} <- FileWriteText({prettyPrintMIROperand path}, {prettyPrintMIROperand content})"
    | MIR.FileAppendText (dest, path, content) ->
        $"{prettyPrintMIRVReg dest} <- FileAppendText({prettyPrintMIROperand path}, {prettyPrintMIROperand content})"
    | MIR.FileDelete (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileDelete({prettyPrintMIROperand path})"
    | MIR.FileSetExecutable (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileSetExecutable({prettyPrintMIROperand path})"
    | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
        $"{prettyPrintMIRVReg dest} <- FileWriteFromPtr({prettyPrintMIROperand path}, {prettyPrintMIROperand ptr}, {prettyPrintMIROperand length})"
    | MIR.FloatSqrt (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatSqrt({prettyPrintMIROperand src})"
    | MIR.FloatAbs (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatAbs({prettyPrintMIROperand src})"
    | MIR.FloatNeg (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatNeg({prettyPrintMIROperand src})"
    | MIR.Int64ToFloat (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- Int64ToFloat({prettyPrintMIROperand src})"
    | MIR.FloatToInt64 (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatToInt64({prettyPrintMIROperand src})"
    | MIR.FloatToBits (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatToBits({prettyPrintMIROperand src})"
    | MIR.RawAlloc (dest, numBytes) ->
        $"{prettyPrintMIRVReg dest} <- RawAlloc({prettyPrintMIROperand numBytes})"
    | MIR.MappedAlloc (dest, numBytes) ->
        $"{prettyPrintMIRVReg dest} <- MappedAlloc({prettyPrintMIROperand numBytes})"
    | MIR.RawFree ptr ->
        $"RawFree({prettyPrintMIROperand ptr})"
    | MIR.MappedFree ptr ->
        $"MappedFree({prettyPrintMIROperand ptr})"
    | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- RawGet({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset})"
        appendTypeSuffix valueType baseText
    | MIR.RawGetByte (dest, ptr, byteOffset) ->
        $"{prettyPrintMIRVReg dest} <- RawGetByte({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset})"
    | MIR.RawWriteWord (ptr, byteOffset, value) ->
        $"RawWriteWord({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset}, {prettyPrintMIROperand value})"
    | MIR.RawWriteByte (ptr, byteOffset, value) ->
        $"RawWriteByte({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset}, {prettyPrintMIROperand value})"
    | MIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        let baseText = $"RawSlotInit({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset}, {prettyPrintMIROperand value})"
        appendTypeSuffix (Some valueType) baseText
    | MIR.StringToRawPtr (dest, value) ->
        $"{prettyPrintMIRVReg dest} <- StringToRawPtr({prettyPrintMIROperand value})"
    | MIR.RawPtrToString (dest, ptr) ->
        $"{prettyPrintMIRVReg dest} <- RawPtrToString({prettyPrintMIROperand ptr})"
    | MIR.BlobToRawPtr (dest, value) ->
        $"{prettyPrintMIRVReg dest} <- BlobToRawPtr({prettyPrintMIROperand value})"
    | MIR.RawPtrToBlob (dest, ptr) ->
        $"{prettyPrintMIRVReg dest} <- RawPtrToBlob({prettyPrintMIROperand ptr})"
    | MIR.DictToRawPtr (dest, dict) ->
        $"{prettyPrintMIRVReg dest} <- DictToRawPtr({prettyPrintMIROperand dict})"
    | MIR.RawPtrToDict (dest, ptr, tag) ->
        $"{prettyPrintMIRVReg dest} <- RawPtrToDict({prettyPrintMIROperand ptr}, {prettyPrintMIROperand tag})"
    | MIR.ListToRawPtr (dest, list) ->
        $"{prettyPrintMIRVReg dest} <- ListToRawPtr({prettyPrintMIROperand list})"
    | MIR.RawPtrToList (dest, ptr, tag) ->
        $"{prettyPrintMIRVReg dest} <- RawPtrToList({prettyPrintMIROperand ptr}, {prettyPrintMIROperand tag})"
    | MIR.RefCountIncString str ->
        $"RefCountIncString({prettyPrintMIROperand str})"
    | MIR.RefCountDecString str ->
        $"RefCountDecString({prettyPrintMIROperand str})"
    | MIR.RefCountIncInt value ->
        $"RefCountIncInt({prettyPrintMIROperand value})"
    | MIR.RefCountDecInt value ->
        $"RefCountDecInt({prettyPrintMIROperand value})"
    | MIR.RefCountIncBlob bytes ->
        $"RefCountIncBlob({prettyPrintMIROperand bytes})"
    | MIR.RefCountDecBlob bytes ->
        $"RefCountDecBlob({prettyPrintMIROperand bytes})"
    | MIR.RandomInt64 dest ->
        $"{prettyPrintMIRVReg dest} <- RandomInt64()"
    | MIR.DateTimeNow dest ->
        $"{prettyPrintMIRVReg dest} <- DateTimeNow()"
    | MIR.Sleep (effectId, dest, delayMs) ->
        $"{prettyPrintMIRVReg dest} <- Sleep#{effectId}({prettyPrintMIROperand delayMs})"
    | MIR.CliNative (dest, operation, args) ->
        let argText = args |> commaSeparated prettyPrintMIROperand
        $"{prettyPrintMIRVReg dest} <- CliNative.{operation}({argText})"
    | MIR.FloatToString (dest, value) ->
        $"{prettyPrintMIRVReg dest} <- FloatToString({prettyPrintMIROperand value})"
    | MIR.Phi (dest, sources, valueType) ->
        let srcStrs =
            sources
            |> commaSeparated (fun (operand, label) -> $"({prettyPrintMIROperand operand}, {prettyPrintMIRLabel label})")
        let baseText = $"{prettyPrintMIRVReg dest} <- Phi([{srcStrs}])"
        appendTypeSuffix valueType baseText
    | MIR.CoverageHit exprId ->
        $"CoverageHit({exprId})"

/// Pretty-print MIR terminator
let private prettyPrintMIRTerminator (term: MIR.Terminator) : string =
    match term with
    | MIR.Ret operand -> $"ret {prettyPrintMIROperand operand}"
    | MIR.Branch (cond, trueLabel, falseLabel) ->
        $"branch {prettyPrintMIROperand cond} ? {prettyPrintMIRLabel trueLabel} : {prettyPrintMIRLabel falseLabel}"
    | MIR.Jump label -> $"jump {prettyPrintMIRLabel label}"

/// Format MIR program with CFG structure
let formatMIR (program: MIR.Program) : string =
    let (MIR.Program (functions, _, _)) = program
    let prettyPrintBlock (block: MIR.BasicBlock) =
        let labelLine = $"  {prettyPrintMIRLabel block.Label}:"
        let instrLines = block.Instrs |> List.map prettyPrintMIRInstr |> List.map (fun line -> $"    {line}")
        let termLine = $"    {prettyPrintMIRTerminator block.Terminator}"
        String.concat "\n" (labelLine :: instrLines @ [termLine])

    let prettyPrintFunction (func: MIR.Function) =
        let entryLabel = func.CFG.Entry
        let entryBlock =
            Map.tryFind entryLabel func.CFG.Blocks
            |> Option.map (fun block -> (entryLabel, block))
        let otherBlocks =
            func.CFG.Blocks
            |> Map.remove entryLabel
            |> Map.toList
            |> List.sortBy (fun (label, _) -> prettyPrintMIRLabel label)
        let orderedBlocks =
            match entryBlock with
            | Some block -> block :: otherBlocks
            | None -> otherBlocks
        let blockLines =
            orderedBlocks
            |> List.map (fun (_, block) -> prettyPrintBlock block)
            |> String.concat "\n"
        if blockLines = "" then
            $"Function {func.Name}:\n  <empty>"
        else
            $"Function {func.Name}:\n{blockLines}"

    functions
    |> List.map prettyPrintFunction
    |> String.concat "\n\n"

/// Format only matching MIR functions, optionally as block/instruction counts.
let formatMIRDump (filter: string option) (summary: bool) (MIR.Program (functions, variants, records)) : string =
    let selected = functions |> List.filter (fun func -> functionNameMatches filter func.Name)
    match selected, summary with
    | [], _ when Option.isSome filter -> noFunctionMatchText filter
    | _, true ->
        let functionLines =
            selected
            |> List.map (fun func ->
                let blockCount = Map.count func.CFG.Blocks
                let instructionCount =
                    func.CFG.Blocks
                    |> Map.toList
                    |> List.sumBy (fun (_, block) -> List.length block.Instrs)
                $"{func.Name}: {blockCount} blocks, {instructionCount} instructions")
        String.concat "\n" ($"Functions: {List.length selected}" :: functionLines)
    | _, false -> formatMIR (MIR.Program (selected, variants, records))
