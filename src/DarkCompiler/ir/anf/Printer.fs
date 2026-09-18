// Printer.fs - Format ANF functions and scoped or summarized dumps.

module ANFPrinter

open MemoryModel
open ANF
open MIR
open LIR
open IRPrinting

/// Pretty-print ANF atom
let private prettyPrintANFAtom = function
    | ANF.UnitLiteral -> "()"
    | ANF.IntLiteral n -> ANF.sizedIntToString n
    | ANF.BoolLiteral b -> if b then "true" else "false"
    | ANF.StringLiteral s -> $"\"{escapeStringContent s}\""
    | ANF.FloatLiteral f -> string f
    | ANF.Var (ANF.TempId n) -> $"t{n}"
    | ANF.FuncRef name -> $"&{name}"

/// Pretty-print ANF binary operator
let private prettyPrintANFOp = function
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
let private prettyPrintANFUnaryOp = function
    | ANF.Neg -> "-"
    | ANF.Not -> "!"
    | ANF.BitNot -> "~~~"

let private prettyPrintANFRcKind = function
    | MemoryModel.GenericHeap -> "generic"
    | MemoryModel.StreamHeap -> "stream"
    | MemoryModel.TaggedList -> "list"
    | MemoryModel.DictHeap -> "dict"
    | MemoryModel.ClosureHeap -> "closure"

let internal prettyPrintCanonicalBufferKind = function
    | MemoryModel.Utf8String -> "utf8-string"
    | MemoryModel.GraphemeCluster -> "grapheme-cluster"

/// Pretty-print ANF complex expression
let private prettyPrintANFCExpr = function
    | ANF.Atom atom -> prettyPrintANFAtom atom
    | ANF.TypedAtom (atom, typ) -> $"{prettyPrintANFAtom atom} : {typ}"
    | ANF.Prim (op, left, right) ->
        $"{prettyPrintANFAtom left} {prettyPrintANFOp op} {prettyPrintANFAtom right}"
    | ANF.UnaryPrim (op, operand) ->
        $"{prettyPrintANFUnaryOp op}{prettyPrintANFAtom operand}"
    | ANF.Call (funcName, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"{funcName}({argStr})"
    | ANF.CanonicalBufferEq (kind, left, right) ->
        $"CanonicalBufferEq[{prettyPrintCanonicalBufferKind kind}]({prettyPrintANFAtom left}, {prettyPrintANFAtom right})"
    | ANF.BorrowedCall (funcName, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"borrowed {funcName}({argStr})"
    | ANF.IndirectCall (func, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"IndirectCall({prettyPrintANFAtom func}, [{argStr}])"
    | ANF.ClosureAlloc (funcName, captures) ->
        let capsStr = captures |> commaSeparated prettyPrintANFAtom
        $"ClosureAlloc({funcName}, [{capsStr}])"
    | ANF.ClosureCall (closure, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"ClosureCall({prettyPrintANFAtom closure}, [{argStr}])"
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        $"if {prettyPrintANFAtom cond} then {prettyPrintANFAtom thenAtom} else {prettyPrintANFAtom elseAtom}"
    | ANF.TupleAlloc elems ->
        let elemsStr = elems |> commaSeparated prettyPrintANFAtom
        $"({elemsStr})"
    | ANF.TupleGet (tupleAtom, index) ->
        $"{prettyPrintANFAtom tupleAtom}.{index}"
    | ANF.RecordAlloc (descriptor, fields) ->
        let fieldsText = fields |> commaSeparated prettyPrintANFAtom
        $"RecordAlloc({descriptor.RuntimeTypeName}, [{fieldsText}])"
    | ANF.RecordGet (descriptor, recordAtom, index) ->
        $"RecordGet({descriptor.RuntimeTypeName}, {prettyPrintANFAtom recordAtom}, {index})"
    | ANF.RecordClone (descriptor, recordAtom, fields) ->
        let fieldsText = fields |> commaSeparated prettyPrintANFAtom
        $"RecordClone({descriptor.RuntimeTypeName}, {prettyPrintANFAtom recordAtom}, [{fieldsText}])"
    | ANF.RecordReuse (descriptor, recordAtom, fields) ->
        let fieldsText = fields |> commaSeparated prettyPrintANFAtom
        $"RecordReuse({descriptor.RuntimeTypeName}, {prettyPrintANFAtom recordAtom}, [{fieldsText}])"
    | ANF.RefCountInc (atom, payloadSize, kind, _) ->
        $"rc_inc({prettyPrintANFAtom atom}, size={payloadSize}, kind={prettyPrintANFRcKind kind})"
    | ANF.RefCountDec (atom, payloadSize, kind, _) ->
        $"rc_dec({prettyPrintANFAtom atom}, size={payloadSize}, kind={prettyPrintANFRcKind kind})"
    | ANF.StringConcat (left, right) ->
        $"{prettyPrintANFAtom left} ++ {prettyPrintANFAtom right}"
    | ANF.Print (atom, valueType) ->
        $"print({prettyPrintANFAtom atom}, type={valueType})"
    | ANF.StdoutWrite (atom, appendNewline) ->
        $"stdout_write({prettyPrintANFAtom atom}, newline={appendNewline})"
    | ANF.StdinReadLine -> "stdin_read_line()"
    | ANF.RuntimeError message ->
        $"runtime_error(\"{escapeStringContent message}\")"
    | ANF.RuntimeErrorString message ->
        $"runtime_error_string({prettyPrintANFAtom message})"
    | ANF.FileReadText path ->
        $"FileReadText({prettyPrintANFAtom path})"
    | ANF.FileExists path ->
        $"FileExists({prettyPrintANFAtom path})"
    | ANF.FileWriteText (path, content) ->
        $"FileWriteText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    | ANF.FileAppendText (path, content) ->
        $"FileAppendText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    | ANF.FileDelete path ->
        $"FileDelete({prettyPrintANFAtom path})"
    | ANF.FileSetExecutable path ->
        $"FileSetExecutable({prettyPrintANFAtom path})"
    | ANF.FileWriteFromPtr (path, ptr, length) ->
        $"FileWriteFromPtr({prettyPrintANFAtom path}, {prettyPrintANFAtom ptr}, {prettyPrintANFAtom length})"
    | ANF.RawAlloc numBytes ->
        $"RawAlloc({prettyPrintANFAtom numBytes})"
    | ANF.MappedAlloc numBytes ->
        $"MappedAlloc({prettyPrintANFAtom numBytes})"
    | ANF.RawFree ptr ->
        $"RawFree({prettyPrintANFAtom ptr})"
    | ANF.MappedFree ptr ->
        $"MappedFree({prettyPrintANFAtom ptr})"
    | ANF.RawGet (ptr, byteOffset, valueType) ->
        let baseText = $"RawGet({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
        appendTypeSuffix valueType baseText
    | ANF.RawTake (ptr, byteOffset, valueType) ->
        let baseText = $"RawTake({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
        appendTypeSuffix valueType baseText
    | ANF.RawGetByte (ptr, byteOffset) ->
        $"RawGetByte({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
    | ANF.RawWriteWord (ptr, byteOffset, value) ->
        $"RawWriteWord({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
    | ANF.RawWriteByte (ptr, byteOffset, value) ->
        $"RawWriteByte({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
    | ANF.RawSlotInit (ptr, byteOffset, value, valueType) ->
        let baseText = $"RawSlotInit({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
        appendTypeSuffix (Some valueType) baseText
    | ANF.StringToRawPtr value ->
        $"StringToRawPtr({prettyPrintANFAtom value})"
    | ANF.RawPtrToString ptr ->
        $"RawPtrToString({prettyPrintANFAtom ptr})"
    | ANF.BlobToRawPtr value ->
        $"BlobToRawPtr({prettyPrintANFAtom value})"
    | ANF.RawPtrToBlob ptr ->
        $"RawPtrToBlob({prettyPrintANFAtom ptr})"
    | ANF.RawPtrToInt128 ptr ->
        $"RawPtrToInt128({prettyPrintANFAtom ptr})"
    | ANF.RawPtrToUInt128 ptr ->
        $"RawPtrToUInt128({prettyPrintANFAtom ptr})"
    | ANF.DictToRawPtr dict ->
        $"DictToRawPtr({prettyPrintANFAtom dict})"
    | ANF.RawPtrToDict (ptr, tag, dictType) ->
        $"RawPtrToDict({prettyPrintANFAtom ptr}, {prettyPrintANFAtom tag}) : {dictType}"
    | ANF.ListToRawPtr list ->
        $"ListToRawPtr({prettyPrintANFAtom list})"
    | ANF.FixedBlockToRawPtr value ->
        $"FixedBlockToRawPtr({prettyPrintANFAtom value})"
    | ANF.RawPtrToList (ptr, tag, listType) ->
        $"RawPtrToList({prettyPrintANFAtom ptr}, {prettyPrintANFAtom tag}) : {listType}"
    | ANF.FloatSqrt atom ->
        $"FloatSqrt({prettyPrintANFAtom atom})"
    | ANF.FloatAbs atom ->
        $"FloatAbs({prettyPrintANFAtom atom})"
    | ANF.FloatNeg atom ->
        $"FloatNeg({prettyPrintANFAtom atom})"
    | ANF.Int64ToFloat atom ->
        $"Int64ToFloat({prettyPrintANFAtom atom})"
    | ANF.FloatToInt64 atom ->
        $"FloatToInt64({prettyPrintANFAtom atom})"
    | ANF.FloatToBits atom ->
        $"FloatToBits({prettyPrintANFAtom atom})"
    | ANF.FloatToString atom ->
        $"FloatToString({prettyPrintANFAtom atom})"
    | ANF.RefCountIncString str ->
        $"RefCountIncString({prettyPrintANFAtom str})"
    | ANF.RefCountDecString str ->
        $"RefCountDecString({prettyPrintANFAtom str})"
    | ANF.RefCountIncBlob bytes ->
        $"RefCountIncBlob({prettyPrintANFAtom bytes})"
    | ANF.RefCountDecBlob bytes ->
        $"RefCountDecBlob({prettyPrintANFAtom bytes})"
    | ANF.RandomInt64 ->
        "RandomInt64()"
    | ANF.DateTimeNow ->
        "DateTimeNow()"
    | ANF.Sleep delayMs ->
        $"Sleep({prettyPrintANFAtom delayMs})"
    | ANF.CliNative (operation, args) ->
        let argText = args |> commaSeparated prettyPrintANFAtom
        $"CliNative.{operation}({argText})"
    | ANF.TailCall (funcName, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"TailCall({funcName}, [{argStr}])"
    | ANF.IndirectTailCall (func, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"IndirectTailCall({prettyPrintANFAtom func}, [{argStr}])"
    | ANF.ClosureTailCall (closure, args) ->
        let argStr = args |> commaSeparated prettyPrintANFAtom
        $"ClosureTailCall({prettyPrintANFAtom closure}, [{argStr}])"

/// Pretty-print ANF expression
let rec private prettyPrintANFExpr = function
    | ANF.Return atom -> $"return {prettyPrintANFAtom atom}"
    | ANF.Jump (target, atom) -> $"jump {target}({prettyPrintANFAtom atom})"
    | ANF.Join (parameter, continuation, entry) ->
        $"join {parameter.Id}: {parameter.Type} =\n{prettyPrintANFExpr continuation}\nin\n{prettyPrintANFExpr entry}"
    | ANF.Let (var, cexpr, body) ->
        let cexprStr = prettyPrintANFCExpr cexpr
        let bodyStr = prettyPrintANFExpr body
        $"let {var} = {cexprStr}\n{bodyStr}"
    | ANF.If (cond, thenBranch, elseBranch) ->
        let condStr = prettyPrintANFAtom cond
        let thenStr = prettyPrintANFExpr thenBranch
        let elseStr = prettyPrintANFExpr elseBranch
        $"if {condStr} then\n{thenStr}\nelse\n{elseStr}"

/// Format ANF program in a pinned format
let formatANF (ANF.Program (functions, mainExpr)) : string =
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

/// Format only matching ANF functions, optionally as a compact inventory.
let formatANFDump (filter: string option) (summary: bool) (ANF.Program (functions, mainExpr)) : string =
    let selected = functions |> List.filter (fun func -> functionNameMatches filter func.Name)
    match selected, summary with
    | [], _ when Option.isSome filter -> noFunctionMatchText filter
    | _, true ->
        let names = selected |> List.map (fun func -> func.Name)
        String.concat "\n" ($"Functions: {List.length selected}" :: names)
    | _, false -> formatANF (ANF.Program (selected, mainExpr))
