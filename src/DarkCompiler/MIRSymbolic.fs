// MIRSymbolic.fs - Symbolic Mid-level Intermediate Representation
//
// Defines a symbolic MIR that stores string/float references by value instead of pool index.
// This delays pool resolution until all constants are known.

module MIRSymbolic

/// Reuse core types from MIR
type VReg = MIR.VReg
type TypedMIRParam = MIR.TypedMIRParam
type BinOp = MIR.BinOp
type UnaryOp = MIR.UnaryOp
type Label = MIR.Label
type VariantInfo = MIR.VariantInfo
type TypeVariants = MIR.TypeVariants
type VariantRegistry = MIR.VariantRegistry
type RecordField = MIR.RecordField
type RecordRegistry = MIR.RecordRegistry

/// Operands (symbolic string/float references)
type Operand =
    | IntConst of int64
    | BoolConst of bool
    | FloatSymbol of float
    | StringSymbol of string
    | Register of VReg
    | FuncAddr of string

/// Instructions (symbolic)
type Instr =
    | Mov of dest:VReg * src:Operand * valueType:AST.Type option
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand * operandType:AST.Type
    | UnaryOp of dest:VReg * op:UnaryOp * src:Operand
    | Call of dest:VReg * funcName:string * args:Operand list * argTypes:AST.Type list * returnType:AST.Type
    | TailCall of funcName:string * args:Operand list * argTypes:AST.Type list * returnType:AST.Type
    | IndirectCall of dest:VReg * func:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type
    | IndirectTailCall of func:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type
    | ClosureAlloc of dest:VReg * funcName:string * captures:Operand list
    | ClosureCall of dest:VReg * closure:Operand * args:Operand list * argTypes:AST.Type list
    | ClosureTailCall of closure:Operand * args:Operand list * argTypes:AST.Type list
    | HeapAlloc of dest:VReg * sizeBytes:int
    | HeapStore of addr:VReg * offset:int * src:Operand * valueType:AST.Type option
    | HeapLoad of dest:VReg * addr:VReg * offset:int * valueType:AST.Type option
    | StringConcat of dest:VReg * left:Operand * right:Operand
    | RefCountInc of addr:VReg * payloadSize:int
    | RefCountDec of addr:VReg * payloadSize:int
    | Print of src:Operand * valueType:AST.Type
    | FileReadText of dest:VReg * path:Operand
    | FileExists of dest:VReg * path:Operand
    | FileWriteText of dest:VReg * path:Operand * content:Operand
    | FileAppendText of dest:VReg * path:Operand * content:Operand
    | FileDelete of dest:VReg * path:Operand
    | FileSetExecutable of dest:VReg * path:Operand
    | FileWriteFromPtr of dest:VReg * path:Operand * ptr:Operand * length:Operand
    | FloatSqrt of dest:VReg * src:Operand
    | FloatAbs of dest:VReg * src:Operand
    | FloatNeg of dest:VReg * src:Operand
    | IntToFloat of dest:VReg * src:Operand
    | FloatToInt of dest:VReg * src:Operand
    | RawAlloc of dest:VReg * numBytes:Operand
    | RawFree of ptr:Operand
    | RawGet of dest:VReg * ptr:Operand * byteOffset:Operand * valueType:AST.Type option
    | RawGetByte of dest:VReg * ptr:Operand * byteOffset:Operand
    | RawSet of ptr:Operand * byteOffset:Operand * value:Operand * valueType:AST.Type option
    | RawSetByte of ptr:Operand * byteOffset:Operand * value:Operand
    | StringHash of dest:VReg * str:Operand
    | StringEq of dest:VReg * left:Operand * right:Operand
    | RefCountIncString of str:Operand
    | RefCountDecString of str:Operand
    | RandomInt64 of dest:VReg
    | FloatToString of dest:VReg * value:Operand
    | Phi of dest:VReg * sources:(Operand * Label) list * valueType:AST.Type option
    | CoverageHit of exprId:int

/// Terminator instructions (control flow)
type Terminator =
    | Ret of Operand
    | Branch of cond:Operand * trueLabel:Label * falseLabel:Label
    | Jump of Label

/// Basic block with label, instructions, and terminator
type BasicBlock = {
    Label: Label
    Instrs: Instr list
    Terminator: Terminator
}

/// Control Flow Graph
type CFG = {
    Entry: Label
    Blocks: Map<Label, BasicBlock>
}

/// MIR function with CFG
type Function = {
    Name: string
    TypedParams: TypedMIRParam list
    ReturnType: AST.Type
    CFG: CFG
    FloatRegs: Set<int>
}

/// Symbolic MIR program (no pools)
type Program = Program of functions:Function list * variants:VariantRegistry * records:RecordRegistry

/// Pool state for resolving symbolic references
type PoolState = {
    StringPool: MIR.StringPool
    FloatPool: MIR.FloatPool
}

/// Convert indexed MIR into symbolic MIR
let fromMIR (program: MIR.Program) : Result<Program, string> =
    let (MIR.Program (functions, stringPool, floatPool, variants, records)) = program

    let rec mapResults (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
        match items with
        | [] -> Ok []
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok mapped ->
                mapResults f rest |> Result.map (fun tail -> mapped :: tail)

    let symbolOperand (op: MIR.Operand) : Result<Operand, string> =
        match op with
        | MIR.IntConst value -> Ok (IntConst value)
        | MIR.BoolConst value -> Ok (BoolConst value)
        | MIR.Register reg -> Ok (Register reg)
        | MIR.FuncAddr name -> Ok (FuncAddr name)
        | MIR.FloatRef idx ->
            match Map.tryFind idx floatPool.Floats with
            | Some value -> Ok (FloatSymbol value)
            | None -> Error $"Missing float pool index {idx}"
        | MIR.StringRef idx ->
            match Map.tryFind idx stringPool.Strings with
            | Some (value, _len) -> Ok (StringSymbol value)
            | None -> Error $"Missing string pool index {idx}"

    let symbolOperands (ops: MIR.Operand list) : Result<Operand list, string> =
        mapResults symbolOperand ops

    let symbolOperandPairs (pairs: (MIR.Operand * Label) list) : Result<(Operand * Label) list, string> =
        mapResults
            (fun (op, lbl) -> symbolOperand op |> Result.map (fun op' -> (op', lbl)))
            pairs

    let symbolInstr (instr: MIR.Instr) : Result<Instr, string> =
        match instr with
        | MIR.Mov (dest, src, valueType) ->
            symbolOperand src |> Result.map (fun src' -> Mov (dest, src', valueType))
        | MIR.BinOp (dest, op, left, right, operandType) ->
            match symbolOperand left with
            | Error err -> Error err
            | Ok left' ->
                symbolOperand right
                |> Result.map (fun right' -> BinOp (dest, op, left', right', operandType))
        | MIR.UnaryOp (dest, op, src) ->
            symbolOperand src |> Result.map (fun src' -> UnaryOp (dest, op, src'))
        | MIR.Call (dest, funcName, args, argTypes, returnType) ->
            symbolOperands args |> Result.map (fun args' -> Call (dest, funcName, args', argTypes, returnType))
        | MIR.TailCall (funcName, args, argTypes, returnType) ->
            symbolOperands args |> Result.map (fun args' -> TailCall (funcName, args', argTypes, returnType))
        | MIR.IndirectCall (dest, func, args, argTypes, returnType) ->
            match symbolOperand func with
            | Error err -> Error err
            | Ok func' ->
                symbolOperands args
                |> Result.map (fun args' -> IndirectCall (dest, func', args', argTypes, returnType))
        | MIR.IndirectTailCall (func, args, argTypes, returnType) ->
            match symbolOperand func with
            | Error err -> Error err
            | Ok func' ->
                symbolOperands args
                |> Result.map (fun args' -> IndirectTailCall (func', args', argTypes, returnType))
        | MIR.ClosureAlloc (dest, funcName, captures) ->
            symbolOperands captures |> Result.map (fun caps' -> ClosureAlloc (dest, funcName, caps'))
        | MIR.ClosureCall (dest, closure, args, argTypes) ->
            match symbolOperand closure with
            | Error err -> Error err
            | Ok closure' ->
                symbolOperands args |> Result.map (fun args' -> ClosureCall (dest, closure', args', argTypes))
        | MIR.ClosureTailCall (closure, args, argTypes) ->
            match symbolOperand closure with
            | Error err -> Error err
            | Ok closure' ->
                symbolOperands args |> Result.map (fun args' -> ClosureTailCall (closure', args', argTypes))
        | MIR.HeapAlloc (dest, sizeBytes) -> Ok (HeapAlloc (dest, sizeBytes))
        | MIR.HeapStore (addr, offset, src, valueType) ->
            symbolOperand src |> Result.map (fun src' -> HeapStore (addr, offset, src', valueType))
        | MIR.HeapLoad (dest, addr, offset, valueType) -> Ok (HeapLoad (dest, addr, offset, valueType))
        | MIR.StringConcat (dest, left, right) ->
            match symbolOperand left with
            | Error err -> Error err
            | Ok left' ->
                symbolOperand right |> Result.map (fun right' -> StringConcat (dest, left', right'))
        | MIR.RefCountInc (addr, payloadSize) -> Ok (RefCountInc (addr, payloadSize))
        | MIR.RefCountDec (addr, payloadSize) -> Ok (RefCountDec (addr, payloadSize))
        | MIR.Print (src, valueType) ->
            symbolOperand src |> Result.map (fun src' -> Print (src', valueType))
        | MIR.FileReadText (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileReadText (dest, path'))
        | MIR.FileExists (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileExists (dest, path'))
        | MIR.FileWriteText (dest, path, content) ->
            match symbolOperand path with
            | Error err -> Error err
            | Ok path' ->
                symbolOperand content |> Result.map (fun content' -> FileWriteText (dest, path', content'))
        | MIR.FileAppendText (dest, path, content) ->
            match symbolOperand path with
            | Error err -> Error err
            | Ok path' ->
                symbolOperand content |> Result.map (fun content' -> FileAppendText (dest, path', content'))
        | MIR.FileDelete (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileDelete (dest, path'))
        | MIR.FileSetExecutable (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileSetExecutable (dest, path'))
        | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
            match symbolOperand path with
            | Error err -> Error err
            | Ok path' ->
                match symbolOperand ptr with
                | Error err -> Error err
                | Ok ptr' ->
                    symbolOperand length
                    |> Result.map (fun len' -> FileWriteFromPtr (dest, path', ptr', len'))
        | MIR.FloatSqrt (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> FloatSqrt (dest, src'))
        | MIR.FloatAbs (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> FloatAbs (dest, src'))
        | MIR.FloatNeg (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> FloatNeg (dest, src'))
        | MIR.IntToFloat (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> IntToFloat (dest, src'))
        | MIR.FloatToInt (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> FloatToInt (dest, src'))
        | MIR.RawAlloc (dest, numBytes) ->
            symbolOperand numBytes |> Result.map (fun numBytes' -> RawAlloc (dest, numBytes'))
        | MIR.RawFree ptr ->
            symbolOperand ptr |> Result.map RawFree
        | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
            match symbolOperand ptr with
            | Error err -> Error err
            | Ok ptr' ->
                symbolOperand byteOffset
                |> Result.map (fun byteOffset' -> RawGet (dest, ptr', byteOffset', valueType))
        | MIR.RawGetByte (dest, ptr, byteOffset) ->
            match symbolOperand ptr with
            | Error err -> Error err
            | Ok ptr' ->
                symbolOperand byteOffset |> Result.map (fun byteOffset' -> RawGetByte (dest, ptr', byteOffset'))
        | MIR.RawSet (ptr, byteOffset, value, valueType) ->
            match symbolOperand ptr with
            | Error err -> Error err
            | Ok ptr' ->
                match symbolOperand byteOffset with
                | Error err -> Error err
                | Ok byteOffset' ->
                    symbolOperand value
                    |> Result.map (fun value' -> RawSet (ptr', byteOffset', value', valueType))
        | MIR.RawSetByte (ptr, byteOffset, value) ->
            match symbolOperand ptr with
            | Error err -> Error err
            | Ok ptr' ->
                match symbolOperand byteOffset with
                | Error err -> Error err
                | Ok byteOffset' ->
                    symbolOperand value |> Result.map (fun value' -> RawSetByte (ptr', byteOffset', value'))
        | MIR.StringHash (dest, str) ->
            symbolOperand str |> Result.map (fun str' -> StringHash (dest, str'))
        | MIR.StringEq (dest, left, right) ->
            match symbolOperand left with
            | Error err -> Error err
            | Ok left' ->
                symbolOperand right |> Result.map (fun right' -> StringEq (dest, left', right'))
        | MIR.RefCountIncString str ->
            symbolOperand str |> Result.map RefCountIncString
        | MIR.RefCountDecString str ->
            symbolOperand str |> Result.map RefCountDecString
        | MIR.RandomInt64 dest -> Ok (RandomInt64 dest)
        | MIR.FloatToString (dest, value) ->
            symbolOperand value |> Result.map (fun value' -> FloatToString (dest, value'))
        | MIR.Phi (dest, sources, valueType) ->
            symbolOperandPairs sources |> Result.map (fun sources' -> Phi (dest, sources', valueType))
        | MIR.CoverageHit exprId -> Ok (CoverageHit exprId)

    let symbolTerminator (term: MIR.Terminator) : Result<Terminator, string> =
        match term with
        | MIR.Ret value ->
            symbolOperand value |> Result.map Ret
        | MIR.Branch (cond, trueLabel, falseLabel) ->
            symbolOperand cond |> Result.map (fun cond' -> Branch (cond', trueLabel, falseLabel))
        | MIR.Jump label -> Ok (Jump label)

    let symbolBlock (block: MIR.BasicBlock) : Result<BasicBlock, string> =
        match mapResults symbolInstr block.Instrs with
        | Error err -> Error err
        | Ok instrs' ->
            match symbolTerminator block.Terminator with
            | Error err -> Error err
            | Ok term' ->
                Ok { Label = block.Label; Instrs = instrs'; Terminator = term' }

    let symbolFunction (func: MIR.Function) : Result<Function, string> =
        func.CFG.Blocks
        |> Map.toList
        |> mapResults (fun (label, block) ->
            symbolBlock block |> Result.map (fun block' -> (label, block')))
        |> Result.map (fun blocks' ->
            let blockMap = Map.ofList blocks'
            { Name = func.Name
              TypedParams = func.TypedParams
              ReturnType = func.ReturnType
              CFG = { Entry = func.CFG.Entry; Blocks = blockMap }
              FloatRegs = func.FloatRegs })

    functions
    |> mapResults symbolFunction
    |> Result.map (fun funcs' -> Program (funcs', variants, records))

/// Resolve symbolic MIR into indexed MIR with pools
let private resolveProgram
    (initialState: PoolState)
    (functions: Function list)
    (variants: VariantRegistry)
    (records: RecordRegistry)
    : Result<MIR.Program, string> =
    let mapWithState
        (f: PoolState -> 'a -> Result<'b * PoolState, string>)
        (state: PoolState)
        (items: 'a list)
        : Result<'b list * PoolState, string> =
        let rec loop acc current remaining =
            match remaining with
            | [] -> Ok (List.rev acc, current)
            | item :: rest ->
                match f current item with
                | Error err -> Error err
                | Ok (mapped, next) -> loop (mapped :: acc) next rest
        loop [] state items

    let resolveOperand (state: PoolState) (op: Operand) : Result<MIR.Operand * PoolState, string> =
        match op with
        | IntConst value -> Ok (MIR.IntConst value, state)
        | BoolConst value -> Ok (MIR.BoolConst value, state)
        | Register reg -> Ok (MIR.Register reg, state)
        | FuncAddr name -> Ok (MIR.FuncAddr name, state)
        | StringSymbol value ->
            let (idx, pool') = MIR.addString state.StringPool value
            Ok (MIR.StringRef idx, { state with StringPool = pool' })
        | FloatSymbol value ->
            let (idx, pool') = MIR.addFloat state.FloatPool value
            Ok (MIR.FloatRef idx, { state with FloatPool = pool' })

    let resolveOperandPairs
        (state: PoolState)
        (pairs: (Operand * Label) list)
        : Result<(MIR.Operand * Label) list * PoolState, string> =
        mapWithState
            (fun st (op, lbl) ->
                resolveOperand st op |> Result.map (fun (op', st') -> ((op', lbl), st')))
            state
            pairs

    let resolveInstr (state: PoolState) (instr: Instr) : Result<MIR.Instr * PoolState, string> =
        match instr with
        | Mov (dest, src, valueType) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.Mov (dest, src', valueType), st))
        | BinOp (dest, op, left, right, operandType) ->
            match resolveOperand state left with
            | Error err -> Error err
            | Ok (left', st1) ->
                resolveOperand st1 right
                |> Result.map (fun (right', st2) -> (MIR.BinOp (dest, op, left', right', operandType), st2))
        | UnaryOp (dest, op, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.UnaryOp (dest, op, src'), st))
        | Call (dest, funcName, args, argTypes, returnType) ->
            mapWithState resolveOperand state args
            |> Result.map (fun (args', st) -> (MIR.Call (dest, funcName, args', argTypes, returnType), st))
        | TailCall (funcName, args, argTypes, returnType) ->
            mapWithState resolveOperand state args
            |> Result.map (fun (args', st) -> (MIR.TailCall (funcName, args', argTypes, returnType), st))
        | IndirectCall (dest, func, args, argTypes, returnType) ->
            match resolveOperand state func with
            | Error err -> Error err
            | Ok (func', st1) ->
                mapWithState resolveOperand st1 args
                |> Result.map (fun (args', st2) -> (MIR.IndirectCall (dest, func', args', argTypes, returnType), st2))
        | IndirectTailCall (func, args, argTypes, returnType) ->
            match resolveOperand state func with
            | Error err -> Error err
            | Ok (func', st1) ->
                mapWithState resolveOperand st1 args
                |> Result.map (fun (args', st2) -> (MIR.IndirectTailCall (func', args', argTypes, returnType), st2))
        | ClosureAlloc (dest, funcName, captures) ->
            mapWithState resolveOperand state captures
            |> Result.map (fun (caps', st) -> (MIR.ClosureAlloc (dest, funcName, caps'), st))
        | ClosureCall (dest, closure, args, argTypes) ->
            match resolveOperand state closure with
            | Error err -> Error err
            | Ok (closure', st1) ->
                mapWithState resolveOperand st1 args
                |> Result.map (fun (args', st2) -> (MIR.ClosureCall (dest, closure', args', argTypes), st2))
        | ClosureTailCall (closure, args, argTypes) ->
            match resolveOperand state closure with
            | Error err -> Error err
            | Ok (closure', st1) ->
                mapWithState resolveOperand st1 args
                |> Result.map (fun (args', st2) -> (MIR.ClosureTailCall (closure', args', argTypes), st2))
        | HeapAlloc (dest, sizeBytes) -> Ok (MIR.HeapAlloc (dest, sizeBytes), state)
        | HeapStore (addr, offset, src, valueType) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.HeapStore (addr, offset, src', valueType), st))
        | HeapLoad (dest, addr, offset, valueType) ->
            Ok (MIR.HeapLoad (dest, addr, offset, valueType), state)
        | StringConcat (dest, left, right) ->
            match resolveOperand state left with
            | Error err -> Error err
            | Ok (left', st1) ->
                resolveOperand st1 right
                |> Result.map (fun (right', st2) -> (MIR.StringConcat (dest, left', right'), st2))
        | RefCountInc (addr, payloadSize) -> Ok (MIR.RefCountInc (addr, payloadSize), state)
        | RefCountDec (addr, payloadSize) -> Ok (MIR.RefCountDec (addr, payloadSize), state)
        | Print (src, valueType) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.Print (src', valueType), st))
        | FileReadText (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (MIR.FileReadText (dest, path'), st))
        | FileExists (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (MIR.FileExists (dest, path'), st))
        | FileWriteText (dest, path, content) ->
            match resolveOperand state path with
            | Error err -> Error err
            | Ok (path', st1) ->
                resolveOperand st1 content
                |> Result.map (fun (content', st2) -> (MIR.FileWriteText (dest, path', content'), st2))
        | FileAppendText (dest, path, content) ->
            match resolveOperand state path with
            | Error err -> Error err
            | Ok (path', st1) ->
                resolveOperand st1 content
                |> Result.map (fun (content', st2) -> (MIR.FileAppendText (dest, path', content'), st2))
        | FileDelete (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (MIR.FileDelete (dest, path'), st))
        | FileSetExecutable (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (MIR.FileSetExecutable (dest, path'), st))
        | FileWriteFromPtr (dest, path, ptr, length) ->
            match resolveOperand state path with
            | Error err -> Error err
            | Ok (path', st1) ->
                match resolveOperand st1 ptr with
                | Error err -> Error err
                | Ok (ptr', st2) ->
                    resolveOperand st2 length
                    |> Result.map (fun (len', st3) -> (MIR.FileWriteFromPtr (dest, path', ptr', len'), st3))
        | FloatSqrt (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.FloatSqrt (dest, src'), st))
        | FloatAbs (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.FloatAbs (dest, src'), st))
        | FloatNeg (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.FloatNeg (dest, src'), st))
        | IntToFloat (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.IntToFloat (dest, src'), st))
        | FloatToInt (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (MIR.FloatToInt (dest, src'), st))
        | RawAlloc (dest, numBytes) ->
            resolveOperand state numBytes |> Result.map (fun (numBytes', st) -> (MIR.RawAlloc (dest, numBytes'), st))
        | RawFree ptr ->
            resolveOperand state ptr |> Result.map (fun (ptr', st) -> (MIR.RawFree ptr', st))
        | RawGet (dest, ptr, byteOffset, valueType) ->
            match resolveOperand state ptr with
            | Error err -> Error err
            | Ok (ptr', st1) ->
                resolveOperand st1 byteOffset
                |> Result.map (fun (byteOffset', st2) -> (MIR.RawGet (dest, ptr', byteOffset', valueType), st2))
        | RawGetByte (dest, ptr, byteOffset) ->
            match resolveOperand state ptr with
            | Error err -> Error err
            | Ok (ptr', st1) ->
                resolveOperand st1 byteOffset
                |> Result.map (fun (byteOffset', st2) -> (MIR.RawGetByte (dest, ptr', byteOffset'), st2))
        | RawSet (ptr, byteOffset, value, valueType) ->
            match resolveOperand state ptr with
            | Error err -> Error err
            | Ok (ptr', st1) ->
                match resolveOperand st1 byteOffset with
                | Error err -> Error err
                | Ok (byteOffset', st2) ->
                    resolveOperand st2 value
                    |> Result.map (fun (value', st3) -> (MIR.RawSet (ptr', byteOffset', value', valueType), st3))
        | RawSetByte (ptr, byteOffset, value) ->
            match resolveOperand state ptr with
            | Error err -> Error err
            | Ok (ptr', st1) ->
                match resolveOperand st1 byteOffset with
                | Error err -> Error err
                | Ok (byteOffset', st2) ->
                    resolveOperand st2 value
                    |> Result.map (fun (value', st3) -> (MIR.RawSetByte (ptr', byteOffset', value'), st3))
        | StringHash (dest, str) ->
            resolveOperand state str |> Result.map (fun (str', st) -> (MIR.StringHash (dest, str'), st))
        | StringEq (dest, left, right) ->
            match resolveOperand state left with
            | Error err -> Error err
            | Ok (left', st1) ->
                resolveOperand st1 right
                |> Result.map (fun (right', st2) -> (MIR.StringEq (dest, left', right'), st2))
        | RefCountIncString str ->
            resolveOperand state str |> Result.map (fun (str', st) -> (MIR.RefCountIncString str', st))
        | RefCountDecString str ->
            resolveOperand state str |> Result.map (fun (str', st) -> (MIR.RefCountDecString str', st))
        | RandomInt64 dest -> Ok (MIR.RandomInt64 dest, state)
        | FloatToString (dest, value) ->
            resolveOperand state value |> Result.map (fun (value', st) -> (MIR.FloatToString (dest, value'), st))
        | Phi (dest, sources, valueType) ->
            resolveOperandPairs state sources
            |> Result.map (fun (sources', st) -> (MIR.Phi (dest, sources', valueType), st))
        | CoverageHit exprId -> Ok (MIR.CoverageHit exprId, state)

    let resolveTerminator (state: PoolState) (term: Terminator) : Result<MIR.Terminator * PoolState, string> =
        match term with
        | Ret value ->
            resolveOperand state value |> Result.map (fun (value', st) -> (MIR.Ret value', st))
        | Branch (cond, trueLabel, falseLabel) ->
            resolveOperand state cond
            |> Result.map (fun (cond', st) -> (MIR.Branch (cond', trueLabel, falseLabel), st))
        | Jump label -> Ok (MIR.Jump label, state)

    let resolveBlock (state: PoolState) (block: BasicBlock) : Result<MIR.BasicBlock * PoolState, string> =
        mapWithState resolveInstr state block.Instrs
        |> Result.bind (fun (instrs', st1) ->
            resolveTerminator st1 block.Terminator
            |> Result.map (fun (term', st2) ->
                ({ Label = block.Label; Instrs = instrs'; Terminator = term' }, st2)))

    let resolveFunction (state: PoolState) (func: Function) : Result<MIR.Function * PoolState, string> =
        let resolveBlockPair st (label, block) =
            resolveBlock st block |> Result.map (fun (block', st') -> ((label, block'), st'))
        mapWithState resolveBlockPair state (Map.toList func.CFG.Blocks)
        |> Result.map (fun (blocks', st) ->
            let blockMap = Map.ofList blocks'
            ({ Name = func.Name
               TypedParams = func.TypedParams
               ReturnType = func.ReturnType
               CFG = { Entry = func.CFG.Entry; Blocks = blockMap }
               FloatRegs = func.FloatRegs }, st))

    mapWithState resolveFunction initialState functions
    |> Result.map (fun (funcs', finalState) ->
        MIR.Program (funcs', finalState.StringPool, finalState.FloatPool, variants, records))

/// Convert symbolic MIR into indexed MIR with fresh pools
let toMIR (Program (functions, variants, records)) : Result<MIR.Program, string> =
    let initialState = { StringPool = MIR.emptyStringPool; FloatPool = MIR.emptyFloatPool }
    resolveProgram initialState functions variants records

/// Convert symbolic MIR into indexed MIR with provided pools
let toMIRWithPools
    (stringPool: MIR.StringPool)
    (floatPool: MIR.FloatPool)
    (Program (functions, variants, records))
    : Result<MIR.Program, string> =
    resolveProgram { StringPool = stringPool; FloatPool = floatPool } functions variants records
