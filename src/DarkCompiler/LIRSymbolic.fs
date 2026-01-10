// LIRSymbolic.fs - Symbolic Low-level Intermediate Representation
//
// Defines a symbolic LIR that stores string/float references by value instead of pool index.
// This is used to delay pool resolution until all constants are known.

module LIRSymbolic

/// Reuse core register and label types from LIR
type PhysReg = LIR.PhysReg
type PhysFPReg = LIR.PhysFPReg
type Reg = LIR.Reg
type FReg = LIR.FReg
type Condition = LIR.Condition
type Label = LIR.Label
type TypedLIRParam = LIR.TypedLIRParam

/// Operands (symbolic string/float references)
type Operand =
    | Imm of int64
    | FloatImm of float
    | Reg of Reg
    | StackSlot of int
    | StringSymbol of string
    | FloatSymbol of float
    | FuncAddr of string

/// Instructions (symbolic)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Phi of dest:Reg * sources:(Operand * Label) list * valueType:AST.Type option
    | Store of stackSlot:int * src:Reg
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg
    | Msub of dest:Reg * mulLeft:Reg * mulRight:Reg * sub:Reg
    | Madd of dest:Reg * mulLeft:Reg * mulRight:Reg * add:Reg
    | Cmp of left:Reg * right:Operand
    | Cset of dest:Reg * cond:Condition
    | And of dest:Reg * left:Reg * right:Reg
    | And_imm of dest:Reg * src:Reg * imm:int64
    | Orr of dest:Reg * left:Reg * right:Reg
    | Eor of dest:Reg * left:Reg * right:Reg
    | Lsl of dest:Reg * src:Reg * shift:Reg
    | Lsr of dest:Reg * src:Reg * shift:Reg
    | Lsl_imm of dest:Reg * src:Reg * shift:int
    | Lsr_imm of dest:Reg * src:Reg * shift:int
    | Mvn of dest:Reg * src:Reg
    | Sxtb of dest:Reg * src:Reg
    | Sxth of dest:Reg * src:Reg
    | Sxtw of dest:Reg * src:Reg
    | Uxtb of dest:Reg * src:Reg
    | Uxth of dest:Reg * src:Reg
    | Uxtw of dest:Reg * src:Reg
    | Call of dest:Reg * funcName:string * args:Operand list
    | TailCall of funcName:string * args:Operand list
    | IndirectCall of dest:Reg * func:Reg * args:Operand list
    | IndirectTailCall of func:Reg * args:Operand list
    | ClosureAlloc of dest:Reg * funcName:string * captures:Operand list
    | ClosureCall of dest:Reg * closure:Reg * args:Operand list
    | ClosureTailCall of closure:Reg * args:Operand list
    | SaveRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    | RestoreRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    | ArgMoves of (PhysReg * Operand) list
    | TailArgMoves of (PhysReg * Operand) list
    | FArgMoves of (PhysFPReg * FReg) list
    | PrintInt of Reg
    | PrintBool of Reg
    | PrintIntNoNewline of Reg
    | PrintBoolNoNewline of Reg
    | PrintFloat of FReg
    | PrintFloatNoNewline of FReg
    | PrintString of string
    | PrintHeapStringNoNewline of Reg
    | PrintChars of byte list
    | PrintBytes of Reg
    | PrintList of listPtr:Reg * elemType:AST.Type
    | PrintSum of sumPtr:Reg * variants:(string * int * AST.Type option) list
    | PrintRecord of recordPtr:Reg * typeName:string * fields:(string * AST.Type) list
    | Exit
    | FPhi of dest:FReg * sources:(FReg * Label) list
    | FMov of dest:FReg * src:FReg
    | FLoad of dest:FReg * floatValue:float
    | FAdd of dest:FReg * left:FReg * right:FReg
    | FSub of dest:FReg * left:FReg * right:FReg
    | FMul of dest:FReg * left:FReg * right:FReg
    | FDiv of dest:FReg * left:FReg * right:FReg
    | FNeg of dest:FReg * src:FReg
    | FAbs of dest:FReg * src:FReg
    | FSqrt of dest:FReg * src:FReg
    | FCmp of left:FReg * right:FReg
    | IntToFloat of dest:FReg * src:Reg
    | FloatToInt of dest:Reg * src:FReg
    | GpToFp of dest:FReg * src:Reg
    | FpToGp of dest:Reg * src:FReg
    | HeapAlloc of dest:Reg * sizeBytes:int
    | HeapStore of addr:Reg * offset:int * src:Operand * valueType:AST.Type option
    | HeapLoad of dest:Reg * addr:Reg * offset:int
    | RefCountInc of addr:Reg * payloadSize:int
    | RefCountDec of addr:Reg * payloadSize:int
    | StringConcat of dest:Reg * left:Operand * right:Operand
    | PrintHeapString of Reg
    | LoadFuncAddr of dest:Reg * funcName:string
    | FileReadText of dest:Reg * path:Operand
    | FileExists of dest:Reg * path:Operand
    | FileWriteText of dest:Reg * path:Operand * content:Operand
    | FileAppendText of dest:Reg * path:Operand * content:Operand
    | FileDelete of dest:Reg * path:Operand
    | FileSetExecutable of dest:Reg * path:Operand
    | FileWriteFromPtr of dest:Reg * path:Operand * ptr:Reg * length:Reg
    | RawAlloc of dest:Reg * numBytes:Reg
    | RawFree of ptr:Reg
    | RawGet of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawGetByte of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawSet of ptr:Reg * byteOffset:Reg * value:Reg
    | RawSetByte of ptr:Reg * byteOffset:Reg * value:Reg
    | StringHash of dest:Reg * str:Operand
    | StringEq of dest:Reg * left:Operand * right:Operand
    | RefCountIncString of str:Operand
    | RefCountDecString of str:Operand
    | RandomInt64 of dest:Reg
    | FloatToString of dest:Reg * value:FReg
    | CoverageHit of exprId:int

/// Terminators
type Terminator =
    | Ret
    | Branch of cond:Reg * trueLabel:Label * falseLabel:Label
    | BranchZero of cond:Reg * zeroLabel:Label * nonZeroLabel:Label
    | BranchBitZero of reg:Reg * bit:int * zeroLabel:Label * nonZeroLabel:Label
    | BranchBitNonZero of reg:Reg * bit:int * nonZeroLabel:Label * zeroLabel:Label
    | CondBranch of cond:Condition * trueLabel:Label * falseLabel:Label
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

/// Function with CFG
type Function = {
    Name: string
    TypedParams: TypedLIRParam list
    CFG: CFG
    StackSize: int
    UsedCalleeSaved: PhysReg list
}

/// Symbolic LIR program (no pools)
type Program = Program of functions:Function list

/// Pool state for resolving symbolic references
type PoolState = {
    StringPool: MIR.StringPool
    FloatPool: MIR.FloatPool
}

/// Convert indexed LIR into symbolic LIR
let fromLIR (program: LIR.Program) : Result<Program, string> =
    let (LIR.Program (functions, stringPool, floatPool)) = program

    let rec mapResults (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
        match items with
        | [] -> Ok []
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok mapped ->
                mapResults f rest |> Result.map (fun tail -> mapped :: tail)

    let symbolOperand (op: LIR.Operand) : Result<Operand, string> =
        match op with
        | LIR.Imm value -> Ok (Imm value)
        | LIR.FloatImm value -> Ok (FloatImm value)
        | LIR.Reg reg -> Ok (Reg reg)
        | LIR.StackSlot slot -> Ok (StackSlot slot)
        | LIR.FuncAddr name -> Ok (FuncAddr name)
        | LIR.StringRef idx ->
            match Map.tryFind idx stringPool.Strings with
            | Some (value, _len) -> Ok (StringSymbol value)
            | None -> Error $"Missing string pool index {idx}"
        | LIR.FloatRef idx ->
            match Map.tryFind idx floatPool.Floats with
            | Some value -> Ok (FloatSymbol value)
            | None -> Error $"Missing float pool index {idx}"

    let symbolOperands (ops: LIR.Operand list) : Result<Operand list, string> =
        mapResults symbolOperand ops

    let symbolInstr (instr: LIR.Instr) : Result<Instr, string> =
        match instr with
        | LIR.Mov (dest, src) ->
            symbolOperand src |> Result.map (fun src' -> Mov (dest, src'))
        | LIR.Phi (dest, sources, valueType) ->
            sources
            |> mapResults (fun (op, lbl) ->
                symbolOperand op |> Result.map (fun op' -> (op', lbl)))
            |> Result.map (fun sources' -> Phi (dest, sources', valueType))
        | LIR.Store (stackSlot, src) -> Ok (Store (stackSlot, src))
        | LIR.Add (dest, left, right) ->
            symbolOperand right |> Result.map (fun right' -> Add (dest, left, right'))
        | LIR.Sub (dest, left, right) ->
            symbolOperand right |> Result.map (fun right' -> Sub (dest, left, right'))
        | LIR.Mul (dest, left, right) -> Ok (Mul (dest, left, right))
        | LIR.Sdiv (dest, left, right) -> Ok (Sdiv (dest, left, right))
        | LIR.Msub (dest, mulLeft, mulRight, sub) -> Ok (Msub (dest, mulLeft, mulRight, sub))
        | LIR.Madd (dest, mulLeft, mulRight, add) -> Ok (Madd (dest, mulLeft, mulRight, add))
        | LIR.Cmp (left, right) ->
            symbolOperand right |> Result.map (fun right' -> Cmp (left, right'))
        | LIR.Cset (dest, cond) -> Ok (Cset (dest, cond))
        | LIR.And (dest, left, right) -> Ok (And (dest, left, right))
        | LIR.And_imm (dest, src, imm) -> Ok (And_imm (dest, src, imm))
        | LIR.Orr (dest, left, right) -> Ok (Orr (dest, left, right))
        | LIR.Eor (dest, left, right) -> Ok (Eor (dest, left, right))
        | LIR.Lsl (dest, src, shift) -> Ok (Lsl (dest, src, shift))
        | LIR.Lsr (dest, src, shift) -> Ok (Lsr (dest, src, shift))
        | LIR.Lsl_imm (dest, src, shift) -> Ok (Lsl_imm (dest, src, shift))
        | LIR.Lsr_imm (dest, src, shift) -> Ok (Lsr_imm (dest, src, shift))
        | LIR.Mvn (dest, src) -> Ok (Mvn (dest, src))
        | LIR.Sxtb (dest, src) -> Ok (Sxtb (dest, src))
        | LIR.Sxth (dest, src) -> Ok (Sxth (dest, src))
        | LIR.Sxtw (dest, src) -> Ok (Sxtw (dest, src))
        | LIR.Uxtb (dest, src) -> Ok (Uxtb (dest, src))
        | LIR.Uxth (dest, src) -> Ok (Uxth (dest, src))
        | LIR.Uxtw (dest, src) -> Ok (Uxtw (dest, src))
        | LIR.Call (dest, name, args) ->
            symbolOperands args |> Result.map (fun args' -> Call (dest, name, args'))
        | LIR.TailCall (name, args) ->
            symbolOperands args |> Result.map (fun args' -> TailCall (name, args'))
        | LIR.IndirectCall (dest, func, args) ->
            symbolOperands args |> Result.map (fun args' -> IndirectCall (dest, func, args'))
        | LIR.IndirectTailCall (func, args) ->
            symbolOperands args |> Result.map (fun args' -> IndirectTailCall (func, args'))
        | LIR.ClosureAlloc (dest, name, captures) ->
            symbolOperands captures |> Result.map (fun caps' -> ClosureAlloc (dest, name, caps'))
        | LIR.ClosureCall (dest, closure, args) ->
            symbolOperands args |> Result.map (fun args' -> ClosureCall (dest, closure, args'))
        | LIR.ClosureTailCall (closure, args) ->
            symbolOperands args |> Result.map (fun args' -> ClosureTailCall (closure, args'))
        | LIR.SaveRegs (intRegs, floatRegs) -> Ok (SaveRegs (intRegs, floatRegs))
        | LIR.RestoreRegs (intRegs, floatRegs) -> Ok (RestoreRegs (intRegs, floatRegs))
        | LIR.ArgMoves moves ->
            moves
            |> mapResults (fun (reg, op) ->
                symbolOperand op |> Result.map (fun op' -> (reg, op')))
            |> Result.map ArgMoves
        | LIR.TailArgMoves moves ->
            moves
            |> mapResults (fun (reg, op) ->
                symbolOperand op |> Result.map (fun op' -> (reg, op')))
            |> Result.map TailArgMoves
        | LIR.FArgMoves moves -> Ok (FArgMoves moves)
        | LIR.PrintInt reg -> Ok (PrintInt reg)
        | LIR.PrintBool reg -> Ok (PrintBool reg)
        | LIR.PrintIntNoNewline reg -> Ok (PrintIntNoNewline reg)
        | LIR.PrintBoolNoNewline reg -> Ok (PrintBoolNoNewline reg)
        | LIR.PrintFloat freg -> Ok (PrintFloat freg)
        | LIR.PrintFloatNoNewline freg -> Ok (PrintFloatNoNewline freg)
        | LIR.PrintString (idx, _len) ->
            match Map.tryFind idx stringPool.Strings with
            | Some (value, _len) -> Ok (PrintString value)
            | None -> Error $"Missing string pool index {idx}"
        | LIR.PrintHeapStringNoNewline reg -> Ok (PrintHeapStringNoNewline reg)
        | LIR.PrintChars chars -> Ok (PrintChars chars)
        | LIR.PrintBytes reg -> Ok (PrintBytes reg)
        | LIR.PrintList (listPtr, elemType) -> Ok (PrintList (listPtr, elemType))
        | LIR.PrintSum (sumPtr, variants) -> Ok (PrintSum (sumPtr, variants))
        | LIR.PrintRecord (recordPtr, typeName, fields) -> Ok (PrintRecord (recordPtr, typeName, fields))
        | LIR.Exit -> Ok Exit
        | LIR.FPhi (dest, sources) -> Ok (FPhi (dest, sources))
        | LIR.FMov (dest, src) -> Ok (FMov (dest, src))
        | LIR.FLoad (dest, idx) ->
            match Map.tryFind idx floatPool.Floats with
            | Some value -> Ok (FLoad (dest, value))
            | None -> Error $"Missing float pool index {idx}"
        | LIR.FAdd (dest, left, right) -> Ok (FAdd (dest, left, right))
        | LIR.FSub (dest, left, right) -> Ok (FSub (dest, left, right))
        | LIR.FMul (dest, left, right) -> Ok (FMul (dest, left, right))
        | LIR.FDiv (dest, left, right) -> Ok (FDiv (dest, left, right))
        | LIR.FNeg (dest, src) -> Ok (FNeg (dest, src))
        | LIR.FAbs (dest, src) -> Ok (FAbs (dest, src))
        | LIR.FSqrt (dest, src) -> Ok (FSqrt (dest, src))
        | LIR.FCmp (left, right) -> Ok (FCmp (left, right))
        | LIR.IntToFloat (dest, src) -> Ok (IntToFloat (dest, src))
        | LIR.FloatToInt (dest, src) -> Ok (FloatToInt (dest, src))
        | LIR.GpToFp (dest, src) -> Ok (GpToFp (dest, src))
        | LIR.FpToGp (dest, src) -> Ok (FpToGp (dest, src))
        | LIR.HeapAlloc (dest, sizeBytes) -> Ok (HeapAlloc (dest, sizeBytes))
        | LIR.HeapStore (addr, offset, src, valueType) ->
            symbolOperand src |> Result.map (fun src' -> HeapStore (addr, offset, src', valueType))
        | LIR.HeapLoad (dest, addr, offset) -> Ok (HeapLoad (dest, addr, offset))
        | LIR.RefCountInc (addr, payloadSize) -> Ok (RefCountInc (addr, payloadSize))
        | LIR.RefCountDec (addr, payloadSize) -> Ok (RefCountDec (addr, payloadSize))
        | LIR.StringConcat (dest, left, right) ->
            match symbolOperand left with
            | Error err -> Error err
            | Ok left' ->
                symbolOperand right
                |> Result.map (fun right' -> StringConcat (dest, left', right'))
        | LIR.PrintHeapString reg -> Ok (PrintHeapString reg)
        | LIR.LoadFuncAddr (dest, funcName) -> Ok (LoadFuncAddr (dest, funcName))
        | LIR.FileReadText (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileReadText (dest, path'))
        | LIR.FileExists (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileExists (dest, path'))
        | LIR.FileWriteText (dest, path, content) ->
            match symbolOperand path with
            | Error err -> Error err
            | Ok path' ->
                symbolOperand content
                |> Result.map (fun content' -> FileWriteText (dest, path', content'))
        | LIR.FileAppendText (dest, path, content) ->
            match symbolOperand path with
            | Error err -> Error err
            | Ok path' ->
                symbolOperand content
                |> Result.map (fun content' -> FileAppendText (dest, path', content'))
        | LIR.FileDelete (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileDelete (dest, path'))
        | LIR.FileSetExecutable (dest, path) ->
            symbolOperand path |> Result.map (fun path' -> FileSetExecutable (dest, path'))
        | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
            symbolOperand path |> Result.map (fun path' -> FileWriteFromPtr (dest, path', ptr, length))
        | LIR.RawAlloc (dest, numBytes) -> Ok (RawAlloc (dest, numBytes))
        | LIR.RawFree ptr -> Ok (RawFree ptr)
        | LIR.RawGet (dest, ptr, byteOffset) -> Ok (RawGet (dest, ptr, byteOffset))
        | LIR.RawGetByte (dest, ptr, byteOffset) -> Ok (RawGetByte (dest, ptr, byteOffset))
        | LIR.RawSet (ptr, byteOffset, value) -> Ok (RawSet (ptr, byteOffset, value))
        | LIR.RawSetByte (ptr, byteOffset, value) -> Ok (RawSetByte (ptr, byteOffset, value))
        | LIR.StringHash (dest, str) ->
            symbolOperand str |> Result.map (fun str' -> StringHash (dest, str'))
        | LIR.StringEq (dest, left, right) ->
            match symbolOperand left with
            | Error err -> Error err
            | Ok left' ->
                symbolOperand right
                |> Result.map (fun right' -> StringEq (dest, left', right'))
        | LIR.RefCountIncString str ->
            symbolOperand str |> Result.map RefCountIncString
        | LIR.RefCountDecString str ->
            symbolOperand str |> Result.map RefCountDecString
        | LIR.RandomInt64 dest -> Ok (RandomInt64 dest)
        | LIR.FloatToString (dest, value) -> Ok (FloatToString (dest, value))
        | LIR.CoverageHit exprId -> Ok (CoverageHit exprId)

    let symbolTerminator (term: LIR.Terminator) : Terminator =
        match term with
        | LIR.Ret -> Ret
        | LIR.Branch (cond, trueLabel, falseLabel) -> Branch (cond, trueLabel, falseLabel)
        | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) -> BranchZero (cond, zeroLabel, nonZeroLabel)
        | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) -> BranchBitZero (reg, bit, zeroLabel, nonZeroLabel)
        | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) -> BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel)
        | LIR.CondBranch (cond, trueLabel, falseLabel) -> CondBranch (cond, trueLabel, falseLabel)
        | LIR.Jump label -> Jump label

    let symbolBlock (block: LIR.BasicBlock) : Result<BasicBlock, string> =
        mapResults symbolInstr block.Instrs
        |> Result.map (fun instrs' ->
            { Label = block.Label
              Instrs = instrs'
              Terminator = symbolTerminator block.Terminator })

    let symbolFunction (func: LIR.Function) : Result<Function, string> =
        func.CFG.Blocks
        |> Map.toList
        |> mapResults (fun (label, block) ->
            symbolBlock block |> Result.map (fun block' -> (label, block')))
        |> Result.map (fun blocks' ->
            let blockMap = Map.ofList blocks'
            { Name = func.Name
              TypedParams = func.TypedParams
              CFG = { Entry = func.CFG.Entry; Blocks = blockMap }
              StackSize = func.StackSize
              UsedCalleeSaved = func.UsedCalleeSaved })

    functions
    |> mapResults symbolFunction
    |> Result.map (fun funcs' -> Program funcs')

/// Convert symbolic LIR into indexed LIR with pools
let toLIR (Program functions) : Result<LIR.Program, string> =
    let addStringWithLen (pool: MIR.StringPool) (value: string) : Result<int * int * MIR.StringPool, string> =
        let (idx, pool') = MIR.addString pool value
        match Map.tryFind idx pool'.Strings with
        | Some (_, len) -> Ok (idx, len, pool')
        | None -> Error $"Missing string pool entry for index {idx}"

    let addFloatValue (pool: MIR.FloatPool) (value: float) : int * MIR.FloatPool =
        MIR.addFloat pool value

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

    let resolveOperand (state: PoolState) (op: Operand) : Result<LIR.Operand * PoolState, string> =
        match op with
        | Imm value -> Ok (LIR.Imm value, state)
        | FloatImm value -> Ok (LIR.FloatImm value, state)
        | Reg reg -> Ok (LIR.Reg reg, state)
        | StackSlot slot -> Ok (LIR.StackSlot slot, state)
        | FuncAddr name -> Ok (LIR.FuncAddr name, state)
        | StringSymbol value ->
            match addStringWithLen state.StringPool value with
            | Error err -> Error err
            | Ok (idx, _len, pool') ->
                Ok (LIR.StringRef idx, { state with StringPool = pool' })
        | FloatSymbol value ->
            let (idx, pool') = addFloatValue state.FloatPool value
            Ok (LIR.FloatRef idx, { state with FloatPool = pool' })

    let resolveOperands (state: PoolState) (ops: Operand list) : Result<LIR.Operand list * PoolState, string> =
        mapWithState resolveOperand state ops

    let resolveOperandPairs
        (state: PoolState)
        (pairs: (Operand * Label) list)
        : Result<(LIR.Operand * Label) list * PoolState, string> =
        mapWithState
            (fun st (op, lbl) ->
                resolveOperand st op |> Result.map (fun (op', st') -> ((op', lbl), st')))
            state
            pairs

    let resolveArgMoves
        (state: PoolState)
        (moves: (PhysReg * Operand) list)
        : Result<(PhysReg * LIR.Operand) list * PoolState, string> =
        mapWithState
            (fun st (reg, op) ->
                resolveOperand st op |> Result.map (fun (op', st') -> ((reg, op'), st')))
            state
            moves

    let resolveInstr (state: PoolState) (instr: Instr) : Result<LIR.Instr * PoolState, string> =
        match instr with
        | Mov (dest, src) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (LIR.Mov (dest, src'), st))
        | Phi (dest, sources, valueType) ->
            resolveOperandPairs state sources
            |> Result.map (fun (sources', st) -> (LIR.Phi (dest, sources', valueType), st))
        | Store (stackSlot, src) -> Ok (LIR.Store (stackSlot, src), state)
        | Add (dest, left, right) ->
            resolveOperand state right |> Result.map (fun (right', st) -> (LIR.Add (dest, left, right'), st))
        | Sub (dest, left, right) ->
            resolveOperand state right |> Result.map (fun (right', st) -> (LIR.Sub (dest, left, right'), st))
        | Mul (dest, left, right) -> Ok (LIR.Mul (dest, left, right), state)
        | Sdiv (dest, left, right) -> Ok (LIR.Sdiv (dest, left, right), state)
        | Msub (dest, mulLeft, mulRight, sub) -> Ok (LIR.Msub (dest, mulLeft, mulRight, sub), state)
        | Madd (dest, mulLeft, mulRight, add) -> Ok (LIR.Madd (dest, mulLeft, mulRight, add), state)
        | Cmp (left, right) ->
            resolveOperand state right |> Result.map (fun (right', st) -> (LIR.Cmp (left, right'), st))
        | Cset (dest, cond) -> Ok (LIR.Cset (dest, cond), state)
        | And (dest, left, right) -> Ok (LIR.And (dest, left, right), state)
        | And_imm (dest, src, imm) -> Ok (LIR.And_imm (dest, src, imm), state)
        | Orr (dest, left, right) -> Ok (LIR.Orr (dest, left, right), state)
        | Eor (dest, left, right) -> Ok (LIR.Eor (dest, left, right), state)
        | Lsl (dest, src, shift) -> Ok (LIR.Lsl (dest, src, shift), state)
        | Lsr (dest, src, shift) -> Ok (LIR.Lsr (dest, src, shift), state)
        | Lsl_imm (dest, src, shift) -> Ok (LIR.Lsl_imm (dest, src, shift), state)
        | Lsr_imm (dest, src, shift) -> Ok (LIR.Lsr_imm (dest, src, shift), state)
        | Mvn (dest, src) -> Ok (LIR.Mvn (dest, src), state)
        | Sxtb (dest, src) -> Ok (LIR.Sxtb (dest, src), state)
        | Sxth (dest, src) -> Ok (LIR.Sxth (dest, src), state)
        | Sxtw (dest, src) -> Ok (LIR.Sxtw (dest, src), state)
        | Uxtb (dest, src) -> Ok (LIR.Uxtb (dest, src), state)
        | Uxth (dest, src) -> Ok (LIR.Uxth (dest, src), state)
        | Uxtw (dest, src) -> Ok (LIR.Uxtw (dest, src), state)
        | Call (dest, name, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.Call (dest, name, args'), st))
        | TailCall (name, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.TailCall (name, args'), st))
        | IndirectCall (dest, func, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.IndirectCall (dest, func, args'), st))
        | IndirectTailCall (func, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.IndirectTailCall (func, args'), st))
        | ClosureAlloc (dest, name, captures) ->
            resolveOperands state captures |> Result.map (fun (caps', st) -> (LIR.ClosureAlloc (dest, name, caps'), st))
        | ClosureCall (dest, closure, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.ClosureCall (dest, closure, args'), st))
        | ClosureTailCall (closure, args) ->
            resolveOperands state args |> Result.map (fun (args', st) -> (LIR.ClosureTailCall (closure, args'), st))
        | SaveRegs (intRegs, floatRegs) -> Ok (LIR.SaveRegs (intRegs, floatRegs), state)
        | RestoreRegs (intRegs, floatRegs) -> Ok (LIR.RestoreRegs (intRegs, floatRegs), state)
        | ArgMoves moves ->
            resolveArgMoves state moves |> Result.map (fun (moves', st) -> (LIR.ArgMoves moves', st))
        | TailArgMoves moves ->
            resolveArgMoves state moves |> Result.map (fun (moves', st) -> (LIR.TailArgMoves moves', st))
        | FArgMoves moves -> Ok (LIR.FArgMoves moves, state)
        | PrintInt reg -> Ok (LIR.PrintInt reg, state)
        | PrintBool reg -> Ok (LIR.PrintBool reg, state)
        | PrintIntNoNewline reg -> Ok (LIR.PrintIntNoNewline reg, state)
        | PrintBoolNoNewline reg -> Ok (LIR.PrintBoolNoNewline reg, state)
        | PrintFloat freg -> Ok (LIR.PrintFloat freg, state)
        | PrintFloatNoNewline freg -> Ok (LIR.PrintFloatNoNewline freg, state)
        | PrintString value ->
            match addStringWithLen state.StringPool value with
            | Error err -> Error err
            | Ok (idx, len, pool') ->
                Ok (LIR.PrintString (idx, len), { state with StringPool = pool' })
        | PrintHeapStringNoNewline reg -> Ok (LIR.PrintHeapStringNoNewline reg, state)
        | PrintChars chars -> Ok (LIR.PrintChars chars, state)
        | PrintBytes reg -> Ok (LIR.PrintBytes reg, state)
        | PrintList (listPtr, elemType) -> Ok (LIR.PrintList (listPtr, elemType), state)
        | PrintSum (sumPtr, variants) -> Ok (LIR.PrintSum (sumPtr, variants), state)
        | PrintRecord (recordPtr, typeName, fields) -> Ok (LIR.PrintRecord (recordPtr, typeName, fields), state)
        | Exit -> Ok (LIR.Exit, state)
        | FPhi (dest, sources) -> Ok (LIR.FPhi (dest, sources), state)
        | FMov (dest, src) -> Ok (LIR.FMov (dest, src), state)
        | FLoad (dest, value) ->
            let (idx, pool') = addFloatValue state.FloatPool value
            Ok (LIR.FLoad (dest, idx), { state with FloatPool = pool' })
        | FAdd (dest, left, right) -> Ok (LIR.FAdd (dest, left, right), state)
        | FSub (dest, left, right) -> Ok (LIR.FSub (dest, left, right), state)
        | FMul (dest, left, right) -> Ok (LIR.FMul (dest, left, right), state)
        | FDiv (dest, left, right) -> Ok (LIR.FDiv (dest, left, right), state)
        | FNeg (dest, src) -> Ok (LIR.FNeg (dest, src), state)
        | FAbs (dest, src) -> Ok (LIR.FAbs (dest, src), state)
        | FSqrt (dest, src) -> Ok (LIR.FSqrt (dest, src), state)
        | FCmp (left, right) -> Ok (LIR.FCmp (left, right), state)
        | IntToFloat (dest, src) -> Ok (LIR.IntToFloat (dest, src), state)
        | FloatToInt (dest, src) -> Ok (LIR.FloatToInt (dest, src), state)
        | GpToFp (dest, src) -> Ok (LIR.GpToFp (dest, src), state)
        | FpToGp (dest, src) -> Ok (LIR.FpToGp (dest, src), state)
        | HeapAlloc (dest, sizeBytes) -> Ok (LIR.HeapAlloc (dest, sizeBytes), state)
        | HeapStore (addr, offset, src, valueType) ->
            resolveOperand state src |> Result.map (fun (src', st) -> (LIR.HeapStore (addr, offset, src', valueType), st))
        | HeapLoad (dest, addr, offset) -> Ok (LIR.HeapLoad (dest, addr, offset), state)
        | RefCountInc (addr, payloadSize) -> Ok (LIR.RefCountInc (addr, payloadSize), state)
        | RefCountDec (addr, payloadSize) -> Ok (LIR.RefCountDec (addr, payloadSize), state)
        | StringConcat (dest, left, right) ->
            match resolveOperand state left with
            | Error err -> Error err
            | Ok (left', st1) ->
                resolveOperand st1 right
                |> Result.map (fun (right', st2) -> (LIR.StringConcat (dest, left', right'), st2))
        | PrintHeapString reg -> Ok (LIR.PrintHeapString reg, state)
        | LoadFuncAddr (dest, funcName) -> Ok (LIR.LoadFuncAddr (dest, funcName), state)
        | FileReadText (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (LIR.FileReadText (dest, path'), st))
        | FileExists (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (LIR.FileExists (dest, path'), st))
        | FileWriteText (dest, path, content) ->
            match resolveOperand state path with
            | Error err -> Error err
            | Ok (path', st1) ->
                resolveOperand st1 content
                |> Result.map (fun (content', st2) -> (LIR.FileWriteText (dest, path', content'), st2))
        | FileAppendText (dest, path, content) ->
            match resolveOperand state path with
            | Error err -> Error err
            | Ok (path', st1) ->
                resolveOperand st1 content
                |> Result.map (fun (content', st2) -> (LIR.FileAppendText (dest, path', content'), st2))
        | FileDelete (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (LIR.FileDelete (dest, path'), st))
        | FileSetExecutable (dest, path) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (LIR.FileSetExecutable (dest, path'), st))
        | FileWriteFromPtr (dest, path, ptr, length) ->
            resolveOperand state path |> Result.map (fun (path', st) -> (LIR.FileWriteFromPtr (dest, path', ptr, length), st))
        | RawAlloc (dest, numBytes) -> Ok (LIR.RawAlloc (dest, numBytes), state)
        | RawFree ptr -> Ok (LIR.RawFree ptr, state)
        | RawGet (dest, ptr, byteOffset) -> Ok (LIR.RawGet (dest, ptr, byteOffset), state)
        | RawGetByte (dest, ptr, byteOffset) -> Ok (LIR.RawGetByte (dest, ptr, byteOffset), state)
        | RawSet (ptr, byteOffset, value) -> Ok (LIR.RawSet (ptr, byteOffset, value), state)
        | RawSetByte (ptr, byteOffset, value) -> Ok (LIR.RawSetByte (ptr, byteOffset, value), state)
        | StringHash (dest, str) ->
            resolveOperand state str |> Result.map (fun (str', st) -> (LIR.StringHash (dest, str'), st))
        | StringEq (dest, left, right) ->
            match resolveOperand state left with
            | Error err -> Error err
            | Ok (left', st1) ->
                resolveOperand st1 right
                |> Result.map (fun (right', st2) -> (LIR.StringEq (dest, left', right'), st2))
        | RefCountIncString str ->
            resolveOperand state str |> Result.map (fun (str', st) -> (LIR.RefCountIncString str', st))
        | RefCountDecString str ->
            resolveOperand state str |> Result.map (fun (str', st) -> (LIR.RefCountDecString str', st))
        | RandomInt64 dest -> Ok (LIR.RandomInt64 dest, state)
        | FloatToString (dest, value) -> Ok (LIR.FloatToString (dest, value), state)
        | CoverageHit exprId -> Ok (LIR.CoverageHit exprId, state)

    let resolveTerminator (term: Terminator) : LIR.Terminator =
        match term with
        | Ret -> LIR.Ret
        | Branch (cond, trueLabel, falseLabel) -> LIR.Branch (cond, trueLabel, falseLabel)
        | BranchZero (cond, zeroLabel, nonZeroLabel) -> LIR.BranchZero (cond, zeroLabel, nonZeroLabel)
        | BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) -> LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel)
        | BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) -> LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel)
        | CondBranch (cond, trueLabel, falseLabel) -> LIR.CondBranch (cond, trueLabel, falseLabel)
        | Jump label -> LIR.Jump label

    let resolveBlock (state: PoolState) (block: BasicBlock) : Result<LIR.BasicBlock * PoolState, string> =
        mapWithState resolveInstr state block.Instrs
        |> Result.map (fun (instrs', st) ->
            ({ Label = block.Label; Instrs = instrs'; Terminator = resolveTerminator block.Terminator }, st))

    let resolveFunction (state: PoolState) (func: Function) : Result<LIR.Function * PoolState, string> =
        let resolveBlockPair st (label, block) =
            resolveBlock st block |> Result.map (fun (block', st') -> ((label, block'), st'))
        mapWithState resolveBlockPair state (Map.toList func.CFG.Blocks)
        |> Result.map (fun (blocks', st) ->
            let blockMap = Map.ofList blocks'
            ({ Name = func.Name
               TypedParams = func.TypedParams
               CFG = { Entry = func.CFG.Entry; Blocks = blockMap }
               StackSize = func.StackSize
               UsedCalleeSaved = func.UsedCalleeSaved }, st))

    let initialState = { StringPool = MIR.emptyStringPool; FloatPool = MIR.emptyFloatPool }

    mapWithState resolveFunction initialState functions
    |> Result.map (fun (funcs', finalState) ->
        LIR.Program (funcs', finalState.StringPool, finalState.FloatPool))
