// CopyPropagation.fs - Resolve and propagate MIR copy equivalences.

module MIRCopyPropagation

open MIR
open MIROptimizationFacts
/// Copy Propagation
/// Replace uses of copy destinations with their sources
/// For: dest = src, replace all uses of dest with src
type CopyMap = Map<VReg, Operand>

let buildCopyMap (cfg: CFG) : CopyMap =
    // SSA phis are a leading block prefix. Later passes either retain that
    // invariant or replace a phi with an ordinary move, so stop as soon as the
    // prefix ends instead of rescanning every instruction in every fixed-point
    // iteration merely to rediscover the same destinations.
    let rec collectPhiPrefix dests instrs =
        match instrs with
        | Phi (dest, _, _) :: rest ->
            collectPhiPrefix (Set.add dest dests) rest
        | _ ->
            dests

    let phiDests =
        cfg.Blocks
        |> Map.fold (fun dests _ block ->
            collectPhiPrefix dests block.Instrs
        ) Set.empty

    cfg.Blocks
    |> Map.fold (fun copies _ block ->
        block.Instrs
        |> List.fold (fun m instr ->
            match instr with
            | Mov (dest, Register src, vt) when dest <> src ->
                // Don't add if dest is a phi destination or already in map
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Mov (dest, (Int64Const _ as src), vt) ->
                // Constant propagation: track constant moves too
                // Only propagate if this is for integer/bool types, not for heap types like strings
                // This prevents incorrectly propagating Int64Const 0L to string variables
                let isIntOrBoolType =
                    match vt with
                    | Some AST.TInt64 | Some AST.TInt32 | Some AST.TInt16 | Some AST.TInt8
                    | Some AST.TUInt64 | Some AST.TUInt32 | Some AST.TUInt16 | Some AST.TUInt8
                    | Some AST.TBool | Some AST.TUnit | None -> true
                    | _ -> false  // Don't propagate for TString, TList, etc.
                if Set.contains dest phiDests || Map.containsKey dest m || not isIntOrBoolType then m
                else Map.add dest src m
            | Mov (dest, (BoolConst _ as src), _) ->
                // Constant propagation: track constant moves too
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest src m
            | Phi (dest, [(Register src, _)], _) when dest <> src ->
                // Trivial phi with single register source
                if Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Phi (dest, sources, vt) ->
                // Check if all sources are the same register
                if Map.containsKey dest m then m
                else
                    match sources with
                    | (Register firstSrc, _) :: rest ->
                        if rest |> List.forall (fun (s, _) -> s = Register firstSrc) then
                            if dest <> firstSrc then
                                Map.add dest (Register firstSrc) m
                            else
                                m
                        else
                            m
                    | _ -> m
            | _ -> m
        ) copies
    ) Map.empty

/// Transitively resolve a copy chain (with cycle detection)
let resolveCopy (copies: CopyMap) (op: Operand) : Operand =
    let rec resolve visited op' =
        match op' with
        | Register vreg ->
            if Set.contains vreg visited then
                // Cycle detected, stop here
                op'
            else
                match Map.tryFind vreg copies with
                | Some resolvedOp -> resolve (Set.add vreg visited) resolvedOp
                | None -> op'
        | _ -> op'
    resolve Set.empty op

/// Resolve every copy destination once so operand propagation only needs one lookup.
let resolveCopyMap (copies: CopyMap) : CopyMap =
    copies
    |> Map.map (fun dest _ -> resolveCopy copies (Register dest))

/// Apply copy propagation to an operand
let propagateCopyOperand (copies: CopyMap) (op: Operand) : Operand =
    match op with
    | Register vreg -> Map.tryFind vreg copies |> Option.defaultValue op
    | _ -> op

/// Apply copy propagation to an instruction
let propagateCopyInstr (copies: CopyMap) (instr: Instr) : Instr =
    let p = propagateCopyOperand copies
    match instr with
    | Mov (dest, src, vt) -> Mov (dest, p src, vt)
    | BinOp (dest, op, left, right, opType) -> BinOp (dest, op, p left, p right, opType)
    | UnaryOp (dest, op, src) -> UnaryOp (dest, op, p src)
    | Call (dest, name, args, argTypes, retType) -> Call (dest, name, List.map p args, argTypes, retType)
    | TailCall (name, args, argTypes, retType) -> TailCall (name, List.map p args, argTypes, retType)
    | IndirectCall (dest, func, args, argTypes, retType) -> IndirectCall (dest, p func, List.map p args, argTypes, retType)
    | IndirectTailCall (func, args, argTypes, retType) -> IndirectTailCall (p func, List.map p args, argTypes, retType)
    | ClosureAlloc (dest, name, captures) -> ClosureAlloc (dest, name, List.map p captures)
    | ClosureCall (dest, closure, args, argTypes, retType) -> ClosureCall (dest, p closure, List.map p args, argTypes, retType)
    | ClosureTailCall (closure, args, argTypes) -> ClosureTailCall (p closure, List.map p args, argTypes)
    | HeapAlloc (dest, size) -> HeapAlloc (dest, size)
    | HeapStore (addr, offset, src, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapStore (addr', offset, p src, vt)
    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapLoad (dest, addr', offset, vt)
    | StringConcat (dest, first, second, remaining) ->
        let stringOperand operand =
            match p operand with
            | (Register _ | StringSymbol _) as propagated -> propagated
            | _ ->
                // RuntimeError continuations can carry the unreachable Unit
                // sentinel through an inlined string-typed parameter. Keep the
                // register form so dead post-error code remains representable.
                operand
        StringConcat (dest, stringOperand first, stringOperand second, List.map stringOperand remaining)
    | CanonicalBufferEq (dest, kind, left, right) ->
        CanonicalBufferEq (dest, kind, p left, p right)
    | RefCountInc (addr, size, kind, sourceType) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountInc (addr', size, kind, sourceType)
    | RefCountDec (addr, size, kind, sourceType) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountDec (addr', size, kind, sourceType)
    | Print (src, vt) -> Print (p src, vt)
    | StdoutWrite (effectId, src, appendNewline) -> StdoutWrite (effectId, p src, appendNewline)
    | StdinReadLine dest -> StdinReadLine dest
    | FileReadBlob (dest, path) -> FileReadBlob (dest, p path)
    | FileExists (dest, path) -> FileExists (dest, p path)
    | FileWriteBlob (dest, path, content) -> FileWriteBlob (dest, p path, p content)
    | FileAppendText (dest, path, content) -> FileAppendText (dest, p path, p content)
    | FileDelete (dest, path) -> FileDelete (dest, p path)
    | FileCreateDirectory (dest, path) -> FileCreateDirectory (dest, p path)
    | FileSetExecutable (dest, path) -> FileSetExecutable (dest, p path)
    | FileWriteFromPtr (dest, path, ptr, length) -> FileWriteFromPtr (dest, p path, p ptr, p length)
    // Don't propagate copies into phi sources - phis are merge points and their
    // sources represent values flowing from specific predecessor blocks
    | Phi (dest, sources, valueType) -> Phi (dest, sources, valueType)
    | RawAlloc (dest, numBytes) -> RawAlloc (dest, p numBytes)
    | MappedAlloc (dest, numBytes) -> MappedAlloc (dest, p numBytes)
    | RawFree ptr -> RawFree (p ptr)
    | MappedFree ptr -> MappedFree (p ptr)
    | RawGet (dest, ptr, byteOffset, valueType) -> RawGet (dest, p ptr, p byteOffset, valueType)
    | RawGetByte (dest, ptr, byteOffset) -> RawGetByte (dest, p ptr, p byteOffset)
    | StringToRawPtr (dest, value) -> StringToRawPtr (dest, p value)
    | RawPtrToString (dest, ptr) -> RawPtrToString (dest, p ptr)
    | BlobToRawPtr (dest, value) -> BlobToRawPtr (dest, p value)
    | RawPtrToBlob (dest, ptr) -> RawPtrToBlob (dest, p ptr)
    | DictToRawPtr (dest, dict) -> DictToRawPtr (dest, p dict)
    | RawPtrToDict (dest, ptr, tag) -> RawPtrToDict (dest, p ptr, p tag)
    | ListToRawPtr (dest, list) -> ListToRawPtr (dest, p list)
    | RawPtrToList (dest, ptr, tag) -> RawPtrToList (dest, p ptr, p tag)
    | RawWriteWord (ptr, byteOffset, value) -> RawWriteWord (p ptr, p byteOffset, p value)
    | RawWriteByte (ptr, byteOffset, value) -> RawWriteByte (p ptr, p byteOffset, p value)
    | RawSlotInit (ptr, byteOffset, value, valueType) -> RawSlotInit (p ptr, p byteOffset, p value, valueType)
    | FloatSqrt (dest, src) -> FloatSqrt (dest, p src)
    | FloatAbs (dest, src) -> FloatAbs (dest, p src)
    | FloatNeg (dest, src) -> FloatNeg (dest, p src)
    | Int64ToFloat (dest, src) -> Int64ToFloat (dest, p src)
    | FloatToInt64 (dest, src) -> FloatToInt64 (dest, p src)
    | FloatToBits (dest, src) -> FloatToBits (dest, p src)
    | RefCountIncString str -> RefCountIncString (p str)
    | RefCountDecString str -> RefCountDecString (p str)
    | RefCountIncBlob bytes -> RefCountIncBlob (p bytes)
    | RefCountDecBlob bytes -> RefCountDecBlob (p bytes)
    | RefCountIncInt value -> RefCountIncInt (p value)
    | RefCountDecInt value -> RefCountDecInt (p value)
    | RandomInt64 dest -> RandomInt64 dest
    | DateTimeNow dest -> DateTimeNow dest
    | Sleep (effectId, dest, delayMs) -> Sleep (effectId, dest, p delayMs)
    | CliNative (dest, operation, args) -> CliNative (dest, operation, List.map p args)
    | FloatToString (dest, value) -> FloatToString (dest, p value)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString message -> RuntimeErrorString (p message)
    | CoverageHit exprId -> CoverageHit exprId

/// Apply copy propagation to terminator
let propagateCopyTerminator (copies: CopyMap) (term: Terminator) : Terminator =
    let p = propagateCopyOperand copies
    match term with
    | Ret op -> Ret (p op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (p cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Apply copy propagation to a CFG, optionally recording its internal scans.
let internal applyCopyPropagationWithTickTrace
    (recordTicks: (string -> int64 -> unit) option)
    (cfg: CFG)
    : CFG * bool =
    let measure name operation =
        match recordTicks with
        | None -> operation ()
        | Some record ->
            let started = System.Diagnostics.Stopwatch.GetTimestamp()
            let result = operation ()
            record name (System.Diagnostics.Stopwatch.GetTimestamp() - started)
            result

    let discoveredCopies =
        measure "MIR Copy Map Discovery" (fun () -> buildCopyMap cfg)
    let copies =
        measure "MIR Copy Chain Resolution" (fun () -> resolveCopyMap discoveredCopies)

    if Map.isEmpty copies then
        (cfg, false)
    else
        measure "MIR Copy Rewrite" (fun () ->
            let usesCopiedRegister instr =
                match instr with
                // Copy propagation intentionally leaves phi inputs unchanged.
                | Phi _ -> false
                | _ ->
                    foldInstrUses
                        (fun found reg -> found || Map.containsKey reg copies)
                        false
                        instr
            let terminatorUsesCopiedRegister term =
                foldTerminatorUses
                    (fun found reg -> found || Map.containsKey reg copies)
                    false
                    term
            let (blocks', changed) =
                cfg.Blocks
                |> Map.fold (fun (acc, changedAcc) label block ->
                    let (instrs', instrChanged) =
                        block.Instrs
                        |> List.fold (fun (instrAcc, ch) instr ->
                            if usesCopiedRegister instr then
                                let instr' = propagateCopyInstr copies instr
                                (instr' :: instrAcc, ch || instr' <> instr)
                            else
                                (instr :: instrAcc, ch)
                        ) ([], false)
                    let instrs' = List.rev instrs'
                    let term' =
                        if terminatorUsesCopiedRegister block.Terminator then
                            propagateCopyTerminator copies block.Terminator
                        else
                            block.Terminator
                    let block' = { block with Instrs = instrs'; Terminator = term' }
                    (Map.add label block' acc, changedAcc || instrChanged || term' <> block.Terminator)
                ) (Map.empty, false)
            ({ cfg with Blocks = blocks' }, changed))

let applyCopyPropagation (cfg: CFG) : CFG * bool =
    applyCopyPropagationWithTickTrace None cfg
