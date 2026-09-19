// Facts.fs - Describe MIR effects and explicit definition/use edges.

module MIROptimizationFacts

open MIR
open SSA_Construction

type OptimizeOptions = {
    EnableConstFolding: bool
    EnableCSE: bool
    EnableCopyProp: bool
    EnableDCE: bool
    EnableCFGSimplify: bool
    EnableLICM: bool
}

let defaultOptimizeOptions = {
    EnableConstFolding = true
    EnableCSE = true
    EnableCopyProp = true
    EnableDCE = true
    EnableCFGSimplify = true
    EnableLICM = true
}

/// Check if an instruction has side effects (must be preserved even if unused)
let hasSideEffects (instr: Instr) : bool =
    match instr with
    | Mov _ -> false
    | BinOp _ -> false
    | UnaryOp _ -> false
    | Phi _ -> false
    | HeapLoad _ -> false
    // These have side effects
    | Call _ -> true  // Function calls may have side effects
    | TailCall _ -> true  // Tail calls have side effects
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true  // Indirect tail calls have side effects
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true  // Closure tail calls have side effects
    | HeapAlloc _ -> true  // Allocates memory
    | HeapStore _ -> true  // Writes to memory
    | StringConcat _ -> true  // Allocates memory
    | CanonicalBufferEq _ -> false
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | StdoutWrite _ -> true
    | StdinReadLine _ -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | MappedAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | MappedFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawWriteWord _ -> true    // Writes to memory
    | RawWriteByte _ -> true  // Writes to memory (byte)
    | RawSlotInit _ -> true  // Writes to memory and may retain a typed edge
    | StringToRawPtr _ -> false
    | RawPtrToString _ -> false
    | BlobToRawPtr _ -> false
    | RawPtrToBlob _ -> false
    | DictToRawPtr _ -> false
    | RawPtrToDict _ -> false
    | ListToRawPtr _ -> false
    | RawPtrToList _ -> false
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false  // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RefCountIncBlob _ -> true    // Mutates refcount
    | RefCountDecBlob _ -> true    // Mutates refcount
    | RandomInt64 _ -> true  // Syscall
    | DateTimeNow _ -> true      // Syscall
    | Sleep _ -> true            // Blocking syscall
    | CliNative _ -> true
    | FloatToString _ -> false  // Pure conversion (allocates but no visible side effect)
    | RuntimeError _ -> true
    | RuntimeErrorString _ -> true
    | CoverageHit _ -> true  // Must not be eliminated (tracking side effect)

/// Find functions whose reachable call-graph components contain no MIR effects.
/// Starting from every locally effect-free function and removing callers of
/// unproven functions computes the greatest fixed point, so mutually recursive
/// and self-recursive components remain provable without assuming unknown calls
/// are safe.
let private directCallee instr =
    match instr with
    | Call (_, funcName, _, _, _)
    | TailCall (funcName, _, _, _) -> Some funcName
    | _ -> None

type private FunctionEffectSummary = {
    Name: string
    LocallyEffectFree: bool
    DirectCallees: Set<string>
}

let private summarizeFunctionEffects (func: Function) : FunctionEffectSummary =
    let (locallyEffectFree, directCallees) =
        func.CFG.Blocks
        |> Map.fold (fun summary _ block ->
            block.Instrs
            |> List.fold (fun (isEffectFree, callees) instr ->
                match directCallee instr with
                | Some callee -> (isEffectFree, Set.add callee callees)
                | None -> (isEffectFree && not (hasSideEffects instr), callees)
            ) summary
        ) (true, Set.empty)
    {
        Name = func.Name
        LocallyEffectFree = locallyEffectFree
        DirectCallees = directCallees
    }

let private directCallees (func: Function) : Set<string> =
    (summarizeFunctionEffects func).DirectCallees

let analyzeEffectFreeFunctions (functions: Function list) : Set<string> =
    // The fixed point changes only the proven-name set. MIR and call edges stay
    // fixed, so retain each function's scan instead of rebuilding it per round.
    let candidates =
        functions
        |> List.map summarizeFunctionEffects
        |> List.filter (fun summary -> summary.LocallyEffectFree)

    let rec removeCallersOfUnprovenFunctions provenNames =
        let next =
            candidates
            |> List.filter (fun summary ->
                summary.DirectCallees
                |> Set.forall (fun callee -> Set.contains callee provenNames))
            |> List.map (fun summary -> summary.Name)
            |> Set.ofList

        if next = provenNames then next else removeCallersOfUnprovenFunctions next

    candidates
    |> List.map (fun summary -> summary.Name)
    |> Set.ofList
    |> removeCallersOfUnprovenFunctions

/// Only direct callees can affect optimization of this function. Restricting
/// the whole-program result to those names gives a compositional cache key.
let effectFreeCallsForFunction
    (effectFreeFunctions: Set<string>)
    (func: Function)
    : Set<string> =
    Set.intersect effectFreeFunctions (directCallees func)

/// Get the destination VReg of an instruction (if any)
let getInstrDest (instr: Instr) : VReg option =
    match instr with
    | Mov (dest, _, _) -> Some dest
    | BinOp (dest, _, _, _, _) -> Some dest
    | UnaryOp (dest, _, _) -> Some dest
    | Call (dest, _, _, _, _) -> Some dest
    | TailCall _ -> None  // Tail calls don't return here
    | IndirectCall (dest, _, _, _, _) -> Some dest
    | IndirectTailCall _ -> None  // Indirect tail calls don't return here
    | ClosureAlloc (dest, _, _) -> Some dest
    | ClosureCall (dest, _, _, _, _) -> Some dest
    | ClosureTailCall _ -> None  // Closure tail calls don't return here
    | HeapAlloc (dest, _) -> Some dest
    | HeapLoad (dest, _, _, _) -> Some dest
    | StringConcat (dest, _, _, _) -> Some dest
    | CanonicalBufferEq (dest, _, _, _) -> Some dest
    | StdinReadLine dest -> Some dest
    | FileReadText (dest, _) -> Some dest
    | FileExists (dest, _) -> Some dest
    | FileWriteText (dest, _, _) -> Some dest
    | FileAppendText (dest, _, _) -> Some dest
    | FileDelete (dest, _) -> Some dest
    | FileSetExecutable (dest, _) -> Some dest
    | FileWriteFromPtr (dest, _, _, _) -> Some dest
    | Phi (dest, _, _) -> Some dest
    | RawAlloc (dest, _) -> Some dest
    | MappedAlloc (dest, _) -> Some dest
    | RawGet (dest, _, _, _) -> Some dest
    | RawGetByte (dest, _, _) -> Some dest
    | StringToRawPtr (dest, _) -> Some dest
    | RawPtrToString (dest, _) -> Some dest
    | BlobToRawPtr (dest, _) -> Some dest
    | RawPtrToBlob (dest, _) -> Some dest
    | DictToRawPtr (dest, _) -> Some dest
    | RawPtrToDict (dest, _, _) -> Some dest
    | ListToRawPtr (dest, _) -> Some dest
    | RawPtrToList (dest, _, _) -> Some dest
    | FloatSqrt (dest, _) -> Some dest
    | FloatAbs (dest, _) -> Some dest
    | FloatNeg (dest, _) -> Some dest
    | Int64ToFloat (dest, _) -> Some dest
    | FloatToInt64 (dest, _) -> Some dest
    | FloatToBits (dest, _) -> Some dest
    | HeapStore _ -> None
    | RefCountInc _ -> None
    | RefCountDec _ -> None
    | Print _ -> None
    | StdoutWrite _ -> None
    | RawFree _ -> None
    | MappedFree _ -> None
    | RawWriteWord _ -> None
    | RawWriteByte _ -> None
    | RawSlotInit _ -> None
    | RefCountIncString _ -> None
    | RefCountDecString _ -> None
    | RefCountIncBlob _ -> None
    | RefCountDecBlob _ -> None
    | RandomInt64 dest -> Some dest
    | DateTimeNow dest -> Some dest
    | Sleep (_, dest, _) -> Some dest
    | CliNative (dest, _, _) -> Some dest
    | FloatToString (dest, _) -> Some dest
    | RuntimeError _ -> None
    | RuntimeErrorString _ -> None
    | CoverageHit _ -> None

/// Fold over the VRegs used by an instruction without allocating an intermediate collection.
let foldInstrUses (folder: 'State -> VReg -> 'State) (state: 'State) (instr: Instr) : 'State =
    let fromOperand state op =
        match op with
        | Register vreg -> folder state vreg
        | _ -> state

    let fromOperands state operands = List.fold fromOperand state operands

    match instr with
    | Mov (_, src, _) -> fromOperand state src
    | BinOp (_, _, left, right, _) -> fromOperand (fromOperand state left) right
    | UnaryOp (_, _, src) -> fromOperand state src
    | Call (_, _, args, _, _)
    | TailCall (_, args, _, _)
    | ClosureAlloc (_, _, args) -> fromOperands state args
    | IndirectCall (_, func, args, _, _)
    | IndirectTailCall (func, args, _, _)
    | ClosureCall (_, func, args, _, _)
    | ClosureTailCall (func, args, _) -> fromOperands (fromOperand state func) args
    | HeapAlloc _ -> state
    | HeapStore (addr, _, src, _) -> fromOperand (folder state addr) src
    | HeapLoad (_, addr, _, _)
    | RefCountInc (addr, _, _, _)
    | RefCountDec (addr, _, _, _) -> folder state addr
    | StringConcat (_, first, second, remaining) ->
        fromOperands state (first :: second :: remaining)
    | CanonicalBufferEq (_, _, left, right)
    | FileWriteText (_, left, right)
    | FileAppendText (_, left, right)
    | RawGet (_, left, right, _)
    | RawGetByte (_, left, right)
    | RawPtrToDict (_, left, right)
    | RawPtrToList (_, left, right) -> fromOperand (fromOperand state left) right
    | Print (src, _)
    | StdoutWrite (_, src, _)
    | FileReadText (_, src)
    | FileExists (_, src)
    | FileDelete (_, src)
    | FileSetExecutable (_, src)
    | RawAlloc (_, src)
    | MappedAlloc (_, src)
    | RawFree src
    | MappedFree src
    | StringToRawPtr (_, src)
    | RawPtrToString (_, src)
    | BlobToRawPtr (_, src)
    | RawPtrToBlob (_, src)
    | DictToRawPtr (_, src)
    | ListToRawPtr (_, src)
    | FloatSqrt (_, src)
    | FloatAbs (_, src)
    | FloatNeg (_, src)
    | Int64ToFloat (_, src)
    | FloatToInt64 (_, src)
    | FloatToBits (_, src)
    | RefCountIncString src
    | RefCountDecString src
    | RefCountIncBlob src
    | RefCountDecBlob src
    | FloatToString (_, src) -> fromOperand state src
    | Sleep (_, _, delayMs) -> fromOperand state delayMs
    | FileWriteFromPtr (_, first, second, third)
    | RawWriteWord (first, second, third)
    | RawWriteByte (first, second, third)
    | RawSlotInit (first, second, third, _) ->
        fromOperand (fromOperand (fromOperand state first) second) third
    | Phi (_, sources, _) ->
        sources |> List.fold (fun acc (op, _) -> fromOperand acc op) state
    | CliNative (_, _, args) -> fromOperands state args
    | RandomInt64 _
    | DateTimeNow _
    | StdinReadLine _
    | RuntimeError _
    | RuntimeErrorString _
    | CoverageHit _ -> state

/// Get all VRegs used by an instruction.
let getInstrUses (instr: Instr) : Set<VReg> =
    foldInstrUses (fun uses vreg -> Set.add vreg uses) Set.empty instr

/// Fold over the VRegs used by a terminator without allocating an intermediate collection.
let foldTerminatorUses (folder: 'State -> VReg -> 'State) (state: 'State) (term: Terminator) : 'State =
    let fromOperand state op =
        match op with
        | Register vreg -> folder state vreg
        | _ -> state

    match term with
    | Ret op -> fromOperand state op
    | Branch (cond, _, _) -> fromOperand state cond
    | Jump _ -> state

/// Get VRegs used by terminator
let getTerminatorUses (term: Terminator) : Set<VReg> =
    foldTerminatorUses (fun uses vreg -> Set.add vreg uses) Set.empty term
