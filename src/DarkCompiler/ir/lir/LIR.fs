// LIR.fs - Symbolic Low-level Intermediate Representation
//
// Defines the LIR (Low-level IR) data structures with symbolic literals.
// Strings and floats are stored by value so pools are resolved late in emission.

module LIR

/// ARM64 general-purpose registers used in LIR.
/// X16/X17 are IP0/IP1 scratch registers, X27 is reserved for free-list state,
/// and X28 is intentionally omitted because it is the target-specific heap bump
/// pointer rather than an allocatable or spillable LIR register.
type PhysReg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17
    | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27
    | X29
    | X30
    | SP

/// ARM64 floating-point registers
/// D0-D7 are caller-saved, D8-D15 are callee-saved.
type PhysFPReg =
    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15

/// Register or virtual register (before allocation)
type Reg =
    | Physical of PhysReg
    | Virtual of int

/// Floating-point register or virtual FP register (before allocation)
type FReg =
    | FPhysical of PhysFPReg
    | FVirtual of int

/// Parameter with register and type bundled (makes invalid states unrepresentable)
type TypedLIRParam = { Reg: Reg; Type: AST.SemanticType }

/// Operands (symbolic string/float references)
type Operand =
    | Imm of int64
    | FloatImm of float
    | Reg of Reg
    | StackSlot of int
    | StringSymbol of string
    | FloatSymbol of float
    | FuncAddr of AST.FunctionId

/// Comparison conditions (for CSET)
type Condition =
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE
    | ULT
    | UGT
    | ULE
    | UGE

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | StreamHeap
    | TaggedList
    | DictHeap
    | ClosureHeap

type CliOperation =
    | Execute
    | RunProcess
    | HostOS
    | HostArchitecture
    | Hostname
    | GetEnv
    | GetEnvironmentPacked
    | SetEnv
    | UnsetEnv
    | DirectoryCurrent
    | DirectoryListPacked
    | FileIsDirectory
    | GetArgv
    | Kill
    | GetPid
    | GetUid
    | CpuCount
    | SpawnProcess
    | ProcessIO
    | TerminateProcess

/// Basic block label (wrapper type for type safety)
type Label = Label of string

/// Instructions (symbolic)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Phi of dest:Reg * sources:(Operand * Label) list * valueType:AST.SemanticType option
    | Store of stackSlot:int * src:Reg
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg
    | Udiv of dest:Reg * left:Reg * right:Reg
    | Msub of dest:Reg * mulLeft:Reg * mulRight:Reg * sub:Reg
    | Madd of dest:Reg * mulLeft:Reg * mulRight:Reg * add:Reg
    | Cmp of left:Reg * right:Operand
    | Cset of dest:Reg * cond:Condition
    /// Select one of two register values using the flags from the preceding comparison.
    | Select of dest:Reg * whenTrue:Reg * whenFalse:Reg * cond:Condition
    | And of dest:Reg * left:Reg * right:Reg
    | And_imm of dest:Reg * src:Reg * imm:int64
    | Orr of dest:Reg * left:Reg * right:Reg
    | Eor of dest:Reg * left:Reg * right:Reg
    | Lsl of dest:Reg * src:Reg * shift:Reg
    | Lsr of dest:Reg * src:Reg * shift:Reg
    | Asr of dest:Reg * src:Reg * shift:Reg
    | Lsl_imm of dest:Reg * src:Reg * shift:int
    | Lsr_imm of dest:Reg * src:Reg * shift:int
    | Asr_imm of dest:Reg * src:Reg * shift:int

    | Neg of dest:Reg * src:Reg
    | Mvn of dest:Reg * src:Reg
    | Sxtb of dest:Reg * src:Reg
    | Sxth of dest:Reg * src:Reg
    | Sxtw of dest:Reg * src:Reg
    | Uxtb of dest:Reg * src:Reg
    | Uxth of dest:Reg * src:Reg
    | Uxtw of dest:Reg * src:Reg
    | Call of dest:Reg * funcName:AST.FunctionId * args:Operand list
    | TailCall of funcName:AST.FunctionId * args:Operand list
    | IndirectCall of dest:Reg * func:Reg * args:Operand list
    | IndirectTailCall of func:Reg * args:Operand list
    | ClosureAlloc of dest:Reg * funcName:AST.FunctionId * captures:Operand list
    | ClosureCall of dest:Reg * closure:Reg * args:Operand list
    | ClosureTailCall of closure:Reg * args:Operand list
    | SaveRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    | RestoreRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    /// Destination registers are stored in ABI argument order.
    | ArgMoves of (PhysReg * Operand) list
    | TailArgMoves of (PhysReg * Operand) list
    | FArgMoves of (PhysFPReg * FReg) list
    | PrintInt64 of Reg
    | PrintUInt64 of Reg
    | PrintBool of Reg
    | PrintInt64NoNewline of Reg
    | PrintUInt64NoNewline of Reg
    | PrintBoolNoNewline of Reg
    | PrintFloat of FReg
    | PrintFloatNoNewline of FReg
    | PrintString of string
    | StdoutWrite of effectId:int * value:Operand * appendNewline:bool
    | StdinReadLine of effectId:int * dest:Reg
    | RuntimeError of string
    | RuntimeErrorString of Reg
    | PrintHeapStringNoNewline of Reg
    | PrintChars of byte list
    | PrintBlob of Reg
    | PrintList of listPtr:Reg * elemType:AST.SemanticType
    | PrintSum of sumPtr:Reg * variants:(string * int * AST.SemanticType option) list
    | PrintRecord of recordPtr:Reg * typeName:string * fields:(string * AST.SemanticType) list
    | Exit
    | FPhi of dest:FReg * sources:(FReg * Label) list
    | FMov of dest:FReg * src:FReg
    | FLoad of dest:FReg * floatValue:float
    | FSpillLoad of dest:FReg * stackSlot:int
    | FSpillStore of stackSlot:int * src:FReg
    | FAdd of dest:FReg * left:FReg * right:FReg
    | FSub of dest:FReg * left:FReg * right:FReg
    | FMul of dest:FReg * left:FReg * right:FReg
    | FMadd of dest:FReg * left:FReg * right:FReg * addend:FReg
    | FDiv of dest:FReg * left:FReg * right:FReg
    | FNeg of dest:FReg * src:FReg
    | FAbs of dest:FReg * src:FReg
    | FSqrt of dest:FReg * src:FReg
    | FCmp of left:FReg * right:FReg
    | Int64ToFloat of dest:FReg * src:Reg
    | FloatToInt64 of dest:Reg * src:FReg
    | FloatToBits of dest:Reg * src:FReg
    | GpToFp of dest:FReg * src:Reg
    | FpToGp of dest:Reg * src:FReg
    | HeapAlloc of dest:Reg * sizeBytes:int
    | HeapStore of addr:Reg * offset:int * src:Operand * valueType:AST.SemanticType option
    | HeapLoad of dest:Reg * addr:Reg * offset:int
    | RefCountInc of addr:Reg * payloadSize:int * kind:RcKind * metadata:MemoryModel.RcMetadata option
    | RefCountDec of addr:Reg * payloadSize:int * kind:RcKind * metadata:MemoryModel.RcMetadata option
    /// Concatenate at least two strings with one allocation and ordered copies.
    | StringConcat of dest:Reg * first:Operand * second:Operand * remaining:Operand list
    | CanonicalBufferEq of dest:Reg * kind:MemoryModel.CanonicalBufferKind * left:Operand * right:Operand
    | PrintHeapString of Reg
    | LoadFuncAddr of dest:Reg * funcName:AST.FunctionId
    | FileReadBlob of dest:Reg * path:Operand
    | FileExists of dest:Reg * path:Operand
    | FileWriteBlob of dest:Reg * path:Operand * content:Operand
    | FileAppendText of dest:Reg * path:Operand * content:Operand
    | FileDelete of dest:Reg * path:Operand
    | FileCreateDirectory of dest:Reg * path:Operand
    | FileSetExecutable of dest:Reg * path:Operand
    | FileWriteFromPtr of dest:Reg * path:Operand * ptr:Reg * length:Reg
    | RawAlloc of dest:Reg * numBytes:Reg
    | MappedAlloc of dest:Reg * numBytes:Reg
    | RawFree of ptr:Reg
    | MappedFree of ptr:Reg
    | RawGet of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawGetByte of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawWriteWord of ptr:Reg * byteOffset:Reg * value:Reg
    | RawWriteByte of ptr:Reg * byteOffset:Reg * value:Reg
    | RawSlotInit of ptr:Reg * byteOffset:Reg * value:Reg * valueType:AST.SemanticType
    | RefCountIncString of str:Operand
    | RefCountDecString of str:Operand
    | RefCountIncBlob of bytes:Operand
    | RefCountDecBlob of bytes:Operand
    | RefCountIncInt of value:Operand
    | RefCountDecInt of value:Operand
    | RandomInt64 of dest:Reg
    | DateTimeNow of dest:Reg
    | Sleep of effectId:int * delayMs:FReg
    | CliNative of dest:Reg * operation:CliOperation * args:Operand list
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

/// Arrange blocks into deterministic successor chains so the backends can use
/// the lexical successor as a fallthrough edge. Entry remains first; each
/// remaining chain starts in label order, and every block occurs exactly once.
/// A single shared return is deferred to the end to fall into the epilogue.
let layoutBlocks (cfg: CFG) : Result<BasicBlock list, string> =
    let commonReturn =
        let returns =
            cfg.Blocks
            |> Map.toList
            |> List.choose (fun (label, block) -> if block.Terminator = Ret then Some label else None)
        match returns with
        | [label] when label <> cfg.Entry ->
            let predecessors =
                cfg.Blocks
                |> Map.toList
                |> List.filter (fun (_, block) ->
                    match block.Terminator with
                    | Ret -> false
                    | Jump target -> target = label
                    | Branch (_, yes, no)
                    | BranchZero (_, yes, no)
                    | BranchBitZero (_, _, yes, no)
                    | BranchBitNonZero (_, _, yes, no)
                    | CondBranch (_, yes, no) -> yes = label || no = label)
            // Count predecessor blocks, not edges: a same-target conditional
            // alone does not make a return shared.
            if List.length predecessors >= 2 then Some label else None
        | _ -> None

    let preferredSuccessor terminator =
        match terminator with
        | Ret -> None
        | Jump target -> Some target
        | Branch (_, _, falseTarget)
        | BranchZero (_, _, falseTarget)
        | BranchBitZero (_, _, _, falseTarget)
        | BranchBitNonZero (_, _, _, falseTarget)
        | CondBranch (_, _, falseTarget) -> Some falseTarget

    let rec followChain visited reversedBlocks label =
        if Set.contains label visited then
            Ok (visited, reversedBlocks)
        else
            match Map.tryFind label cfg.Blocks with
            | None -> Error $"LIR layout: CFG references missing block {label}"
            | Some block ->
                let visited = Set.add label visited
                let reversedBlocks = block :: reversedBlocks
                match preferredSuccessor block.Terminator with
                // Layout orders the blocks it is given; malformed successor
                // references remain the consumer's responsibility so layout
                // does not mask a more specific backend diagnostic.
                | Some successor when
                    not (Set.contains successor visited)
                    && Some successor <> commonReturn
                    && Map.containsKey successor cfg.Blocks ->
                    followChain visited reversedBlocks successor
                | _ -> Ok (visited, reversedBlocks)

    match Map.tryFind cfg.Entry cfg.Blocks with
    | None -> Error $"LIR layout: CFG missing entry block {cfg.Entry}"
    | Some _ ->
        followChain Set.empty [] cfg.Entry
        |> Result.bind (fun (initialVisited, initialBlocks) ->
            cfg.Blocks
            |> Map.toList
            |> List.map fst
            |> List.partition (fun label -> Some label <> commonReturn)
            |> fun (ordinary, deferred) -> ordinary @ deferred
            |> List.fold
                (fun state label ->
                    state
                    |> Result.bind (fun (visited, reversedBlocks) ->
                        if Set.contains label visited then Ok (visited, reversedBlocks)
                        else followChain visited reversedBlocks label))
                (Ok (initialVisited, initialBlocks))
            |> Result.map (fun (_, reversedBlocks) -> List.rev reversedBlocks))

/// Ordered memo key for RC planning. Small plans remain structural because
/// comparing them is cheaper than hashing; large plans carry a compact key.
type RcReleasePlanMemoKey =
    | FingerprintedReleasePlan of string
    | StructuralReleasePlan of MemoryModel.RcReleasePlan option

let rcReleasePlanMemoKey (metadata: MemoryModel.RcMetadata option) : RcReleasePlanMemoKey =
    match metadata |> Option.bind (fun value -> value.ReleasePlanCacheKey) with
    | Some cacheKey -> FingerprintedReleasePlan cacheKey
    | None ->
        metadata
        |> Option.bind (fun value -> value.ReleasePlan)
        |> StructuralReleasePlan

/// ARM64 helpers implied by traversing one reference-count release plan.
type Arm64ReleasePlanSummary = {
    ListDecHelperLabels: Set<string>
    PlannedListDecHelpers: Map<string, int * MemoryModel.RcReleasePlan>
    ExpensiveGenericDecHelper: (string * int * MemoryModel.RcReleasePlan) option
    DictDecHelperLabels: Set<string>
    PlannedDictDecHelpers: Map<string, MemoryModel.RcReleasePlan>
    NeedsClosureRcDecHelper: bool
    NeedsStreamRcDecHelper: bool
}

/// One allocator-visible helper for a complex generic release. The ownership
/// policy is semantic: ordinary functions own single-payload sum fields while
/// most Stdlib functions borrow them.
type Arm64PlannedGenericDecHelper = {
    ReleasePlanMemoKeys: Set<RcReleasePlanMemoKey>
    PayloadSize: int
    ReleasePlan: MemoryModel.RcReleasePlan
    OwnsSinglePayloadSum: bool
}

/// ARM64 helper requirements already planned for one function. The summary
/// memo is retained so registry-dependent closure captures can reuse it after
/// reachable functions are combined.
type Arm64RcHelperRequirements = {
    ListDecHelperLabels: Set<string>
    PlannedListDecHelpers: Map<string, int * MemoryModel.RcReleasePlan>
    PlannedGenericDecHelpers: Map<string, Arm64PlannedGenericDecHelper>
    PlannedDictDecHelpers: Map<string, MemoryModel.RcReleasePlan>
    DictDecHelperLabels: Set<string>
    NeedsListRcIncHelper: bool
    NeedsDictRcIncHelper: bool
    NeedsClosureRcIncHelper: bool
    NeedsClosureRcDecHelper: bool
    NeedsStreamRcDecHelper: bool
    /// Memoized by the release plan's stable fingerprint. Using the expanded
    /// recursive plan as the key makes every lookup repeat a deep comparison.
    ReleasePlanSummaries: Map<bool * RcReleasePlanMemoKey, Arm64ReleasePlanSummary>
}

/// The ownership action emitted after initializing one typed raw slot. ARM64
/// resolves this from nominal registries during function preparation so the
/// finalized function no longer depends on a whole-program registry context.
type Arm64SlotInitRootRetainTarget =
    | SlotInitListRootRetain
    | SlotInitDictRootRetain
    | SlotInitDynamicBufferRetain
    | SlotInitClosureRootRetain
    | SlotInitGenericRootRetain of payloadSize:int

/// Compact, register-independent facts needed while assembling native code.
/// These are computed once from finalized symbolic LIR, then travel with the
/// function through register allocation and tree shaking. Backends therefore
/// only combine facts from the functions that survived reachability instead of
/// rescanning every instruction in every executable.
type FunctionCodegenFacts = {
    ClosurePayloadSizeFromParams: int option
    ClosureCaptureTypes: AST.SemanticType list option
    ClosurePayloadSizesFromAllocs: (AST.FunctionId * int) list
    RecursiveReleaseTypes: Set<AST.SemanticType>
    /// Deduplicated by kind and compact plan identity. Keeping recursive
    /// metadata out of the ordered key prevents deep structural comparisons.
    RefCountDecRequirements: Map<RcKind * RcReleasePlanMemoKey, MemoryModel.RcMetadata option>
    RefCountIncRequirements: Set<RcKind>
    RawSlotInitTypes: Set<AST.SemanticType>
    /// Some after ARM64 preparation. Values are None for slot types that do
    /// not need a root retain; the outer option distinguishes an empty plan
    /// from legacy/unprepared LIR.
    Arm64RawSlotInitRetainTargets: Map<AST.SemanticType, Arm64SlotInitRootRetainTarget option> option
    NeedsCliRuntimeState: bool
    NeedsCliArgvHelper: bool
    NeedsCliExecuteHelper: bool
    NeedsCliRunProcessHelper: bool
    NeedsCliProcessLifecycleHelpers: bool
    /// The function can terminate through a runtime error or allocation
    /// failure and therefore needs the backend's shared error routine.
    NeedsRuntimeErrorHelper: bool
    Arm64RcHelperRequirements: Arm64RcHelperRequirements option
}

/// Function with CFG. CodegenFacts is absent only for hand-built or legacy LIR;
/// production lowering attaches it before register allocation.
type Function = {
    Id: AST.FunctionId
    Name: string
    TypedParams: TypedLIRParam list
    CFG: CFG
    StackSize: int
    /// Register allocation stores these in physical-register order so backend
    /// prologue and epilogue generation can consume them directly.
    UsedCalleeSaved: PhysReg list
    CodegenFacts: FunctionCodegenFacts option
}

/// Derive the compact code-generation facts for a single function. Keeping
/// this public also gives backend tests a slow-path oracle for carried facts.
let analyzeFunctionCodegenFacts (func: Function) : FunctionCodegenFacts =
    let closurePayloadSizeFromParams, closureCaptureTypes =
        match func.TypedParams with
        | { Type = AST.TTuple fields } :: _ ->
            let captures =
                match fields with
                | _funcPtrType :: captures -> Some captures
                | [] -> None
            (Some (List.length fields * 8), captures)
        | _ ->
            (None, None)

    let mutable closurePayloadSizesFromAllocsRev = []
    let mutable recursiveReleaseTypes = Set.empty
    let mutable refCountDecRequirements = Map.empty
    let mutable refCountIncRequirements = Set.empty
    let mutable rawSlotInitTypes = Set.empty
    let mutable needsCliRuntimeState = false
    let mutable needsCliArgvHelper = false
    let mutable needsCliExecuteHelper = false
    let mutable needsCliRunProcessHelper = false
    let mutable needsCliProcessLifecycleHelpers = false

    for KeyValue (_, block) in func.CFG.Blocks do
        for instr in block.Instrs do
            match instr with
            | ClosureAlloc (_, funcName, captures) ->
                closurePayloadSizesFromAllocsRev <-
                    (funcName, (List.length captures + 1) * 8)
                    :: closurePayloadSizesFromAllocsRev
            | RefCountDec (_, _, kind, metadata) ->
                refCountDecRequirements <-
                    Map.add (kind, rcReleasePlanMemoKey metadata) metadata refCountDecRequirements
                recursiveReleaseTypes <-
                    metadata
                    |> Option.bind (fun metadata -> metadata.ReleasePlan)
                    |> Option.map MemoryPlanning.recursiveReleaseTypes
                    |> Option.defaultValue Set.empty
                    |> Set.union recursiveReleaseTypes
            | RefCountInc (_, _, kind, _) ->
                match kind with
                | TaggedList
                | DictHeap
                | ClosureHeap ->
                    refCountIncRequirements <-
                        Set.add kind refCountIncRequirements
                | GenericHeap
                | StreamHeap ->
                    ()
            | RawSlotInit (_, _, _, valueType) ->
                rawSlotInitTypes <- Set.add valueType rawSlotInitTypes
            | CliNative (_, operation, _) ->
                needsCliRuntimeState <- true
                if operation = GetArgv then needsCliArgvHelper <- true
                if operation = Execute then needsCliExecuteHelper <- true
                if operation = RunProcess then needsCliRunProcessHelper <- true
                if operation = SpawnProcess || operation = ProcessIO || operation = TerminateProcess then
                    needsCliProcessLifecycleHelpers <- true
            | _ ->
                ()

    {
        ClosurePayloadSizeFromParams = closurePayloadSizeFromParams
        ClosureCaptureTypes = closureCaptureTypes
        ClosurePayloadSizesFromAllocs = List.rev closurePayloadSizesFromAllocsRev
        RecursiveReleaseTypes = recursiveReleaseTypes
        RefCountDecRequirements = refCountDecRequirements
        RefCountIncRequirements = refCountIncRequirements
        RawSlotInitTypes = rawSlotInitTypes
        Arm64RawSlotInitRetainTargets = None
        NeedsCliRuntimeState = needsCliRuntimeState
        NeedsCliArgvHelper = needsCliArgvHelper
        NeedsCliExecuteHelper = needsCliExecuteHelper
        NeedsCliRunProcessHelper = needsCliRunProcessHelper
        NeedsCliProcessLifecycleHelpers = needsCliProcessLifecycleHelpers
        NeedsRuntimeErrorHelper =
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | RuntimeError _
                    | RuntimeErrorString _
                    | HeapAlloc _
                    | RawAlloc _
                    | MappedAlloc _
                    | MappedFree _ -> true
                    | _ -> false))
        Arm64RcHelperRequirements = None
    }

let attachFunctionCodegenFacts (func: Function) : Function =
    { func with CodegenFacts = Some (analyzeFunctionCodegenFacts func) }

/// Record definitions needed by late, type-specialized runtime helpers.
type RecordRegistry = Map<string, (string * AST.SemanticType) list>

/// Info about a single sum variant needed by late, type-specialized runtime helpers.
type VariantInfo = {
    Name: string
    Tag: int
    Payload: AST.SemanticType option
}

/// All variants for a sum type, with type parameters.
type TypeVariants = {
    TypeParams: string list
    Variants: VariantInfo list
}

/// Sum definitions needed by late, type-specialized runtime helpers.
type VariantRegistry = Map<string, TypeVariants>

/// LIR program (symbolic literals, no pools)
type Program = Program of functions:Function list * variants:VariantRegistry * records:RecordRegistry

let attachCodegenFacts (Program (functions, variants, records)) : Program =
    Program (List.map attachFunctionCodegenFacts functions, variants, records)

/// Count the number of CoverageHit instructions in a program
let countCoverageHits (Program (functions, _, _)) : int =
    functions
    |> List.collect (fun f ->
        f.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs))
    |> List.filter (function CoverageHit _ -> true | _ -> false)
    |> List.length
