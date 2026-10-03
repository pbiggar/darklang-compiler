(* LIR.fs - Symbolic Low-level Intermediate Representation. *)
[@@@warning "-4-30"]
type physReg =    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17
    | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27
    | X29
    | X30
    | SP
type physFPReg =    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15
type reg =    | Physical of physReg
    | Virtual of int
type fReg =    | FPhysical of physFPReg
    | FVirtual of int
type typedLIRParam = {reg : reg; typ : AST.semanticType}
type operand =    | Imm of int64
    | FloatImm of float
    | Reg of reg
    | StackSlot of int
    | StringSymbol of string
    | FloatSymbol of float
    | FuncAddr of AST.functionId
type condition =    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE
    | ULT
    | UGT
    | ULE
    | UGE
type rcKind =    | GenericHeap
    | StreamHeap
    | TaggedList
    | DictHeap
    | ClosureHeap
type cliOperation =    | Execute
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
    | FileCreateExclusive
    | GetArgv
    | Kill
    | GetPid
    | GetUid
    | CpuCount
    | SpawnProcess
    | ProcessIO
    | TerminateProcess
    | SocketTcp4
    | SocketTcp6
    | SocketUdp4
    | SocketUdp6
    | SocketConnect4
    | SocketConnect6
    | SocketSend
    | SocketReceive
    | SocketReceiveTimeout
    | SocketSendTimeout
    | SocketClose
    | SecureRandomFill
type label = Label of string
module LabelMap : Map.S with type key = label
module LabelSet : Set.S with type elt = label
type instr =    | Mov of reg * operand
    | Phi of reg * (operand * label) list * AST.semanticType option
    | Store of int * reg
    | Add of reg * reg * operand
    | Sub of reg * reg * operand
    | Mul of reg * reg * reg
    | Sdiv of reg * reg * reg
    | Udiv of reg * reg * reg
    | Msub of reg * reg * reg * reg
    | Madd of reg * reg * reg * reg
    | Cmp of reg * operand
    | Cset of reg * condition
    | Select of reg * reg * reg * condition
    | And of reg * reg * reg
    | And_imm of reg * reg * int64
    | Orr of reg * reg * reg
    | Eor of reg * reg * reg
    | Lsl of reg * reg * reg
    | Lsr of reg * reg * reg
    | Asr of reg * reg * reg
    | Lsl_imm of reg * reg * int
    | Lsr_imm of reg * reg * int
    | Asr_imm of reg * reg * int
    | Neg of reg * reg
    | Mvn of reg * reg
    | Sxtb of reg * reg
    | Sxth of reg * reg
    | Sxtw of reg * reg
    | Uxtb of reg * reg
    | Uxth of reg * reg
    | Uxtw of reg * reg
    | Call of reg * AST.functionId * operand list
    | TailCall of AST.functionId * operand list
    | IndirectCall of reg * reg * operand list
    | IndirectTailCall of reg * operand list
    | ClosureAlloc of reg * AST.functionId * operand list
    | ClosureCall of reg * reg * operand list
    | ClosureTailCall of reg * operand list
    | SaveRegs of physReg list * physFPReg list
    | RestoreRegs of physReg list * physFPReg list
    | ArgMoves of (physReg * operand) list
    | TailArgMoves of (physReg * operand) list
    | FArgMoves of (physFPReg * fReg) list
    | PrintInt64 of reg
    | PrintUInt64 of reg
    | PrintBool of reg
    | PrintInt64NoNewline of reg
    | PrintUInt64NoNewline of reg
    | PrintBoolNoNewline of reg
    | PrintFloat of fReg
    | PrintFloatNoNewline of fReg
    | PrintString of string
    | StdoutWrite of int * operand * bool
    | StdinReadLine of int * reg
    | RuntimeError of string
    | RuntimeErrorString of reg
    | PrintHeapStringNoNewline of reg
    | PrintChars of int list
    | PrintBlob of reg
    | PrintList of reg * AST.semanticType
    | PrintSum of reg * (string * int * AST.semanticType option) list * bool
    | PrintRecord of reg * string * (string * AST.semanticType) list
    | Exit
    | FPhi of fReg * (fReg * label) list
    | FMov of fReg * fReg
    | FLoad of fReg * float
    | FSpillLoad of fReg * int
    | FSpillStore of int * fReg
    | FAdd of fReg * fReg * fReg
    | FSub of fReg * fReg * fReg
    | FMul of fReg * fReg * fReg
    | FMadd of fReg * fReg * fReg * fReg
    | FDiv of fReg * fReg * fReg
    | FNeg of fReg * fReg
    | FAbs of fReg * fReg
    | FSqrt of fReg * fReg
    | FCmp of fReg * fReg
    | Int64ToFloat of fReg * reg
    | FloatToInt64 of reg * fReg
    | FloatToBits of reg * fReg
    | GpToFp of fReg * reg
    | FpToGp of reg * fReg
    | HeapAlloc of reg * int
    | HeapStore of reg * int * operand * AST.semanticType option
    | HeapLoad of reg * reg * int
    | RefCountInc of reg * int * rcKind * MemoryModel.rcMetadata option
    | RefCountDec of reg * int * rcKind * MemoryModel.rcMetadata option
    | StringConcat of reg * operand * operand * operand list
    | CanonicalBufferEq of reg * MemoryModel.canonicalBufferKind * operand * operand
    | PrintHeapString of reg
    | LoadFuncAddr of reg * AST.functionId
    | FileReadBlob of reg * operand
    | FileExists of reg * operand
    | FileWriteBlob of reg * operand * operand
    | FileAppendText of reg * operand * operand
    | FileDelete of reg * operand
    | FileCreateDirectory of reg * operand
    | FileSetExecutable of reg * operand
    | FileWriteFromPtr of reg * operand * reg * reg
    | RawAlloc of reg * reg
    | MappedAlloc of reg * reg
    | RawFree of reg
    | MappedFree of reg
    | RawGet of reg * reg * reg
    | RawGetByte of reg * reg * reg
    | RawWriteWord of reg * reg * reg
    | RawWriteByte of reg * reg * reg
    | RawSlotInit of reg * reg * reg * AST.semanticType
    | RefCountIncString of operand
    | RefCountDecString of operand
    | RefCountIncBlob of operand
    | RefCountDecBlob of operand
    | RefCountIncInt of operand
    | RefCountDecInt of operand
    | RandomInt64 of reg
    | DateTimeNow of reg
    | Sleep of int * fReg
    | CliNative of reg * cliOperation * operand list
    | FloatToString of reg * fReg
    | CoverageHit of int
type terminator =    | Ret
    | Branch of reg * label * label
    | BranchZero of reg * label * label
    | BranchBitZero of reg * int * label * label
    | BranchBitNonZero of reg * int * label * label
    | CondBranch of condition * label * label
    | Jump of label
type basicBlock = {label : label; instrs : instr list; terminator : terminator}
type cfg = {entry : label; blocks : basicBlock LabelMap.t}
type rcReleasePlanMemoKey = FingerprintedReleasePlan of string | StructuralReleasePlan of MemoryModel.rcReleasePlan option
module RcReleasePlanMemoKeySet : Set.S with type elt = rcReleasePlanMemoKey
module ReleasePlanSummaryMap : Map.S with type key = bool * rcReleasePlanMemoKey
module RefCountDecRequirementMap : Map.S with type key = rcKind * rcReleasePlanMemoKey
module RcKindSet : Set.S with type elt = rcKind
module SemanticTypeMap : Map.S with type key = AST.semanticType
type arm64ReleasePlanSummary = {
 listDecHelperLabels : StringOrder.Set.t;
 plannedListDecHelpers : (int * MemoryModel.rcReleasePlan) StringOrder.Map.t;
 expensiveGenericDecHelper : (string * int * MemoryModel.rcReleasePlan) option;
 dictDecHelperLabels : StringOrder.Set.t;
 plannedDictDecHelpers : MemoryModel.rcReleasePlan StringOrder.Map.t;
 needsClosureRcDecHelper : bool;
 needsStreamRcDecHelper : bool
}
type arm64PlannedGenericDecHelper = {
 releasePlanMemoKeys : RcReleasePlanMemoKeySet.t;
 payloadSize : int;
 releasePlan : MemoryModel.rcReleasePlan;
 ownsSinglePayloadSum : bool
}
type arm64RcHelperRequirements = {
 listDecHelperLabels : StringOrder.Set.t;
 plannedListDecHelpers : (int * MemoryModel.rcReleasePlan) StringOrder.Map.t;
 plannedGenericDecHelpers : arm64PlannedGenericDecHelper StringOrder.Map.t;
 plannedDictDecHelpers : MemoryModel.rcReleasePlan StringOrder.Map.t;
 dictDecHelperLabels : StringOrder.Set.t;
 needsListRcIncHelper : bool;
 needsDictRcIncHelper : bool;
 needsClosureRcIncHelper : bool;
 needsClosureRcDecHelper : bool;
 needsStreamRcDecHelper : bool;
 releasePlanSummaries : arm64ReleasePlanSummary ReleasePlanSummaryMap.t
}
type arm64SlotInitRootRetainTarget = SlotInitListRootRetain | SlotInitDictRootRetain | SlotInitDynamicBufferRetain | SlotInitClosureRootRetain | SlotInitGenericRootRetain of int
type functionCodegenFacts = {
 arm64UsedCalleeSavedF : physFPReg list;
 closurePayloadSizeFromParams : int option;
 closureCaptureTypes : AST.semanticType list option;
 closurePayloadSizesFromAllocs : (AST.functionId * int) list;
 recursiveReleaseTypes : MemoryPlanning.SemanticTypeSet.t;
 refCountDecRequirements : MemoryModel.rcMetadata option RefCountDecRequirementMap.t;
 refCountIncRequirements : RcKindSet.t;
 rawSlotInitTypes : MemoryPlanning.SemanticTypeSet.t;
 arm64RawSlotInitRetainTargets : arm64SlotInitRootRetainTarget option SemanticTypeMap.t option;
 needsCliRuntimeState : bool;
 needsCliArgvHelper : bool;
 needsCliExecuteHelper : bool;
 needsCliRunProcessHelper : bool;
 needsCliProcessLifecycleHelpers : bool;
 needsRuntimeErrorHelper : bool;
 arm64RcHelperRequirements : arm64RcHelperRequirements option;
 arm64GenericHelperIds : AST.functionId StringOrder.Map.t
}
type functionDef = {id : AST.functionId; name : string; typedParams : typedLIRParam list; cfg : cfg; stackSize : int; usedCalleeSaved : physReg list; codegenFacts : functionCodegenFacts option}
type recordRegistry = (string * AST.semanticType) list StringOrder.Map.t
type variantInfo = {name : string; tag : int; payload : AST.semanticType option; fieldCount : int}
type typeVariants = {typeParams : string list; variants : variantInfo list}
type variantRegistry = typeVariants StringOrder.Map.t
type program = Program of functionDef list * variantRegistry * recordRegistry
val layoutBlocks : cfg -> (basicBlock list, string) result
val rcReleasePlanMemoKey : MemoryModel.rcMetadata option -> rcReleasePlanMemoKey
val analyzeFunctionCodegenFacts : functionDef -> functionCodegenFacts
val attachFunctionCodegenFacts : functionDef -> functionDef
val attachCodegenFacts : program -> program
val countCoverageHits : program -> int
