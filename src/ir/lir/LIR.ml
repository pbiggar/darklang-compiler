(*
   Defines the LIR (Low-level IR) data structures with symbolic literals.
   Strings and floats are stored by value so pools are resolved late in emission.
   Control Flow Graph
   Function with CFG. CodegenFacts is absent only for hand-built or legacy LIR;
   production lowering attaches it before register allocation.
   Register allocation stores these in physical-register order so backend
   prologue and epilogue generation can consume them directly.
   LIR program (symbolic literals, no pools)
*)
(* LIR.ml - Symbolic Low-level Intermediate Representation. *)
[@@@warning "-4-30"]

(*
   ARM64 general-purpose registers used in LIR.
   X16/X17 are IP0/IP1 scratch registers, X27 is reserved for free-list state,
   and X28 is intentionally omitted because it is the target-specific heap bump
   pointer rather than an allocatable or spillable LIR register.
*)
type physReg =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X29
  | X30
  | SP

(*
   ARM64 floating-point registers
   D0-D7 are caller-saved, D8-D15 are callee-saved.
*)
type physFPReg =
  | D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15

(*
   Register or virtual register (before allocation)
*)
type reg = Physical of physReg | Virtual of int

(*
   Floating-point register or virtual FP register (before allocation).
   FVirtual -1 is reserved for a Float return crossing RestoreRegs.
*)
type fReg = FPhysical of physFPReg | FVirtual of int

(*
   Parameter with register and type bundled (makes invalid states unrepresentable)
*)
type typedLIRParam = { reg : reg; typ : AST.semanticType }

(*
   Operands (symbolic string/float references)
*)
type operand =
  | Imm of int64
  | FloatImm of float
  | Reg of reg
  | StackSlot of int
  | StringSymbol of string
  | FloatSymbol of float
  | FuncAddr of AST.functionId

(*
   Comparison conditions (for CSET)
*)
type condition = EQ | NE | LT | GT | LE | GE | ULT | UGT | ULE | UGE

(*
   Reference-count operation kind
*)
type rcKind = GenericHeap | StreamHeap | TaggedList | DictHeap | ClosureHeap

type cliOperation =
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
  | SocketBind4
  | SocketListen
  | SocketAccept
  | SocketCloexec
  | SocketReuseAddress
  | SocketPoll
  | SignalBlock
  | SignalRestore
  | SignalPending
  | SignalWait
  | MonotonicTime
  | SecureRandomFill

(*
   Basic block label (wrapper type for type safety)
*)
type label = Label of string

module LabelMap = Map.Make (struct
  type t = label

  let compare (Label a) (Label b) = StringOrder.compare a b
end)

module LabelSet = Set.Make (struct
  type t = label

  let compare (Label a) (Label b) = StringOrder.compare a b
end)

(*
   Instructions (symbolic)
   Select one of two register values using the flags from the preceding comparison.
   Destination registers are stored in ABI argument order.
   Concatenate at least two strings with one allocation and ordered copies.
*)
type instr =
  | Mov of reg * operand
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
  | CanonicalBufferEq of
      reg * MemoryModel.canonicalBufferKind * operand * operand
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

(*
   Terminators
*)
type terminator =
  | Ret
  | Branch of reg * label * label
  | BranchZero of reg * label * label
  | BranchBitZero of reg * int * label * label
  | BranchBitNonZero of reg * int * label * label
  | CondBranch of condition * label * label
  | Jump of label

(*
   Basic block with label, instructions, and terminator
*)
type basicBlock = {
  label : label;
  instrs : instr list;
  terminator : terminator;
}

type cfg = { entry : label; blocks : basicBlock LabelMap.t }

(*
   Ordered memo key for RC planning. Small plans remain structural because
   comparing them is cheaper than hashing; large plans carry a compact key.
*)
type rcReleasePlanMemoKey =
  | FingerprintedReleasePlan of string
  | StructuralReleasePlan of MemoryModel.rcReleasePlan option

let thenCompare order next = if order = 0 then next () else order

let rec compareList compare left right =
  match (left, right) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | a :: aa, b :: bb ->
      thenCompare (compare a b) (fun () -> compareList compare aa bb)

let compareOption compare left right =
  match (left, right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some a, Some b -> compare a b

let compareOperation left right =
  let tag = function
    | MemoryModel.FixedSizeRoot _ -> 0
    | MemoryModel.DynamicStringBuffer -> 1
    | MemoryModel.DynamicBlobBuffer -> 2
    | MemoryModel.DynamicIntBuffer -> 3
  in
  match (left, right) with
  | ( MemoryModel.FixedSizeRoot (size, kind),
      MemoryModel.FixedSizeRoot (size', kind') ) ->
      thenCompare (Int.compare size size') (fun () -> Stdlib.compare kind kind')
  | _ -> Int.compare (tag left) (tag right)

let rec compareReleasePlan left right =
  let tag = function
    | MemoryModel.NoReleasePlan -> 0
    | MemoryModel.DynamicBufferRelease _ -> 1
    | MemoryModel.RecursiveRelease _ -> 2
    | MemoryModel.RootRelease _ -> 3
  in
  match (left, right) with
  | MemoryModel.DynamicBufferRelease a, MemoryModel.DynamicBufferRelease b ->
      compareOperation a b
  | MemoryModel.RecursiveRelease a, MemoryModel.RecursiveRelease b ->
      AST.compareSemanticType a b
  | ( MemoryModel.RootRelease (size, kind, payload),
      MemoryModel.RootRelease (size', kind', payload') ) ->
      thenCompare (Int.compare size size') (fun () ->
          thenCompare (Stdlib.compare kind kind') (fun () ->
              comparePayload payload payload'))
  | _ -> Int.compare (tag left) (tag right)

and comparePayload left right =
  let tag = function
    | MemoryModel.NoPayloadRelease -> 0
    | MemoryModel.FixedBlockPayloadRelease _ -> 1
    | MemoryModel.BoxedSumPayloadRelease _ -> 2
    | MemoryModel.TaggedListPayloadRelease _ -> 3
    | MemoryModel.DictPayloadRelease _ -> 4
    | MemoryModel.ClosurePayloadRelease _ -> 5
  in
  match (left, right) with
  | ( MemoryModel.FixedBlockPayloadRelease (size, fields),
      MemoryModel.FixedBlockPayloadRelease (size', fields') ) ->
      thenCompare (Int.compare size size') (fun () ->
          compareList compareField fields fields')
  | ( MemoryModel.BoxedSumPayloadRelease (size, fields, variants),
      MemoryModel.BoxedSumPayloadRelease (size', fields', variants') ) ->
      thenCompare (Int.compare size size') (fun () ->
          thenCompare (compareList compareField fields fields') (fun () ->
              compareList compareVariant variants variants'))
  | ( MemoryModel.TaggedListPayloadRelease a,
      MemoryModel.TaggedListPayloadRelease b ) ->
      compareReleasePlan a b
  | ( MemoryModel.DictPayloadRelease (a, b),
      MemoryModel.DictPayloadRelease (a', b') ) ->
      thenCompare (compareReleasePlan a a') (fun () -> compareReleasePlan b b')
  | MemoryModel.ClosurePayloadRelease a, MemoryModel.ClosurePayloadRelease b ->
      compareList compareField a b
  | _ -> Int.compare (tag left) (tag right)

and compareField (MemoryModel.FieldRelease (offset, plan))
    (MemoryModel.FieldRelease (offset', plan')) =
  thenCompare (Int.compare offset offset') (fun () ->
      compareReleasePlan plan plan')

and compareVariant (left : MemoryModel.rcBoxedSumVariantRelease)
    (right : MemoryModel.rcBoxedSumVariantRelease) =
  thenCompare (Int.compare left.MemoryModel.tag right.MemoryModel.tag)
    (fun () ->
      compareList compareField left.MemoryModel.fieldReleases
        right.MemoryModel.fieldReleases)

let compareMemoKey left right =
  match (left, right) with
  | FingerprintedReleasePlan a, FingerprintedReleasePlan b ->
      StringOrder.compare a b
  | StructuralReleasePlan a, StructuralReleasePlan b ->
      compareOption compareReleasePlan a b
  | FingerprintedReleasePlan _, StructuralReleasePlan _ -> -1
  | StructuralReleasePlan _, FingerprintedReleasePlan _ -> 1

module RcReleasePlanMemoKeySet = Set.Make (struct
  type t = rcReleasePlanMemoKey

  let compare = compareMemoKey
end)

module ReleasePlanSummaryMap = Map.Make (struct
  type t = bool * rcReleasePlanMemoKey

  let compare (a, b) (a', b') =
    thenCompare (Bool.compare a a') (fun () -> compareMemoKey b b')
end)

module RefCountDecRequirementMap = Map.Make (struct
  type t = rcKind * rcReleasePlanMemoKey

  let compare (a, b) (a', b') =
    thenCompare (Stdlib.compare a a') (fun () -> compareMemoKey b b')
end)

module RcKindSet = Set.Make (struct
  type t = rcKind

  let compare = Stdlib.compare
end)

module SemanticTypeMap = Map.Make (struct
  type t = AST.semanticType

  let compare = AST.compareSemanticType
end)

(*
   ARM64 helpers implied by traversing one reference-count release plan.
*)
type arm64ReleasePlanSummary = {
  listDecHelperLabels : StringOrder.Set.t;
  plannedListDecHelpers : (int * MemoryModel.rcReleasePlan) StringOrder.Map.t;
  expensiveGenericDecHelper : (string * int * MemoryModel.rcReleasePlan) option;
  dictDecHelperLabels : StringOrder.Set.t;
  plannedDictDecHelpers : MemoryModel.rcReleasePlan StringOrder.Map.t;
  needsClosureRcDecHelper : bool;
  needsStreamRcDecHelper : bool;
}

(*
   One allocator-visible helper for a complex generic release. The ownership
   policy is semantic: ordinary functions own single-payload sum fields while
   most Stdlib functions borrow them.
*)
type arm64PlannedGenericDecHelper = {
  releasePlanMemoKeys : RcReleasePlanMemoKeySet.t;
  payloadSize : int;
  releasePlan : MemoryModel.rcReleasePlan;
  ownsSinglePayloadSum : bool;
}

(*
   ARM64 helper requirements already planned for one function. The summary
   memo is retained so registry-dependent closure captures can reuse it after
   reachable functions are combined.
   Memoized by the release plan's stable fingerprint. Using the expanded
   recursive plan as the key makes every lookup repeat a deep comparison.
*)
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
  releasePlanSummaries : arm64ReleasePlanSummary ReleasePlanSummaryMap.t;
}

(*
   The ownership action emitted after initializing one typed raw slot. ARM64
   resolves this from nominal registries during function preparation so the
   finalized function no longer depends on a whole-program registry context.
*)
type arm64SlotInitRootRetainTarget =
  | SlotInitListRootRetain
  | SlotInitDictRootRetain
  | SlotInitDynamicBufferRetain
  | SlotInitClosureRootRetain
  | SlotInitGenericRootRetain of int

(*
   Compact, register-independent facts needed while assembling native code.
   These are computed once from finalized symbolic LIR, then travel with the
   function through register allocation and tree shaking. Backends therefore
   only combine facts from the functions that survived reachability instead of
   rescanning every instruction in every executable.
   ARM64 float registers assigned by the allocator that the function must preserve.
   Deduplicated by kind and compact plan identity. Keeping recursive
   metadata out of the ordered key prevents deep structural comparisons.
   Some after ARM64 preparation. Values are None for slot types that do
   not need a root retain; the outer option distinguishes an empty plan
   from legacy/unprepared LIR.
   The function can terminate through a runtime error or allocation
   failure and therefore needs the backend's shared error routine.
*)
type functionCodegenFacts = {
  arm64UsedCalleeSavedF : physFPReg list;
  closurePayloadSizeFromParams : int option;
  closureCaptureTypes : AST.semanticType list option;
  closurePayloadSizesFromAllocs : (AST.functionId * int) list;
  recursiveReleaseTypes : MemoryPlanning.SemanticTypeSet.t;
  refCountDecRequirements :
    MemoryModel.rcMetadata option RefCountDecRequirementMap.t;
  refCountIncRequirements : RcKindSet.t;
  rawSlotInitTypes : MemoryPlanning.SemanticTypeSet.t;
  arm64RawSlotInitRetainTargets :
    arm64SlotInitRootRetainTarget option SemanticTypeMap.t option;
  needsCliRuntimeState : bool;
  needsCliArgvHelper : bool;
  needsCliExecuteHelper : bool;
  needsCliRunProcessHelper : bool;
  needsCliProcessLifecycleHelpers : bool;
  needsRuntimeErrorHelper : bool;
  arm64RcHelperRequirements : arm64RcHelperRequirements option;
  arm64GenericHelperIds : AST.functionId StringOrder.Map.t;
}

type functionDef = {
  id : AST.functionId;
  name : string;
  typedParams : typedLIRParam list;
  cfg : cfg;
  stackSize : int;
  usedCalleeSaved : physReg list;
  codegenFacts : functionCodegenFacts option;
}

(*
   Record definitions needed by late, type-specialized runtime helpers.
*)
type recordRegistry = (string * AST.semanticType) list StringOrder.Map.t

(*
   Info about a single sum variant needed by late, type-specialized runtime helpers.
*)
type variantInfo = {
  name : string;
  tag : int;
  payload : AST.semanticType option;
  fieldCount : int;
}

(*
   All variants for a sum type, with type parameters.
*)
type typeVariants = { typeParams : string list; variants : variantInfo list }

(*
   Sum definitions needed by late, type-specialized runtime helpers.
*)
type variantRegistry = typeVariants StringOrder.Map.t
type program = Program of functionDef list * variantRegistry * recordRegistry

let labelText (Label value) =
  StructuralFormat.format
    (StructuralFormat.Union ("Label", [ StructuralFormat.Text value ]))

(*
   Arrange blocks into deterministic successor chains so the backends can use
   the lexical successor as a fallthrough edge. Entry remains first; each
   remaining chain starts in label order, and every block occurs exactly once.
   A single shared return is deferred to the end to fall into the epilogue.
   Count predecessor blocks, not edges: a same-target conditional
   alone does not make a return shared.
   Layout orders the blocks it is given; malformed successor
   references remain the consumer's responsibility so layout
   does not mask a more specific backend diagnostic.
*)
let layoutBlocks (cfg : cfg) =
  let commonReturn =
    let returns =
      LabelMap.bindings cfg.blocks
      |> List.filter_map (fun (label, block) ->
          if block.terminator = Ret then Some label else None)
    in
    match returns with
    | [ label ] when label <> cfg.entry ->
        let predecessors =
          LabelMap.bindings cfg.blocks
          |> List.filter (fun (_, block) ->
              match block.terminator with
              | Ret -> false
              | Jump target -> target = label
              | Branch (_, yes, no)
              | BranchZero (_, yes, no)
              | BranchBitZero (_, _, yes, no)
              | BranchBitNonZero (_, _, yes, no)
              | CondBranch (_, yes, no) ->
                  yes = label || no = label)
        in
        if List.length predecessors >= 2 then Some label else None
    | _ -> None
  in
  let preferredSuccessor = function
    | Ret -> None
    | Jump target -> Some target
    | Branch (_, _, target)
    | BranchZero (_, _, target)
    | BranchBitZero (_, _, _, target)
    | BranchBitNonZero (_, _, _, target)
    | CondBranch (_, _, target) ->
        Some target
  in
  let rec followChain visited reversedBlocks label =
    if LabelSet.mem label visited then Ok (visited, reversedBlocks)
    else
      match LabelMap.find_opt label cfg.blocks with
      | None ->
          Error ("LIR layout: CFG references missing block " ^ labelText label)
      | Some block -> (
          let visited = LabelSet.add label visited in
          let reversedBlocks = block :: reversedBlocks in
          match preferredSuccessor block.terminator with
          | Some successor
            when (not (LabelSet.mem successor visited))
                 && Some successor <> commonReturn
                 && LabelMap.mem successor cfg.blocks ->
              followChain visited reversedBlocks successor
          | _ -> Ok (visited, reversedBlocks))
  in
  match LabelMap.find_opt cfg.entry cfg.blocks with
  | None -> Error ("LIR layout: CFG missing entry block " ^ labelText cfg.entry)
  | Some _ ->
      Result.bind (followChain LabelSet.empty [] cfg.entry) (fun initial ->
          let ordinary, deferred =
            List.partition
              (fun label -> Some label <> commonReturn)
              (List.map fst (LabelMap.bindings cfg.blocks))
          in
          List.fold_left
            (fun state label ->
              Result.bind state (fun (visited, reversed) ->
                  if LabelSet.mem label visited then Ok (visited, reversed)
                  else followChain visited reversed label))
            (Ok initial) (ordinary @ deferred)
          |> Result.map (fun (_, reversed) -> List.rev reversed))

let rcReleasePlanMemoKey (metadata : MemoryModel.rcMetadata option) =
  match
    Option.bind metadata (fun value -> value.MemoryModel.releasePlanCacheKey)
  with
  | Some key -> FingerprintedReleasePlan key
  | None ->
      StructuralReleasePlan
        (Option.bind metadata (fun value -> value.MemoryModel.releasePlan))

let wrappedMultiply a b =
  Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

let wrappedAdd a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))

(*
   Derive the compact code-generation facts for a single function. Keeping
   this public also gives backend tests a slow-path oracle for carried facts.
*)
let analyzeFunctionCodegenFacts (func : functionDef) =
  let closurePayloadSizeFromParams, closureCaptureTypes =
    match func.typedParams with
    | { typ = AST.TTuple fields; _ } :: _ ->
        ( Some (wrappedMultiply (List.length fields) 8),
          match fields with _ :: captures -> Some captures | [] -> None )
    | _ -> (None, None)
  in
  let closurePayloadSizesFromAllocsRev = ref [] in
  let recursiveReleaseTypes = ref MemoryPlanning.SemanticTypeSet.empty in
  let refCountDecRequirements = ref RefCountDecRequirementMap.empty in
  let refCountIncRequirements = ref RcKindSet.empty in
  let rawSlotInitTypes = ref MemoryPlanning.SemanticTypeSet.empty in
  let needsCliRuntimeState = ref false in
  let needsCliArgvHelper = ref false in
  let needsCliExecuteHelper = ref false in
  let needsCliRunProcessHelper = ref false in
  let needsCliProcessLifecycleHelpers = ref false in
  LabelMap.iter
    (fun _ block ->
      List.iter
        (function
          | ClosureAlloc (_, fn, captures) ->
              closurePayloadSizesFromAllocsRev :=
                (fn, wrappedMultiply (wrappedAdd (List.length captures) 1) 8)
                :: !closurePayloadSizesFromAllocsRev
          | RefCountDec (_, _, kind, metadata) ->
              refCountDecRequirements :=
                RefCountDecRequirementMap.add
                  (kind, rcReleasePlanMemoKey metadata)
                  metadata !refCountDecRequirements;
              recursiveReleaseTypes :=
                MemoryPlanning.SemanticTypeSet.union !recursiveReleaseTypes
                  (match
                     Option.bind metadata (fun metadata ->
                         metadata.MemoryModel.releasePlan)
                   with
                  | None -> MemoryPlanning.SemanticTypeSet.empty
                  | Some plan -> MemoryPlanning.recursiveReleaseTypes plan)
          | RefCountInc (_, _, kind, _) -> (
              match kind with
              | TaggedList | DictHeap | ClosureHeap ->
                  refCountIncRequirements :=
                    RcKindSet.add kind !refCountIncRequirements
              | GenericHeap | StreamHeap -> ())
          | RawSlotInit (_, _, _, typ) ->
              rawSlotInitTypes :=
                MemoryPlanning.SemanticTypeSet.add typ !rawSlotInitTypes
          | CliNative (_, operation, _) ->
              needsCliRuntimeState := true;
              if operation = GetArgv then needsCliArgvHelper := true;
              if operation = Execute then needsCliExecuteHelper := true;
              if operation = RunProcess then needsCliRunProcessHelper := true;
              if
                operation = SpawnProcess || operation = ProcessIO
                || operation = TerminateProcess
              then needsCliProcessLifecycleHelpers := true
          | _ -> ())
        block.instrs)
    func.cfg.blocks;
  {
    arm64UsedCalleeSavedF = [];
    closurePayloadSizeFromParams;
    closureCaptureTypes;
    closurePayloadSizesFromAllocs = List.rev !closurePayloadSizesFromAllocsRev;
    recursiveReleaseTypes = !recursiveReleaseTypes;
    refCountDecRequirements = !refCountDecRequirements;
    refCountIncRequirements = !refCountIncRequirements;
    rawSlotInitTypes = !rawSlotInitTypes;
    arm64RawSlotInitRetainTargets = None;
    needsCliRuntimeState = !needsCliRuntimeState;
    needsCliArgvHelper = !needsCliArgvHelper;
    needsCliExecuteHelper = !needsCliExecuteHelper;
    needsCliRunProcessHelper = !needsCliRunProcessHelper;
    needsCliProcessLifecycleHelpers = !needsCliProcessLifecycleHelpers;
    needsRuntimeErrorHelper =
      LabelMap.exists
        (fun _ block ->
          List.exists
            (function
              | RuntimeError _ | RuntimeErrorString _ | HeapAlloc _ | RawAlloc _
              | MappedAlloc _ | MappedFree _ ->
                  true
              | _ -> false)
            block.instrs)
        func.cfg.blocks;
    arm64RcHelperRequirements = None;
    arm64GenericHelperIds = StringOrder.Map.empty;
  }

let attachFunctionCodegenFacts (func : functionDef) =
  { func with codegenFacts = Some (analyzeFunctionCodegenFacts func) }

let attachCodegenFacts (Program (functions, variants, records)) =
  Program (List.map attachFunctionCodegenFacts functions, variants, records)

(*
   Count the number of CoverageHit instructions in a program
*)
let countCoverageHits (Program (functions, _, _)) =
  let instructions =
    List.concat_map
      (fun (func : functionDef) ->
        LabelMap.bindings func.cfg.blocks
        |> List.concat_map (fun (_, block) -> block.instrs))
      functions
  in
  List.length
    (List.filter (function CoverageHit _ -> true | _ -> false) instructions)
