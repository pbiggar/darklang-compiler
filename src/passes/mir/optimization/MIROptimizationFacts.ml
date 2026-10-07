(* MIROptimizationFacts.ml - Describe MIR effects and explicit definition/use edges. *)
[@@@warning "-4"]

open MIR
module Functions = SpecializationIdentity.FunctionSet

type optimizeOptions = {
  enableSCCP : bool;
  enableCSE : bool;
  enableDCE : bool;
  enableLICM : bool;
}

let defaultOptimizeOptions =
  { enableSCCP = true; enableCSE = true; enableDCE = true; enableLICM = true }

(*
   Check if an instruction has side effects (must be preserved even if unused)
   These have side effects
   Function calls may have side effects
   Tail calls have side effects
   Indirect tail calls have side effects
   Allocates memory
   Closure tail calls have side effects
   Writes to memory
   File I/O
   Frees memory
   Pure memory read
   Pure memory read (byte)
   Writes to memory (byte)
   Writes to memory and may retain a typed edge
   Pure float operation
   Pure conversion
   Mutates refcount
   Syscall
   Blocking syscall
   Pure conversion (allocates but no visible side effect)
   Must not be eliminated (tracking side effect)
*)
let hasSideEffects (instruction : instr) =
  match instruction with
  | Mov _ -> false
  | BinOp (_, Div, _, _, typ) when typ <> AST.TFloat64 -> true
  | BinOp _ -> false
  | UnaryOp _ -> false
  | Phi _ -> false
  | HeapLoad _ -> false
  | Call _ -> true
  | TailCall _ -> true
  | IndirectCall _ -> true
  | IndirectTailCall _ -> true
  | ClosureAlloc _ -> true
  | ClosureCall _ -> true
  | ClosureTailCall _ -> true
  | HeapAlloc _ -> true
  | HeapStore _ -> true
  | StringConcat _ -> true
  | CanonicalBufferEq _ -> false
  | RefCountInc _ -> true
  | RefCountDec _ -> true
  | Print _ -> true
  | StdoutWrite _ -> true
  | StdinReadLine _ -> true
  | FileReadBlob _ -> true
  | FileExists _ -> true
  | FileWriteBlob _ -> true
  | FileAppendText _ -> true
  | FileDelete _ -> true
  | FileCreateDirectory _ -> true
  | FileSetExecutable _ -> true
  | FileWriteFromPtr _ -> true
  | RawAlloc _ -> true
  | MappedAlloc _ -> true
  | RawFree _ -> true
  | MappedFree _ -> true
  | RawGet _ -> false
  | RawGetByte _ -> false
  | RawWriteWord _ -> true
  | RawWriteByte _ -> true
  | RawSlotInit _ -> true
  | StringToRawPtr _ -> false
  | RawPtrToString _ -> false
  | BlobToRawPtr _ -> false
  | RawPtrToBlob _ -> false
  | DictToRawPtr _ -> false
  | RawPtrToDict _ -> false
  | ListToRawPtr _ -> false
  | RawPtrToList _ -> false
  | FloatSqrt _ -> false
  | FloatAbs _ -> false
  | FloatNeg _ -> false
  | Int64ToFloat _ -> false
  | FloatToInt64 _ -> false
  | FloatToBits _ -> false
  | RefCountIncString _ -> true
  | RefCountDecString _ -> true
  | RefCountIncBlob _ -> true
  | RefCountDecBlob _ -> true
  | RefCountIncInt _ -> true
  | RefCountDecInt _ -> true
  | RandomInt64 _ -> true
  | DateTimeNow _ -> true
  | Sleep _ -> true
  | CliNative _ -> true
  | FloatToString _ -> false
  | RuntimeError _ -> true
  | RuntimeErrorString _ -> true
  | CoverageHit _ -> true

(*
   Find functions whose reachable call-graph components contain no MIR effects.
   Starting from every locally effect-free function and removing callers of
   unproven functions computes the greatest fixed point, so mutually recursive
   and self-recursive components remain provable without assuming unknown calls
   are safe.
*)
let directCallee = function
  | Call (_, id, _, _, _) | TailCall (id, _, _, _) -> Some id
  | _ -> None

type functionEffectSummary = {
  id : AST.functionId;
  locallyEffectFree : bool;
  directCallees : Functions.t;
}

let summarizeFunctionEffects (func : functionDef) =
  let locallyEffectFree, directCallees =
    LabelMap.fold
      (fun _ block summary ->
        List.fold_left
          (fun (free, callees) instruction ->
            match directCallee instruction with
            | Some callee -> (free, Functions.add callee callees)
            | None -> (free && not (hasSideEffects instruction), callees))
          summary block.instrs)
      func.cfg.blocks (true, Functions.empty)
  in
  { id = func.id; locallyEffectFree; directCallees }

let directCallees func = (summarizeFunctionEffects func).directCallees

(*
   The fixed point changes only the proven-name set. MIR and call edges stay
   fixed, so retain each function's scan instead of rebuilding it per round.
*)
let analyzeEffectFreeFunctionsWithKnown known functions =
  let candidates =
    List.filter
      (fun summary -> summary.locallyEffectFree)
      (List.map summarizeFunctionEffects functions)
  in
  let rec converge proven =
    let next =
      List.filter
        (fun summary ->
          Functions.for_all
            (fun callee ->
              Functions.mem callee proven || Functions.mem callee known)
            summary.directCallees)
        candidates
      |> List.map (fun (summary : functionEffectSummary) -> summary.id)
      |> Functions.of_list
    in
    if Functions.equal next proven then next else converge next
  in
  converge
    (Functions.of_list
       (List.map
          (fun (summary : functionEffectSummary) -> summary.id)
          candidates))

let analyzeEffectFreeFunctions functions =
  analyzeEffectFreeFunctionsWithKnown Functions.empty functions

(*
   These fields distinguish a reusable result from a merely effect-free body.
   Unknown callees conservatively have every hazard. The fixed point unions
   hazards, so a previously proven property never becomes true by omission.
*)
type puritySummary = {
  observableEffects : bool;
  readsMutableState : bool;
  mayTrap : bool;
  mayDiverge : bool;
}

let noHazards =
  {
    observableEffects = false;
    readsMutableState = false;
    mayTrap = false;
    mayDiverge = false;
  }

let unknownHazards =
  {
    observableEffects = true;
    readsMutableState = true;
    mayTrap = true;
    mayDiverge = true;
  }

let unknownPurity = unknownHazards

let unionHazards left right =
  {
    observableEffects = left.observableEffects || right.observableEffects;
    readsMutableState = left.readsMutableState || right.readsMutableState;
    mayTrap = left.mayTrap || right.mayTrap;
    mayDiverge = left.mayDiverge || right.mayDiverge;
  }

let isPure summary = summary = noHazards

let hasControlCycle (cfg : cfg) =
  let rec visit active visited label =
    if LabelSet.mem label active then (true, visited)
    else if LabelSet.mem label visited then (false, visited)
    else
      let block =
        match LabelMap.find_opt label cfg.blocks with
        | Some block -> block
        | None -> Crash.crash "MIR purity graph has a missing block"
      in
      let successors =
        match block.terminator with
        | Ret _ -> []
        | Jump next -> [ next ]
        | Branch (_, left, right) -> [ left; right ]
      in
      let active = LabelSet.add label active
      and visited = LabelSet.add label visited in
      List.fold_left
        (fun (cycle, seen) next ->
          if cycle then (cycle, seen) else visit active seen next)
        (false, visited) successors
  in
  fst (visit LabelSet.empty LabelSet.empty cfg.entry)

let localHazards (func : functionDef) =
  let instructionHazards = function
    | Call _ | TailCall _ -> noHazards
    | IndirectCall _ | IndirectTailCall _ | ClosureCall _ | ClosureTailCall _ ->
        unknownHazards
    | HeapLoad _ | RawGet _ | RawGetByte _ | CanonicalBufferEq _ ->
        { noHazards with readsMutableState = true; mayTrap = true }
    | BinOp (_, Div, _, _, typ) when typ <> AST.TFloat64 ->
        { noHazards with mayTrap = true }
    | BinOp (_, Mod, _, _, typ) when typ <> AST.TFloat64 ->
        { noHazards with mayTrap = true }
    | FloatToString _ | StringToRawPtr _ | RawPtrToString _ | BlobToRawPtr _
    | RawPtrToBlob _ | DictToRawPtr _ | RawPtrToDict _ | ListToRawPtr _
    | RawPtrToList _ ->
        unknownHazards
    | instruction when hasSideEffects instruction -> unknownHazards
    | _ -> noHazards
  in
  let local =
    LabelMap.fold
      (fun _ block summary ->
        List.fold_left
          (fun summary instruction ->
            unionHazards summary (instructionHazards instruction))
          summary block.instrs)
      func.cfg.blocks noHazards
  in
  if hasControlCycle func.cfg then { local with mayDiverge = true } else local

let analyzePurityWithKnown known functions =
  let ids =
    Functions.of_list (List.map (fun (func : functionDef) -> func.id) functions)
  in
  let calls =
    FunctionIdMap.ofList
      (List.map
         (fun (func : functionDef) -> (func.id, directCallees func))
         functions)
  in
  let recursiveIds =
    InliningCommon.findSCCs ids calls
    |> List.filter (fun members ->
        Functions.cardinal members > 1
        || Functions.exists
             (fun id ->
               Functions.mem id
                 (Option.value ~default:Functions.empty
                    (FunctionIdMap.tryFind id calls)))
             members)
    |> List.fold_left Functions.union Functions.empty
  in
  let initial =
    FunctionIdMap.ofList
      (List.map
         (fun (func : functionDef) ->
           let hazards = localHazards func in
           ( func.id,
             if Functions.mem func.id recursiveIds then
               { hazards with mayDiverge = true }
             else hazards ))
         functions)
  in
  let rec converge previous =
    let next =
      FunctionIdMap.map
        (fun id local ->
          Functions.fold
            (fun callee hazards ->
              let calleeHazards =
                match FunctionIdMap.tryFind callee previous with
                | Some value -> value
                | None ->
                    Option.value ~default:unknownHazards
                      (FunctionIdMap.tryFind callee known)
              in
              unionHazards hazards calleeHazards)
            (Option.value ~default:Functions.empty
               (FunctionIdMap.tryFind id calls))
            local)
        initial
    in
    if FunctionIdMap.toList next = FunctionIdMap.toList previous then next
    else converge next
  in
  converge initial

(*
   CSE and LICM can move or reuse calls only when the full hazard profile is
   empty. A recursive SCC remains unproven because termination is unknown.
*)
let analyzePureFunctionsWithKnown known functions =
  let known =
    FunctionIdMap.ofList
      (List.map (fun id -> (id, noHazards)) (Functions.elements known))
  in
  analyzePurityWithKnown known functions
  |> FunctionIdMap.toList
  |> List.filter_map (fun (id, summary) ->
      if isPure summary then Some id else None)
  |> Functions.of_list

(*
   Only direct callees can affect optimization of this function. Restricting
   the whole-program result to those names gives a compositional cache key.
*)
let effectFreeCallsForFunction effectFree func =
  Functions.filter (fun id -> Functions.mem id effectFree) (directCallees func)

(*
   Get the destination VReg of an instruction (if any)
   Tail calls don't return here
   Indirect tail calls don't return here
   Closure tail calls don't return here
*)
let getInstrDest (instruction : instr) =
  match instruction with
  | Mov (dest, _, _) -> Some dest
  | BinOp (dest, _, _, _, _) -> Some dest
  | UnaryOp (dest, _, _) -> Some dest
  | Call (dest, _, _, _, _) -> Some dest
  | TailCall _ -> None
  | IndirectCall (dest, _, _, _, _) -> Some dest
  | IndirectTailCall _ -> None
  | ClosureAlloc (dest, _, _) -> Some dest
  | ClosureCall (dest, _, _, _, _) -> Some dest
  | ClosureTailCall _ -> None
  | HeapAlloc (dest, _) -> Some dest
  | HeapLoad (dest, _, _, _) -> Some dest
  | StringConcat (dest, _, _, _) -> Some dest
  | CanonicalBufferEq (dest, _, _, _) -> Some dest
  | StdinReadLine dest -> Some dest
  | FileReadBlob (dest, _) -> Some dest
  | FileExists (dest, _) -> Some dest
  | FileWriteBlob (dest, _, _) -> Some dest
  | FileAppendText (dest, _, _) -> Some dest
  | FileDelete (dest, _) -> Some dest
  | FileCreateDirectory (dest, _) -> Some dest
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
  | RefCountIncInt _ -> None
  | RefCountDecInt _ -> None
  | RandomInt64 dest -> Some dest
  | DateTimeNow dest -> Some dest
  | Sleep (_, dest, _) -> Some dest
  | CliNative (dest, _, _) -> Some dest
  | FloatToString (dest, _) -> Some dest
  | RuntimeError _ -> None
  | RuntimeErrorString _ -> None
  | CoverageHit _ -> None

(*
   Fold over the VRegs used by an instruction without allocating an intermediate collection.
*)
let foldInstrUses folder state (instruction : instr) =
  let operand state = function
    | Register reg -> folder state reg
    | _ -> state
  in
  let operands = List.fold_left operand in
  match instruction with
  | Mov (_, source, _) -> operand state source
  | BinOp (_, _, left, right, _) -> operand (operand state left) right
  | UnaryOp (_, _, source) -> operand state source
  | Call (_, _, args, _, _)
  | TailCall (_, args, _, _)
  | ClosureAlloc (_, _, args) ->
      operands state args
  | IndirectCall (_, func, args, _, _)
  | IndirectTailCall (func, args, _, _)
  | ClosureCall (_, func, args, _, _)
  | ClosureTailCall (func, args, _) ->
      operands (operand state func) args
  | HeapAlloc _ -> state
  | HeapStore (addr, _, source, _) -> operand (folder state addr) source
  | HeapLoad (_, addr, _, _)
  | RefCountInc (addr, _, _, _)
  | RefCountDec (addr, _, _, _) ->
      folder state addr
  | StringConcat (_, first, second, remaining) ->
      operands state (first :: second :: remaining)
  | CanonicalBufferEq (_, _, left, right)
  | FileWriteBlob (_, left, right)
  | FileAppendText (_, left, right)
  | RawGet (_, left, right, _)
  | RawGetByte (_, left, right)
  | RawPtrToDict (_, left, right)
  | RawPtrToList (_, left, right) ->
      operand (operand state left) right
  | Print (source, _)
  | StdoutWrite (_, source, _)
  | FileReadBlob (_, source)
  | FileExists (_, source)
  | FileDelete (_, source)
  | FileCreateDirectory (_, source)
  | FileSetExecutable (_, source)
  | RawAlloc (_, source)
  | MappedAlloc (_, source)
  | RawFree source
  | MappedFree source
  | StringToRawPtr (_, source)
  | RawPtrToString (_, source)
  | BlobToRawPtr (_, source)
  | RawPtrToBlob (_, source)
  | DictToRawPtr (_, source)
  | ListToRawPtr (_, source)
  | FloatSqrt (_, source)
  | FloatAbs (_, source)
  | FloatNeg (_, source)
  | Int64ToFloat (_, source)
  | FloatToInt64 (_, source)
  | FloatToBits (_, source)
  | RefCountIncString source
  | RefCountDecString source
  | RefCountIncBlob source
  | RefCountDecBlob source
  | RefCountIncInt source
  | RefCountDecInt source
  | FloatToString (_, source) ->
      operand state source
  | Sleep (_, _, delay) -> operand state delay
  | FileWriteFromPtr (_, first, second, third)
  | RawWriteWord (first, second, third)
  | RawWriteByte (first, second, third)
  | RawSlotInit (first, second, third, _) ->
      operand (operand (operand state first) second) third
  | Phi (_, sources, _) ->
      List.fold_left
        (fun state (source, _) -> operand state source)
        state sources
  | CliNative (_, _, args) -> operands state args
  | RandomInt64 _ | DateTimeNow _ | StdinReadLine _ | RuntimeError _
  | RuntimeErrorString _ | CoverageHit _ ->
      state

(*
   Get all VRegs used by an instruction.
*)
let getInstrUses instruction =
  foldInstrUses (fun uses reg -> VRegSet.add reg uses) VRegSet.empty instruction

(*
   Fold over the VRegs used by a terminator without allocating an intermediate collection.
*)
let foldTerminatorUses folder state (terminator : terminator) =
  let operand state = function
    | Register reg -> folder state reg
    | _ -> state
  in
  match terminator with
  | Ret source -> operand state source
  | Branch (condition, _, _) -> operand state condition
  | Jump _ -> state

(*
   Get VRegs used by terminator
*)
let getTerminatorUses terminator =
  foldTerminatorUses
    (fun uses reg -> VRegSet.add reg uses)
    VRegSet.empty terminator
