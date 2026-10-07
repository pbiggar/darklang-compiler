(* ARM64Functions.ml - Lower allocated LIR functions with target frame and return conventions. *)
[@@@warning "-4"]

open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Frames
open ARM64Blocks
open ProcessLifecycle

(*
   Convert LIR function to ARM64 instructions with prologue and epilogue
   Generate epilogue label for this function (passed to convertCFG for Ret terminators)
   Create function-specific context with stack info for tail call epilogue generation
   Convert CFG to ARM64 instructions
   Generate prologue (save FP/LR, allocate stack)
   Generate heap initialization for _start only
   Note: Coverage buffer is in BSS section (zero-initialized by OS)
   No runtime initialization needed - CoverageHit uses ADRP+ADD to access it
   Shared cold path for allocation overflow in this function.
   Generate epilogue (deallocate stack, restore FP/LR, return or exit)
   For _start, flush coverage (if enabled) then exit instead of return
   Remove RET
   Add function entry label (for BL to branch to)
   Terminate _start's frame-pointer chain so CLI helpers can recover
   argv/envp from the original stack without reserving X18.
   Parameter stack stores and parallel register moves are already the first
   instructions in the allocated CFG entry block.
   Keep the epilogue immediately after the CFG so its final Ret terminator
   can fall through. The terminating epilogue makes the overflow trap a
   cold out-of-line block reached only by explicit allocation branches.
*)
let convertFunction heapOverflowTrapBody (ctx : codeGenContext)
    (func : LIR.functionDef) =
  let epilogueLabel = "_epilogue_" ^ func.LIR.name in
  let overflowLabel = heapOverflowLabelPrefix ^ func.LIR.name in
  let needsHeapOverflowTrap =
    LIR.LabelMap.exists
      (fun _ (block : LIR.basicBlock) ->
        List.exists
          (function
            | LIR.HeapAlloc _ | LIR.RawAlloc _ | LIR.MappedAlloc _
            | LIR.MappedFree _ ->
                true
            | _ -> false)
          block.LIR.instrs)
      func.LIR.cfg.LIR.blocks
  in
  let usedCalleeSavedF =
    Option.map
      (fun facts -> facts.LIR.arm64UsedCalleeSavedF)
      func.LIR.codegenFacts
    |> Option.value ~default:[]
  in
  let funcCtx =
    {
      ctx with
      functionName = func.LIR.name;
      rawSlotInitRetainTargets =
        Option.bind func.LIR.codegenFacts (fun facts ->
            facts.LIR.arm64RawSlotInitRetainTargets);
      stackSize = func.LIR.stackSize;
      usedCalleeSaved = func.LIR.usedCalleeSaved;
      usedCalleeSavedF;
      heapOverflowLabel = overflowLabel;
    }
  in
  match convertCFG funcCtx epilogueLabel func.LIR.cfg with
  | Error error -> Error error
  | Ok cfgInstructions ->
      let prologue =
        generatePrologue func.LIR.usedCalleeSaved usedCalleeSavedF
          func.LIR.stackSize
      in
      let heapInit =
        if func.LIR.name = "_start" then generateHeapInit ctx.target else []
      in
      let heapOverflowTrap =
        if needsHeapOverflowTrap then
          generateHeapOverflowTrapBlock heapOverflowTrapBody overflowLabel
        else []
      in
      let epilogueLabelInstructions =
        [ Symbolic.Label ("_epilogue_" ^ func.LIR.name) ]
      in
      let epilogue =
        if func.LIR.name = "_start" then
          let coverageFlush =
            if ctx.options.enableCoverage then
              runtimeInstrs
                (Coverage.generateCoverageFlush ctx.target
                   ctx.options.coverageExprCount)
            else []
          in
          let leakCheckReport = generateLeakCheckReport ctx in
          (generateEpilogue func.LIR.usedCalleeSaved usedCalleeSavedF
             func.LIR.stackSize
          |> List.filter (function Symbolic.RET -> false | _ -> true))
          @ coverageFlush @ leakCheckReport
          @ runtimeInstrs (PrintAndExit.generateExit ctx.target)
        else
          generateEpilogue func.LIR.usedCalleeSaved usedCalleeSavedF
            func.LIR.stackSize
      in
      let functionEntryLabel = [ Symbolic.Label func.LIR.name ] in
      let rootFrameInit =
        if func.LIR.name = "_start" then
          [
            Symbolic.MOVZ (Symbolic.X29, 0, 0);
            Symbolic.MOVZ (Symbolic.X25, 0, 0);
          ]
        else []
      in
      Ok
        (functionEntryLabel @ rootFrameInit @ prologue @ heapInit
       @ cfgInstructions @ epilogueLabelInstructions @ epilogue
       @ heapOverflowTrap)
