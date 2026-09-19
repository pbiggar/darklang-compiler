// GenericReferenceCounts.fs - Outline reusable fixed-layout destruction helpers.

module ARM64GenericReferenceCounts

open ARM64CodeGenTypes
open ARM64ListReferenceCounts
open ARM64Instructions

let internal generatePlannedGenericRefCountDecHelper
    (helperLabel: string)
    (spec: LIR.Arm64PlannedGenericDecHelper)
    (ctx: CodeGenContext)
    : ARM64Symbolic.Instr list =
    // The normal generic-release lowering remains the single source of truth.
    // Give borrowed helpers a unique Stdlib-shaped function identity so the
    // existing single-payload sum ownership rule is preserved exactly.
    let helperFunctionName =
        if spec.OwnsSinglePayloadSum then helperLabel
        else $"Darklang.Stdlib.{helperLabel}"
    let helperCtx = {
        ctx with
            FunctionName = helperFunctionName
            InstructionSite = "root"
    }
    let metadata = {
        MemoryModel.ReleasePlanCacheKey = None
        MemoryModel.ReleasePlan = Some spec.ReleasePlan
        MemoryModel.SourceType = None
    }

    match convertInstr
              helperCtx
              (LIR.RefCountDec (
                  LIR.Physical LIR.X0,
                  spec.PayloadSize,
                  LIR.GenericHeap,
                  Some metadata)) with
    | Ok body ->
        [
            ARM64Symbolic.Label helperLabel
            // The body may call nested release helpers. Preserve the root and
            // our caller's link register until the complete plan has finished.
            ARM64Symbolic.STP_pre (
                ARM64Symbolic.X0,
                ARM64Symbolic.X30,
                ARM64Symbolic.SP,
                -16s)
        ]
        @ body
        @ [
            ARM64Symbolic.LDP_post (
                ARM64Symbolic.X0,
                ARM64Symbolic.X30,
                ARM64Symbolic.SP,
                16s)
            ARM64Symbolic.RET
        ]
    | Error error ->
        Crash.crash $"ARM64 generic release helper generation failed for {helperLabel}: {error}"

/// The compilation-session function cache also stores immutable generic
/// release helpers. Their reserved stable label fully identifies the planned
/// body; the cache separately keys target and codegen options.
let internal plannedGenericRefCountDecHelperCacheKey
    (helperLabel: string)
    : LIR.Function =
    let entry = LIR.Label "cache_entry"
    let block : LIR.BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = LIR.Ret
    }
    {
        Id = AST.functionIdForName helperLabel
        Name = helperLabel
        TypedParams = []
        CFG = {
            Entry = entry
            Blocks = Map.ofList [entry, block]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

let isPlannedGenericRefCountDecHelperCacheKey
    (func: LIR.Function)
    : bool =
    Option.isNone func.CodegenFacts
    && func.Name.StartsWith(plannedGenericRefCountDecHelperLabelPrefix)
