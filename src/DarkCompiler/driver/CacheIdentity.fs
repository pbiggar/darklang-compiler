// CacheIdentity.fs - Define stable dependency and native-code cache identities.

module CompilationCacheIdentity

/// The finalized immutable LIR object is the version token. A function cache
/// hit retains that object, while a changed body produces another one. The
/// target and options complete the identity without hashing a whole LIR body
/// on every session-cache lookup.
[<Sealed>]
type FunctionVersion
    (unitName: string,
     functionId: AST.FunctionId,
     target: Platform.Target,
     options: CompilerOptions.CompilerOptions,
     body: LIR.Function) =
    member _.Unit = unitName
    member _.Function = functionId
    member _.Target = target
    member _.Options = options
    member _.Body = body

    override _.Equals(other: obj) =
        match other with
        | :? FunctionVersion as version ->
            unitName = version.Unit
            && functionId = version.Function
            && target = version.Target
            && options = version.Options
            && System.Object.ReferenceEquals(body, version.Body)
        | _ -> false

    override _.GetHashCode() =
        System.HashCode.Combine(
            unitName,
            functionId,
            target,
            options,
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(body))

/// The catalog indexes by canonical ID for direct-call lookup, while Version
/// retains the exact producer identity. None marks a collision or ambiguity.
type FunctionSummary = {
    Version: FunctionVersion option
    Purity: MIROptimizationFacts.PuritySummary
    ConstantReturn: (AST.SemanticType * MIR.Operand) option
    Arm64Writes: ARM64CalleeClobbers.Writes option
    X64Writes: X64CalleeClobbers.Writes option
}

/// Cache reuse depends on the facts a pass can observe. A new producer body
/// with identical facts leaves its callers' optimized output unchanged.
type FunctionSummaryFacts = {
    Purity: MIROptimizationFacts.PuritySummary
    ConstantReturn: (AST.SemanticType * MIR.Operand) option
    Arm64Writes: ARM64CalleeClobbers.Writes option
    X64Writes: X64CalleeClobbers.Writes option
}

let summaryFacts (summary: FunctionSummary) = {
    Purity = summary.Purity
    ConstantReturn = summary.ConstantReturn
    Arm64Writes = summary.Arm64Writes
    X64Writes = summary.X64Writes
}

/// An explicit pessimistic result for a direct callee whose body is unavailable
/// or whose canonical ID resolves to more than one body.
let unknownSummary = {
    Version = None
    Purity = {
        ObservableEffects = true
        ReadsMutableState = true
        MayTrap = true
        MayDiverge = true
    }
    ConstantReturn = None
    Arm64Writes = None
    X64Writes = None
}

let mergeFunctionSummaries left right =
    right
    |> FunctionIdMap.fold (fun summaries id summary ->
        match FunctionIdMap.tryFind id summaries with
        | None -> FunctionIdMap.add id summary summaries
        | Some existing when existing = summary && Option.isSome summary.Version -> summaries
        | Some _ -> FunctionIdMap.add id unknownSummary summaries) left

open ARM64CodeGenTypes
open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions

type internal LirFunctionReferenceComparer() =
    interface IEqualityComparer<LIR.Function> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(func) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func)

[<NoComparison>]
type internal AllocatedLirFunctionKey = {
    Arch: Platform.Arch
    Function: LIR.Function
}

type internal AllocatedLirFunctionKeyNameHashComparer() =
    interface IEqualityComparer<AllocatedLirFunctionKey> with
        member _.Equals(left, right) =
            left.Arch = right.Arch && left.Function = right.Function
        member _.GetHashCode(key) =
            StringComparer.Ordinal.GetHashCode(key.Function.Name)

type internal ObjectReferenceComparer() =
    interface IEqualityComparer<obj> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(value) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(value)

[<NoComparison>]
type internal AnfDependencyKey = {
    Functions: CheckedAST.FunctionDef list
    LocalRegistries: AST_to_ANF.Registries
    NonInlineableFunctionNames: Set<AST.FunctionId>
}

type internal AnfDependencyKeyNameHashComparer() =
    interface IEqualityComparer<AnfDependencyKey> with
        member _.Equals(left, right) = left = right
        member _.GetHashCode(key) =
            let addId hash functionId =
                (hash * 397) ^^^ LanguagePrimitives.GenericHash functionId
            let functionHash =
                key.Functions
                |> List.fold (fun current func -> addId current func.Id) 17
            key.NonInlineableFunctionNames
            |> Set.fold addId functionHash

[<NoComparison>]
type internal CompiledDependencyConfig = {
    Target: Platform.Target
    Options: CompilerOptions
    NonInlineableFunctionNames: Set<AST.FunctionId>
    KnownSummaries: FunctionIdMap<FunctionSummaryFacts>
}

[<NoComparison>]
type MirOptimizationKey = {
    Function: MIR.Function
    Options: MIROptimizationFacts.OptimizeOptions
    EffectFreeCalls: Set<AST.FunctionId>
}

type internal MirOptimizationKeyNameHashComparer() =
    interface IEqualityComparer<MirOptimizationKey> with
        member _.Equals(left, right) =
            left.Function = right.Function
            && left.Options = right.Options
            && left.EffectFreeCalls = right.EffectFreeCalls
        member _.GetHashCode(key) =
            StringComparer.Ordinal.GetHashCode(key.Function.Name)

type internal MirOptimizationCache =
    MirOptimizationKey -> (unit -> MIR.Function) -> MIR.Function

type internal AllocatedLirFunctionCache =
    Platform.Arch -> LIR.Function -> (unit -> LIR.Function) -> LIR.Function

type internal CallAwareLirFunctionCache =
    LIR.Function
        -> FunctionIdMap<ARM64CalleeClobbers.Writes>
        -> (unit -> LIR.Function)
        -> LIR.Function

[<NoComparison>]
type internal CallAwareLirFunctionKey = {
    Base: LIR.Function
    Callees: FunctionIdMap<ARM64CalleeClobbers.Writes>
}

type internal CallAwareLirFunctionKeyComparer() =
    interface IEqualityComparer<CallAwareLirFunctionKey> with
        member _.Equals(left, right) =
            Object.ReferenceEquals(left.Base, right.Base)
            && left.Callees = right.Callees
        member _.GetHashCode(key) =
            System.HashCode.Combine(
                System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(key.Base),
                hash key.Callees)

type internal FunctionCompilationCaches = {
    OptimizeMir: MirOptimizationCache
    AllocateLir: AllocatedLirFunctionCache
    AllocateCallAwareLir: CallAwareLirFunctionCache
}

type internal Arm64InstructionChunkReferenceComparer() =
    interface IEqualityComparer<ARM64Symbolic.Instr list> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(instructions) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(instructions)

type internal Arm64InstructionChunkGroupReferenceComparer() =
    interface IEqualityComparer<ARM64Symbolic.Instr list list> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(instructionParts) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(instructionParts)

[<NoComparison>]
type internal Arm64MetadataGroupKey = {
    Functions: LIR.Function list
}

type internal Arm64MetadataGroupKeyComparer() =
    interface IEqualityComparer<Arm64MetadataGroupKey> with
        member _.Equals(left, right) =
            List.length left.Functions = List.length right.Functions
            && List.forall2
                (fun leftFunction rightFunction ->
                    Object.ReferenceEquals(leftFunction, rightFunction))
                left.Functions
                right.Functions
        member _.GetHashCode(key) =
            key.Functions
            |> List.fold
                (fun hash func ->
                    (hash * 397)
                    ^^^ System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func))
                17

[<NoComparison>]
type internal Arm64FunctionGroupKey = {
    Functions: LIR.Function list
    Target: ARM64.TargetConfig
    Options: ARM64CodeGenTypes.CodeGenOptions
}

type internal Arm64FunctionGroupKeyComparer() =
    interface IEqualityComparer<Arm64FunctionGroupKey> with
        member _.Equals(left, right) =
            left.Target = right.Target
            && left.Options = right.Options
            && List.length left.Functions = List.length right.Functions
            && List.forall2
                (fun leftFunction rightFunction ->
                    Object.ReferenceEquals(leftFunction, rightFunction))
                left.Functions
                right.Functions
        member _.GetHashCode(key) =
            let functionHash =
                key.Functions
                |> List.fold
                    (fun hash func ->
                        (hash * 397)
                        ^^^ System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func))
                    17
            System.HashCode.Combine(functionHash, key.Target, key.Options)

[<NoComparison>]
type internal Arm64HelperCacheKey = {
    Target: ARM64.TargetConfig
    Options: ARM64CodeGenTypes.CodeGenOptions
    Helper: CodeGen.HelperCacheKey
}

let private mergeMirRegistryOverlay baseRegistry overlay =
    Map.fold (fun merged name value -> Map.add name value merged) baseRegistry overlay

let internal projectMirRegistryOverlay
    ((baseVariants, baseRecords): MIR.VariantRegistry * MIR.RecordRegistry)
    (localVariantLookup: LoweringPrimitives.VariantLookup)
    (localRecordFields: Map<string, (string * AST.SemanticType) list>)
    : MIR.VariantRegistry * MIR.RecordRegistry =
    let localVariants = ANF_to_MIR.buildVariantRegistry localVariantLookup
    let localRecords = ANF_to_MIR.buildRecordRegistry localRecordFields
    (mergeMirRegistryOverlay baseVariants localVariants,
     mergeMirRegistryOverlay baseRecords localRecords)
