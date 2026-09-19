// Contexts.fs - Define stdlib, preamble, and user-compilation interfaces.

module CompilationContexts

open ARM64CodeGenTypes
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession

let internal buildBaseFuncNames
    (registries: AST_to_ANF.Registries)
    : Set<string> =
    registries.FuncParams
    |> Map.fold (fun acc name _ -> Set.add name acc) Set.empty

let internal reserveBaseFunctionParams
    (funcParams: Map<string, (string * AST.Type) list>)
    (baseFuncNames: Set<string>)
    : Map<string, (string * AST.Type) list> =
    baseFuncNames
    |> Set.fold (fun acc name ->
        if Map.containsKey name acc then acc else Map.add name [] acc) funcParams

let internal mergeReturnTypes
    (baseReturnTypes: Map<string, AST.Type>)
    (overlayReturnTypes: Map<string, AST.Type>)
    : Map<string, AST.Type> =
    Map.fold (fun acc k v -> Map.add k v acc) baseReturnTypes overlayReturnTypes

let internal packageCatalogFunctionNames =
    Set.ofList [
        "Builtin.pmFindValuesByValueType"
        "Builtin.pmGetLocationsByValue"
        "Builtin.pmEvaluateValue"
    ] |> Set.map AST.functionIdForName

/// Generic functions whose call graph can reach a package-catalog intrinsic.
/// This lets ordinary programs skip catalog specialization without changing
/// the behavior of generic wrappers around the catalog API.
let private buildPackageCatalogGenericCallers
    (genericFuncDefs: SpecializationIdentity.GenericFuncDefs)
    : Set<AST.FunctionId> =
    let callsByFunction =
        genericFuncDefs
        |> Map.toList
        |> List.map (fun (name, definition) ->
            AST.functionIdForName name,
            Monomorphization.collectCalledFunctions definition.Function.Body)
        |> Map.ofList
    let rec findFixedPoint callers =
        let targets = Set.union packageCatalogFunctionNames callers
        let next =
            callsByFunction
            |> Map.fold (fun found name calls ->
                if calls |> Set.exists (fun called -> Set.contains called targets) then
                    Set.add name found
                else
                    found) callers
        if Set.count next = Set.count callers then callers
        else findFixedPoint next
    findFixedPoint Set.empty

/// Shared compilation context used across pipeline steps
type CheckedValueArtifact = {
    Symbols: CheckedAST.Symbols
    Type: AST.Type
    Body: CheckedAST.Expr
}

let internal checkedValueArtifacts (program: CheckedAST.Program) : Map<string, CheckedValueArtifact> =
    let symbols = CheckedAST.programSymbols program
    CheckedAST.programValues program
    |> Map.map (fun _ (typ, body) -> { Symbols = symbols; Type = typ; Body = body })

type PipelineContext = {
    Target: Platform.Target
    TypeCheckEnv: CheckingTypes.TypeCheckEnv
    CheckedValues: Map<string, CheckedValueArtifact>
    GenericFuncDefs: SpecializationIdentity.GenericFuncDefs
    SpecRegistry: SpecializationIdentity.SpecRegistry
    Registries: AST_to_ANF.Registries
    BaseFuncNames: Set<string>
    LambdaLiftFuncParams: Map<string, (string * AST.Type) list>
    LambdaLiftTypeReg: TypeRegistries.TypeRegistry
    LambdaLiftVariantLookup: LoweringPrimitives.VariantLookup
    ProjectedMirRegistries: MIR.VariantRegistry * MIR.RecordRegistry
    ReturnTypes: Map<string, AST.Type>
    PackageCatalogGenericCallers: Set<AST.FunctionId>
}

let internal buildContext
    (target: Platform.Target)
    (typeCheckEnv: CheckingTypes.TypeCheckEnv)
    (checkedValues: Map<string, CheckedValueArtifact>)
    (genericFuncDefs: SpecializationIdentity.GenericFuncDefs)
    (specRegistry: SpecializationIdentity.SpecRegistry)
    (registries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (returnTypes: Map<string, AST.Type>)
    : PipelineContext =
    let (lambdaLiftTypeReg, lambdaLiftVariantLookup) =
        LiftFunctions.prepareLambdaLiftBaseTypes
            registries.TypeReg
            registries.VariantLookup
    {
        Target = target
        TypeCheckEnv = typeCheckEnv
        CheckedValues = checkedValues
        GenericFuncDefs = genericFuncDefs
        SpecRegistry = specRegistry
        Registries = registries
        BaseFuncNames = baseFuncNames
        LambdaLiftFuncParams =
            reserveBaseFunctionParams registries.FuncParams baseFuncNames
        LambdaLiftTypeReg = lambdaLiftTypeReg
        LambdaLiftVariantLookup = lambdaLiftVariantLookup
        ProjectedMirRegistries =
            (ANF_to_MIR.buildVariantRegistry registries.VariantLookup,
             ANF_to_MIR.buildRecordRegistry registries.RecordFieldsReg)
        ReturnTypes = returnTypes
        PackageCatalogGenericCallers =
            buildPackageCatalogGenericCallers genericFuncDefs
    }

/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended compilation context (stdlib + preamble)
    Context: PipelineContext
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIR.Function list
}

/// Parsed and typechecked preamble analysis for suite-level specialization
type PreambleAnalysis = {
    TypedAST: CheckedAST.Program
    TypeCheckEnv: CheckingTypes.TypeCheckEnv
    GenericFuncDefs: SpecializationIdentity.GenericFuncDefs
}

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: CheckedAST.Program
    /// Shared compilation context (typecheck env + registries)
    Context: PipelineContext
    /// Pre-allocated stdlib functions (physical registers assigned, ready for merge)
    AllocatedFunctions: LIR.Function list
    /// Call graph for dead code elimination (which stdlib funcs call which other funcs)
    StdlibCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>
    /// Stdlib ANF functions indexed by name (for coverage analysis)
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Pre-reference-count bodies available to optimizations that introduce
    /// calls to already-monomorphized stdlib helpers.
    StdlibANFOptimizationCandidates: Map<string, ANF.Function>
    /// Pre-reference-count stdlib ANF functions available as user inlining candidates
    StdlibInlineCandidates: Map<AST.FunctionId, ANF_Inlining.FunctionInfo>
    /// Call graph at ANF level (for coverage analysis reachability)
    StdlibANFCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>
    /// TypeMap from RC insertion (needed for getReachableStdlibFunctions)
    StdlibTypeMap: ANF.TypeMap
}

/// Context for compiling user code
type CompileContext =
    | StdlibOnly of StdlibResult
    | StdlibWithPreamble of StdlibResult * PreambleContext

/// Recursive custom-type identity retained only at the immutable package-value
/// catalog boundary. Runtime type arguments use the same exact nested custom
/// identities as ValueSearch's ValueType query.
type PackageCustomType = {
    Hash: string
    TypeArguments: PackageCustomType list
}

/// A branch-visible package location. Input order is the interpreter package
/// manager's branch-prioritized order and remains observable during selection.
type CatalogPackageLocation = {
    VisibleInBranches: string list
    Owner: string
    Modules: string list
    Name: string
}

/// Evaluation availability is explicit; missing and failed package evaluation
/// both become None at the public primitive, but are distinct catalog states.
type PackageValueEvaluatorState =
    | Available of AST.Expr
    | Unavailable
    | EvaluationFailure

/// The evaluator's concrete result type is checked before its expression can
/// cross into a monomorphized ValueSearch caller.
type TypedPackageValueEvaluator = {
    ResultType: AST.Type
    State: PackageValueEvaluatorState
}

type PackageValueCatalogEntry = {
    ValueHash: string
    RuntimeType: PackageCustomType
    Locations: CatalogPackageLocation list
    Evaluator: TypedPackageValueEvaluator
}

/// Explicit AOT package snapshot. Unlike the interpreter package manager this
/// value is immutable and contains no database or live branch traversal.
type PackageValueCatalog = PackageValueCatalog of PackageValueCatalogEntry list

let emptyPackageValueCatalog : PackageValueCatalog = PackageValueCatalog []

/// One independently parsed source unit. Ordering is caller-owned and is
/// preserved when declaration overlays are composed.
type SourceUnit = {
    Name: string
    Purpose: NameSyntax.SourceUnitPurpose
    Source: string
}

/// Request for compiling source code
type CompileRequest = {
    Context: CompileContext
    Mode: CompileMode
    Sources: AST.NonEmptyList<SourceUnit>
    AllowInternal: bool
    Verbosity: int
    Options: CompilerOptions
    PackageValues: PackageValueCatalog
    /// Hosted ProgramTypes package resolver. None explicitly disables package loading.
    PackageManager: PackageManager.Config option
    PassTimingRecorder: PassTimingRecorder option
    /// Optional caller-owned bounded reuse scope.
    Session: CompilationSession option
}
