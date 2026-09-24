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

let internal buildLambdaLiftFunctionCatalog
    (registries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (returnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : LiftFunctions.FunctionCatalog =
    let parameters =
        registries.FuncParams
        |> Map.toSeq
        |> Seq.map (fun (name, parameters) ->
            AST.functionIdForName name, parameters |> List.map snd)
        |> Map.ofSeq
        |> fun parameters ->
            registries.ModuleRegistry
            |> Map.fold (fun current name moduleFunc ->
                Map.add (AST.functionIdForName name) moduleFunc.ParamTypes current) parameters
        |> fun parameters ->
            baseFuncNames
            |> Set.fold (fun current name ->
                let id = AST.functionIdForName name
                if Map.containsKey id current then current else Map.add id [] current) parameters
    let genericDefs =
        registries.ModuleRegistry
        |> Map.toSeq
        |> Seq.choose (fun (name, moduleFunc) ->
            if List.isEmpty moduleFunc.TypeParams then None
            else
                Some (
                    AST.functionIdForName name,
                    (moduleFunc.TypeParams, moduleFunc.ReturnType)
                ))
        |> Map.ofSeq
    {
        Params = parameters
        ReturnTypes =
            registries.ModuleRegistry
            |> Map.fold (fun current name moduleFunc ->
                Map.add (AST.functionIdForName name) moduleFunc.ReturnType current)
                (returnTypes |> Map.map (fun _ (_, returnType) -> returnType))
        GenericDefs = genericDefs
    }

let internal mergeReturnTypes
    (baseReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    (overlayReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : Map<AST.FunctionId, string * AST.SemanticType> =
    Map.fold (fun acc k v -> Map.add k v acc) baseReturnTypes overlayReturnTypes

let internal packageCatalogFunctionNames =
    Set.ofList [
        "Builtin.pmFindValuesByValueType"
        "Builtin.pmGetLocationsByValue"
        "Builtin.pmEvaluateValue"
    ]

/// Generic functions whose call graph can reach a package-catalog intrinsic.
/// This lets ordinary programs skip catalog specialization without changing
/// the behavior of generic wrappers around the catalog API.
let private buildPackageCatalogGenericCallers
    (genericFuncDefs: SpecializationIdentity.GenericFuncDefs)
    : Set<string> =
    let callsByFunction =
        genericFuncDefs
        |> Map.toList
        |> List.map (fun (name, definition) ->
            name,
            definition.DirectDependencies
            |> Set.toList
            |> List.choose (fun id -> CheckedAST.functionName id definition.Symbols)
            |> Set.ofList)
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
    Type: AST.SemanticType
    Body: CheckedAST.Expr
}

let internal checkedValueArtifacts (program: CheckedAST.Program) : Map<string, CheckedValueArtifact> =
    let symbols = CheckedAST.programSymbols program
    CheckedAST.programValues program
    |> Map.map (fun _ (typ, body) ->
        let artifactSymbols = CheckedAST.catalogForCheckedUnit symbols
        { Symbols = artifactSymbols; Type = typ; Body = body })

type PipelineContext = {
    Symbols: CheckedAST.Symbols
    Target: Platform.Target
    TypeCheckEnv: CheckingTypes.TypeCheckEnv
    CheckedValues: Map<string, CheckedValueArtifact>
    GenericFuncDefs: SpecializationIdentity.GenericFuncDefs
    SpecRegistry: SpecializationIdentity.SpecRegistry
    Registries: AST_to_ANF.Registries
    BaseFuncNames: Set<string>
    LambdaLiftFunctions: LiftFunctions.FunctionCatalog
    LambdaLiftTypeReg: TypeRegistries.TypeRegistry
    LambdaLiftVariantLookup: LoweringPrimitives.VariantLookup
    ProjectedMirRegistries: MIR.VariantRegistry * MIR.RecordRegistry
    ReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>
    PackageCatalogGenericCallers: Set<string>
}

let internal buildContext
    (target: Platform.Target)
    (symbols: CheckedAST.Symbols)
    (typeCheckEnv: CheckingTypes.TypeCheckEnv)
    (checkedValues: Map<string, CheckedValueArtifact>)
    (genericFuncDefs: SpecializationIdentity.GenericFuncDefs)
    (specRegistry: SpecializationIdentity.SpecRegistry)
    (registries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (returnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : PipelineContext =
    let (lambdaLiftTypeReg, lambdaLiftVariantLookup) =
        LiftFunctions.prepareLambdaLiftBaseTypes
            registries.TypeReg
            registries.VariantLookup
    {
        Symbols = symbols
        Target = target
        TypeCheckEnv = typeCheckEnv
        CheckedValues = checkedValues
        GenericFuncDefs = genericFuncDefs
        SpecRegistry = specRegistry
        Registries = registries
        BaseFuncNames = baseFuncNames
        LambdaLiftFunctions =
            buildLambdaLiftFunctionCatalog registries baseFuncNames returnTypes
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
    /// Direct-call summary computed once with the reusable preamble unit.
    SymbolicCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>
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
    AST: AST.ParsedProgram
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
    ResultType: AST.SemanticType
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
