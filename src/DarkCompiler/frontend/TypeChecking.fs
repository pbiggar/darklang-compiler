// TypeChecking.fs - Orchestrate name resolution and checked-program construction.

module TypeChecking

open AST
open CheckingDiagnostics
open CheckingTypes
open ResolveDeclarations
open CheckDeclarations
open CheckResolvedProgram

let private checkProgramInternalWithTrace
    (phaseRecorder: (string -> float -> unit) option)
    (baseEnv: TypeCheckEnv option)
    (hideCompilerImplementationNames: bool)
    (requireExplicitTypeArgsForBareCalls: bool)
    (validateDeclarations: bool)
    (requireEntry: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * Program * TypeCheckEnv, TypeError> =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let measure phase operation =
        match phaseRecorder with
        | None -> operation ()
        | Some record ->
            let start = sw.Elapsed.TotalMilliseconds
            let result = operation ()
            record phase (sw.Elapsed.TotalMilliseconds - start)
            result
    // These declarations exist only to implement retained portable stdlib APIs.
    // They are checked while the stdlib is built in isolation, but must never
    // become candidates while resolving a separately compiled source program.
    let compilerImplementationNames =
        Set.ofList [
            "Darklang.Stdlib.String.__byteAtUnchecked"; "Darklang.Stdlib.String.__toCodepoints"; "Darklang.Stdlib.String.__codepointLength"
            "Darklang.Stdlib.Float.__toBits"; "Darklang.Stdlib.Float.__toInt64Unchecked"
            "Darklang.Stdlib.File.currentDirectory"; "Darklang.Stdlib.File.listDirectoryPacked"; "Darklang.Stdlib.File.readBlob"; "Darklang.Stdlib.File.exists"; "Darklang.Stdlib.File.isDirectory"; "Darklang.Stdlib.File.writeBlob"; "Darklang.Stdlib.File.appendText"; "Darklang.Stdlib.File.delete"; "Darklang.Stdlib.File.createDirectory"; "Darklang.Stdlib.File.setExecutable"; "Darklang.Stdlib.File.writeFromPtr" ]
    let isCompilerImplementationCandidate (candidate: NameResolution.Candidate) =
        match candidate.Provenance with
        | NameResolution.SourceDeclaration name
        | NameResolution.CompilerExtension name -> Set.contains name compilerImplementationNames
        | _ -> false
    let (Program topLevels) = program
    let (declarationValidation, resolutionEnv) =
        measure "TypeCheck: Environment Preparation" (fun () ->
            let declarationValidation =
                if validateDeclarations then
                    validateTopLevelTypeDeclarations baseEnv topLevels
                else
                    // Only synthetic test preambles skip this: they concatenate
                    // declarations that do not coexist in an original source unit.
                    Ok ()
            let moduleRegistry =
                match baseEnv with
                | Some existingEnv -> existingEnv.ModuleRegistry
                | None -> Stdlib.buildModuleRegistry ()
            let localResolutionEnv =
                match baseEnv, topLevels with
                | Some _, [Expression _] -> NameResolution.empty
                | _ ->
                    declarationResolutionEnvironment
                        topLevels
                        moduleRegistry
                        (Option.isNone baseEnv)
            let resolutionEnv =
                match baseEnv with
                | Some existingEnv when hideCompilerImplementationNames ->
                    existingEnv.ResolutionEnv
                    |> NameResolution.filterCandidates (isCompilerImplementationCandidate >> not)
                    |> fun publicBaseEnv -> NameResolution.merge publicBaseEnv localResolutionEnv
                | Some existingEnv -> NameResolution.merge existingEnv.ResolutionEnv localResolutionEnv
                | None -> localResolutionEnv
            (declarationValidation, resolutionEnv))

    let resolvedProgramResult =
        measure "TypeCheck: Name Resolution" (fun () ->
            declarationValidation
            |> Result.bind (fun () ->
                let winningTypeDefs =
                    topLevels
                    |> List.choose (function
                        | TypeDef typeDef -> Some typeDef
                        | _ -> None)
                    |> List.rev
                    |> List.distinctBy typeDefName
                    |> List.rev
                let localAliases =
                    winningTypeDefs
                    |> List.choose (function
                        | TypeAlias (name, typeParams, target) -> Some (name, (typeParams, target))
                        | _ -> None)
                    |> Map.ofList
                let aliases =
                    match baseEnv with
                    | Some existing -> Map.fold (fun acc name value -> Map.add name value acc) existing.AliasReg localAliases
                    | None -> localAliases
                let localRecordTypeNames =
                    winningTypeDefs
                    |> List.choose (function RecordDef (name, _, _) -> Some name | _ -> None)
                    |> Set.ofList
                let recordTypeNames =
                    match baseEnv with
                    | Some existing -> Set.union localRecordTypeNames existing.RecordTypeNames
                    | None -> localRecordTypeNames
                let resolvedNames =
                    measure "TypeCheck: Symbol Resolution" (fun () ->
                        resolveProgramNames resolutionEnv aliases recordTypeNames program)
                resolvedNames
                |> Result.map (fun (Program resolvedTopLevels) ->
                    measure "TypeCheck: Recursive Group Resolution" (fun () ->
                        Program (resolveRecursiveDeclarationGroups resolvedTopLevels)))))

    resolvedProgramResult
    |> Result.bind (fun resolvedProgram ->
        measure "TypeCheck: Semantic Checking" (fun () ->
            match baseEnv, requireEntry, resolvedProgram with
            | Some existingEnv, true, Program [Expression (_, expr)] ->
                checkResolvedExpressionWithBaseEnv
                    existingEnv
                    resolutionEnv
                    requireExplicitTypeArgsForBareCalls
                    warningSettings
                    expr
            | _ ->
                checkResolvedProgramInternal
                    baseEnv
                    requireExplicitTypeArgsForBareCalls
                    warningSettings
                    requireEntry
                    resolvedProgram))

let private checkProgramInternal
    (baseEnv: TypeCheckEnv option)
    (hideCompilerImplementationNames: bool)
    (requireExplicitTypeArgsForBareCalls: bool)
    (validateDeclarations: bool)
    (requireEntry: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * Program * TypeCheckEnv, TypeError> =
    checkProgramInternalWithTrace
        None
        baseEnv
        hideCompilerImplementationNames
        requireExplicitTypeArgsForBareCalls
        validateDeclarations
        requireEntry
        warningSettings
        program

let private constructCheckedProgram
    (typ, program, (env: TypeCheckEnv))
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    CheckedAST.ofTypedProgram env.VariantLookup (env.Values |> Map.keys |> Set.ofSeq) program
    |> Result.map (fun checkedProgram -> (typ, checkedProgram, env))
    |> Result.mapError GenericError

/// Type-check a program
/// Returns the type of the main expression and the transformed program
/// The transformed program has Call nodes converted to TypeApp where type inference was applied
let checkProgram (program: Program) : Result<SemanticType * CheckedAST.Program, TypeError> =
    checkProgramInternal None false false true true AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram
    |> Result.map (fun (typ, prog, _env) -> (typ, prog))

/// Type-check the public source policy without a base environment.
/// Used by focused declaration tests and tools that already parsed an isolated
/// public program.
let checkPublicProgram (program: Program) : Result<SemanticType * CheckedAST.Program, TypeError> =
    checkProgramInternal None false true true true AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram
    |> Result.map (fun (typ, prog, _env) -> (typ, prog))

/// Type-check a program and return the type checking environment
/// Use this when you need to reuse the environment (e.g., for stdlib caching)
let checkProgramWithEnv (program: Program) : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal None false false true true AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram

/// Type-check a declaration-only program without synthesizing an expression.
let checkDeclarationProgramWithEnv (program: Program) : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal None false false true false AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram

/// Type-check a program with a pre-populated base environment (for separate compilation)
/// The program's definitions are merged with the base environment, allowing lookups
/// of types/functions from both the base (e.g., stdlib) and the program (e.g., user code)
let checkProgramWithBaseEnv (baseEnv: TypeCheckEnv) (program: Program) : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false false true true AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram

let checkDeclarationProgramWithBaseEnv
    (baseEnv: TypeCheckEnv)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false false true false AST.defaultWarningSettings program
    |> Result.bind constructCheckedProgram

/// Type-check a program with a pre-populated base environment, generic-call policy override,
/// and warning compatibility settings from the compiler driver.
let checkProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false requireExplicitTypeArgsForBareCalls true true warningSettings program
    |> Result.bind constructCheckedProgram

let checkProgramWithBaseEnvAndSettingsWithTrace
    (phaseRecorder: string -> float -> unit)
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternalWithTrace
        (Some phaseRecorder)
        (Some baseEnv)
        false
        requireExplicitTypeArgsForBareCalls
        true
        true
        warningSettings
        program
    |> Result.bind constructCheckedProgram

let checkDeclarationProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false requireExplicitTypeArgsForBareCalls true false warningSettings program
    |> Result.bind constructCheckedProgram

/// Analyze a synthetic preamble assembled from otherwise independent tests.
/// Such preambles can repeat declarations that never coexist in a source
/// program, so declaration-namespace validation belongs to each original
/// source rather than this harness artifact.
let checkSyntheticPreambleWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false requireExplicitTypeArgsForBareCalls false false warningSettings program
    |> Result.bind constructCheckedProgram

/// Type-check source at the compiler-driver boundary, where implementation-only
/// stdlib names must not be visible to user programs.
let checkPublicProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) true requireExplicitTypeArgsForBareCalls true true warningSettings program
    |> Result.bind constructCheckedProgram

/// Cross the parsed/semantic boundary exactly once, immediately before name
/// resolution. Tests and compiler-generated programs may use the semantic
/// entry points above; source-driven callers use these wrappers.
let checkParsedProgram (program: ParsedProgram) : Result<SemanticType * CheckedAST.Program, TypeError> =
    program |> semanticProgramOfParsed |> checkProgram

let checkParsedPublicProgram (program: ParsedProgram) : Result<SemanticType * CheckedAST.Program, TypeError> =
    program |> semanticProgramOfParsed |> checkPublicProgram

let checkParsedProgramWithEnv
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program |> semanticProgramOfParsed |> checkProgramWithEnv

let checkParsedDeclarationProgramWithEnv
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program |> semanticProgramOfParsed |> checkDeclarationProgramWithEnv

let checkParsedProgramWithBaseEnv
    (baseEnv: TypeCheckEnv)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program |> semanticProgramOfParsed |> checkProgramWithBaseEnv baseEnv

let checkParsedDeclarationProgramWithBaseEnv
    (baseEnv: TypeCheckEnv)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program |> semanticProgramOfParsed |> checkDeclarationProgramWithBaseEnv baseEnv

let checkParsedProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program
    |> semanticProgramOfParsed
    |> checkProgramWithBaseEnvAndSettings baseEnv requireExplicitTypeArgsForBareCalls warningSettings

let checkParsedProgramWithBaseEnvAndSettingsWithTrace
    (phaseRecorder: string -> float -> unit)
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program
    |> semanticProgramOfParsed
    |> checkProgramWithBaseEnvAndSettingsWithTrace
        phaseRecorder
        baseEnv
        requireExplicitTypeArgsForBareCalls
        warningSettings

let checkParsedDeclarationProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program
    |> semanticProgramOfParsed
    |> checkDeclarationProgramWithBaseEnvAndSettings
        baseEnv
        requireExplicitTypeArgsForBareCalls
        warningSettings

let checkParsedSyntheticPreambleWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program
    |> semanticProgramOfParsed
    |> checkSyntheticPreambleWithBaseEnvAndSettings
        baseEnv
        requireExplicitTypeArgsForBareCalls
        warningSettings

let checkParsedPublicProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: ParsedProgram)
    : Result<SemanticType * CheckedAST.Program * TypeCheckEnv, TypeError> =
    program
    |> semanticProgramOfParsed
    |> checkPublicProgramWithBaseEnvAndSettings
        baseEnv
        requireExplicitTypeArgsForBareCalls
        warningSettings
