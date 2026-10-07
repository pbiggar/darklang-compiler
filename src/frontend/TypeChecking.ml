(* TypeChecking.fs - Orchestrate name resolution and checked-program construction. *)
open! AST
open! Types
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
let[@warning "-4"] checkProgramInternalWithTrace phaseRecorder baseEnv hideCompilerImplementationNames requireExplicitTypeArgsForBareCalls validateDeclarations requireEntry warningSettings (Program topLevels as program) =
 let measure phase operation = match phaseRecorder with None -> operation () | Some record -> let start = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result = operation () in record phase ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start); result in
 (* These declarations exist only to implement retained portable stdlib APIs.
    They are checked while the stdlib is built in isolation, but must never
    become candidates while resolving a separately compiled source program. *)
 let compilerImplementationNames = S.of_list [
  "Darklang.Stdlib.String.__byteAtUnchecked"; "Darklang.Stdlib.String.__toCodepoints"; "Darklang.Stdlib.String.__codepointLength";
  "Darklang.Stdlib.Float.__toBits"; "Darklang.Stdlib.Float.__toInt64Unchecked";
  "Darklang.Stdlib.File.currentDirectory"; "Darklang.Stdlib.File.listDirectoryPacked"; "Darklang.Stdlib.File.readBlob"; "Darklang.Stdlib.File.exists"; "Darklang.Stdlib.File.isDirectory"; "Darklang.Stdlib.File.writeBlob"; "Darklang.Stdlib.File.appendText"; "Darklang.Stdlib.File.delete"; "Darklang.Stdlib.File.createDirectory"; "Darklang.Stdlib.File.setExecutable"; "Darklang.Stdlib.File.writeFromPtr"] in
 let isCompilerImplementationCandidate (candidate : NameResolution.candidate) = match candidate.NameResolution.provenance with NameResolution.SourceDeclaration name | NameResolution.CompilerExtension name -> S.mem name compilerImplementationNames | _ -> false in
 let declarationValidation, resolutionEnv = measure "TypeCheck: Environment Preparation" (fun () ->
  let declarationValidation = if validateDeclarations then Declarations.validateTopLevelTypeDeclarations baseEnv topLevels else
   (* Only synthetic test preambles skip this: they concatenate
      declarations that do not coexist in an original source unit. *)
   Ok () in
  let moduleRegistry = match baseEnv with Some existing -> existing.moduleRegistry | None -> DarkStdlib.buildModuleRegistry () in
  let localResolutionEnv = match baseEnv, topLevels with Some _, [Expression _] -> NameResolution.empty | _ -> ResolveDeclarations.declarationResolutionEnvironment topLevels moduleRegistry (Option.is_none baseEnv) in
  let resolutionEnv = match baseEnv with
  | Some existing when hideCompilerImplementationNames -> NameResolution.merge (NameResolution.filterCandidates (fun candidate -> not (isCompilerImplementationCandidate candidate)) existing.resolutionEnv) localResolutionEnv
  | Some existing -> NameResolution.merge existing.resolutionEnv localResolutionEnv
  | None -> localResolutionEnv in
  declarationValidation, resolutionEnv) in
 let resolvedProgramResult = measure "TypeCheck: Name Resolution" (fun () -> Result.bind declarationValidation (fun () ->
  let winningTypeDefs = List.filter_map (function TypeDef value -> Some value | _ -> None) topLevels |> List.rev in
  let _, winningTypeDefs = List.fold_left (fun (seen, values) value -> let name = Declarations.typeDefName value in if S.mem name seen then seen, values else S.add name seen, value :: values) (S.empty, []) winningTypeDefs in
  let localAliases = M.of_list (List.filter_map (function TypeAlias (name, params, target) -> Some (name, (params, target)) | _ -> None) winningTypeDefs) in
  let aliases = match baseEnv with Some existing -> M.fold M.add localAliases existing.aliasReg | None -> localAliases in
  let localRecordTypeNames = S.of_list (List.filter_map (function RecordDef (name, _, _) -> Some name | _ -> None) winningTypeDefs) in
  let recordTypeNames = match baseEnv with Some existing -> S.union localRecordTypeNames existing.recordTypeNames | None -> localRecordTypeNames in
  let resolvedNames = measure "TypeCheck: Symbol Resolution" (fun () -> ResolveDeclarations.resolveProgramNames resolutionEnv aliases recordTypeNames program) in
  Result.map (fun (Program resolvedTopLevels) -> measure "TypeCheck: Recursive Group Resolution" (fun () -> Program (ResolveDeclarations.resolveRecursiveDeclarationGroups resolvedTopLevels))) resolvedNames)) in
 Result.bind resolvedProgramResult (fun resolvedProgram -> measure "TypeCheck: Semantic Checking" (fun () -> match baseEnv, requireEntry, resolvedProgram with
 | Some existing, true, Program [Expression (_, expr)] -> ResolvedProgram.checkResolvedExpressionWithBaseEnv existing resolutionEnv requireExplicitTypeArgsForBareCalls warningSettings expr
 | _ -> ResolvedProgram.checkResolvedProgramInternal baseEnv requireExplicitTypeArgsForBareCalls warningSettings requireEntry resolvedProgram))
let checkProgramInternal = checkProgramInternalWithTrace None
let constructCheckedProgram (typ, program, (env : typeCheckEnv)) =
 let recordFieldCounts name = Option.map (fun (info : recordTypeInfo) -> List.length info.fields) (M.find_opt name env.indexedTypeReg) in
 CheckedAST.ofTypedProgram env.variantLookup (S.of_list (List.map fst (M.bindings env.values))) env.typeCatalog (CheckedAST.includeFunctionNames (List.to_seq (List.map fst (M.bindings env.moduleRegistry))) env.functionCatalog) recordFieldCounts program
 |> Result.map (fun checkedProgram -> let symbols = CheckedAST.programSymbols checkedProgram in CheckedAST.normalizeInferenceType typ, checkedProgram, {env with typeCatalog = CheckedAST.typeCatalog symbols; functionCatalog = CheckedAST.functionCatalog symbols})
 |> Result.map_error (fun message -> GenericError message)
(* Type-check a program
   Returns the type of the main expression and the transformed program
   The transformed program has Call nodes converted to TypeApp where type inference was applied *)
let checkProgram program = Result.bind (checkProgramInternal None false false true true AST.defaultWarningSettings program) constructCheckedProgram |> Result.map (fun (typ, program, _) -> typ, program)
(* Type-check the public source policy without a base environment.
   Used by focused declaration tests and tools that already parsed an isolated
   public program. *)
let checkPublicProgram program = Result.bind (checkProgramInternal None false true true true AST.defaultWarningSettings program) constructCheckedProgram |> Result.map (fun (typ, program, _) -> typ, program)
(* Type-check a program and return the type checking environment
   Use this when you need to reuse the environment (e.g., for stdlib caching) *)
let checkProgramWithEnv program = Result.bind (checkProgramInternal None false false true true AST.defaultWarningSettings program) constructCheckedProgram
(* Type-check a declaration-only program without synthesizing an expression. *)
let checkDeclarationProgramWithEnv program = Result.bind (checkProgramInternal None false false true false AST.defaultWarningSettings program) constructCheckedProgram
(* Type-check a program with a pre-populated base environment (for separate compilation)
   The program's definitions are merged with the base environment, allowing lookups
   of types/functions from both the base (e.g., stdlib) and the program (e.g., user code) *)
let checkProgramWithBaseEnv baseEnv program = Result.bind (checkProgramInternal (Some baseEnv) false false true true AST.defaultWarningSettings program) constructCheckedProgram
let checkDeclarationProgramWithBaseEnv baseEnv program = Result.bind (checkProgramInternal (Some baseEnv) false false true false AST.defaultWarningSettings program) constructCheckedProgram
(* Type-check a program with a pre-populated base environment, generic-call policy override,
   and warning compatibility settings from the compiler driver. *)
let checkProgramWithBaseEnvAndSettings baseEnv explicit warnings program = Result.bind (checkProgramInternal (Some baseEnv) false explicit true true warnings program) constructCheckedProgram
let checkProgramWithBaseEnvAndSettingsWithTrace phaseRecorder baseEnv explicit warnings program =
 Result.bind (checkProgramInternalWithTrace (Some phaseRecorder) (Some baseEnv) false explicit true true warnings program) (fun checkedResult ->
  let start = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result = constructCheckedProgram checkedResult in phaseRecorder "TypeCheck: Checked AST Construction" ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start); result)
let checkDeclarationProgramWithBaseEnvAndSettings baseEnv explicit warnings program = Result.bind (checkProgramInternal (Some baseEnv) false explicit true false warnings program) constructCheckedProgram
(* Analyze a synthetic preamble assembled from otherwise independent tests.
   Such preambles can repeat declarations that never coexist in a source
   program, so declaration-namespace validation belongs to each original
   source rather than this harness artifact. *)
let checkSyntheticPreambleWithBaseEnvAndSettings baseEnv explicit warnings program = Result.bind (checkProgramInternal (Some baseEnv) false explicit false false warnings program) constructCheckedProgram
(* Type-check source at the compiler-driver boundary, where implementation-only
   stdlib names must not be visible to user programs. *)
let checkPublicProgramWithBaseEnvAndSettings baseEnv explicit warnings program = Result.bind (checkProgramInternal (Some baseEnv) true explicit true true warnings program) constructCheckedProgram
