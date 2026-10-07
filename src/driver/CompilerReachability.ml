(* CompilerReachability.ml - Query standard-library reachability through the compilation pipeline. *)
module X=CompilationContexts
module M=StringOrder.Map
module S=StringOrder.Set
module R=AST_to_ANF
let (let*)=Result.bind
(* Get all stdlib function names from the prebuilt stdlib. *)
let getAllStdlibFunctionNamesFromStdlib (stdlib:X.stdlibResult)=M.to_seq stdlib.X.stdlibAnfFunctions |> Seq.map fst |> S.of_seq
(* Get the set of stdlib function names reachable from user code (using prebuilt stdlib).
   Used for coverage analysis without re-compiling stdlib. *)
let getReachableStdlibFunctionsFromStdlib (stdlib:X.stdlibResult) source=
 (* Parse user code. *)
 let* parsed=WrittenParsing.parse Validation.Script source |> Result.map_error (fun error->"Parse error: "^error) in
 (* Type check with stdlib environment. *)
 let* programType,typed,_=WrittenChecking.checkSourceUnitsWithBase stdlib.X.context.X.writtenEnvironment false true [parsed] in
 let env=Types.mergeTypeCheckEnv stdlib.X.context.X.typeCheckEnv (WrittenChecking.typeCheckEnvironment typed) in
 let planned=JsonPlanning.rewriteProgram env typed in
 let typ=Types.resolveType env.Types.aliasReg programType in
 let rendered,boundary=if typ=AST.TUnit then planned,AST.TUnit else ValueRendering.rewriteProgram env.Types.indexedTypeReg env.Types.indexedSumTypeReg typ planned,AST.TString in
 (* Convert to ANF. *)
 let* user=SourcePreparation.convertTypedProgramToUserOnly stdlib.X.context rendered |> Result.map_error (fun error->"ANF conversion error: "^error) in
 let options={CompilerOptions.defaultOptions with CompilerOptions.disableANFOpt=true;disableInlining=true} in
 let start=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let elapsed ()=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)-.start in
 let entryId,symbols=CheckedAST.internFunction "_start" user.R.symbols in
 let entry=R.synthesizeEntryFunction entryId "_start" boundary user.R.mainExpr in
 let registries={R.scopeContracts=user.R.scopeContracts;inertFunctionScopes=user.R.inertFunctionScopes;typeReg=user.R.typeReg;typeNames=user.R.typeNames;recordFieldsReg=user.R.recordFieldsReg;recordTypeParamsReg=user.R.recordTypeParamsReg;variantLookup=user.R.variantLookup;sumMetadata=user.R.sumMetadata;rcSumShapeReg=user.R.rcSumShapeReg;funcReg=user.R.funcReg;functionIds=user.R.functionIds;functionNames=user.R.functionNames;funcParams=user.R.funcParams;moduleRegistry=user.R.moduleRegistry;recursiveMembers=user.R.recursiveMembers} in
 let* printed=PrintInsertion.insertPrintInEntry user.R.functionIds "_start" boundary (entry::user.R.userFunctions) |> Result.map_error (fun error->"Print insertion error: "^error) in
 let* functions,_,_=ANFPipeline.buildAnf 0 options elapsed registries (CheckedAST.nextFunctionOrdinal symbols) InliningCommon.defaultConfig FunctionIdMap.empty M.empty user.R.nonInlineableFunctionNames printed user.R.ownershipContracts false None in
 let reachable=ANFDeadCodeElimination.getReachableStdlib stdlib.X.stdlibAnfCallGraph functions in
 let names=M.bindings stdlib.X.stdlibAnfFunctions |> List.map (fun (name,(func:ANF.functionDef))->func.ANF.id,name) |> FunctionIdMap.ofList in
 Ok (SpecializationIdentity.FunctionSet.elements reachable |> List.filter_map (fun id->FunctionIdMap.tryFind id names) |> S.of_list)
