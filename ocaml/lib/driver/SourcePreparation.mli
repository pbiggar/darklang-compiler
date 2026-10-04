(* SourcePreparation.fs - Prepare checked declarations through specialization and closure lowering. *)
val extractReturnTypes : TypeRegistries.functionRegistry -> (string * AST.semanticType) FunctionIdMap.t
val emptyRegistries : AST.moduleRegistry -> AST_to_ANF.registries
val liftLambdasWithBase : TypeRegistries.typeRegistry -> LoweringPrimitives.variantLookup -> LiftFunctions.functionCatalog -> CompilerOptions.passTimingRecorder option -> CheckedAST.program -> (CheckedAST.program,string) result
val mergeSpecRegistries : SpecializationIdentity.specRegistry -> SpecializationIdentity.specRegistry -> SpecializationIdentity.specRegistry
val collectLocalSpecs : SpecializationIdentity.genericFuncDefs -> CheckedAST.program -> SpecializationIdentity.SpecSet.t
type monomorphizationMode=Monomorphize of SpecializationIdentity.genericFuncDefs option|ReplaceTypeApps of SpecializationIdentity.specRegistry|SpecializeLocalAndReplace of SpecializationIdentity.specRegistry
val importInheritedValues : CompilerOptions.passTimingRecorder option -> CompilationContexts.checkedValueArtifact StringOrder.Map.t -> CheckedAST.program -> CheckedAST.program
val materializeProgramValues : CheckedAST.program -> CheckedAST.program
val prepareProgramForAnf : monomorphizationMode -> TypeRegistries.typeRegistry -> LoweringPrimitives.variantLookup -> StringOrder.Set.t -> LiftFunctions.functionCatalog -> CompilationContexts.checkedValueArtifact StringOrder.Map.t -> CompilerOptions.passTimingRecorder option -> CheckedAST.program -> (CheckedAST.program,string) result
val buildRegistriesForProgram : CompilerOptions.passTimingRecorder option -> int64 -> CheckedAST.symbols -> bool -> AST.moduleRegistry -> AST_to_ANF.registries -> AST.typeDef list -> CheckedAST.functionDef list -> AST_to_ANF.registries * AST_to_ANF.registries * CheckedAST.functionDef list
type declarationConversion={symbols:CheckedAST.symbols;functions:ANF.functionDef list;registries:AST_to_ANF.registries;localReturnTypes:(string*AST.semanticType) FunctionIdMap.t}
val splitDeclarations : CheckedAST.program -> (AST.typeDef list * CheckedAST.functionDef list,string) result
val convertTypedDeclarationsWithTrace : CompilerOptions.passTimingRecorder option -> CompilationContexts.pipelineContext option -> monomorphizationMode -> CheckedAST.program -> (declarationConversion,string) result
val convertTypedDeclarations : CompilationContexts.pipelineContext option -> monomorphizationMode -> CheckedAST.program -> (declarationConversion,string) result
val convertTypedProgramToConversionResult : AST.moduleRegistry -> CheckedAST.program -> (AST_to_ANF.conversionResult,string) result
val convertTypedProgramToUserOnlyWithMode : CompilationContexts.pipelineContext -> monomorphizationMode -> Types.typeCheckEnv -> CompilationSession.compilationSession option -> CompilerOptions.passTimingRecorder option -> CheckedAST.program -> (AST_to_ANF.userOnlyResult * Obj.t,string) result
val convertTypedProgramToUserOnly : CompilationContexts.pipelineContext -> CheckedAST.program -> (AST_to_ANF.userOnlyResult,string) result
val convertTypedProgramToUserOnlyWithTrace : CompilationContexts.pipelineContext -> CompilerOptions.passTimingRecorder option -> CheckedAST.program -> (AST_to_ANF.userOnlyResult,string) result
val tryDeleteFile : string -> unit
(* Native process start data passed explicitly by the execution boundary. *)
type processStartInfo={fileName:string;arguments:string list;environment:(string*string) list;stdin:Unix.file_descr;stdout:Unix.file_descr;stderr:Unix.file_descr}
val tryStartProcess : processStartInfo -> (int,string) result
