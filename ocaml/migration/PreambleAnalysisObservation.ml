(* Compare reusable preamble analysis against complete checking and catalog contracts. *)
open Dark_compiler
module X=CompilationContexts
module C=CheckedAST
module M=StringOrder.Map
module K=SpecializationIdentity.SpecMap
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let observe source=
 let baseSource="let inherited (x: Int64) : Int64 = x + 1L\nval saved = 4L\n()" in
 let fixture=WrittenParsing.parse Validation.Script baseSource |> fun parsed->Result.bind parsed (fun parsed->WrittenChecking.checkSourceUnitsWithBase None true false [parsed]) in
 attempt Fun.id (fun ()->Result.map (fun (_,program,written)->
  let env=WrittenChecking.typeCheckEnvironment program in let symbols=C.programSymbols program in
  let typeDefs,functions,_=match AST_to_ANF.splitTopLevels program with Ok value->value|Error error->failwith error in
  let registries=AST_to_ANF.buildRegistries symbols M.empty typeDefs (AST_to_ANF.buildAliasRegistry typeDefs) functions in
  let context=X.buildContext Platform.LinuxX86_64 symbols env (X.checkedValueArtifacts program) (SpecializationIdentity.extractGenericFuncDefs program) K.empty registries (X.buildBaseFuncNames registries) (SourcePreparation.extractReturnTypes registries.AST_to_ANF.funcReg) in
  let make writtenEnvironment={X.typedAST=program;context={context with X.writtenEnvironment};allocatedFunctions=[];callGraphSummaries=FunctionIdMap.empty;stdlibCallGraph=FunctionIdMap.empty;stdlibAnfFunctions=M.empty;stdlibAnfOptimizationCandidates=M.empty;stdlibInlineCandidates=FunctionIdMap.empty;stdlibAnfCallGraph=FunctionIdMap.empty;stdlibTypeMap=ANF.TypeMap.empty} in
  let inputs=[source;"()";"inherited 1L";"saved";"let newer (x: Int64) : Int64 = inherited x";"val local = saved + 1L";"let id (x: 'a) : 'a = x";"type R = { x: Int64 }";"inherited \"bad\"";"let broken =";"let newer (x: Int64) : Int64 = x\nnewer 2L"] in
  list (fun writtenEnvironment->let stdlib=make writtenEnvironment in list (fun allowInternal->list (fun input->result (fun (analysis:X.preambleAnalysis)->let expected=Types.mergeTypeCheckEnv env (WrittenChecking.typeCheckEnvironment analysis.X.typedAST) in
   tuple [DriverObservation.program analysis.X.typedAST;`Bool (analysis.X.typeCheckEnv=expected);`Bool (analysis.X.genericFuncDefs=SpecializationIdentity.extractGenericFuncDefs analysis.X.typedAST);`Bool (Option.is_some analysis.X.writtenEnvironment);result (fun (_,program,_)->DriverObservation.program program) (WrittenParsing.parse Validation.Script "()" |> fun parsed->Result.bind parsed (fun parsed->WrittenChecking.checkSourceUnitsWithBase analysis.X.writtenEnvironment allowInternal false [parsed]))]) (PreambleAnalysis.analyzePreamble allowInternal stdlib input)) inputs) [false;true]) [None;Some written]) fixture)
