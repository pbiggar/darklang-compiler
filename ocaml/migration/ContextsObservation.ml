(* Exercise context construction, catalog closure, overlays and checked values. *)
open Dark_compiler
module C=CompilationContexts
module M=StringOrder.Map
module S=StringOrder.Set
module F=SpecializationIdentity.FunctionSet
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let typ=SemanticAST.semanticType
let map f xs=`Assoc ["map",list (fun (key,value)->tuple [str key;f value]) (M.bindings xs)]
let ids f xs=SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (key,value)->let key=AST.functionIdValue key in let key=Z.to_string (if key<0L then Z.add (Z.of_int64 key) (Z.shift_left Z.one 64) else Z.of_int64 key) in tuple [`Assoc ["kind",`String "uint64";"value",`String key];f value]) (FunctionIdMap.toList xs)]]
let set xs=`Assoc ["set",list str (S.elements xs)]
let returns=ids (fun (name,value)->tuple [str name;typ value])
let fields=list (fun (name,value)->tuple [str name;typ value])
let typeReg=map (fun (value:TypeRegistries.recordTypeInfo)->SemanticJson.record "RecordTypeInfo" ["TypeParams",list str value.TypeRegistries.typeParams;"Fields",fields value.TypeRegistries.fields])
let variants=map (fun (owner,params,tag,values)->tuple [str owner;list str params;SemanticJson.int32 tag;list typ values])
let catalog (value:LiftFunctions.functionCatalog)=SemanticJson.record "FunctionCatalog" ["Params",ids (list typ) value.LiftFunctions.params;"ReturnTypes",ids typ value.LiftFunctions.returnTypes;"GenericDefs",ids (fun (names,value)->tuple [list str names;typ value]) value.LiftFunctions.genericDefs]
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try Ok (action ()) with Failure message|Invalid_argument message->Error message)
let observe source=
 let fid n=AST.functionId (Int64.of_int n) in
 let names=["declared";"module";"base";"wrapper";"middle";"cycle";"other";"Builtin.pmEvaluateValue"] in
 let symbols=List.fold_left (fun symbols name->snd (CheckedAST.internFunction name symbols)) (CheckedAST.emptySymbols ()) names in
 let registries=AST_to_ANF.buildRegistries symbols M.empty [] M.empty [] in
 let registries={registries with AST_to_ANF.functionIds=CheckedAST.functionIds symbols;functionNames=CheckedAST.functionNames symbols;funcParams=M.of_list ["declared",["x",AST.TInt64;"y",AST.TString];"module",["ignored",AST.TBool]];moduleRegistry=M.singleton "module" {AST.name="module";typeParams=["a"];paramTypes=[AST.TVar "a"];returnType=AST.TList (AST.TVar "a")};typeReg=M.of_list ["R",{TypeRegistries.typeParams=[];fields=[source,AST.TString]}];recordFieldsReg=M.singleton "R" [source,AST.TString];variantLookup=M.of_list ["S.A",("S",[],0,[]);"S.B",("S",[],1,[AST.TString])]} in
 let baseNames=S.of_list ["declared";"module";"base"] in
 let returnTypes=FunctionIdMap.ofList [fid 0,("declared",AST.TInt64);fid 1,("module",AST.TBool);AST.functionId (-1L),(source,AST.TString)] in
 let mkGeneric name dependencies=
  let id=Option.get (CheckedAST.tryFindFunctionId name symbols) in
  let func={CheckedAST.id;name;typeParams=["a"];params=CheckedAST.checkedParams (NonEmptyList.singleton (AST.bindingId 0,AST.TVar "a"));returnType=CheckedAST.checkedType (AST.TVar "a");body=CheckedAST.Local (AST.bindingId 0);recursion=None} in
  name,{SpecializationIdentity.symbols;func;directDependencies=F.of_list (List.map fid dependencies)} in
 let generic=M.of_list [mkGeneric "wrapper" [6];mkGeneric "middle" [9];mkGeneric "cycle" [7];mkGeneric "other" [7;99]] in
 let checked input=attempt Fun.id (fun ()->match WrittenParsing.parse Validation.Script input |> fun parsed->Result.bind parsed (fun parsed->WrittenChecking.checkSourceUnitsWithBase None true false [parsed]) with
 |Error message->failwith message
 |Ok (_,program,written)->
  let env=WrittenChecking.typeCheckEnvironment program in
  let checkedValues=C.checkedValueArtifacts program in
  let values=map (fun (value:C.checkedValueArtifact)->tuple [SemanticJson.int32 value.C.bindingCursor;typ value.C.typ;str (CheckedStructuralFormat.expr value.C.body)]) checkedValues in
  let targets=[Platform.LinuxX86_64;Platform.ARM64Backend Platform.LinuxARM64;Platform.ARM64Backend Platform.MacOSARM64] in
  let contexts=list (fun target->
   let context=C.buildContext target symbols env checkedValues generic (SpecializationIdentity.SpecMap.singleton ("module",[AST.TString]) "module_str") registries baseNames returnTypes in
   let describe (ctx:C.pipelineContext)=tuple [CacheIdentityObservation.target ctx.C.target;map ProductionMIR.functionId (CheckedAST.functionIds ctx.C.symbols);ids str (CheckedAST.functionNames ctx.C.symbols);set ctx.C.baseFuncNames;catalog ctx.C.lambdaLiftFunctions;typeReg ctx.C.lambdaLiftTypeReg;variants ctx.C.lambdaLiftVariantLookup;tuple [ProductionMIR.variantRegistry (fst ctx.C.projectedMirRegistries);ProductionMIR.recordRegistry (snd ctx.C.projectedMirRegistries)];returns ctx.C.returnTypes;set ctx.C.packageCatalogGenericCallers;map ProductionMIR.functionId ctx.C.registries.AST_to_ANF.functionIds;ids str ctx.C.registries.AST_to_ANF.functionNames;returns ctx.C.registries.AST_to_ANF.funcReg;`Bool ({ctx.C.typeCheckEnv with Types.functionCatalog=env.Types.functionCatalog}=env);`Bool (ctx.C.genericFuncDefs=generic);`Bool (ctx.C.specRegistry=SpecializationIdentity.SpecMap.singleton ("module",[AST.TString]) "module_str");`Bool (ctx.C.checkedValues=checkedValues);`Bool (ctx.C.typeCheckEnv.Types.functionCatalog=CheckedAST.functionCatalog ctx.C.symbols);`Bool (Option.fold ~none:true ~some:(fun environment->environment=WrittenChecking.includeAllocatedFunctions ctx.C.symbols written) ctx.C.writtenEnvironment);`Bool ({ctx.C.registries with AST_to_ANF.functionIds=registries.AST_to_ANF.functionIds;functionNames=registries.AST_to_ANF.functionNames;funcReg=registries.AST_to_ANF.funcReg}=registries)] in
   let make id name returnType={ANF.id;name;typedParams=[{ANF.id=ANF.TempId 0;typ=AST.TString}];returnType;returnOwnership=ANF.BorrowedReturn;body=ANF.Return (ANF.StringLiteral source)} in
   let functions=[make (fid 3) "module" AST.TString;make (fid 4) "base" AST.TBool;make (fid 20) ("generated_"^source) AST.TInt64;make (fid 20) ("generated_"^source) AST.TString] in
   let updated=C.includeCompiledFunctions functions {context with C.writtenEnvironment=Some written} in
   tuple [describe context;`Bool (context.C.writtenEnvironment=None);describe updated;`Bool (Option.is_some updated.C.writtenEnvironment);describe (C.includeCompiledFunctions functions updated);describe (C.includeCompiledFunctions [] context)]) targets in
  tuple [values;contexts]) in
 let catalogTests=list (fun names->attempt catalog (fun ()->C.buildLambdaLiftFunctionCatalog registries names returnTypes)) [S.empty;baseNames;S.singleton "missing"] in
 let baseTests=set (C.buildBaseFuncNames registries) in
 let merged=returns (C.mergeReturnTypes returnTypes (FunctionIdMap.ofList [fid 1,(source,AST.TString);fid 8,("new",AST.TBool)])) in
 let callerTests=list (fun defs->set (C.buildPackageCatalogGenericCallers defs)) [M.empty;generic;M.remove "middle" generic;M.add "cycle" (snd (mkGeneric "cycle" [7;5])) generic] in
 tuple [baseTests;catalogTests;merged;set C.packageCatalogFunctionNames;callerTests;checked source;list checked ["()";"val a = 1L\nval b = \"é😀\"\na";"type R = { x: Int64 }\nval r = R { x = 1L }\nr";"val a = [1L,2L]\na"]]
