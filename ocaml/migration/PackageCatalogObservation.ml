(* Compare complete package lookup materialization and source ownership. *)
open Dark_compiler
module X=CompilationContexts
module A=AST
module M=StringOrder.Map
module K=SpecializationIdentity.SpecMap
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let hashName="Darklang.LanguageTools.ProgramTypes.Hash"
let locationName="Darklang.LanguageTools.ProgramTypes.PackageLocation"
let runtimeName="Darklang.LanguageTools.RuntimeTypes.ValueType"
let optionName="Darklang.Stdlib.Option.Option"
let hashType=A.TSum (hashName,[])
let runtimeType=A.TSum (runtimeName,[])
let optionType typ=A.TSum (optionName,[typ])
let fn name typeParams params returnType body=A.FunctionDef {A.name;typeParams;params=NonEmptyList.fromList params;returnType;body;recursion=None}
let call name args=A.applyNamed name (NonEmptyList.fromList args)
let typedCall name typ args=A.applyNamedWithTypes name [typ] (NonEmptyList.fromList args)
let constructor name case fields=A.Constructor (A.UnresolvedConstructor (Some name),case,fields)
let hash value=constructor hashName "Hash" [A.StringLiteral value]
let observe source=
 let base=A.Program [A.TypeDef (A.SumTypeDef (hashName,[],[{A.name="Hash";fields=[A.TString]}]));A.TypeDef (A.RecordDef (locationName,[],["owner",A.TString;"modules",A.TList A.TString;"name",A.TString]));A.TypeDef (A.SumTypeDef (runtimeName,[],[{A.name="Dummy";fields=[]}]));A.TypeDef (A.SumTypeDef (optionName,["a"],[{A.name="None";fields=[]};{A.name="Some";fields=[A.TVar "a"]}]));fn "Darklang.LanguageTools.ProgramTypes.hashToString" [] ["value",hashType] A.TString (A.StringLiteral "hash");fn "Darklang.LanguageTools.RuntimeTypes.__isCustomTypeWithNoTypeArguments" [] ["value",runtimeType;"hash",A.TString] A.TBool (A.BoolLiteral false);fn "wrap" ["a"] ["hash",hashType] (optionType (A.TVar "a")) (typedCall "Builtin.pmEvaluateValue" (A.TVar "a") [A.Var "hash"]);A.Expression ([],A.UnitLiteral)] in
 let fixture=TypeChecking.checkProgramWithEnv base |> Result.map_error CheckingDiagnostics.typeErrorToString in
 let catalogs=
  let location={X.visibleInBranches=["branch";"other";"branch"];owner=source;modules=["A";"B"];name="value"} in
  let entry valueHash runtimeHash arguments resultType state={X.valueHash;runtimeType={X.hash=runtimeHash;typeArguments=arguments};locations=[location;{location with X.visibleInBranches=["branch"];name="second"}];evaluator={X.resultType;state}} in
  let a=entry "a" "r1" [] A.TInt64 (X.Available (A.Int64Literal 7L)) in
  let b=entry "b" "r1" [] A.TInt64 X.Unavailable in
  let c=entry "c" "r2" [] A.TString (X.Available (A.StringLiteral source)) in
  List.map (fun entries->X.PackageValueCatalog entries) [[];[a];[a;b;c];[c;b;a];[a;{b with X.evaluator={X.resultType=A.TInt64;state=X.EvaluationFailure}}];[a;{a with X.locations=[]}];[{a with X.evaluator={X.resultType=A.TInt64;state=X.Available (A.StringLiteral "wrong")}}];[entry "generic" "r1" [{X.hash="arg";typeArguments=[]}] A.TInt64 (X.Available (A.Int64Literal 9L));a;c]] in
 let catalogOutputs=attempt Fun.id (fun ()->Result.map (fun (_,baseProgram,env)->
  let symbols=CheckedAST.programSymbols baseProgram in let typeDefs,functions,_=match AST_to_ANF.splitTopLevels baseProgram with Ok value->value|Error error->failwith error in
  let registries=AST_to_ANF.buildRegistries symbols env.Types.moduleRegistry typeDefs (AST_to_ANF.buildAliasRegistry typeDefs) functions in
  let context=X.buildContext Platform.LinuxX86_64 symbols env M.empty (SpecializationIdentity.extractGenericFuncDefs baseProgram) K.empty registries (X.buildBaseFuncNames registries) (SourcePreparation.extractReturnTypes registries.AST_to_ANF.funcReg) in
  let valueType=constructor runtimeName "Dummy" [] in
  let evalInt=typedCall "Builtin.pmEvaluateValue" A.TInt64 [hash "a"] in let evalString=typedCall "Builtin.pmEvaluateValue" A.TString [hash "c"] in
  let inputs=[A.UnitLiteral;call "Builtin.pmFindValuesByValueType" [valueType];call "Builtin.pmGetLocationsByValue" [A.StringLiteral "branch";hash "a"];evalInt;evalString;A.Let (A.LPVariable "first",evalInt,evalString);A.Let (A.LPVariable "found",call "Builtin.pmFindValuesByValueType" [valueType],evalInt);A.Let (A.LPVariable "locations",call "Builtin.pmGetLocationsByValue" [A.StringLiteral "branch";hash "a"],evalInt);typedCall "wrap" A.TInt64 [hash "a"]] in
  list (fun expr->let checked=TypeChecking.checkProgramWithBaseEnv env (A.Program [A.Expression ([],expr)]) |> Result.map_error CheckingDiagnostics.typeErrorToString in result (fun (_,program,_)->tuple [DriverObservation.program program;list (fun catalog->attempt DriverObservation.program (fun ()->PackageCatalog.materializePackageValueCatalog context A.defaultWarningSettings catalog program)) catalogs]) checked) inputs) fixture) in
 let parseOutputs=list (fun allowInternal->list (fun requireEntry->list (fun units->result (fun parsed->result (fun (_,program,_)->DriverObservation.program program) (WrittenChecking.checkSourceUnitsWithBase None allowInternal requireEntry parsed)) (PackageCatalog.parseWrittenSourceProgram allowInternal requireEntry (NonEmptyList.fromList units))) [[{X.name="input.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source}];[{X.name="library.dark";purpose=NameSyntax.SourceUnitPurpose.Library;source="let f (x: Int64) : Int64 = x"};{X.name="main.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source="f 1L"}];[{X.name="a.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source="()"};{X.name="b.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source="()"}];[{X.name="bad name";purpose=NameSyntax.SourceUnitPurpose.Executable;source="()"}];[{X.name="library.dark";purpose=NameSyntax.SourceUnitPurpose.Library;source="1L"}]]) [false;true]) [false;true] in
 tuple [catalogOutputs;parseOutputs]
