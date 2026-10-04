(* Validate source preparation, inherited values, declarations and conversion reuse. *)
open Dark_compiler
module P=SourcePreparation
module C=CheckedAST
module X=CompilationContexts
module M=StringOrder.Map
module K=SpecializationIdentity.SpecMap
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let parse source=WrittenParsing.parse Validation.Script source |> fun parsed->Result.bind parsed (fun parsed->WrittenChecking.checkSourceUnitsWithBase None true false [parsed])
let observe source=
 let fixtures=[source;"()";"1L";"let f (x: Int64) : Int64 = x + 1L\nf 2L";"let id (x: 'a) : 'a = x\nid 1L";"val a = 1L\na";"val a = [1L,2L]\na";"val a = 1L\nval b = a + 2L\nlet f (x: Int64) : Int64 = b + x\nf a";"val unused = [1L,2L]\n()";"type R = { x: Int64 }\nval r = R { x = 1L }\nr";"type S = A of Int64 | B\nS.A 1L";"let add (x: Int64) (y: Int64) : Int64 = x + y\nlet f (x: Int64) : Int64 = let g = add x in g 2L\nf 1L";"let f (x: Int64) : Int64 = let g (y: Int64) : Int64 = x + y in g 2L\nf 1L";"let recur (x: Int64) : Int64 = if x == 0L then x else recur (x - 1L)\nrecur 1L";"let f (x: Int64) : Int64 = x + 1L";"type Box<'a> = { value: 'a }\nlet id (x: 'a) : 'a = x\nval a = Box { value = id 1L }\na"] in
 let observeInput input=attempt Fun.id (fun ()->Result.map (fun (_,program,_)->
  let env=WrittenChecking.typeCheckEnvironment program in
  let moduleRegistry=DarkStdlib.buildModuleRegistry () in let empty=P.emptyRegistries moduleRegistry in
  let symbols=C.programSymbols program in let generic=SpecializationIdentity.extractGenericFuncDefs program in
  let localSpecs=P.collectLocalSpecs generic program in
  let specialization=Monomorphization.specializeFromSpecs symbols generic localSpecs in
  let modes=[P.Monomorphize None;P.Monomorphize (Some generic);P.ReplaceTypeApps specialization.SpecializationIdentity.specRegistry;P.SpecializeLocalAndReplace K.empty] in
  let baseNames=X.buildBaseFuncNames empty in let catalog=X.buildLambdaLiftFunctionCatalog empty baseNames FunctionIdMap.empty in
  let preparations=list (fun mode->
   let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=tuple [str value.CompilerOptions.pass;`Bool (value.CompilerOptions.elapsed>=0L)]:: !phases in
   let output=attempt DriverObservation.program (fun ()->P.prepareProgramForAnf mode M.empty M.empty baseNames catalog M.empty (Some record) program) in
   let phases=List.rev !phases in tuple [output;`List phases;attempt DriverObservation.declaration (fun ()->P.convertTypedDeclarations None mode program)]) modes in
  let context=X.buildContext Platform.LinuxX86_64 (C.emptySymbols ()) env M.empty M.empty K.empty empty baseNames FunctionIdMap.empty in
  let session=new CompilationSession.compilationSession () in
  let run ()=P.convertTypedProgramToUserOnlyWithMode context (P.Monomorphize (Some generic)) env (Some session) None program in
  let first=run () in let firstCount=session#cachedAnfDependencyCount in let second=run () in
  let cacheIdentity=match first,second with Ok (_,a),Ok (_,b)->a==b|(Error _,_)|(_,Error _)->false in
  let converted=tuple [result (fun (value,_)->DriverObservation.user value) first;result (fun (value,_)->DriverObservation.user value) second;SemanticJson.int32 firstCount;SemanticJson.int32 session#cachedAnfDependencyCount;`Bool cacheIdentity] in
  tuple [DriverObservation.program program;attempt DriverObservation.program (fun ()->Ok (P.materializeProgramValues program));preparations;attempt DriverObservation.conversion (fun ()->P.convertTypedProgramToConversionResult moduleRegistry program);converted]) (parse input)) in
 let id,symbols=C.internValue "inherited" (C.emptySymbols ()) in
 let body=C.StringLiteral source in
 let inherited=M.of_list ["inherited",{X.bindingCursor=17;typ=AST.TString;body};"second",{X.bindingCursor=31;typ=AST.TInt64;body=C.Int64Literal 42L}] in
 let inheritedTests=list (fun tops->let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in let output=P.importInheritedValues (Some record) inherited (C.programFromCheckedParts (symbols,tops)) in tuple [DriverObservation.program output;list str (List.rev !phases)]) [[C.Expression (C.Local id)];[C.ValueDef {C.id;name="inherited";typ=C.checkedType AST.TString;body=C.StringLiteral "local"};C.Expression (C.Local id)];[]] in
 let a,symbols=C.internValue "a" symbols in let b,symbols=C.internValue "b" symbols in
 let value name id body=C.ValueDef {C.id;name;typ=C.checkedType AST.TInt64;body} in
 let cycles=list (fun body->attempt DriverObservation.program (fun ()->Ok (P.materializeProgramValues (C.programFromCheckedParts (symbols,[value "a" a (C.Local b);value "b" b body;C.Expression (C.Local a)]))))) [C.Local a;C.Int64Literal 2L;C.Local b] in
 let extract=list (fun typValue->attempt (fun values->DriverObservation.registries {(P.emptyRegistries M.empty) with AST_to_ANF.funcReg=values}) (fun ()->Ok (P.extractReturnTypes (FunctionIdMap.ofList [AST.functionId 7L,(source,typValue)]) |> FunctionIdMap.map (fun _ (name,typValue)->name,typValue)))) [AST.TFunction ([AST.TInt64],AST.TString);AST.TString] in
 tuple [list observeInput fixtures;inheritedTests;cycles;extract]
