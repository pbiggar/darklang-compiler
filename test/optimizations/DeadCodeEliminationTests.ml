(* DeadCodeEliminationTests.fs - Unit tests for LIR-level function reachability
   Verifies tree shaking keeps function-address references that flow through
   lower-level call setup instructions, not just direct call instructions. *)
[@@@warning "-4-42"]
open Dark_compiler
module L=LIR
module F=StructuralFormat
type testResult=(unit,string) result
let blockWith instrs={L.label=L.Label "entry";instrs;terminator=L.Ret}
let namedFunctionWith name instrs={L.id=TestIds.functionIdForName name;name;typedParams=[];cfg={L.entry=L.Label "entry";blocks=L.LabelMap.singleton (L.Label "entry") (blockWith instrs)};stackSize=0;usedCalleeSaved=[];codegenFacts=None}
let functionWith instrs=namedFunctionWith "user" instrs
let formatStrings values=F.format (F.Sequence (List.map (fun value->F.Text value) values))
let expectCalls expected instrs=
 let ids=List.map (fun name->name,TestIds.functionIdForName name) expected |> StringOrder.Map.of_list in
 let actual=DeadCodeElimination.getCalledFunctions ids (functionWith instrs) in
 let expectedIds=List.map TestIds.functionIdForName expected |> SpecializationIdentity.FunctionSet.of_list in
 if SpecializationIdentity.FunctionSet.equal actual expectedIds then Ok () else Error ("Expected calls "^formatStrings expected^", got "^F.format (F.Sequence (List.map AST.DiagnosticFormatting.func (SpecializationIdentity.FunctionSet.elements actual))))
let testArgMovesFunctionAddressIsReachable ()=expectCalls ["Darklang.Stdlib.List.map"] [L.ArgMoves [L.X0,L.FuncAddr (TestIds.functionIdForName "Darklang.Stdlib.List.map")]]
let testListDisplayHelperIsReachableByCanonicalIdentity ()=expectCalls ["Darklang.Stdlib.List.__toDisplayString_i64"] [L.PrintSum (L.Physical L.X0,["Values",0,Some (AST.TList AST.TInt64)],false)]
let testFilteredFunctionsPreserveReachableSetAndInputOrder ()=
 let userFunctions=[namedFunctionWith "user" [L.Call (L.Virtual 0,TestIds.functionIdForName "stdlib_b",[L.FuncAddr (TestIds.functionIdForName "stdlib_a")])]] in
 let stdlibFunctions=[namedFunctionWith "unused" [];namedFunctionWith "stdlib_c" [];namedFunctionWith "stdlib_a" [];namedFunctionWith "stdlib_b" []] in
 let callGraph=FunctionIdMap.ofList [TestIds.functionIdForName "stdlib_a",SpecializationIdentity.FunctionSet.empty;TestIds.functionIdForName "stdlib_b",SpecializationIdentity.FunctionSet.singleton (TestIds.functionIdForName "stdlib_c");TestIds.functionIdForName "stdlib_c",SpecializationIdentity.FunctionSet.empty;TestIds.functionIdForName "unused",SpecializationIdentity.FunctionSet.empty] in
 let ids=List.map (fun (func:L.functionDef)->func.L.name,func.L.id) (userFunctions@stdlibFunctions) |> StringOrder.Map.of_list in
 let actual=DeadCodeElimination.filterFunctions callGraph ids userFunctions stdlibFunctions |> List.map (fun (func:L.functionDef)->func.L.name) in
 let expected=["stdlib_c";"stdlib_a";"stdlib_b"] in
 if actual=expected then Ok () else Error ("Expected reachable functions in order "^formatStrings expected^", got "^formatStrings actual)
let tests=["arg moves function address is reachable",testArgMovesFunctionAddressIsReachable;"list display helper is reachable by canonical identity",testListDisplayHelperIsReachableByCanonicalIdentity;"filtered functions preserve reachable set and input order",testFilteredFunctionsPreserveReachableSetAndInputOrder]
let runAll ()=let rec run=function []->Ok ()|(name,test)::rest->match test () with Ok ()->run rest|Error message->Error (name^" test failed: "^message) in run tests
