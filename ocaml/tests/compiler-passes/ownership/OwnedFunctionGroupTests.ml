(* OwnedFunctionGroupTests.fs - Deterministic owned-HIR call-group laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module G = OwnedFunctionGroups
module S = SpecializationIdentity.FunctionSet
module F = OwnershipTestFormatting
type testLeaf = TestLeaf [@@warning "-37"]
let unitValue : H.value = {H.id = H.ValueId 0; typ = AST.TUnit}
let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty}
let block operations : (testLeaf, string) O.block = {O.body = {H.parameters = []; operations; result = unitValue}}
let call target = O.Evaluate (H.Call {H.target = TestIds.functionIdForName target; arguments = [unitValue]; result = unitValue})
let branch yes no = O.Evaluate (H.Branch (unitValue, condition, block yes, block no))
let definition name operations : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName name; name; body = {O.body = {H.parameters = [{H.binding = AST.bindingId 0; value = unitValue}]; operations; result = unitValue}}}; ownership = {O.parameters = [O.UnmanagedParameter]; result = O.UnmanagedResult}}
let summary group = List.map (fun (definition : (testLeaf, string) O.functionDef) -> definition.O.definition.H.name) (G.functions group), G.isRecursive group, G.internalDependencies group, G.externalTargets group
let svSet values = StructuralValue.Union ("set", [StructuralValue.Sequence (List.map AST.DiagnosticFormatting.func (S.elements values))])
let svSummary (names, recursive, dependencies, targets) = StructuralValue.Tuple [StructuralValue.Sequence (List.map (fun name -> StructuralValue.Text name) names); StructuralValue.Scalar (string_of_bool recursive); svSet dependencies; svSet targets]
let svGroup group = match G.functions group with
 | head :: tail -> StructuralValue.Union ("Group", [F.functionDef (fun TestLeaf -> StructuralValue.Union ("TestLeaf", [])) (fun id -> StructuralValue.Text id) head; StructuralValue.Sequence (List.map (F.functionDef (fun TestLeaf -> StructuralValue.Union ("TestLeaf", [])) (fun id -> StructuralValue.Text id)) tail); StructuralValue.Scalar (string_of_bool (G.isRecursive group)); svSet (G.internalDependencies group); svSet (G.externalTargets group)])
 | [] -> failwith "Owned function SCC partition produced an empty component"
let svError (G.DuplicateFunctionName name) = StructuralValue.Union ("DuplicateFunctionName", [AST.DiagnosticFormatting.func name])
let svResult encode = function Ok value -> StructuralValue.Union ("Ok", [encode value]) | Error error -> StructuralValue.Union ("Error", [svError error])
let showSummaries result = HostStructuralFormat.format (svResult (fun values -> StructuralValue.Sequence (List.map svSummary values)) result)
let showGroups result = HostStructuralFormat.format (svResult (fun groups -> StructuralValue.Sequence (List.map svGroup groups)) result)
let testDiscoversCalleeFirstRecursiveGroups () =
 let entry = definition "entry" [branch [call "mutualA"] [call "external"]] in
 let mutualA = definition "mutualA" [call "mutualB"] in
 let opaqueCall : H.operand = {H.expression = CheckedAST.Call (TestIds.functionIdForName "opaque", NonEmptyList.singleton CheckedAST.UnitLiteral); typ = AST.TUnit; inputs = CheckedAST.BindingIdMap.empty} in
 let leaf = definition "leaf" [O.Evaluate (H.ScalarBinding (unitValue, opaqueCall))] in
 let self = definition "self" [call "self"] in let mutualB = definition "mutualB" [call "mutualA"; call "leaf"] in
 let actual = G.discover [entry; mutualA; leaf; self; mutualB] |> Result.map (List.map summary) in
 let expected = Ok [
  ["leaf"], false, S.empty, S.empty;
  ["mutualA"; "mutualB"], true, S.singleton (TestIds.functionIdForName "leaf"), S.empty;
  ["entry"], false, S.singleton (TestIds.functionIdForName "mutualA"), S.singleton (TestIds.functionIdForName "external");
  ["self"], true, S.empty, S.empty] in
 if actual = expected then Ok () else Error ("Expected stable callee-first owned function groups " ^ showSummaries expected ^ ", got " ^ showSummaries actual)
let testRejectsDuplicateFunctionNames () =
 let duplicate = definition "duplicate" [] in let actual = G.discover [duplicate; duplicate] in
 let expected = Error (G.DuplicateFunctionName (TestIds.functionIdForName "duplicate")) in
 if actual = expected then Ok () else Error ("Expected duplicate owned functions to fail grouping, got " ^ showGroups actual)
let testAcceptsEmptyProgram () = match G.discover [] with Ok [] -> Ok () | actual -> Error ("Expected an empty program to contain no call groups, got " ^ showGroups actual)
let testDiscoversDeepAcyclicGraphCalleeFirst () =
 let functionCount = 256 in let name index = Printf.sprintf "chain%04i" index in
 let definitions = List.init functionCount (fun index -> let operations = if index + 1 < functionCount then [call (name (index + 1))] else [] in definition (name index) operations) in
 let actual = G.discover definitions |> Result.map (List.map (fun group -> List.map (fun (definition : (testLeaf, string) O.functionDef) -> definition.O.definition.H.name) (G.functions group))) in
 let expected = Ok (List.init functionCount (fun index -> [name (functionCount - 1 - index)])) in
 if actual = expected then Ok () else Error "Expected every deep acyclic owned-HIR group in callee-first order"
let tests = [
 "Owned HIR discovers stable callee-first recursive groups", testDiscoversCalleeFirstRecursiveGroups;
 "Owned HIR call grouping rejects duplicate function names", testRejectsDuplicateFunctionNames;
 "Owned HIR call grouping accepts an empty program", testAcceptsEmptyProgram;
 "Owned HIR discovers a deep acyclic graph in callee-first order", testDiscoversDeepAcyclicGraphCalleeFirst
]
