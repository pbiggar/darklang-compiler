(* WholeFunctionOwnershipTests.fs - Whole-function boundary inference and ownership placement laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module E = ElaborateFunctionOwnership
module V = VerifyOwnedHIR.Make (ListLiveness.Identity)
module F = OwnershipTestFormatting
type testLeaf = {inputs : (H.value * bool) list; outputs : H.value list}
type testBlock = TestBlock of (testLeaf, testBlock) H.operation H.block
let body (TestBlock block) = block
let managed id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let unitValue id : H.value = {H.id = H.ValueId id; typ = AST.TUnit}
let binding name = AST.bindingId (Int32.to_int (Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (Text.scalars name)))
let parameter name value : H.parameter = {H.binding = binding name; value}
let block parameters operations result = TestBlock {H.parameters; operations; result}
let definition name parameters operations result : testBlock H.functionDef = {H.id = TestIds.functionIdForName name; name; body = block parameters operations result}
let scalar (result : H.value) inputs = H.ScalarBinding (result, {H.expression = CheckedAST.BoolLiteral true; typ = result.H.typ; inputs = CheckedAST.BindingIdMap.of_list (List.map (fun (name, value) -> binding name, value) inputs)})
let leaf inputs outputs = H.Leaf {inputs; outputs}
let call target arguments result = H.Call {H.target = TestIds.functionIdForName target; arguments; result}
let isManaged (value : H.value) = match value.H.typ with AST.TList _ -> true | _ -> false
let leafOwnership leaf : H.valueId O.contract = {O.inputs = List.filter_map (fun ((value : H.value), consumed) -> if not (isManaged value) then None else if consumed then Some (O.Consumed value.H.id) else Some (O.Borrowed value.H.id)) leaf.inputs; outputs = List.filter_map (fun (value : H.value) -> if isManaged value then Some value.H.id else None) leaf.outputs}
let dialect : (testLeaf, testBlock) E.dialect = {E.body; leafOwnership; leafUniqueness = (fun leaf -> {E.Ownership.requiredInputs = H.ValueSet.empty; uniqueOutputs = H.ValueSet.of_list (List.filter_map (fun (value : H.value) -> if isManaged value then Some value.H.id else None) leaf.outputs)}); isManaged; externalCallOwnership = (fun _ -> None)}
let primitiveContract leaf : H.primitiveContract = {H.inputs = List.map fst leaf.inputs; operands = []; outputs = List.map (fun value -> {H.value; alias = if isManaged value then H.FreshManaged else H.NoManagedAlias}) leaf.outputs; effects = H.EffectSet.empty}
let callContract (call : H.functionCall) : H.primitiveContract = {H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = if isManaged call.H.result then H.UnknownManagedAlias else H.NoManagedAlias}]; effects = H.EffectSet.singleton H.MayInvokeUserCode}
let errorToString = function E.UnknownCallOwnership target -> StructuralFormat.format (StructuralValue.Union ("UnknownCallOwnership", [AST.DiagnosticFormatting.func target])) | E.InconsistentCallParameters target -> StructuralFormat.format (StructuralValue.Union ("InconsistentCallParameters", [AST.DiagnosticFormatting.func target])) | E.InvalidFunctionBoundary (target, error) -> "InvalidFunctionBoundary (" ^ StructuralFormat.format (AST.DiagnosticFormatting.func target) ^ ", " ^ E.Ownership.errorToString (fun (H.ValueId id) -> StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])) error ^ ")"
let elaborate definitions = match E.elaborateFunctions dialect definitions with
 | Error error -> Error ("Ownership elaboration failed: " ^ errorToString error)
 | Ok analysis -> let contracts : testLeaf VerifyOwnedHIR.hirContracts = {VerifyOwnedHIR.leaf = primitiveContract; callSignature = (fun _ -> None); callContract = (fun call -> Some (callContract call))} in
   match V.verifyFunctions contracts (E.semantics analysis) (E.functions analysis) with
   | Ok () -> Ok analysis
   | Error error -> Error ("Owned HIR verification failed: " ^ (match error with V.HIRVerificationFailed error -> "HIRVerificationFailed (" ^ VerifyHIR.errorToString error ^ ")" | V.OwnershipVerificationFailed error -> "OwnershipVerificationFailed (" ^ V.Ownership.errorToString (fun (H.ValueId id) -> StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])) error ^ ")"))
let findFunction name analysis = List.find_opt (fun (definition : (testLeaf, H.valueId) O.functionDef) -> definition.O.definition.H.name = name) (E.functions analysis)
let svId (H.ValueId id) = StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])
let svLeaf leaf = StructuralValue.Record ["Inputs", StructuralValue.Sequence (List.map (fun (value, consumed) -> StructuralValue.Tuple [F.value value; StructuralValue.Scalar (string_of_bool consumed)]) leaf.inputs); "Outputs", StructuralValue.Sequence (List.map F.value leaf.outputs)]
let showOwned actual = StructuralFormat.format (F.option (F.functionDef svLeaf svId) actual)
let showSteps actual = StructuralFormat.format (F.steps svLeaf svId actual)
let boundary parameters result : H.valueId O.functionSignature = {O.parameters; result}
let testInfersBorrowedOpaqueInput () =
 let input = managed 0 in let result = unitValue 1 in let source = definition "borrow" [parameter "input" input] [scalar result ["input", input]] result in
 match elaborate [source] with Error error -> Error error | Ok analysis -> match findFunction "borrow" analysis with
 | Some owned when owned.O.ownership = boundary [O.BorrowedParameter input.H.id] O.UnmanagedResult -> Ok ()
 | actual -> Error ("Expected an opaque managed use to infer a borrowed parameter, got " ^ showOwned actual)
let testInfersConsumedInput () =
 let input = managed 0 in let result = unitValue 1 in let source = definition "consume" [parameter "input" input] [leaf [input, true] [result]] result in
 match elaborate [source] with Error error -> Error error | Ok analysis -> match findFunction "consume" analysis with
 | Some owned when owned.O.ownership = boundary [O.ConsumedParameter input.H.id] O.UnmanagedResult -> Ok ()
 | actual -> Error ("Expected the leaf demand to infer a consumed parameter, got " ^ showOwned actual)
let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty}
let testDropsOppositeBranchInputs () =
 let first = managed 0 in let second = managed 1 in let yesResult = unitValue 2 in let noResult = unitValue 3 in let result = unitValue 4 in
 let branch = H.Branch (result, condition, block [] [leaf [first, true] [yesResult]] yesResult, block [] [leaf [second, true] [noResult]] noResult) in
 let source = definition "branch" [parameter "first" first; parameter "second" second] [branch] result in
 match elaborate [source] with Error error -> Error error | Ok analysis -> match findFunction "branch" analysis with
 | None -> Error "Elaboration omitted the branch function"
 | Some owned -> match owned.O.definition.H.body.O.body.H.operations with
   | [O.Evaluate (H.Branch (_, _, yes, no))] when yes.O.body.H.operations = [O.Drop second.H.id; O.Evaluate (leaf [first, true] [yesResult])] && no.O.body.H.operations = [O.Drop first.H.id; O.Evaluate (leaf [second, true] [noResult])] -> Ok ()
   | actual -> Error ("Expected branch-edge cleanup for the opposite consumed input, got " ^ showSteps actual)
let testPreservesLiveBranchResults () =
 let first = managed 0 in let second = managed 1 in let selected = managed 2 in let result = unitValue 3 in
 let branch = H.Branch (selected, condition, block [] [] first, block [] [] second) in
 let source = definition "select" [parameter "first" first; parameter "second" second] [branch; scalar result ["first", first; "second", second]] result in
 match elaborate [source] with Error error -> Error error | Ok analysis -> match findFunction "select" analysis with
 | None -> Error "Elaboration omitted the select function"
 | Some owned -> match owned.O.definition.H.body.O.body.H.operations with
   | O.Evaluate (H.Branch (_, _, yes, no)) :: _ when yes.O.body.H.operations = [O.Dup first.H.id] && no.O.body.H.operations = [O.Dup second.H.id] -> Ok ()
   | actual -> Error ("Expected branch results that remain live to be duplicated on each edge, got " ^ showSteps actual)
let testDuplicatesLiveValueBeforeConsumingCall () =
 let sinkInput = managed 0 in let sinkResult = unitValue 1 in let sink = definition "sink" [parameter "input" sinkInput] [leaf [sinkInput, true] [sinkResult]] sinkResult in
 let input = managed 0 in let callResult = unitValue 1 in let result = unitValue 2 in
 let caller = definition "caller" [parameter "input" input] [call "sink" [input] callResult; scalar result ["input", input]] result in
 match elaborate [sink; caller] with Error error -> Error error | Ok analysis -> match findFunction "caller" analysis with
 | None -> Error "Elaboration omitted the caller"
 | Some owned -> match owned.O.definition.H.body.O.body.H.operations with
   | [O.Dup id; O.Evaluate (H.Call _); O.Evaluate (H.ScalarBinding _); O.Drop dropped] when id = input.H.id && dropped = input.H.id -> Ok ()
   | actual -> Error ("Expected a duplicate before the consuming call and cleanup after the final borrow, got " ^ showSteps actual)
let testCleansUnusedManagedResults () =
 let sourceInput = managed 0 in let produced = managed 1 in let callee = definition "produce" [parameter "input" sourceInput] [scalar produced ["input", sourceInput]] produced in
 let input = managed 0 in let unused = managed 1 in let result = unitValue 2 in let caller = definition "discard" [parameter "input" input] [call "produce" [input] unused; leaf [] [result]] result in
 let scalarParameter = unitValue 0 in let unusedScalar = managed 1 in let scalarResult = unitValue 2 in
 let scalarDiscard = definition "discardScalar" [parameter "unit" scalarParameter] [scalar unusedScalar []; leaf [] [scalarResult]] scalarResult in
 match elaborate [callee; caller; scalarDiscard] with Error error -> Error error | Ok analysis -> match findFunction "discard" analysis, findFunction "discardScalar" analysis with
 | Some callOwned, Some scalarOwned -> (match callOwned.O.definition.H.body.O.body.H.operations, scalarOwned.O.definition.H.body.O.body.H.operations with
   | [O.Evaluate (H.Call _); O.Drop callId; O.Evaluate (H.Leaf _)], [O.Evaluate (H.ScalarBinding _); O.Drop scalarId; O.Evaluate (H.Leaf _)] when callId = unused.H.id && scalarId = unusedScalar.H.id -> Ok ()
   | actual -> Error ("Expected immediate cleanup of unused call and scalar results, got " ^ StructuralFormat.format (StructuralValue.Tuple [F.steps svLeaf svId (fst actual); F.steps svLeaf svId (snd actual)])))
 | _ -> Error "Elaboration omitted a discard function"
let testStabilizesRecursiveBoundaries () =
 let input = managed 0 in let recursiveResult = managed 1 in let loop = definition "loop" [parameter "input" input] [call "loop" [input] recursiveResult] recursiveResult in
 match elaborate [loop] with Error error -> Error error | Ok analysis -> match findFunction "loop" analysis with
 | Some owned when owned.O.ownership = boundary [O.ConsumedParameter input.H.id] (O.ProducedResult recursiveResult.H.id) -> Ok ()
 | actual -> Error ("Expected the recursive boundary to reach a stable transfer contract, got " ^ showOwned actual)
let consumed analysis name (input : H.value) = match findFunction name analysis with Some owned -> owned.O.ownership = boundary [O.ConsumedParameter input.H.id] O.UnmanagedResult | None -> false
let testStabilizesMutualRecursiveBoundaries () =
 let firstInput = managed 0 in let firstResult = unitValue 1 in let first = definition "first" [parameter "input" firstInput] [call "second" [firstInput] firstResult] firstResult in
 let secondInput = managed 0 in let secondResult = unitValue 1 in let second = definition "second" [parameter "input" secondInput] [leaf [secondInput, true] [secondResult]] secondResult in
 match elaborate [first; second] with Error error -> Error error | Ok analysis -> if consumed analysis "first" firstInput && consumed analysis "second" secondInput then Ok () else Error "Expected consumption to stabilize across the mutually visible call group"
let testStabilizesActualMutualRecursion () =
 let firstInput = managed 0 in let firstResult = unitValue 1 in let first = definition "recursiveFirst" [parameter "input" firstInput] [call "recursiveSecond" [firstInput] firstResult] firstResult in
 let secondInput = managed 0 in let secondResult = unitValue 1 in let second = definition "recursiveSecond" [parameter "input" secondInput] [call "recursiveFirst" [secondInput] secondResult] secondResult in
 match elaborate [first; second] with Error error -> Error error | Ok analysis -> if consumed analysis "recursiveFirst" firstInput && consumed analysis "recursiveSecond" secondInput then Ok () else Error "Expected a mutually recursive ownership group to converge atomically"
let testInfersDeepAcyclicBoundariesCalleeFirst () =
 let functionCount = 256 in let name index = Printf.sprintf "ownershipChain%04i" index in
 let definitions = List.init functionCount (fun index -> let input = managed 0 in let result = unitValue 1 in let operations = if index + 1 < functionCount then [call (name (index + 1)) [input] result] else [scalar result ["input", input]] in definition (name index) [parameter "input" input] operations result) in
 match elaborate definitions with Error error -> Error error | Ok analysis -> if List.for_all (fun (owned : (testLeaf, H.valueId) O.functionDef) -> owned.O.ownership = boundary [O.BorrowedParameter (H.ValueId 0)] O.UnmanagedResult) (E.functions analysis) then Ok () else Error "Expected deep acyclic ownership boundaries to propagate callee-first"
let testRejectsMissingCallOwnership () =
 let input = managed 0 in let result = managed 1 in let source = definition "caller" [parameter "input" input] [call "missing" [input] result] result in
 match E.elaborateFunctions dialect [source] with
 | Error (E.UnknownCallOwnership target) when target = TestIds.functionIdForName "missing" -> Ok ()
 | actual -> Error ("Expected explicit rejection of missing call ownership, got " ^ (match actual with Error error -> "Error (" ^ errorToString error ^ ")" | Ok analysis -> "Ok " ^ StructuralFormat.format (StructuralValue.Record ["Functions", StructuralValue.Sequence (List.map (F.functionDef svLeaf svId) (E.functions analysis)); "Semantics", StructuralValue.Scalar "<fun>"])))
let testRejectsMismatchedCallOwnership () =
 let input = managed 0 in let result = unitValue 1 in let target = TestIds.functionIdForName "external" in
 let source = definition "caller" [parameter "input" input] [call "external" [input] result] result in
 let mismatchedDialect = {dialect with E.externalCallOwnership = (fun call -> if call.H.target = target then Some ({O.parameters = [O.UnmanagedCallParameter]; result = O.UnmanagedCallResult} : O.callSignature) else None)} in
 match E.elaborateFunctions mismatchedDialect [source] with
 | Error (E.InconsistentCallParameters actual) when actual = target -> Ok ()
 | actual -> Error ("Expected explicit rejection of mismatched call ownership, got " ^ (match actual with Error error -> "Error (" ^ errorToString error ^ ")" | Ok analysis -> "Ok " ^ StructuralFormat.format (StructuralValue.Record ["Functions", StructuralValue.Sequence (List.map (F.functionDef svLeaf svId) (E.functions analysis)); "Semantics", StructuralValue.Scalar "<fun>"])))
let tests = [
 "Whole-function ownership infers borrowed opaque inputs", testInfersBorrowedOpaqueInput;
 "Whole-function ownership infers consumed inputs", testInfersConsumedInput;
 "Whole-function ownership inserts branch-edge cleanup", testDropsOppositeBranchInputs;
 "Whole-function ownership preserves live branch results", testPreservesLiveBranchResults;
 "Whole-function ownership duplicates values live after consuming calls", testDuplicatesLiveValueBeforeConsumingCall;
 "Whole-function ownership cleans unused managed results", testCleansUnusedManagedResults;
 "Whole-function ownership stabilizes recursive boundaries", testStabilizesRecursiveBoundaries;
 "Whole-function ownership stabilizes mutual call boundaries", testStabilizesMutualRecursiveBoundaries;
 "Whole-function ownership stabilizes an actual recursive group", testStabilizesActualMutualRecursion;
 "Whole-function ownership infers a deep call chain callee-first", testInfersDeepAcyclicBoundariesCalleeFirst;
 "Whole-function ownership rejects calls without contracts", testRejectsMissingCallOwnership;
 "Whole-function ownership rejects mismatched call contracts", testRejectsMismatchedCallOwnership
]
