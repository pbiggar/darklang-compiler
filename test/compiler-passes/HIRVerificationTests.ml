(* HIRVerificationTests.ml - Normalized value identity and structured-edge verifier laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module V = VerifyHIR
let fid = TestIds.functionIdForName
type testBlock = TestBlock of (H.primitiveContract, testBlock) H.operation H.block
let value id typ : H.value = {H.id = H.ValueId id; typ}
let literal typ expression : H.operand = {H.expression; typ; inputs = CheckedAST.BindingIdMap.empty}
let binding name = Text.scalars name |> Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l |> Int32.to_int |> AST.bindingId
let reference name input typ : H.operand = let id = binding name in {H.expression = CheckedAST.Local id; typ; inputs = CheckedAST.BindingIdMap.singleton id input}
let namedParameter name value : H.parameter = {H.binding = binding name; value}
let orderedBlock parameters operations result = TestBlock {H.parameters; operations; result}
(*
   Each branch produces the binding consumed by the enclosing continuation.
   Keeping the continuation in this sequence avoids duplicating it per path.
*)
let block parameters operations result = orderedBlock (CheckedAST.BindingIdMap.bindings parameters |> List.map (fun (binding, value) -> {H.binding; value})) operations result
let leaf inputs operands outputs = let outputs = List.map (fun value -> {H.value; alias = H.NoManagedAlias}) outputs in H.Leaf {H.inputs; operands; outputs; effects = H.EffectSet.empty}
let contractedWithOperands inputs operands outputs effects = H.Leaf {H.inputs; operands; outputs; effects = H.EffectSet.of_list effects}
let contracted inputs outputs effects = contractedWithOperands inputs [] outputs effects
let signature target = if target = fid "callee" || target = fid "recursive" || target = fid "uncontracted" then Some ({H.parameters = [AST.TInt64]; result = AST.TBool} : H.functionSignature) else None
let callContract (call : H.functionCall) = if call.H.target = fid "callee" || call.H.target = fid "recursive" || call.H.target = fid "derivedRecursive" then Some ({H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = H.NoManagedAlias}]; effects = H.EffectSet.singleton H.MayInvokeUserCode} : H.primitiveContract) else None
let dialect callSignature : (H.primitiveContract, testBlock) V.dialect = {V.body = (fun (TestBlock body) -> body); leaf = Fun.id; callSignature; callContract}
let display = function Ok () -> "Ok ()" | Error error -> "Error (" ^ V.errorToString error ^ ")"
let check expected root () = let actual = V.verify (dialect signature) root in if actual = expected then Ok () else Error ("Expected " ^ display expected ^ ", got " ^ display actual)
let checkFunctions expected callSignature definitions () = let actual = V.verifyFunctions (dialect callSignature) definitions in if actual = expected then Ok () else Error ("Expected " ^ display expected ^ ", got " ^ display actual)
let tests =
 let parameter = value 0 AST.TInt64 in let result = value 1 AST.TInt64 in let condition = literal AST.TBool (CheckedAST.BoolLiteral true) in
 let branchResult = value 2 AST.TInt64 in let branchLocal = value 3 AST.TInt64 in let managedInput = value 5 (AST.TList AST.TInt64) in let managedResult = value 6 (AST.TList AST.TInt64) in let callResult = value 7 AST.TBool in
 let call target arguments result = H.Call {H.target = fid target; arguments; result} in
 let parameters name value = CheckedAST.BindingIdMap.singleton (binding name) value in
 ["HIR accepts normalized parameter and operand identities", check (Ok ()) (block (parameters "input" parameter) [H.ScalarBinding (result, reference "input" parameter AST.TInt64)] result);
 "HIR rejects an operand with an unknown identity", check (Error (V.UnknownValue parameter.H.id)) (block CheckedAST.BindingIdMap.empty [H.ScalarBinding (result, reference "input" parameter AST.TInt64)] result);
 "HIR rejects sibling definitions with the same identity", check (Error (V.DuplicateDefinition branchLocal.H.id)) (block CheckedAST.BindingIdMap.empty [H.Branch (branchResult, condition, block CheckedAST.BindingIdMap.empty [leaf [] [] [branchLocal]] branchLocal, block CheckedAST.BindingIdMap.empty [leaf [] [] [branchLocal]] branchLocal)] branchResult);
 "HIR rejects a branch result whose type disagrees with its target", check (Error (V.InconsistentBranchResult branchResult.H.id)) (let boolean = value 4 AST.TBool in block CheckedAST.BindingIdMap.empty [H.Branch (branchResult, condition, block CheckedAST.BindingIdMap.empty [leaf [] [] [branchLocal]] branchLocal, block CheckedAST.BindingIdMap.empty [leaf [] [] [boolean]] boolean)] branchResult);
 "HIR accepts a result that may reuse a typed input", check (Ok ()) (block (parameters "input" managedInput) [contracted [managedInput] [{H.value = managedResult; alias = H.MayReuseInput managedInput}] [H.ReadsOwnedStorage;H.WritesOwnedStorage]] managedResult);
 "HIR rejects reuse provenance outside primitive inputs", check (Error (V.InvalidAliasSource (managedResult.H.id, managedInput.H.id))) (block (parameters "input" managedInput) [contracted [] [{H.value = managedResult; alias = H.MayReuseInput managedInput}] []] managedResult);
 "HIR rejects reuse provenance with incompatible types", check (Error (V.IncompatibleAliasTypes (result.H.id, managedInput.H.id))) (block (parameters "input" managedInput) [contracted [managedInput] [{H.value = result; alias = H.MayReuseInput managedInput}] []] result);
 "HIR rejects duplicate may-alias candidates", check (Error (V.DuplicateAliasSource (managedResult.H.id, managedInput.H.id))) (block (parameters "input" managedInput) [contracted [managedInput] [{H.value = managedResult; alias = H.MayAliasInputs (managedInput, [managedInput])}] []] managedResult);
 "HIR rejects unaccounted opaque operand effects", check (Error V.UnaccountedOpaqueEffects) (block CheckedAST.BindingIdMap.empty [contractedWithOperands [] [literal AST.TInt64 (CheckedAST.Int64Literal 1L)] [{H.value = result; alias = H.NoManagedAlias}] []] result);
 "HIR accepts registered direct calls", check (Ok ()) (block (parameters "input" parameter) [call "callee" [parameter] callResult] callResult);
 "HIR resolves recursive calls through an explicit registry entry", check (Ok ()) (block (parameters "input" parameter) [call "recursive" [parameter] callResult] callResult);
 "HIR rejects calls without a typed registry entry", check (Error (V.UnknownCallTarget (fid "opaque"))) (block (parameters "input" parameter) [call "opaque" [parameter] callResult] callResult);
 "HIR rejects registered calls without effect and alias contracts", check (Error (V.MissingCallContract (fid "uncontracted"))) (block (parameters "input" parameter) [call "uncontracted" [parameter] callResult] callResult);
 "HIR rejects direct-call argument count mismatches", check (Error (V.InvalidCallArgumentCount (fid "callee"))) (block CheckedAST.BindingIdMap.empty [call "callee" [] callResult] callResult);
 "HIR rejects direct-call argument type mismatches", check (Error (V.InvalidCallArgumentType (fid "callee", 0))) (let boolean = value 8 AST.TBool in block (parameters "input" boolean) [call "callee" [boolean] callResult] callResult);
 "HIR rejects direct-call result type mismatches", check (Error (V.InvalidCallResultType (fid "callee"))) (let invalidResult = value 9 AST.TInt64 in block (parameters "input" parameter) [call "callee" [parameter] invalidResult] invalidResult);
 "HIR rejects duplicate ordered parameter bindings", check (Error (V.DuplicateParameterBinding (binding "input"))) (orderedBlock [namedParameter "input" parameter;namedParameter "input" (value 10 AST.TBool)] [] parameter);
 "HIR function signatures retain parameter order", (fun () -> let boolean = value 10 AST.TBool in let definition : testBlock H.functionDef = {H.id = fid "ordered"; name = "ordered"; body = orderedBlock [namedParameter "integer" parameter;namedParameter "boolean" boolean] [] boolean} in let actual = V.functionSignature (dialect (fun _ -> None)) definition in let expected : H.functionSignature = {H.parameters = [AST.TInt64;AST.TBool]; result = AST.TBool} in if actual = expected then Ok () else let display (signature : H.functionSignature) = StructuralFormat.format (StructuralValue.Record ["Parameters", StructuralValue.Sequence (List.map StructuralFormat.semanticValue signature.H.parameters); "Result", StructuralFormat.semanticValue signature.H.result]) in Error ("Expected " ^ display expected ^ ", got " ^ display actual));
 "HIR function groups derive recursive call signatures", checkFunctions (Ok ()) (fun _ -> None) [{H.id = fid "derivedRecursive"; name = "derivedRecursive"; body = orderedBlock [namedParameter "input" parameter] [call "derivedRecursive" [parameter] callResult] callResult}];
 "HIR function groups reject duplicate definitions", checkFunctions (Error (V.DuplicateFunctionName (fid "duplicate"))) (fun _ -> None) [{H.id = fid "duplicate"; name = "duplicate"; body = block (parameters "result" result) [] result};{H.id = fid "duplicate"; name = "duplicate"; body = block (parameters "result" result) [] result}];
 "HIR function groups reject conflicting registered signatures", checkFunctions (Error (V.InconsistentRegisteredFunctionSignature (fid "derivedRecursive"))) (fun target -> if target = fid "derivedRecursive" then Some ({H.parameters = [AST.TBool]; result = AST.TBool} : H.functionSignature) else None) [{H.id = fid "derivedRecursive"; name = "derivedRecursive"; body = orderedBlock [namedParameter "input" parameter] [leaf [] [] [callResult]] callResult}]
 ]
