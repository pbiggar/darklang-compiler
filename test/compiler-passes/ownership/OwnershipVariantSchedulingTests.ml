(* OwnershipVariantSchedulingTests.fs - Fixed-point ownership specialization scheduling laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module O = OwnedIR
module H = HIR
module A = ANF
module M = MaterializeOwnershipVariants
module S = ScheduleOwnershipVariants
module Scheduler = S.Make (ListLiveness.Identity)
module Lowering = LowerOwnershipVariants.Make (ListLiveness.Identity)
module Ownership = Scheduler.Ownership
let ( let* ) = Result.bind
type leaf = Fresh of H.value [@@warning "-37"]
let fid = TestIds.functionIdForName
let value id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let parameter (value : H.value) : H.parameter = let H.ValueId id = value.H.id in {H.binding = AST.bindingId id; value}
let signature parameters result : H.valueId O.functionSignature = {O.parameters; result}
let block parameters operations result : (leaf, H.valueId) O.block = {O.body = {H.parameters = List.map parameter parameters; operations; result}}
let definition name ownership body : (leaf, H.valueId) O.functionDef = {O.definition = {H.id = fid name; name; body}; ownership}
let call target argument result : H.functionCall = {H.target = fid target; arguments = [argument]; result}
let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty}
let semantics : leaf Ownership.semantics = {Ownership.leaf = (fun (Fresh output) -> {O.inputs = []; outputs = [output.H.id]});
 leafUniqueness = (fun (Fresh output) -> {Ownership.requiredInputs = H.ValueSet.empty; uniqueOutputs = H.ValueSet.singleton output.H.id});
 callOwnership = (fun _ -> None); scalarUses = (fun operand -> H.ValueSet.of_list (List.map (fun (_, (input : H.value)) -> input.H.id) (CheckedAST.BindingIdMap.bindings operand.H.inputs)));
 scalarEscapes = (fun _ -> H.ValueSet.empty); blockArgument = (fun (input : H.value) -> if input.H.typ = AST.TUnit then O.Unmanaged else O.Managed input.H.id)}
let contracts : leaf VerifyOwnedHIR.hirContracts = {VerifyOwnedHIR.leaf = (fun (Fresh output) -> {H.inputs = []; operands = []; outputs = [{H.value = output; alias = H.FreshManaged}]; effects = H.EffectSet.singleton H.MayAllocate}); callSignature = (fun _ -> None);
 callContract = (fun invocation -> match invocation.H.arguments with first :: rest -> Some {H.inputs = invocation.H.arguments; operands = []; outputs = [{H.value = invocation.H.result; alias = H.MayAliasInputs (first, rest)}]; effects = H.EffectSet.singleton H.MayInvokeUserCode} | [] -> None)}
let fixture () =
 let calleeInput = value 0 in let identity = definition "identity" (signature [O.ConsumedParameter calleeInput.H.id] (O.ProducedResult calleeInput.H.id)) (block [calleeInput] [] calleeInput) in
 let input, middle, output = value 10, value 11, value 12 in let first, second = call "identity" input middle, call "identity" middle output in
 let caller = definition "caller" (signature [O.UniqueParameter input.H.id] (O.ProducedResult output.H.id)) (block [input] [O.Evaluate (H.Call first); O.Evaluate (H.Call second)] output) in [identity; caller], first, second
let svId (H.ValueId id) = StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])
let errorLabel = function
 | Scheduler.InvalidLimits _ -> "InvalidLimits"
 | Scheduler.InvalidFunctionBoundary (_, cause) -> "InvalidFunctionBoundary: " ^ Ownership.errorToString svId cause
 | Scheduler.InferenceFailed _ -> "InferenceFailed"
 | Scheduler.CatalogFailed _ -> "CatalogFailed"
 | Scheduler.AnalysisFailed _ -> "AnalysisFailed"
 | Scheduler.SelectionFailed _ -> "SelectionFailed"
 | Scheduler.MaterializationFailed _ -> "MaterializationFailed"
 | Scheduler.IterationLimitExceeded count -> "IterationLimitExceeded " ^ string_of_int count
 | Scheduler.GeneratedGroupLimitExceeded count -> "GeneratedGroupLimitExceeded " ^ string_of_int count
 | Scheduler.RewrittenCallLimitExceeded count -> "RewrittenCallLimitExceeded " ^ string_of_int count
 | Scheduler.MissingOriginalCall _ -> "MissingOriginalCall"
let run definitions = Scheduler.schedule S.defaultLimits contracts semantics FunctionIdMap.empty definitions |> Result.map_error errorLabel
let testPropagatesUniquenessToFixedPoint () = let definitions, first, second = fixture () in let* plan = run definitions in
 let sites = M.rewrites (S.materialization plan) |> List.map (fun rewrite -> rewrite.M.site.O.result) |> H.ValueSet.of_list in
 if H.ValueSet.equal sites (H.ValueSet.of_list [first.H.result.H.id; second.H.result.H.id]) && List.length (S.iterations plan) >= 2 then Ok () else Error ("Expected both calls to specialize across iterations, got set " ^ HostStructuralFormat.format (StructuralValue.Sequence (List.map svId (H.ValueSet.elements sites))))
let testBoundsConvergence () = let definitions, _, _ = fixture () in let limits = {S.defaultLimits with S.maxIterations = 1} in
 match Scheduler.schedule limits contracts semantics FunctionIdMap.empty definitions with Error (Scheduler.IterationLimitExceeded 1) -> Ok () | Error error -> Error ("Expected the scheduler iteration bound, got " ^ errorLabel error) | Ok _ -> Error "Expected the scheduler iteration bound, got Ok"
let testSkipsUnusedWideVariantSearch () =
 let unitInput : H.value = {H.id = H.ValueId 100; typ = AST.TUnit} in let managedInputs = List.init 9 value in
 let wide = definition "unusedWide" (signature (O.UnmanagedParameter :: List.map (fun (input : H.value) -> O.ConsumedParameter input.H.id) managedInputs) O.UnmanagedResult) (block (unitInput :: managedInputs) (List.map (fun (input : H.value) -> O.Drop input.H.id) managedInputs) unitInput) in
 let* plan = run [wide] in let materialized = S.materialization plan in if M.groups materialized = [] && M.rewrites materialized = [] then Ok () else Error "Expected an uncalled wide function to produce no ownership variants"
let testEmptyDemandStillValidatesMaterialization () = let input = value 40 in let invalid = definition "invalid" (signature [O.ConsumedParameter input.H.id] (O.ProducedResult input.H.id)) (block [input] [O.Drop input.H.id] input) in
 match Scheduler.schedule S.defaultLimits contracts semantics FunctionIdMap.empty [invalid] with Error (Scheduler.MaterializationFailed (Scheduler.Materialize.InvalidOriginalProgram _)) -> Ok () | Error error -> Error ("Expected empty-demand scheduling to run materialization validation, got " ^ errorLabel error) | Ok _ -> Error "Expected empty-demand scheduling to run materialization validation, got Ok"
let testSchedulesRecursiveDemandAtomically () = let loopInput, recursiveResult, loopResult = value 20, value 21, value 22 in
 let recursiveCall = call "loop" loopInput recursiveResult in let recursiveBranch = block [] [O.Evaluate (H.Call recursiveCall)] recursiveResult in let baseBranch = block [] [] loopInput in
 let loop = definition "loop" (signature [O.ConsumedParameter loopInput.H.id] (O.ProducedResult loopResult.H.id)) (block [loopInput] [O.Evaluate (H.Branch (loopResult, condition, recursiveBranch, baseBranch))] loopResult) in
 let callerInput, callerResult = value 30, value 31 in let externalCall = call "loop" callerInput callerResult in
 let caller = definition "recursiveCaller" (signature [O.UniqueParameter callerInput.H.id] (O.ProducedResult callerResult.H.id)) (block [callerInput] [O.Evaluate (H.Call externalCall)] callerResult) in
 let* plan = run [loop; caller] in let materialized = S.materialization plan in
 match M.groups materialized, M.rewrites materialized with [group], [rewrite] when List.length (NonEmptyList.toList group.M.members) = 1 && rewrite.M.site.O.result = externalCall.H.result.H.id && rewrite.M.ownership = {O.parameters = [O.UniqueCallParameter]; result = O.UniqueProducedCallResult} -> Ok () | _ -> Error "Expected one atomic recursive specialization"
let testLowersSpecializedCallsAndContracts () = let definitions, _, _ = fixture () in let* scheduled = run definitions in
 let identity : A.functionDef = {A.id = fid "identity"; name = "identity"; typedParams = [{A.id = A.TempId 0; typ = AST.TList AST.TInt64}]; returnType = AST.TList AST.TInt64; returnOwnership = A.OwnedReturn; body = A.Return (A.Var (A.TempId 0))} in
 let caller : A.functionDef = {A.id = fid "caller"; name = "caller"; typedParams = [{A.id = A.TempId 10; typ = AST.TList AST.TInt64}]; returnType = AST.TList AST.TInt64; returnOwnership = A.OwnedReturn;
 body = A.Let (A.TempId 11, A.Call (fid "identity", [A.Var (A.TempId 10)]), A.Let (A.TempId 12, A.Call (fid "identity", [A.Var (A.TempId 11)]), A.Return (A.Var (A.TempId 12))))} in
 let* lowered = Lowering.lower definitions (S.materialization scheduled) [identity; caller] (A.VarGen 100) LowerOwnershipVariants.SiteSet.empty |> Result.map_error (function LowerOwnershipVariants.MissingSourceFunction _ -> "MissingSourceFunction" | LowerOwnershipVariants.MissingSourceCalls _ -> "MissingSourceCalls" | LowerOwnershipVariants.InvalidOwnershipBoundary _ -> "InvalidOwnershipBoundary") in
 let cloneContracts = FunctionIdMap.toList lowered.LowerOwnershipVariants.contracts in
 let rec targets = function A.Let (_, A.Call (target, _), body) -> target :: targets body | A.Let (_, _, body) -> targets body | A.If (_, yes, no) -> let yes = targets yes in let no = targets no in yes @ no | A.Join (_, continuation, entry) -> let entry = targets entry in let continuation = targets continuation in entry @ continuation | A.Return _ | A.Jump _ -> [] in
 let rewrittenTargets = match List.find_opt (fun (definition : A.functionDef) -> definition.A.id = caller.A.id) lowered.LowerOwnershipVariants.functions with Some definition -> targets definition.A.body | None -> [] in
 match cloneContracts, rewrittenTargets with [(clone, boundary)], [first; second] when clone = first && first = second && boundary = {O.parameters = [O.UniqueCallParameter]; result = O.UniqueProducedCallResult} && List.length lowered.LowerOwnershipVariants.functions = 3 -> Ok () | _ -> Error "Expected one explicit ANF clone contract and two routed calls"
let tests = [
 "Ownership specialization propagates uniqueness to a fixed point", testPropagatesUniquenessToFixedPoint;
 "Ownership specialization has an explicit convergence bound", testBoundsConvergence;
 "Ownership specialization skips unused wide functions", testSkipsUnusedWideVariantSearch;
 "Ownership specialization validates empty-demand materialization", testEmptyDemandStillValidatesMaterialization;
 "Ownership specialization schedules recursive demand atomically", testSchedulesRecursiveDemandAtomically;
 "Ownership specialization lowers clones calls and contracts into ANF", testLowersSpecializedCallsAndContracts
]
