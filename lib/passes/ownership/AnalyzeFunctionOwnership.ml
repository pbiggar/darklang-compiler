(* AnalyzeFunctionOwnership.fs - Schedule verified whole-function ownership over checked HIR. *)
[@@@warning "-4-42"]
module H = HIR
module O = OwnedIR
module C = ConstructHIRFunctions
module E = ElaborateFunctionOwnership
module S = ScheduleOwnershipVariants
module Ownership = O.Make (ListLiveness.Identity)
module Verification = VerifyOwnedHIR.Make (ListLiveness.Identity)
module Verify = VerifyOwnership.Make (ListLiveness.Identity)
module Scheduler = S.Make (ListLiveness.Identity)
let ( let* ) = Result.bind
type context = {typeReg : TypeRegistries.typeRegistry; typeNames : TypeRegistries.typeNameRegistry; recordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParamsReg : string list StringOrder.Map.t; variantLookup : LoweringPrimitives.variantLookup; sumMetadata : LoweringPrimitives.sumMetadata; rcSumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; functionNames : TypeRegistries.functionNameRegistry; moduleRegistry : AST.moduleRegistry}
type analysisError = HIRConstructionFailed of C.constructionError | OwnershipElaborationFailed of E.elaborationError | OwnedHIRVerificationFailed of Verification.verificationError | FunctionOwnershipVerificationFailed of AST.functionId * Ownership.verificationError | SpecializationSchedulingFailed of Scheduler.schedulingError
type analysis = {ownership : C.primitive E.analysis; hir : C.primitive VerifyOwnedHIR.hirContracts; schedule : (C.primitive, H.valueId) S.plan}
let functions analysis = S.functions analysis.schedule
let semantics analysis = Scheduler.ownershipSemantics analysis.schedule (E.semantics analysis.ownership)
let hirContracts analysis = S.hirContracts analysis.schedule analysis.hir
let schedule analysis = analysis.schedule
let originalFunctions analysis = E.functions analysis.ownership
let callContract isManaged (call : H.functionCall) : H.primitiveContract = {H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = if isManaged call.H.result then H.UnknownManagedAlias else H.NoManagedAlias}]; effects = H.EffectSet.singleton H.MayInvokeUserCode}
(*
   HIR construction and ownership verification ask about the same semantic
   types repeatedly. Keep their representation decisions in this analysis.
*)
let analyzeWithTrace recordTiming context functions =
 let measure name operation = let start = HostClock.milliseconds () in let result = operation () in let elapsed = HostClock.milliseconds () -. start in Option.iter (fun record -> record name elapsed) recordTiming; result in
 let managedTypes = Hashtbl.create 32 in
 let isManaged (value : H.value) = match Hashtbl.find_opt managedTypes value.H.typ with Some managed -> managed | None ->
  let managed = MemoryPlanning.rcShapeOfTypeWithSums context.recordFieldsReg context.recordTypeParamsReg context.rcSumShapeReg value.H.typ |> MemoryPlanning.rcShapeNeedsOwnedScopeRelease in Hashtbl.replace managedTypes value.H.typ managed; managed in
 let infer types expression = LoweringTypeInference.inferTypeCore context.sumMetadata context.typeNames expression types context.typeReg context.variantLookup context.funcReg context.functionNames context.moduleRegistry in
 let calls : C.callContracts = {C.externalSignature = (fun _ -> None); contract = (fun _ -> Some (callContract isManaged))} in
 let* definitions = measure "Ownership detail: HIR construction" (fun () -> C.constructFunctionsWithOpaqueFallback context.functionNames infer (fun expression -> ClosureAnalysis.freeVars expression ClosureAnalysis.BindingSet.empty) calls functions) |> Result.map_error (fun error -> HIRConstructionFailed error) in
 let leafOwnership primitive : H.valueId O.contract =
  let contract = C.primitiveContract primitive in
  let inputs = match primitive with
   | C.ListTransform (_, input, _) -> List.filter_map (fun (value : H.value) -> if value.H.id = input.H.id then Some (O.Consumed input.H.id) else if isManaged value then Some (O.Borrowed value.H.id) else None) contract.H.inputs
   | C.Literal _ | C.Unary _ | C.Binary _ | C.FreshManaged _ -> List.filter_map (fun (value : H.value) -> if isManaged value then Some (O.Borrowed value.H.id) else None) contract.H.inputs in
  let outputs = List.filter_map (fun (output : H.outputContract) -> if isManaged output.H.value then Some output.H.value.H.id else None) contract.H.outputs in
  {O.inputs; outputs} in
 let dialect : (C.primitive, C.block) E.dialect = {E.body = C.body; leafOwnership;
  leafUniqueness = (fun primitive -> match primitive with C.FreshManaged (output, _) | C.ListTransform (output, _, _) -> {Ownership.requiredInputs = H.ValueSet.empty; uniqueOutputs = H.ValueSet.singleton output.H.id} | C.Literal _ | C.Unary _ | C.Binary _ -> {Ownership.requiredInputs = H.ValueSet.empty; uniqueOutputs = H.ValueSet.empty}); isManaged; externalCallOwnership = (fun _ -> None)} in
 let* analysis = E.elaborateFunctionsWithTrace recordTiming dialect definitions |> Result.map_error (fun error -> OwnershipElaborationFailed error) in
 let hir : C.primitive VerifyOwnedHIR.hirContracts = {VerifyOwnedHIR.leaf = C.primitiveContract; callSignature = (fun _ -> None); callContract = (fun call -> Some (callContract isManaged call))} in
 let ownedFunctions = E.functions analysis in let ownership = E.semantics analysis in
 let* () = measure "Ownership detail: Initial HIR and ownership verification" (fun () -> Verification.verifyFunctions hir ownership ownedFunctions) |> Result.map_error (fun error -> match error with
  | Verification.OwnershipVerificationFailed _ -> Option.value (List.find_map (fun (definition : (C.primitive, H.valueId) O.functionDef) -> match Verify.verifyFunction ownership definition.O.ownership definition.O.definition.H.body with Ok () -> None | Error functionError -> Some (FunctionOwnershipVerificationFailed (definition.O.definition.H.id, functionError))) ownedFunctions) ~default:(OwnedHIRVerificationFailed error)
  | Verification.HIRVerificationFailed _ -> OwnedHIRVerificationFailed error) in
 let* scheduled = measure "Ownership detail: Specialization scheduling" (fun () -> Scheduler.scheduleWithTrace recordTiming S.defaultLimits hir ownership context.functionNames ownedFunctions) |> Result.map_error (fun error -> SpecializationSchedulingFailed error) in
 Ok {ownership = analysis; hir; schedule = scheduled}
let analyze context functions = analyzeWithTrace None context functions
