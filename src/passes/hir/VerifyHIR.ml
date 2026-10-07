(* VerifyHIR.ml - Verify normalized HIR value identities and structured control-flow edges. *)
type verificationError = UnknownValue of HIR.valueId | DuplicateDefinition of HIR.valueId | DuplicateParameterBinding of AST.bindingId | DuplicateFunctionName of AST.functionId | InconsistentValueType of HIR.valueId | BindingTypeMismatch of HIR.valueId | InvalidBranchCondition of AST.semanticType | InconsistentBranchResult of HIR.valueId | InvalidAliasSource of HIR.valueId * HIR.valueId | IncompatibleAliasTypes of HIR.valueId * HIR.valueId | DuplicateAliasSource of HIR.valueId * HIR.valueId | UnaccountedOpaqueEffects | UnknownCallTarget of AST.functionId | MissingCallContract of AST.functionId | InvalidCallArgumentCount of AST.functionId | InvalidCallArgumentType of AST.functionId * int | InvalidCallResultType of AST.functionId | InconsistentCallContract of AST.functionId | InconsistentRegisteredFunctionSignature of AST.functionId
type ('leaf, 'block) dialect = {body : 'block -> ('leaf, 'block) HIR.operation HIR.block; leaf : 'leaf -> HIR.primitiveContract; callSignature : AST.functionId -> HIR.functionSignature option; callContract : HIR.functionCall -> HIR.primitiveContract option}
[@@@warning "-4"]
module H = HIR
module M = H.ValueMap
let ( let* ) = Result.bind
let duplicate (type a) compare (values : a list) =
 let module Counts = Map.Make (struct type t = a let compare = compare end) in
 let counts = List.fold_left (fun counts value -> Counts.update value (fun count -> Some (Option.value count ~default:0 + 1)) counts) Counts.empty values in
 List.find_opt (fun value -> Counts.find value counts > 1) values
let compareValueId (H.ValueId a) (H.ValueId b) = Int.compare a b
let verify (dialect : ('leaf, 'block) dialect) root =
 let require visible (value : H.value) = match M.find_opt value.H.id visible with None -> Error (UnknownValue value.H.id) | Some typ when typ = value.H.typ -> Ok () | Some _ -> Error (InconsistentValueType value.H.id) in
 let requireMany visible values = List.fold_left (fun result value -> let* () = result in require visible value) (Ok ()) values in
 let operand visible (value : H.operand) = CheckedAST.BindingIdMap.bindings value.H.inputs |> List.map snd |> requireMany visible in
 let define declared visible (value : H.value) = if M.mem value.H.id declared then Error (DuplicateDefinition value.H.id) else Ok (M.add value.H.id value.H.typ declared, M.add value.H.id value.H.typ visible) in
 let defineMany declared visible values = List.fold_left (fun result value -> let* declared, visible = result in define declared visible value) (Ok (declared, visible)) values in
 let parameters (values : H.parameter list) = match duplicate AST.compareBindingId (List.map (fun (value : H.parameter) -> value.H.binding) values) with Some id -> Error (DuplicateParameterBinding id) | None -> Ok (List.map (fun (value : H.parameter) -> value.H.value) values) in
 let aliases (contract : H.primitiveContract) =
  let inputs = List.fold_left (fun inputs (value : H.value) -> M.add value.H.id value inputs) M.empty contract.H.inputs in
  let validate (output : H.outputContract) (source : H.value) = match M.find_opt source.H.id inputs with None -> Error (InvalidAliasSource (output.H.value.H.id, source.H.id)) | Some input when input.H.typ <> source.H.typ || output.H.value.H.typ <> source.H.typ -> Error (IncompatibleAliasTypes (output.H.value.H.id, source.H.id)) | Some _ -> Ok () in
  let validateMany (output : H.outputContract) first rest = let sources = first :: rest in match duplicate compareValueId (List.map (fun (value : H.value) -> value.H.id) sources) with Some source -> Error (DuplicateAliasSource (output.H.value.H.id, source)) | None -> List.fold_left (fun result source -> let* () = result in validate output source) (Ok ()) sources in
  List.fold_left (fun result (output : H.outputContract) -> let* () = result in match output.H.alias with H.NoManagedAlias | H.UnknownManagedAlias | H.FreshManaged -> Ok () | H.MayReuseInput source -> validate output source | H.MayAliasInputs (first, rest) -> validateMany output first rest) (Ok ()) contract.H.outputs in
 let effects (contract : H.primitiveContract) = if contract.H.operands = [] || H.EffectSet.mem H.MayEvaluateOpaqueSource contract.H.effects then Ok () else Error UnaccountedOpaqueEffects in
 let primitive declared visible (contract : H.primitiveContract) =
  let* () = requireMany visible contract.H.inputs in let* () = List.fold_left (fun result value -> let* () = result in operand visible value) (Ok ()) contract.H.operands in
  let* () = effects contract in let* () = aliases contract in defineMany declared visible (List.map (fun (output : H.outputContract) -> output.H.value) contract.H.outputs) in
 let call declared visible (call : H.functionCall) = match dialect.callSignature call.H.target with
 | None -> Error (UnknownCallTarget call.H.target)
 | Some signature when List.length signature.H.parameters <> List.length call.H.arguments -> Error (InvalidCallArgumentCount call.H.target)
 | Some signature ->
   let invalid = List.combine signature.H.parameters call.H.arguments |> List.mapi (fun index (expected, (actual : H.value)) -> index, expected, actual.H.typ) |> List.find_opt (fun (_, expected, actual) -> expected <> actual) in
   (match invalid with Some (index, _, _) -> Error (InvalidCallArgumentType (call.H.target, index)) | None when signature.H.result <> call.H.result.H.typ -> Error (InvalidCallResultType call.H.target) | None ->
    match dialect.callContract call with None -> Error (MissingCallContract call.H.target) | Some contract when contract.H.inputs <> call.H.arguments || contract.H.operands <> [] || List.map (fun (output : H.outputContract) -> output.H.value) contract.H.outputs <> [call.H.result] -> Error (InconsistentCallContract call.H.target) | Some contract -> primitive declared visible contract) in
 let rec operations declared visible = function [] -> Ok (declared, visible) | operation :: rest ->
  let next = match operation with
  | H.Leaf leaf -> let contract = dialect.leaf leaf in primitive declared visible contract
  | H.ScalarBinding (result, value) -> if result.H.typ <> value.H.typ then Error (BindingTypeMismatch result.H.id) else let* () = operand visible value in define declared visible result
  | H.Call value -> call declared visible value
  | H.Branch (result, condition, yes, no) -> if condition.H.typ <> AST.TBool then Error (InvalidBranchCondition condition.H.typ) else
    let* () = operand visible condition in let* afterYes, (yesResult : H.value) = block declared visible yes in let* afterNo, (noResult : H.value) = block afterYes visible no in
    if yesResult.H.typ <> result.H.typ || noResult.H.typ <> result.H.typ then Error (InconsistentBranchResult result.H.id) else define afterNo visible result in
  let* declared, visible = next in operations declared visible rest
 and block declared visible block = let body = dialect.body block in let* values = parameters body.H.parameters in
  let* declared, visible = defineMany declared visible values in let* declared, visible = operations declared visible body.H.operations in
  let* () = require visible body.H.result in Ok (declared, body.H.result) in
 let* _ = block M.empty M.empty root in Ok ()
let functionSignature (dialect : ('leaf, 'block) dialect) (definition : 'block H.functionDef) : H.functionSignature =
 let body = dialect.body definition.H.body in {H.parameters = List.map (fun (param : H.parameter) -> param.H.value.H.typ) body.H.parameters; result = body.H.result.H.typ}
let verifyFunction dialect (definition : 'block H.functionDef) = verify dialect definition.H.body
(*
   Verify a mutually visible function group. Internal typed signatures are
   derived from definitions; independently registered signatures for the same
   names must agree. Primitive call contracts remain a separate dialect input.
*)
let verifyFunctions dialect (definitions : 'block H.functionDef list) =
 match duplicate (fun left right -> Int64.unsigned_compare (AST.functionIdValue left) (AST.functionIdValue right)) (List.map (fun (definition : 'block H.functionDef) -> definition.H.id) definitions) with Some name -> Error (DuplicateFunctionName name) | None ->
 let signatures = FunctionIdMap.ofList (List.map (fun (definition : 'block H.functionDef) -> definition.H.id, functionSignature dialect definition) definitions) in
 let inconsistent = List.find_map (fun (definition : 'block H.functionDef) -> match dialect.callSignature definition.H.id with Some registered when registered <> functionSignature dialect definition -> Some (InconsistentRegisteredFunctionSignature definition.H.id) | _ -> None) definitions in
 match inconsistent with Some error -> Error error | None ->
 let dialect = {dialect with callSignature = fun target -> match FunctionIdMap.tryFind target signatures with Some signature -> Some signature | None -> dialect.callSignature target} in
 List.fold_left (fun result definition -> let* () = result in verifyFunction dialect definition) (Ok ()) definitions
let errorValue error =
 let open StructuralValue in
 let value (H.ValueId id) = Union ("ValueId", [Scalar (string_of_int id)]) in
 let unary name value = Union (name, [value]) in
 let two name left right = Union (name, [left;right]) in
 let func = AST.DiagnosticFormatting.func in
 let description = match error with
 | UnknownValue id -> unary "UnknownValue" (value id)
 | DuplicateDefinition id -> unary "DuplicateDefinition" (value id)
 | DuplicateParameterBinding id -> unary "DuplicateParameterBinding" (AST.DiagnosticFormatting.binding id)
 | DuplicateFunctionName id -> unary "DuplicateFunctionName" (func id)
 | InconsistentValueType id -> unary "InconsistentValueType" (value id)
 | BindingTypeMismatch id -> unary "BindingTypeMismatch" (value id)
 | InvalidBranchCondition typ -> unary "InvalidBranchCondition" (StructuralFormat.semanticValue typ)
 | InconsistentBranchResult id -> unary "InconsistentBranchResult" (value id)
 | InvalidAliasSource (result, source) -> two "InvalidAliasSource" (value result) (value source)
 | IncompatibleAliasTypes (result, source) -> two "IncompatibleAliasTypes" (value result) (value source)
 | DuplicateAliasSource (result, source) -> two "DuplicateAliasSource" (value result) (value source)
 | UnaccountedOpaqueEffects -> Union ("UnaccountedOpaqueEffects", [])
 | UnknownCallTarget id -> unary "UnknownCallTarget" (func id)
 | MissingCallContract id -> unary "MissingCallContract" (func id)
 | InvalidCallArgumentCount id -> unary "InvalidCallArgumentCount" (func id)
 | InvalidCallArgumentType (id, index) -> two "InvalidCallArgumentType" (func id) (Scalar (string_of_int index))
 | InvalidCallResultType id -> unary "InvalidCallResultType" (func id)
 | InconsistentCallContract id -> unary "InconsistentCallContract" (func id)
 | InconsistentRegisteredFunctionSignature id -> unary "InconsistentRegisteredFunctionSignature" (func id) in
 description
let errorToString error = StructuralFormat.format (errorValue error)
