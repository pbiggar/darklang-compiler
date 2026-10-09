(* VerifyHIR.mli - Verify normalized HIR value identities and structured control-flow edges. *)
type verificationError =
  | UnknownValue of HIR.valueId
  | DuplicateDefinition of HIR.valueId
  | DuplicateParameterBinding of AST.bindingId
  | DuplicateFunctionName of AST.functionId
  | InconsistentValueType of HIR.valueId
  | BindingTypeMismatch of HIR.valueId
  | InvalidBranchCondition of AST.semanticType
  | InconsistentBranchResult of HIR.valueId
  | InvalidAliasSource of HIR.valueId * HIR.valueId
  | IncompatibleAliasTypes of HIR.valueId * HIR.valueId
  | DuplicateAliasSource of HIR.valueId * HIR.valueId
  | UnaccountedOpaqueEffects
  | UnknownCallTarget of AST.functionId
  | MissingCallContract of AST.functionId
  | InvalidCallArgumentCount of AST.functionId
  | InvalidCallArgumentType of AST.functionId * int
  | InvalidCallResultType of AST.functionId
  | InconsistentCallContract of AST.functionId
  | InconsistentRegisteredFunctionSignature of AST.functionId

type ('leaf, 'block) dialect = {
  body : 'block -> ('leaf, 'block) HIR.operation HIR.block;
  leaf : 'leaf -> HIR.primitiveContract;
  callSignature : AST.functionId -> HIR.functionSignature option;
  callContract : HIR.functionCall -> HIR.primitiveContract option;
}

val verify :
  ('leaf, 'block) dialect -> 'block -> (unit, verificationError) result

val functionSignature :
  ('leaf, 'block) dialect -> 'block HIR.functionDef -> HIR.functionSignature

val verifyFunction :
  ('leaf, 'block) dialect ->
  'block HIR.functionDef ->
  (unit, verificationError) result

val verifyFunctions :
  ('leaf, 'block) dialect ->
  'block HIR.functionDef list ->
  (unit, verificationError) result

val errorValue : verificationError -> StructuralValue.value
val errorToString : verificationError -> string
