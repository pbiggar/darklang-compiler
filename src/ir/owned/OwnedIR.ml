(*
   Function parameters retain every typed call position. Unmanaged positions
   stay outside ownership accounting; borrowed parameters remain owned by the
   caller; consumed parameters transfer one unit without proving that external
   aliases are absent; unique parameters additionally establish exclusivity
   provenance.
   A borrowed result transfers no unit, while a produced result transfers one
   unit to the caller. A unique produced result additionally certifies that no
   aliases remain. HIR alias provenance remains a separate semantic contract.
   Lists retain multiplicity: a duplicated unit can satisfy two consuming
   inputs, while duplicate value definitions remain invalid.
   Exclusivity provenance is independent of unit transfer. Required inputs
   must have one local unit and no untracked aliases. Unique outputs establish
   that fact for fresh storage or a verified ownership-preserving transfer.
   Ownership actions are ordered alongside evaluation. Dup creates one
   additional unit for an accessible identity; Drop destroys one owned unit.
   Evaluation contracts may borrow or consume units and produce fresh ones.
   An owned function pairs one typed HIR definition with its independent
   ownership boundary. Parameter order comes from the HIR entry block; the
   ownership signature classifies those same positions without copying types.
   A dialect must describe every access and possible escape, including opaque
   scalar operands and typed block results. A managed result transfers its
   ownership identity to the branch target; Unmanaged means the value has no
   ownership unit.
*)
(* OwnedIR.ml - Structured region ownership and explicit unit-transfer contracts. *)
[@@@warning "-4"]
module IntSet = Set.Make (Int)
(*
   Values are local to a function; together these identities locate a direct
   call, including one inside a nested branch.
*)
type callSiteIdentity = {caller : AST.functionId; result : HIR.valueId}
type 'id input = Borrowed of 'id | Consumed of 'id
type 'id blockArgument = Unmanaged | Managed of 'id
type 'id parameterOwnership = UnmanagedParameter | BorrowedParameter of 'id | ConsumedParameter of 'id | UniqueParameter of 'id
type 'id resultOwnership = UnmanagedResult | BorrowedResult of 'id | ProducedResult of 'id | UniqueProducedResult of 'id
type 'id functionSignature = {parameters : 'id parameterOwnership list; result : 'id resultOwnership}
(*
   Call-site modes include unmanaged positions so they align exactly with the
   typed HIR signature. A borrowed result names the borrowed parameter whose
   ownership identity it aliases; produced results transfer a fresh unit, with
   unique modes carrying the exclusivity certificate across the call boundary.
*)
type callParameterOwnership = UnmanagedCallParameter | BorrowedCallParameter | ConsumedCallParameter | UniqueCallParameter
type callResultOwnership = UnmanagedCallResult | BorrowedCallResult of int | ProducedCallResult | UniqueProducedCallResult
type callSignature = {parameters : callParameterOwnership list; result : callResultOwnership}
(*
   Facts describe the state immediately before argument transfer under the
   established contract. They are valid only for the analyzed definitions.
*)
type callSiteFacts = {caller : AST.functionId; call : HIR.functionCall; established : callSignature; uniqueArguments : IntSet.t}
let callSiteIdentity (facts : callSiteFacts) : callSiteIdentity = {caller = facts.caller; result = facts.call.HIR.result.HIR.id}
type 'id contract = {inputs : 'id input list; outputs : 'id list}
type ('leaf, 'id) step = Evaluate of ('leaf, ('leaf, 'id) block) HIR.operation | Dup of 'id | Drop of 'id
and ('leaf, 'id) block = {body : ('leaf, 'id) step HIR.block}
type ('leaf, 'id) functionDef = {definition : ('leaf, 'id) block HIR.functionDef; ownership : 'id functionSignature}
module type Identity = sig type t val compare : t -> t -> int module Set : Set.S with type elt = t end
module Make (Identity : Identity) = struct
 type uniquenessContract = {requiredInputs : Identity.Set.t; uniqueOutputs : Identity.Set.t}
 type 'leaf semantics = {leaf : 'leaf -> Identity.t contract; leafUniqueness : 'leaf -> uniquenessContract; callOwnership : HIR.functionCall -> callSignature option; scalarUses : HIR.operand -> Identity.Set.t; scalarEscapes : HIR.operand -> Identity.Set.t; blockArgument : HIR.value -> Identity.t blockArgument}
 type verificationError = InvalidUse of Identity.t | InvalidDrop of Identity.t | NonUniqueUse of Identity.t | InvalidUniquenessContract of Identity.t | DuplicateDefinition of Identity.t | DuplicateParameter of Identity.t | InconsistentFunctionParameters | InconsistentFunctionResult | InvalidBorrowedResult of Identity.t | InvalidProducedResult of Identity.t | UnknownCallOwnership of AST.functionId | InconsistentCallOwnershipParameters of AST.functionId | InconsistentCallOwnershipArgument of AST.functionId * int | InconsistentCallOwnershipResult of AST.functionId | InvalidBorrowedCallResult of AST.functionId * int | DuplicateFunctionName of AST.functionId | InconsistentRegisteredCallOwnership of AST.functionId | InconsistentJoin | InconsistentBlockArgument | UndroppedValues of Identity.Set.t
 let errorValue identity error =
  let open StructuralValue in let unary name value = Union (name, [value]) in
  let func = AST.DiagnosticFormatting.func in let index value = Scalar (string_of_int value) in
  let two name id value = Union (name, [func id; index value]) in
  let description = match error with
  | InvalidUse id -> unary "InvalidUse" (identity id)
  | InvalidDrop id -> unary "InvalidDrop" (identity id)
  | NonUniqueUse id -> unary "NonUniqueUse" (identity id)
  | InvalidUniquenessContract id -> unary "InvalidUniquenessContract" (identity id)
  | DuplicateDefinition id -> unary "DuplicateDefinition" (identity id)
  | DuplicateParameter id -> unary "DuplicateParameter" (identity id)
  | InconsistentFunctionParameters -> Union ("InconsistentFunctionParameters", [])
  | InconsistentFunctionResult -> Union ("InconsistentFunctionResult", [])
  | InvalidBorrowedResult id -> unary "InvalidBorrowedResult" (identity id)
  | InvalidProducedResult id -> unary "InvalidProducedResult" (identity id)
  | UnknownCallOwnership id -> unary "UnknownCallOwnership" (func id)
  | InconsistentCallOwnershipParameters id -> unary "InconsistentCallOwnershipParameters" (func id)
  | InconsistentCallOwnershipArgument (id, parameter) -> two "InconsistentCallOwnershipArgument" id parameter
  | InconsistentCallOwnershipResult id -> unary "InconsistentCallOwnershipResult" (func id)
  | InvalidBorrowedCallResult (id, parameter) -> two "InvalidBorrowedCallResult" id parameter
  | DuplicateFunctionName id -> unary "DuplicateFunctionName" (func id)
  | InconsistentRegisteredCallOwnership id -> unary "InconsistentRegisteredCallOwnership" (func id)
  | InconsistentJoin -> Union ("InconsistentJoin", [])
  | InconsistentBlockArgument -> Union ("InconsistentBlockArgument", [])
  | UndroppedValues ids -> unary "UndroppedValues" (Union ("set", [Sequence (List.map identity (Identity.Set.elements ids))])) in
  description
 let errorToString identity error = StructuralFormat.format (errorValue identity error)

end
