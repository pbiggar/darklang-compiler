(* OwnedIR.fs - Structured region ownership and explicit unit-transfer contracts. *)
module IntSet : Set.S with type elt = int
type callSiteIdentity = {caller : AST.functionId; result : HIR.valueId}
type 'id input = Borrowed of 'id | Consumed of 'id
type 'id blockArgument = Unmanaged | Managed of 'id
type 'id parameterOwnership = UnmanagedParameter | BorrowedParameter of 'id | ConsumedParameter of 'id | UniqueParameter of 'id
type 'id resultOwnership = UnmanagedResult | BorrowedResult of 'id | ProducedResult of 'id | UniqueProducedResult of 'id
type 'id functionSignature = {parameters : 'id parameterOwnership list; result : 'id resultOwnership}
type callParameterOwnership = UnmanagedCallParameter | BorrowedCallParameter | ConsumedCallParameter | UniqueCallParameter
type callResultOwnership = UnmanagedCallResult | BorrowedCallResult of int | ProducedCallResult | UniqueProducedCallResult
type callSignature = {parameters : callParameterOwnership list; result : callResultOwnership}
type callSiteFacts = {caller : AST.functionId; call : HIR.functionCall; established : callSignature; uniqueArguments : IntSet.t}
val callSiteIdentity : callSiteFacts -> callSiteIdentity
type 'id contract = {inputs : 'id input list; outputs : 'id list}
type ('leaf, 'id) step = Evaluate of ('leaf, ('leaf, 'id) block) HIR.operation | Dup of 'id | Drop of 'id
and ('leaf, 'id) block = {body : ('leaf, 'id) step HIR.block}
type ('leaf, 'id) functionDef = {definition : ('leaf, 'id) block HIR.functionDef; ownership : 'id functionSignature}
module type Identity = sig type t val compare : t -> t -> int module Set : Set.S with type elt = t end
module Make (Identity : Identity) : sig
 type uniquenessContract = {requiredInputs : Identity.Set.t; uniqueOutputs : Identity.Set.t}
 type 'leaf semantics = {leaf : 'leaf -> Identity.t contract; leafUniqueness : 'leaf -> uniquenessContract; callOwnership : HIR.functionCall -> callSignature option; scalarUses : HIR.operand -> Identity.Set.t; scalarEscapes : HIR.operand -> Identity.Set.t; blockArgument : HIR.value -> Identity.t blockArgument}
 type verificationError = InvalidUse of Identity.t | InvalidDrop of Identity.t | NonUniqueUse of Identity.t | InvalidUniquenessContract of Identity.t | DuplicateDefinition of Identity.t | DuplicateParameter of Identity.t | InconsistentFunctionParameters | InconsistentFunctionResult | InvalidBorrowedResult of Identity.t | InvalidProducedResult of Identity.t | UnknownCallOwnership of AST.functionId | InconsistentCallOwnershipParameters of AST.functionId | InconsistentCallOwnershipArgument of AST.functionId * int | InconsistentCallOwnershipResult of AST.functionId | InvalidBorrowedCallResult of AST.functionId * int | DuplicateFunctionName of AST.functionId | InconsistentRegisteredCallOwnership of AST.functionId | InconsistentJoin | InconsistentBlockArgument | UndroppedValues of Identity.Set.t
 val errorToString : (Identity.t -> StructuralValue.value) -> verificationError -> string
end
