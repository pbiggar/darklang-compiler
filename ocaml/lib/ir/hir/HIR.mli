(* HIR.fs - Normalized values and typed structured control flow shared by semantic dialects. *)
type valueId = ValueId of int
module ValueMap : Map.S with type key = valueId
module ValueSet : Set.S with type elt = valueId
type value = {id : valueId; typ : AST.semanticType}
type operand = {expression : CheckedAST.expr; typ : AST.semanticType; inputs : value CheckedAST.BindingIdMap.t}
type functionSignature = {parameters : AST.semanticType list; result : AST.semanticType}
type functionCall = {target : AST.functionId; arguments : value list; result : value}
type ('leaf, 'block) operation = Leaf of 'leaf | ScalarBinding of value * operand | Call of functionCall | Branch of value * operand * 'block * 'block
type parameter = {binding : AST.bindingId; value : value}
type 'operation block = {parameters : parameter list; operations : 'operation list; result : value}
type 'block functionDef = {id : AST.functionId; name : string; body : 'block}
type primitiveEffect = MayEvaluateOpaqueSource | MayAllocate | MayFail | MayInvokeUserCode | ReadsOwnedStorage | WritesOwnedStorage
module EffectSet : Set.S with type elt = primitiveEffect
type resultAlias = NoManagedAlias | UnknownManagedAlias | FreshManaged | MayReuseInput of value | MayAliasInputs of value * value list
type outputContract = {value : value; alias : resultAlias}
type primitiveContract = {inputs : value list; operands : operand list; outputs : outputContract list; effects : EffectSet.t}
val managedOutputs : primitiveContract -> value list
