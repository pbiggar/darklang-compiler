(*
   Unsupported source expressions remain opaque evaluation payloads. Their
   lexical inputs are explicit value identities, while normalized leaf dialects
   supply purity and aliasing claims through verified primitive contracts.
   Each branch produces the binding consumed by the enclosing continuation.
   Keeping the continuation in this sequence avoids duplicating it per path.
   A normalized function owns one ordered entry block. Its typed signature is
   derived from the parameter and result values rather than duplicated here.
*)
(* HIR.ml - Normalized values and typed structured control flow shared by semantic dialects. *)
[@@@warning "-4"]
type valueId = ValueId of int
module ValueIdentity = struct type t = valueId let compare (ValueId left) (ValueId right) = Int.compare left right end
module ValueMap = Map.Make (ValueIdentity)
module ValueSet = Set.Make (ValueIdentity)
type value = {id : valueId; typ : AST.semanticType}
type operand = {expression : CheckedAST.expr; typ : AST.semanticType; inputs : value CheckedAST.BindingIdMap.t}
(*
   Function signatures describe only the typed call boundary. Effects and
   alias provenance remain separate primitive contracts, while ownership is
   supplied by OwnedIR.
*)
type functionSignature = {parameters : AST.semanticType list; result : AST.semanticType}
(*
   Only resolved direct calls enter normalized HIR. Unknown and indirect calls
   remain inside opaque scalar evaluation until their boundaries are known.
*)
type functionCall = {target : AST.functionId; arguments : value list; result : value}
type ('leaf, 'block) operation = Leaf of 'leaf | ScalarBinding of value * operand | Call of functionCall | Branch of value * operand * 'block * 'block
(*
   Parameter bindings identify operands; declaration order aligns function-call
   arguments with the signature. Values carry the normalized identity.
*)
type parameter = {binding : AST.bindingId; value : value}
type 'operation block = {parameters : parameter list; operations : 'operation list; result : value}
type 'block functionDef = {id : AST.functionId; name : string; body : 'block}
(*
   Effects constrain reordering independently of ownership. Owned-storage
   reads and writes describe compiler-selected representations, not visible
   source mutation.
*)
type primitiveEffect = MayEvaluateOpaqueSource | MayAllocate | MayFail | MayInvokeUserCode | ReadsOwnedStorage | WritesOwnedStorage
module EffectSet = Set.Make (struct type t = primitiveEffect let compare = Stdlib.compare end)
(*
   Alias provenance is a storage-selection capability, not a mutation
   guarantee. UnknownManagedAlias exposes a managed output without claiming
   freshness or an input relationship. MayReuseInput permits either fresh
   storage or ownership transfer.
*)
type resultAlias = NoManagedAlias | UnknownManagedAlias | FreshManaged | MayReuseInput of value | MayAliasInputs of value * value list
type outputContract = {value : value; alias : resultAlias}
(*
   A leaf interface exposes ordered value edges, opaque scalar operands,
   execution effects, and result provenance without assigning ownership.
*)
type primitiveContract = {inputs : value list; operands : operand list; outputs : outputContract list; effects : EffectSet.t}
let managedOutputs contract = List.filter_map (fun output -> match output.alias with NoManagedAlias -> None | UnknownManagedAlias | FreshManaged | MayReuseInput _ | MayAliasInputs _ -> Some output.value) contract.outputs
