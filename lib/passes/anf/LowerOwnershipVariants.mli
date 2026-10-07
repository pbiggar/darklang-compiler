(* LowerOwnershipVariants.fs - Preserve scheduled ownership clones and call routing in ANF. *)
type lowered = {functions : ANF.functionDef list; contracts : OwnedIR.callSignature FunctionIdMap.t; varGen : ANF.varGen}
type loweringError = MissingSourceFunction of AST.functionId | MissingSourceCalls of AST.functionId * OwnedIR.callSiteIdentity list | InvalidOwnershipBoundary of AST.functionId
module SiteSet : Set.S with type elt = OwnedIR.callSiteIdentity
module Make (Identity : OwnedIR.Identity) : sig
 val lower : ('leaf, Identity.t) OwnedIR.functionDef list -> ('leaf, Identity.t) MaterializeOwnershipVariants.plan -> ANF.functionDef list -> ANF.varGen -> SiteSet.t -> (lowered, loweringError) result
end
