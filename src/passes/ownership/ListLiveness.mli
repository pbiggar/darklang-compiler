(* ListLiveness.mli - Representation-independent collection liveness and entry verification. *)
module Identity : sig
  type t = HIR.valueId

  val compare : t -> t -> int

  module Set = HIR.ValueSet
end

module Liveness : module type of ValueLiveness.Make (Identity)

val valueContract :
  ( (ListRegion.transform * ListRegion.reuseSelection) ListRegion.operation,
    ListRegion.functionalBlock )
  HIR.operation ->
  Liveness.contract

val verifyFunctional : ListRegion.functionalRegion -> (unit, string) result
