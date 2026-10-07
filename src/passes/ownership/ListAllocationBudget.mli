(* ListAllocationBudget.mli - Piecewise allocation accounting across shared continuations. *)
val allocationBudget : ListRegion.ownedRegion -> ListRegion.allocationBudget
