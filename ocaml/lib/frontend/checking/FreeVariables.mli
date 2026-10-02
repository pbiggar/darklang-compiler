(* FreeVariables.mli - Expression and pattern binding dependencies for closures. *)
val collectFreeVars : AST.expr -> StringOrder.Set.t -> StringOrder.Set.t
val collectPatternBindings : AST.pattern -> StringOrder.Set.t
