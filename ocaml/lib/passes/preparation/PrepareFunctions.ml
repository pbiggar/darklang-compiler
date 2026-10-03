(* PrepareFunctions.fs - Expose whole-program generic preparation entry points. *)
let monomorphize program = Monomorphization.monomorphizeWithGenericFuncDefs (SpecializationIdentity.extractGenericFuncDefs program) program
(* Monomorphize a program with access to external generic function definitions.
   Used when user code needs to specialize stdlib generics - the stdlib generic
   function bodies are passed in as externalGenericDefs so they can be specialized
   without merging the full stdlib AST with user code.
   Uses iterative approach: keep specializing until no new concrete TypeApps are found *)
let monomorphizeWithExternalDefs externalDefs program =
 let local = SpecializationIdentity.extractGenericFuncDefs program in
 (* Merge external defs with local defs (local takes precedence) *)
 let definitions = StringOrder.Map.fold StringOrder.Map.add local externalDefs in
 Monomorphization.monomorphizeWithGenericFuncDefs definitions program
(* Convert CheckedAST.BinOp to ANF.BinOp
   Note: StringConcat is handled separately as ANF.StringConcat CExpr *)
