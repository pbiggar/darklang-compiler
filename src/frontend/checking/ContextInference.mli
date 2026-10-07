(* ContextInference.mli - Continuation evidence used by expression checking. *)
type checker = AST.expr -> AST.semanticType option -> (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result
val tryFindCallArguments : string -> AST.expr -> AST.expr list option
val inferFunctionExpectationFromArguments : checker -> int -> AST.expr list -> AST.semanticType option
val expectedTypeForNestedVariable : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> string -> AST.semanticType -> AST.expr -> AST.semanticType option
val tryFindFunctionValueExpectation : checker -> Types.typeEnv -> Types.indexedTypeRegistry -> Types.variantLookup -> AST.moduleRegistry -> Types.aliasRegistry -> string -> AST.expr -> AST.semanticType option
val orElse : 'a option -> (unit -> 'a option) -> 'a option
val filter : ('a -> bool) -> 'a option -> 'a option
