(* Contextual lambda inference and currying from WrittenChecking.fs. *)
type expressionChecker = WrittenTypeSupport.globals -> WrittenTypeSupport.locals -> CheckedAST.symbols -> AST.semanticType option -> WrittenTypes.expr -> (WrittenTypeSupport.checkedExpression, string) result
val check : expressionChecker -> WrittenTypeSupport.globals -> WrittenTypeSupport.locals -> CheckedAST.symbols -> AST.semanticType option -> WrittenTypes.range -> WrittenTypes.letPattern list -> WrittenTypes.expr -> WrittenTypes.range -> WrittenTypes.range -> (WrittenTypeSupport.checkedExpression, string) result
