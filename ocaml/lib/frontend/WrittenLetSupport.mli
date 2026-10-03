(* Continuation inference and local recursive bindings from WrittenChecking.fs. *)
val check : WrittenLambdaSupport.expressionChecker -> WrittenTypeSupport.globals -> WrittenTypeSupport.locals -> CheckedAST.symbols -> AST.semanticType option -> WrittenTypes.range -> WrittenTypes.letPattern -> WrittenTypes.expr -> WrittenTypes.expr -> (WrittenTypeSupport.checkedExpression, string) result
