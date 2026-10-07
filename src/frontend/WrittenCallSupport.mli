(* Declared full and partial applications from WrittenChecking.fs. *)
type literalChecker = AST.semanticType option -> CheckedAST.symbols -> AST.semanticType -> CheckedAST.expr -> (WrittenTypeSupport.checkedExpression, string) result
val checkNamed : WrittenLambdaSupport.expressionChecker -> literalChecker -> WrittenTypeSupport.globals -> WrittenTypeSupport.locals -> CheckedAST.symbols -> AST.semanticType option -> WrittenTypes.range -> WrittenTypes.qualifiedFnIdentifier -> WrittenTypes.typeReference list -> WrittenTypes.expr list -> (WrittenTypeSupport.checkedExpression, string) result
