(* Direct checked declarations and source-unit composition from WrittenChecking.fs. *)
type environment
val includeAllocatedFunctions : CheckedAST.symbols -> environment -> environment
val predeclareTypes : WrittenSource.item list -> (WrittenTypeSupport.typeInventory, string) result
val checkTypeDeclaration : bool -> WrittenTypeSupport.typeInventory -> StringOrder.Set.t -> CheckedAST.symbols -> string list -> WrittenTypes.typeDecl -> (CheckedAST.topLevel * CheckedAST.symbols, string) result
val predeclareFunctions : bool -> WrittenTypeSupport.typeInventory -> WrittenSource.item list -> CheckedAST.symbols -> (WrittenTypeSupport.functionSignature StringOrder.Map.t * CheckedAST.symbols, string) result
val checkFunction : WrittenLambdaSupport.expressionChecker -> WrittenTypeSupport.globals -> CheckedAST.symbols -> string list -> WrittenTypes.fnDecl -> (CheckedAST.functionDef * CheckedAST.symbols, string) result
val attachRecursiveGroups : CheckedAST.program -> CheckedAST.program
val checkItems : WrittenLambdaSupport.expressionChecker -> environment option -> bool -> bool -> WrittenSource.item list -> (AST.semanticType * CheckedAST.program * environment, string) result
