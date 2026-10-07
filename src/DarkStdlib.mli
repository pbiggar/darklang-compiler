(* DarkStdlib.mli - Complete intrinsic standard library signatures and lookup. *)
val boolIntrinsicModule : AST.moduleDef
val int64IntrinsicModule : AST.moduleDef
val floatIntrinsicModule : AST.moduleDef
val resultType : AST.semanticType -> AST.semanticType
val cliIntrinsicModule : AST.moduleDef
val fileIntrinsicModule : AST.moduleDef
val networkIntrinsicModule : AST.moduleDef
val randomModule : AST.moduleDef
val dateTimeModule : AST.moduleDef
val builtinPresentationModule : AST.moduleDef
val packageCatalogModule : AST.moduleDef
val rawMemoryIntrinsics : AST.moduleFunc list
val allModules : AST.moduleDef list
val buildModuleRegistry : unit -> AST.moduleRegistry
val tryGetFunction : AST.moduleRegistry -> string -> (AST.moduleFunc * string) option
val getFunctionType : AST.moduleFunc -> AST.semanticType
