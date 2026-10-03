open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
 let registry = DarkStdlib.buildModuleRegistry () in
 let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
 let names = source :: "missing" :: "Builtin.print_v0" :: List.map fst (StringOrder.Map.bindings registry) in
 tuple [list SemanticAST.observationModuleDef DarkStdlib.allModules; list SemanticAST.observationModuleFunc DarkStdlib.rawMemoryIntrinsics;
  `Assoc ["map", list (fun (name, definition) -> tuple [SemanticJson.string name; SemanticAST.observationModuleFunc definition]) (StringOrder.Map.bindings registry)];
  list (fun name -> option (fun (definition, name) -> tuple [SemanticAST.observationModuleFunc definition; SemanticJson.string name]) (DarkStdlib.tryGetFunction registry name)) names;
  list (fun (_, definition) -> SemanticAST.semanticType (DarkStdlib.getFunctionType definition)) (StringOrder.Map.bindings registry);
  SemanticAST.semanticType (DarkStdlib.resultType (AST.TVar source))]
