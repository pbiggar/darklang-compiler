(* Publish completed direct-checked declarations to later compiler passes. *)
module C = CheckedAST
module M = StringOrder.Map
module S = StringOrder.Set
module N = NameResolution

(* Publish the checked declarations to the later compiler passes. This reads
   the completed checked program; source checking has already finished. *)
let[@warning "-4"] typeCheckEnvironment program =
  let symbols = C.programSymbols program
  and topLevels = C.programTopLevels program in
  let typeDefs =
    List.filter_map
      (function
        | C.TypeDef (_, definition) -> Some (C.semanticTypeDef definition)
        | _ -> None)
      topLevels
  in
  let recordTypes, recordParams, aliases, variantLookup =
    List.fold_left
      (fun (records, parameters, aliases, variants) definition ->
        match definition with
        | AST.RecordDef (name, params, fields) ->
            ( M.add name fields records,
              M.add name params parameters,
              aliases,
              variants )
        | AST.TypeAlias (name, params, target) ->
            (records, parameters, M.add name (params, target) aliases, variants)
        | AST.SumTypeDef (name, params, cases) ->
            let variants =
              List.fold_left
                (fun variants (variant : AST.variant) ->
                  let tag =
                    match
                      C.tryFindConstructorId name variant.AST.name symbols
                    with
                    | Some id -> AST.constructorRuntimeTag id
                    | None ->
                        Crash.crash
                          ("Missing constructor identity for '" ^ name ^ "."
                         ^ variant.AST.name ^ "'")
                  in
                  let info = (name, params, tag, variant.AST.fields) in
                  M.add
                    (name ^ "." ^ variant.AST.name)
                    info
                    (M.add variant.AST.name info variants))
                variants cases
            in
            (records, parameters, aliases, variants))
      (M.empty, M.empty, M.empty, M.empty)
      typeDefs
  in
  let indexedRecords =
    Types.indexTypeRegistry variantLookup recordParams recordTypes
  and indexedSums = Types.indexSumTypeRegistry variantLookup in
  let moduleRegistry = DarkStdlib.buildModuleRegistry () in
  let functions, values, parameterNames, genericFunctions =
    List.fold_left
      (fun (functions, values, names, generics) item ->
        match item with
        | C.FunctionDef definition ->
            let parameters = C.functionParameterTypes definition in
            let parameterNames =
              List.map
                (fun (id, _) ->
                  match C.bindingName id symbols with
                  | Some name -> name
                  | None ->
                      Crash.crash
                        ("Missing parameter name in '" ^ definition.C.name ^ "'"))
                (NonEmptyList.toList parameters)
            in
            let signature =
              AST.TFunction
                ( List.map snd (NonEmptyList.toList parameters),
                  C.functionReturnType definition )
            in
            let genericFunctions =
              if definition.C.typeParams = [] then generics
              else M.add definition.C.name definition.C.typeParams generics
            in
            ( M.add definition.C.name signature functions,
              values,
              M.add definition.C.name parameterNames names,
              genericFunctions )
        | C.ValueDef definition ->
            let typ = C.semanticType definition.C.typ in
            ( M.add definition.C.name typ functions,
              M.add definition.C.name typ values,
              names,
              generics )
        | _ -> (functions, values, names, generics))
      (M.empty, M.empty, M.empty, M.empty)
      topLevels
  in
  let sourceFunctions =
    S.of_list
      (List.filter_map
         (function
           | C.FunctionDef definition -> Some definition.C.name | _ -> None)
         topLevels)
  in
  let registeredFunctions =
    S.union sourceFunctions
      (S.of_list (List.map fst (M.bindings moduleRegistry)))
  in
  let candidate name identity provenance =
    match N.candidate name identity provenance with
    | Some item -> item
    | None -> Crash.crash ("Invalid checked declaration name '" ^ name ^ "'")
  in
  let namespaceAndTerminal name =
    match List.rev (String.split_on_char '.' name) with
    | terminal :: reversedModule ->
        let namespace =
          match NonEmptyList.tryFromList (List.rev reversedModule) with
          | None -> N.RootNamespace
          | Some modules -> N.ModuleNamespace modules
        in
        (namespace, terminal)
    | [] -> Crash.crash "Checked declaration has an empty name"
  in
  let sourceCandidates =
    List.concat_map
      (fun (index, item) ->
        match item with
        | C.FunctionDef definition ->
            let namespace, terminal = namespaceAndTerminal definition.C.name in
            let identity =
              N.ModuleFunction
                ( namespace,
                  terminal,
                  "source:" ^ string_of_int index ^ ":" ^ definition.C.name )
            in
            let versioned = definition.C.name ^ "_v0" in
            let hasVersion = String.ends_with ~suffix:"_v0" definition.C.name in
            let visible =
              if hasVersion || S.mem versioned registeredFunctions then
                [ definition.C.name ]
              else [ definition.C.name; versioned ]
            in
            List.map
              (fun spelling ->
                candidate spelling identity
                  (N.SourceDeclaration definition.C.name))
              visible
        | C.ValueDef definition ->
            let namespace, terminal = namespaceAndTerminal definition.C.name in
            [
              candidate definition.C.name
                (N.ModuleValue (namespace, terminal))
                (N.SourceDeclaration definition.C.name);
            ]
        | C.TypeDef (_, definition) ->
            let typeName, cases =
              match C.semanticTypeDef definition with
              | AST.RecordDef (name, _, _) | AST.TypeAlias (name, _, _) ->
                  (name, [])
              | AST.SumTypeDef (name, _, cases) -> (name, cases)
            in
            let typeCandidate =
              candidate typeName (N.UserType typeName)
                (N.SourceDeclaration typeName)
            in
            let constructors =
              List.concat_map
                (fun (variant : AST.variant) ->
                  let identity =
                    N.ConstructorSymbol (typeName, variant.AST.name)
                  in
                  let provenance =
                    N.SourceDeclaration (typeName ^ "." ^ variant.AST.name)
                  in
                  [
                    candidate variant.AST.name identity provenance;
                    candidate
                      (typeName ^ "." ^ variant.AST.name)
                      identity provenance;
                  ])
                cases
            in
            typeCandidate :: constructors
        | C.Expression _ -> [])
      (List.mapi (fun index item -> (index, item)) topLevels)
  in
  let resolutionEnv =
    ResolveDeclarations.declarationResolutionEnvironment [] moduleRegistry true
    |> N.filterCandidates (fun (candidate : N.candidate) ->
        match candidate.N.provenance with
        | N.CompilerExtension name -> not (S.mem name sourceFunctions)
        | _ -> true)
    |> N.addCandidates sourceCandidates
  in
  {
    Types.typeCatalog = C.typeCatalog symbols;
    functionCatalog = C.functionCatalog symbols;
    typeReg = recordTypes;
    indexedTypeReg = indexedRecords;
    recordTypeNames = S.of_list (List.map fst (M.bindings recordTypes));
    variantLookup;
    indexedSumTypeReg = indexedSums;
    sumTypeNames = S.of_list (List.map fst (M.bindings indexedSums));
    funcEnv = functions;
    values;
    funcParamNames = parameterNames;
    genericFuncReg =
      {
        Types.functions = genericFunctions;
        requireExplicitTypeArgsForBareCalls = false;
      };
    genericFuncDefs = M.empty;
    moduleRegistry;
    aliasReg = aliases;
    resolutionEnv;
  }
