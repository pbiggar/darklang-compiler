(* WrittenTraitSupport.ml - Frontend trait identities, concrete validation and static selection. *)
[@@@warning "-4"]

open WrittenTypeSupport
module WT = WrittenTypes
module WS = WrittenSource
module C = CheckedAST
module M = StringOrder.Map
module S = StringOrder.Set

let bind = Result.bind
let map = Result.map

let resolveTrait (globals : globals) path name =
  List.find_map
    (fun candidate ->
      Option.map
        (fun signature -> (candidate, signature))
        (M.find_opt candidate globals.traits))
    (NameResolution.candidateSpellings NameResolution.Type path name
    @ if String.contains name '.' then [] else [ "Darklang.Stdlib." ^ name ])

let parameterTypes convert parameters =
  ResultList.traverse
    (function
      | WT.FPUnit _ -> Ok AST.TUnit
      | WT.FPNormal (_, _, typ, _, _, _, _) -> convert typ)
    parameters

let predeclare allowInternal types (base : globals) items symbols =
  List.fold_left
    (fun result item ->
      bind result (fun (globals, symbols) ->
          match item with
          | WS.Trait (path, declaration) ->
              let name =
                String.concat "." (path @ [ declaration.WT.name.WT.name ])
              in
              if M.mem name globals.traits then
                Error ("Duplicate trait '" ^ name ^ "'")
              else
                let traitParams = List.map fst declaration.WT.typeParams in
                if List.length traitParams <> S.cardinal (S.of_list traitParams)
                then Error ("Duplicate type parameter in trait '" ^ name ^ "'")
                else
                  bind
                    (List.fold_left
                       (fun result (method_ : WT.traitMethodDecl) ->
                         bind result (fun (methods, symbols) ->
                             let methodName = method_.WT.name.WT.name in
                             if M.mem methodName methods then
                               Error
                                 ("Duplicate trait method '" ^ methodName ^ "'")
                             else
                               let typeParams =
                                 traitParams
                                 @ List.map fst method_.WT.typeParams
                               in
                               if
                                 List.length typeParams
                                 <> S.cardinal (S.of_list typeParams)
                               then
                                 Error
                                   ("Duplicate type parameter in trait method '"
                                  ^ methodName ^ "'")
                               else
                                 let convert =
                                   resolveWrittenType allowInternal types path
                                     (S.of_list typeParams)
                                 in
                                 bind
                                   (parameterTypes convert method_.WT.parameters)
                                   (fun parameters ->
                                     map
                                       (fun return ->
                                         let id, symbols =
                                           C.internFunction
                                             (name ^ "." ^ methodName)
                                             symbols
                                         in
                                         ( M.add methodName
                                             {
                                               id;
                                               traitMethod =
                                                 Some (name, methodName);
                                               bounds = [];
                                               typeParams;
                                               parameters;
                                               return;
                                             }
                                             methods,
                                           symbols ))
                                       (convert method_.WT.returnType))))
                       (Ok (M.empty, symbols))
                       declaration.WT.methods)
                    (fun (methods, symbols) ->
                      Ok
                        ( {
                            globals with
                            traits =
                              M.add name
                                { traitParams; requirements = []; methods }
                                globals.traits;
                          },
                          symbols ))
          | _ -> Ok (globals, symbols)))
    (Ok (base, symbols))
    items

(* Implementation bodies are ordinary functions in a private source module. *)
let materialize offset items =
  let rec loop ordinal reversed implementations = function
    | [] -> (List.rev reversed, List.rev implementations)
    | (WS.Impl (path, declaration) as item) :: rest ->
        let methodPath = path @ [ "__trait_impl_" ^ string_of_int ordinal ] in
        let methods =
          List.filter_map
            (function
              | WT.IMethod fn ->
                  Some
                    (WS.Function
                       ( methodPath,
                         {
                           fn with
                           WT.typeParams =
                             declaration.WT.typeParams @ fn.WT.typeParams;
                           bounds = declaration.WT.bounds @ fn.WT.bounds;
                         } ))
              | WT.IAlias _ -> None)
            declaration.WT.members
        in
        loop (ordinal + 1)
          (List.rev methods @ (item :: reversed))
          ((path, methodPath, declaration) :: implementations)
          rest
    | item :: rest -> loop ordinal (item :: reversed) implementations rest
  in
  loop offset [] [] items

let resolveBounds (globals : globals) path params bounds =
  ResultList.traverse
    (fun (bound : WT.typeParamBound) ->
      if not (List.mem bound.WT.param params) then
        Error ("Undeclared bound type parameter '" ^ bound.WT.param ^ "'")
      else
        let spelling = WS.qualifiedType bound.WT.trait_ in
        match resolveTrait globals path spelling with
        | None -> Error ("Unknown trait '" ^ spelling ^ "'")
        | Some (name, trait_) ->
            bind
              (ResultList.traverse
                 (resolveWrittenType globals.allowInternal globals.types path
                    (S.of_list params))
                 bound.WT.trait_.WT.typeArgs)
              (fun args ->
                let args = AST.TVar bound.WT.param :: args in
                if List.length args <> List.length trait_.traitParams then
                  Error ("Trait '" ^ name ^ "' type argument count mismatch")
                else Ok (name, args)))
    bounds

let validate (globals : globals) implementations =
  List.fold_left
    (fun result (path, methodPath, (declaration : WT.implDecl)) ->
      bind result (fun globals ->
          let spelling = WS.qualifiedType declaration.WT.trait_ in
          match resolveTrait globals path spelling with
          | None -> Error ("Unknown trait '" ^ spelling ^ "'")
          | Some (traitName, trait_) ->
              let typeParams = List.map fst declaration.WT.typeParams in
              if List.length typeParams <> S.cardinal (S.of_list typeParams)
              then Error "Duplicate implementation type parameter"
              else
                let convert =
                  resolveWrittenType globals.allowInternal globals.types path
                    (S.of_list typeParams)
                in
                bind (convert declaration.WT.forType) (fun selfType ->
                    bind
                      (ResultList.traverse convert
                         declaration.WT.trait_.WT.typeArgs) (fun args ->
                        let traitArgs = selfType :: args in
                        if
                          List.length traitArgs
                          <> List.length trait_.traitParams
                        then
                          Error
                            ("Trait '" ^ traitName
                           ^ "' type argument count mismatch")
                        else
                          let substitution =
                            M.of_list
                              (List.combine trait_.traitParams traitArgs)
                          in
                          bind
                            (resolveBounds globals path typeParams
                               declaration.WT.bounds)
                            (fun implementationBounds ->
                              let bounds =
                                implementationBounds
                                @ List.map
                                    (TraitResolution.substitute substitution)
                                    trait_.requirements
                              in
                              let patternVars =
                                List.fold_left
                                  (fun vars typ ->
                                    Types.collectTypeVarsInType typ vars)
                                  [] traitArgs
                              in
                              if
                                List.exists
                                  (fun param ->
                                    not (List.mem param patternVars))
                                  typeParams
                              then
                                Error "Unresolved implementation type parameter"
                              else
                                bind
                                  (List.fold_left
                                     (fun result member ->
                                       bind result (fun (methods, arguments) ->
                                           let name, signature =
                                             match member with
                                             | WT.IMethod fn ->
                                                 let name =
                                                   fn.WT.name.WT.name
                                                 in
                                                 ( name,
                                                   M.find_opt
                                                     (String.concat "."
                                                        (methodPath @ [ name ]))
                                                     globals.functions )
                                             | WT.IAlias value ->
                                                 let source =
                                                   match value.WT.body with
                                                   | WT.EVariable (_, name) ->
                                                       Some [ name ]
                                                   | WT.EFnName (_, name) ->
                                                       Some
                                                         (qualifiedFnName name)
                                                   | _ -> None
                                                 in
                                                 ( value.WT.name.WT.name,
                                                   Option.bind source
                                                     (resolveFunction
                                                        {
                                                          globals with
                                                          modulePath = path;
                                                        }) )
                                           in
                                           if M.mem name methods then
                                             Error
                                               ("Duplicate implementation \
                                                 method '" ^ name ^ "'")
                                           else
                                             match
                                               ( M.find_opt name trait_.methods,
                                                 signature )
                                             with
                                             | None, _ ->
                                                 Error
                                                   ("Extra trait method '"
                                                  ^ name ^ "'")
                                             | _, None ->
                                                 Error
                                                   ("Unknown implementation \
                                                     method alias '" ^ name
                                                  ^ "'")
                                             | Some expected, Some actual ->
                                                 let methodParams =
                                                   List.filter
                                                     (fun param ->
                                                       not
                                                         (List.mem param
                                                            trait_.traitParams))
                                                     expected.typeParams
                                                 in
                                                 let fresh, renaming =
                                                   CheckingDiagnostics
                                                   .freshenTypeParams
                                                     (Some
                                                        (String.concat "."
                                                           (methodPath
                                                          @ [ name ])))
                                                     methodParams
                                                 in
                                                 let expectedSubst =
                                                   List.fold_left
                                                     (fun subst (old, fresh) ->
                                                       M.add old
                                                         (AST.TVar fresh) subst)
                                                     substitution
                                                     (List.combine methodParams
                                                        fresh)
                                                 in
                                                 let expectedParameters =
                                                   List.map
                                                     (Types.applySubst
                                                        expectedSubst)
                                                     expected.parameters
                                                 and expectedReturn =
                                                   Types.applySubst
                                                     expectedSubst
                                                     expected.return
                                                 in
                                                 if
                                                   Option.is_some
                                                     actual.traitMethod
                                                 then
                                                   Error
                                                     ("Trait method signature \
                                                       mismatch: '" ^ name ^ "'"
                                                     )
                                                 else
                                                   bind
                                                     (Result.map_error
                                                        (fun error ->
                                                          "Trait method \
                                                           signature mismatch: \
                                                           '" ^ name ^ "': "
                                                          ^ error)
                                                        (Unification
                                                         .inferTypeArgs
                                                           actual.typeParams
                                                           actual.parameters
                                                           expectedParameters
                                                           (Some actual.return)
                                                           (Some expectedReturn)))
                                                     (fun inferred ->
                                                       let actualSubst =
                                                         M.of_list
                                                           (List.combine
                                                              actual.typeParams
                                                              inferred)
                                                       in
                                                       let allowed =
                                                         bounds
                                                         @ List.map
                                                             (TraitResolution
                                                              .substitute
                                                                expectedSubst)
                                                             expected.bounds
                                                       in
                                                       let free =
                                                         List.fold_left
                                                           (fun vars typ ->
                                                             Types
                                                             .collectTypeVarsInType
                                                               typ vars)
                                                           [] inferred
                                                       in
                                                       if
                                                         List.map
                                                           (Types.applySubst
                                                              actualSubst)
                                                           actual.parameters
                                                         <> expectedParameters
                                                         || Types.applySubst
                                                              actualSubst
                                                              actual.return
                                                            <> expectedReturn
                                                         || List.exists
                                                              (fun var ->
                                                                not
                                                                  (List.mem var
                                                                     (typeParams
                                                                    @ fresh)))
                                                              free
                                                         || List.exists
                                                              (fun obligation ->
                                                                not
                                                                  (List.mem
                                                                     (TraitResolution
                                                                      .substitute
                                                                        actualSubst
                                                                        obligation)
                                                                     allowed))
                                                              actual.bounds
                                                       then
                                                         Error
                                                           ("Trait method \
                                                             signature \
                                                             mismatch: '" ^ name
                                                          ^ "'")
                                                       else
                                                         let _ = renaming in
                                                         Ok
                                                           ( M.add name actual
                                                               methods,
                                                             M.add name
                                                               (inferred, fresh)
                                                               arguments ))))
                                     (Ok (M.empty, M.empty))
                                     declaration.WT.members)
                                  (fun (methodFunctions, methodArguments) ->
                                    match
                                      List.find_opt
                                        (fun (name, _) ->
                                          not (M.mem name methodFunctions))
                                        (M.bindings trait_.methods)
                                    with
                                    | Some (name, _) ->
                                        Error
                                          ("Missing trait method '" ^ name ^ "'")
                                    | None ->
                                        Ok
                                          {
                                            globals with
                                            implementations =
                                              globals.implementations
                                              @ [
                                                  {
                                                    identity =
                                                      String.concat "."
                                                        methodPath;
                                                    traitName;
                                                    selfType;
                                                    traitArgs;
                                                    typeParams;
                                                    bounds;
                                                    methodFunctions;
                                                    methodArguments;
                                                  };
                                                ];
                                          }))))))
    (Ok globals) implementations

let functions (globals : globals) =
  M.fold
    (fun traitName trait_ functions ->
      M.fold
        (fun methodName signature functions ->
          M.add (traitName ^ "." ^ methodName) signature functions)
        trait_.methods functions)
    globals.traits globals.functions

let prepareBounds (globals : globals) items =
  List.fold_left
    (fun result item ->
      bind result (fun globals ->
          match item with
          | WS.Function (path, fn) -> (
              let name = String.concat "." (path @ [ fn.WT.name.WT.name ]) in
              match M.find_opt name globals.functions with
              | None -> Crash.crash "Missing predeclared bounded function"
              | Some signature ->
                  Result.map
                    (fun bounds ->
                      {
                        globals with
                        functions =
                          M.add name { signature with bounds } globals.functions;
                      })
                    (resolveBounds globals path signature.typeParams
                       fn.WT.bounds))
          | WS.Trait (path, declaration) -> (
              let name =
                String.concat "." (path @ [ declaration.WT.name.WT.name ])
              in
              match M.find_opt name globals.traits with
              | None -> Crash.crash "Missing predeclared bounded trait"
              | Some trait_ ->
                  bind
                    (resolveBounds globals path trait_.traitParams
                       declaration.WT.bounds) (fun requirements ->
                      bind
                        (List.fold_left
                           (fun result (method_ : WT.traitMethodDecl) ->
                             bind result (fun methods ->
                                 let methodName = method_.WT.name.WT.name in
                                 match M.find_opt methodName methods with
                                 | None ->
                                     Crash.crash
                                       "Missing predeclared bounded trait \
                                        method"
                                 | Some (signature : functionSignature) ->
                                     Result.map
                                       (fun bounds ->
                                         M.add methodName
                                           { signature with bounds } methods)
                                       (resolveBounds globals path
                                          signature.typeParams
                                          (declaration.WT.bounds
                                         @ method_.WT.bounds))))
                           (Ok trait_.methods) declaration.WT.methods)
                        (fun methods ->
                          Ok
                            {
                              globals with
                              traits =
                                M.add name
                                  { trait_ with methods; requirements }
                                  globals.traits;
                            })))
          | _ -> Ok globals))
    (Ok globals) items

let catalog (globals : globals) symbols =
  let dispatches =
    M.fold
      (fun traitName trait_ catalog ->
        M.fold
          (fun methodName _ catalog ->
            M.add
              (traitName ^ "." ^ methodName)
              {
                TraitCatalog.traitName;
                methodName;
                arity = List.length trait_.traitParams;
              }
              catalog)
          trait_.methods catalog)
      globals.traits M.empty
  in
  let nameOf id =
    match C.functionName id symbols with
    | Some name -> name
    | None -> Crash.crash "Trait method function lost its canonical identity"
  in
  let implementations =
    List.fold_left
      (fun catalog implementation ->
        let methods =
          M.mapi
            (fun methodName (signature : functionSignature) ->
              match M.find_opt methodName implementation.methodArguments with
              | None ->
                  Crash.crash
                    "Validated method lost its generic argument mapping"
              | Some (typeArgs, methodParams) ->
                  {
                    TraitCatalog.name = nameOf signature.id;
                    typeArgs;
                    methodParams;
                  })
            implementation.methodFunctions
        in
        let identity = implementation.identity in
        M.add identity
          {
            TraitCatalog.identity;
            traitName = implementation.traitName;
            typeParams = implementation.typeParams;
            typeArgs = implementation.traitArgs;
            bounds = implementation.bounds;
            methods;
          }
          catalog)
      M.empty globals.implementations
  in
  let functionBounds =
    M.fold
      (fun name (signature : functionSignature) catalog ->
        if signature.bounds = [] then catalog
        else M.add name (signature.typeParams, signature.bounds) catalog)
      globals.functions M.empty
  in
  bind
    (M.fold
       (fun name (entry : typeEntry) result ->
         bind result (fun catalog ->
             Result.map
               (fun bounds ->
                 if bounds = [] then catalog
                 else M.add name (entry.params, bounds) catalog)
               (resolveBounds globals entry.path entry.params entry.bounds)))
       globals.types (Ok M.empty))
    (fun typeBounds ->
      let catalog =
        { TraitCatalog.dispatches; implementations; functionBounds; typeBounds }
      in
      bind
        (M.fold
           (fun _ (signature : functionSignature) result ->
             bind result (fun () ->
                 List.fold_left
                   (fun result typ ->
                     bind result (fun () ->
                         TraitResolution.checkType catalog signature.bounds typ))
                   (Ok ())
                   (signature.return :: signature.parameters)))
           globals.functions (Ok ()))
        (fun () -> Ok catalog))

let checkSignatureBounds (globals : globals) symbols
    (signature : functionSignature) types =
  if signature.bounds = [] then Ok ()
  else if List.length signature.typeParams <> List.length types then
    Error "Trait-bound type argument count mismatch"
  else
    TraitResolution.checkBounds (C.traitCatalog symbols) globals.bounds
      (M.of_list (List.combine signature.typeParams types))
      signature.bounds

let selectCall symbols traitName methodName types =
  match
    M.find_opt
      (traitName ^ "." ^ methodName)
      (C.traitCatalog symbols).TraitCatalog.dispatches
  with
  | None -> Error ("Unknown trait method '" ^ traitName ^ "." ^ methodName ^ "'")
  | Some dispatch ->
      bind
        (TraitResolution.select (C.traitCatalog symbols) dispatch types)
        (fun (name, args) ->
          match C.tryFindFunctionId name symbols with
          | None -> Error ("Unresolved implementation method '" ^ name ^ "'")
          | Some id -> Ok (id, args))

let namedMethod range traitName methodName : WT.qualifiedFnIdentifier =
  {
    WT.range;
    modules =
      List.map
        (fun name -> (({ WT.range; name } : WT.identifier), range))
        (String.split_on_char '.' traitName);
    fn = { WT.range; name = methodName };
  }

let receiverMethod (globals : globals) symbols typ methodName =
  let applicable =
    M.bindings globals.traits
    |> List.filter_map (fun (name, trait_) ->
        if not (M.mem methodName trait_.methods) then None
        else
          let supplied =
            List.exists
              (fun (owner, args) ->
                owner = name
                && match args with self :: _ -> self = typ | [] -> false)
              globals.bounds
          in
          if supplied then Some name
          else if List.length trait_.traitParams <> 1 then
            let matches =
              M.bindings (C.traitCatalog symbols).TraitCatalog.implementations
              |> List.exists
                   (fun (_, (implementation : TraitCatalog.implementation)) ->
                     implementation.TraitCatalog.traitName = name
                     &&
                     match implementation.TraitCatalog.typeArgs with
                     | self :: _ ->
                         Option.is_some
                           (TraitResolution.matchArguments [ self ] [ typ ])
                     | [] -> false)
            in
            if matches then Some name else None
          else
            match
              TraitResolution.check (C.traitCatalog symbols) globals.bounds
                (name, [ typ ])
            with
            | Ok () -> Some name
            | Error error
              when String.starts_with ~prefix:"Ambiguous implementation" error
              ->
                Some name
            | Error _ -> None)
  in
  match applicable with
  | [] -> Ok None
  | [ name ] -> Ok (Some name)
  | _ -> Error ("Ambiguous receiver method '" ^ methodName ^ "'")

let proofAvailable (globals : globals) symbols traitName typ =
  Result.is_ok
    (TraitResolution.check (C.traitCatalog symbols) globals.bounds
       (traitName, [ typ ]))

let inferFromBounds (globals : globals) (signature : functionSignature) inferred
    =
  match signature.traitMethod with
  | None -> inferred
  | Some (traitName, _) -> (
      match M.find_opt traitName globals.traits with
      | None -> inferred
      | Some trait_ -> (
          let arity = List.length trait_.traitParams in
          let traitArgs = List.filteri (fun i _ -> i < arity) inferred in
          let flexible = function
            | AST.TVar name -> not (S.mem name globals.typeParams)
            | AST.TInferenceVar _ -> true
            | _ -> false
          in
          let candidates =
            List.filter_map
              (fun (name, args) ->
                if
                  name <> traitName
                  || List.length args <> List.length traitArgs
                  || not
                       (List.for_all2
                          (fun current wanted ->
                            current = wanted || flexible current)
                          traitArgs args)
                then None
                else Some args)
              globals.bounds
          in
          match candidates with
          | [ args ] ->
              let rec fill args current =
                match (args, current) with
                | [], rest -> rest
                | wanted :: bounds, typ :: rest ->
                    (if flexible typ then wanted else typ) :: fill bounds rest
                | _ -> Crash.crash "Checked trait bound inference arity changed"
              in
              fill args inferred
          | _ -> inferred))
