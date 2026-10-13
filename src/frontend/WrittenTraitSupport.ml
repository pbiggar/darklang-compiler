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
    (NameResolution.candidateSpellings NameResolution.Type path name)

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
                              M.add name { traitParams; methods } globals.traits;
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
              | WT.IMethod fn -> Some (WS.Function (methodPath, fn))
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

let validate (globals : globals) implementations =
  List.fold_left
    (fun result (path, methodPath, (declaration : WT.implDecl)) ->
      bind result (fun globals ->
          let spelling = WS.qualifiedType declaration.WT.trait_ in
          match resolveTrait globals path spelling with
          | None -> Error ("Unknown trait '" ^ spelling ^ "'")
          | Some (traitName, trait_) ->
              if declaration.WT.typeParams <> [] || declaration.WT.bounds <> []
              then
                Error
                  "Generic and conditional implementations are not implemented"
              else
                let convert =
                  resolveWrittenType globals.allowInternal globals.types path
                    S.empty
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
                            (List.fold_left
                               (fun result member ->
                                 bind result (fun methods ->
                                     let name, signature =
                                       match member with
                                       | WT.IMethod fn ->
                                           let name = fn.WT.name.WT.name in
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
                                                 Some (qualifiedFnName name)
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
                                         ("Duplicate implementation method '"
                                        ^ name ^ "'")
                                     else
                                       match
                                         ( M.find_opt name trait_.methods,
                                           signature )
                                       with
                                       | None, _ ->
                                           Error
                                             ("Extra trait method '" ^ name
                                            ^ "'")
                                       | _, None ->
                                           Error
                                             ("Unknown implementation method \
                                               alias '" ^ name ^ "'")
                                       | Some expected, Some actual ->
                                           let methodParams =
                                             List.filter
                                               (fun param ->
                                                 not
                                                   (List.mem param
                                                      trait_.traitParams))
                                               expected.typeParams
                                           in
                                           if
                                             List.length methodParams
                                             <> List.length actual.typeParams
                                           then
                                             Error
                                               ("Trait method signature \
                                                 mismatch: '" ^ name ^ "'")
                                           else
                                             let substitution =
                                               List.fold_left2
                                                 (fun subst param actual ->
                                                   M.add param (AST.TVar actual)
                                                     subst)
                                                 substitution methodParams
                                                 actual.typeParams
                                             in
                                             let parameters =
                                               List.map
                                                 (Types.applySubst substitution)
                                                 expected.parameters
                                             and return =
                                               Types.applySubst substitution
                                                 expected.return
                                             in
                                             if
                                               parameters <> actual.parameters
                                               || return <> actual.return
                                               || List.exists
                                                    (fun bound ->
                                                      not
                                                        (List.mem bound
                                                           (List.map
                                                              (TraitResolution
                                                               .substitute
                                                                 substitution)
                                                              expected.bounds)))
                                                    actual.bounds
                                               || Option.is_some
                                                    actual.traitMethod
                                             then
                                               Error
                                                 ("Trait method signature \
                                                   mismatch: '" ^ name ^ "'")
                                             else Ok (M.add name actual methods)))
                               (Ok M.empty) declaration.WT.members)
                            (fun methodFunctions ->
                              match
                                List.find_opt
                                  (fun (name, _) ->
                                    not (M.mem name methodFunctions))
                                  (M.bindings trait_.methods)
                              with
                              | Some (name, _) ->
                                  Error ("Missing trait method '" ^ name ^ "'")
                              | None ->
                                  Ok
                                    {
                                      globals with
                                      implementations =
                                        globals.implementations
                                        @ [
                                            {
                                              traitName;
                                              selfType;
                                              traitArgs;
                                              methodFunctions;
                                            };
                                          ];
                                    })))))
    (Ok globals) implementations

let functions (globals : globals) =
  M.fold
    (fun traitName trait_ functions ->
      M.fold
        (fun methodName signature functions ->
          M.add (traitName ^ "." ^ methodName) signature functions)
        trait_.methods functions)
    globals.traits globals.functions

let select (globals : globals) traitName methodName traitArgs =
  if List.exists Unification.containsTVar traitArgs then
    Error
      ("Unresolved generic trait call '" ^ traitName ^ "." ^ methodName ^ "'")
  else
    let matches =
      List.filter
        (fun implementation ->
          implementation.traitName = traitName
          && implementation.traitArgs = traitArgs)
        globals.implementations
    in
    match matches with
    | [] -> Error ("No implementation of trait '" ^ traitName ^ "'")
    | [ implementation ] -> (
        match M.find_opt methodName implementation.methodFunctions with
        | Some method_ -> Ok method_
        | None ->
            Crash.crash "Validated implementation is missing a trait method")
    | _ -> Error ("Ambiguous implementation of trait '" ^ traitName ^ "'")

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
                    (List.fold_left
                       (fun result (method_ : WT.traitMethodDecl) ->
                         bind result (fun methods ->
                             let methodName = method_.WT.name.WT.name in
                             match M.find_opt methodName methods with
                             | None ->
                                 Crash.crash
                                   "Missing predeclared bounded trait method"
                             | Some (signature : functionSignature) ->
                                 Result.map
                                   (fun bounds ->
                                     M.add methodName { signature with bounds }
                                       methods)
                                   (resolveBounds globals path
                                      signature.typeParams
                                      (declaration.WT.bounds @ method_.WT.bounds))))
                       (Ok trait_.methods) declaration.WT.methods)
                    (fun methods ->
                      Ok
                        {
                          globals with
                          traits =
                            M.add name { trait_ with methods } globals.traits;
                        }))
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
          M.map
            (fun (signature : functionSignature) ->
              {
                TraitCatalog.name = nameOf signature.id;
                typeParams = signature.typeParams;
              })
            implementation.methodFunctions
        in
        let identity =
          String.concat ":"
            (List.map
               (fun (_, target) -> target.TraitCatalog.name)
               (M.bindings methods))
        in
        M.add identity
          {
            TraitCatalog.identity;
            traitName = implementation.traitName;
            typeParams = [];
            typeArgs = implementation.traitArgs;
            bounds = [];
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
