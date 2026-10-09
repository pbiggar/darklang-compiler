(* Direct checked declarations and source-unit composition from WrittenDeclarations.ml. *)
open WrittenTypeSupport
module WT = WrittenTypes
module C = CheckedAST
module M = StringOrder.Map
module S = StringOrder.Set
module WS = WrittenSource

let bind = Result.bind
let map = Result.map

(* Declarations retained by a checked source batch for separately checked
   source units. The representation stays inside this direct checker. *)
type environment = Environment of globals * C.symbols

type expressionChecker =
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  CheckedAST.symbols ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  (AST.semanticType * CheckedAST.expr * CheckedAST.symbols, string) result

let includeAllocatedFunctions allocated (Environment (globals, symbols)) =
  Environment (globals, C.includeAllocatedFunctionNames allocated symbols)

let duplicate names =
  let rec loop seen = function
    | [] -> None
    | name :: rest ->
        if S.mem name seen then Some name else loop (S.add name seen) rest
  in
  loop S.empty names

(* Declaration validity belongs to the same source checker as body validity.
   Preserve rejection of malformed declarations after removing the AST checker. *)
let[@warning "-4"] validateTypes types =
  let colliding = collidingCaseNames types in
  let checked =
    M.fold
      (fun owner entry result ->
        bind result (fun tags ->
            match duplicate entry.params with
            | Some name ->
                Error ("Duplicate type parameter: " ^ name ^ " in " ^ owner)
            | None -> (
                match entry.definition with
                | WT.TDRecord [] ->
                    Error
                      ("Record declaration must contain at least one field: "
                     ^ owner)
                | WT.TDEnum [] ->
                    Error
                      ("Enum declaration must contain at least one case: "
                     ^ owner)
                | WT.TDEnum cases -> (
                    let names =
                      List.map
                        (fun (_, (case : WT.enumCaseSyntax)) ->
                          snd case.WT.name)
                        cases
                    in
                    match duplicate names with
                    | Some name ->
                        Error
                          ("Duplicate constructor declaration: " ^ owner ^ "."
                         ^ name)
                    | None ->
                        List.fold_left
                          (fun result (index, name) ->
                            bind result (fun tags ->
                                let tag = caseTag colliding owner name index in
                                let identity = owner ^ "." ^ name in
                                let key = string_of_int tag in
                                if not (S.mem name colliding) then Ok tags
                                else
                                  match M.find_opt key tags with
                                  | Some existing
                                    when existing <> identity
                                         && S.mem name colliding ->
                                      Error
                                        ("Constructor identity collision " ^ key
                                       ^ ": " ^ existing ^ ", " ^ identity)
                                  | _ -> Ok (M.add key identity tags)))
                          (Ok tags)
                          (List.mapi (fun index name -> (index, name)) names))
                | _ -> Ok tags)))
      types (Ok M.empty)
  in
  map (fun _ -> types) checked

let[@warning "-4"] predeclareTypes items =
  bind
    (List.fold_left
       (fun result item ->
         bind result (fun types ->
             match item with
             | WS.Type (path, declaration) ->
                 let name =
                   String.concat "." (path @ [ declaration.WT.name.WT.name ])
                 in
                 if M.mem name types then Error ("Duplicate type '" ^ name ^ "'")
                 else
                   let kind =
                     match declaration.WT.definition with
                     | WT.TDRecord _ -> RecordKind
                     | WT.TDEnum _ -> SumKind
                     | WT.TDAlias _ -> AliasKind
                   in
                   Ok
                     (M.add name
                        {
                          kind;
                          params = List.map fst declaration.WT.typeParams;
                          path;
                          definition = declaration.WT.definition;
                        }
                        types)
             | _ -> Ok types))
       (Ok M.empty) items)
    validateTypes

let checkTypeDeclaration allowInternal types colliding symbols path
    (declaration : WT.typeDecl) =
  let name = String.concat "." (path @ [ declaration.WT.name.WT.name ])
  and typeParams = List.map fst declaration.WT.typeParams in
  let convert =
    resolveWrittenType allowInternal types path (S.of_list typeParams)
  in
  let definition =
    match declaration.WT.definition with
    | WT.TDAlias target ->
        map (fun typ -> AST.TypeAlias (name, typeParams, typ)) (convert target)
    | WT.TDRecord fields -> (
        let counts =
          List.fold_left
            (fun counts ((field : WT.recordFieldSyntax), _) ->
              let name = snd field.WT.name in
              M.add name
                (1 + Option.value (M.find_opt name counts) ~default:0)
                counts)
            M.empty fields
        in
        match
          List.find_opt
            (fun ((field : WT.recordFieldSyntax), _) ->
              M.find (snd field.WT.name) counts > 1)
            fields
        with
        | Some (field, _) ->
            Error
              ("Duplicate field '" ^ snd field.WT.name ^ "' in record type "
             ^ name)
        | None ->
            map
              (fun fields -> AST.RecordDef (name, typeParams, fields))
              (ResultList.traverse
                 (fun ((field : WT.recordFieldSyntax), _) ->
                   map
                     (fun typ -> (snd field.WT.name, typ))
                     (convert field.WT.typ))
                 fields))
    | WT.TDEnum cases ->
        map
          (fun variants -> AST.SumTypeDef (name, typeParams, variants))
          (ResultList.traverse
             (fun (_, (case : WT.enumCaseSyntax)) ->
               map
                 (fun fields ->
                   ({ AST.name = snd case.WT.name; fields } : AST.variant))
                 (ResultList.traverse
                    (fun (field : WT.enumFieldSyntax) -> convert field.WT.typ)
                    case.WT.fields))
             cases)
  in
  map
    (fun definition ->
      let id, symbols = C.internType name symbols in
      let symbols =
        match definition with
        | AST.RecordDef (_, _, fields) ->
            List.fold_left
              (fun symbols (index, (nameField, _)) ->
                snd (C.internField name nameField index symbols))
              symbols
              (List.mapi (fun index value -> (index, value)) fields)
        | AST.SumTypeDef (_, _, variants) ->
            List.fold_left
              (fun symbols (index, (variant : AST.variant)) ->
                snd
                  (C.internConstructor name variant.AST.name
                     (caseTag colliding name variant.AST.name index)
                     symbols))
              symbols
              (List.mapi (fun index value -> (index, value)) variants)
        | AST.TypeAlias _ -> symbols
      in
      (C.TypeDef (id, C.checkedTypeDef definition), symbols))
    definition

(* Predeclare annotated functions before checking bodies, so calls use stable
   identities regardless of source order. The nominal registry will replace
   the restricted resolver as type declarations are brought into this path. *)
let[@warning "-4"] predeclareFunctions allowInternal types items symbols =
  let builtinFunctions, builtinSymbols =
    List.fold_left
      (fun (functions, symbols) name ->
        let id, symbols = C.internFunction name symbols in
        ( M.add name
            {
              id;
              typeParams = [];
              parameters = [ AST.TString ];
              return = AST.TNever;
            }
            functions,
          symbols ))
      (M.empty, symbols)
      [ "Builtin.testRuntimeError"; "Builtin.crash" ]
  in
  let intrinsicFunctions, intrinsicSymbols =
    M.fold
      (fun name (entry : AST.moduleFunc) (functions, symbols) ->
        let id, symbols = C.internFunction name symbols in
        ( M.add name
            {
              id;
              typeParams = entry.AST.typeParams;
              parameters = entry.AST.paramTypes;
              return = entry.AST.returnType;
            }
            functions,
          symbols ))
      (DarkStdlib.buildModuleRegistry ())
      (builtinFunctions, builtinSymbols)
  in
  let result =
    List.fold_left
      (fun result item ->
        bind result (fun (functions, symbols) ->
            match item with
            | WS.Function (path, fn) ->
                let name = String.concat "." (path @ [ fn.WT.name.WT.name ]) in
                if M.mem name functions then
                  Error ("Duplicate function '" ^ name ^ "'")
                else
                  let explicitParams = List.map fst fn.WT.typeParams in
                  let allParams =
                    List.fold_left
                      (fun found param ->
                        match param with
                        | WT.FPUnit _ -> found
                        | WT.FPNormal (_, _, typ, _, _, _, _) ->
                            collectWrittenTypeParams found typ)
                      explicitParams fn.WT.parameters
                    |> fun found ->
                    collectWrittenTypeParams found fn.WT.returnType
                  in
                  let typeParams = S.of_list allParams in
                  bind
                    (ResultList.traverse
                       (function
                         | WT.FPUnit _ -> Ok AST.TUnit
                         | WT.FPNormal (_, _, typ, _, _, _, _) ->
                             resolveWrittenType allowInternal types path
                               typeParams typ)
                       fn.WT.parameters)
                    (fun parameters ->
                      map
                        (fun return ->
                          let id, symbols = C.internFunction name symbols in
                          ( M.add name
                              { id; typeParams = allParams; parameters; return }
                              functions,
                            symbols ))
                        (resolveWrittenType allowInternal types path typeParams
                           fn.WT.returnType))
            | _ -> Ok (functions, symbols)))
      (Ok (M.empty, intrinsicSymbols))
      items
  in
  map
    (fun (functions, symbols) ->
      (M.fold M.add functions intrinsicFunctions, symbols))
    result

let checkFunction (checkExpression : expressionChecker) globals symbols path
    (fn : WT.fnDecl) =
  let name = String.concat "." (path @ [ fn.WT.name.WT.name ]) in
  match M.find_opt name globals.functions with
  | None -> Error ("Function '" ^ name ^ "' was not predeclared")
  | Some signature -> (
      let parameterNames =
        List.mapi
          (fun index param ->
            match param with
            | WT.FPUnit _ -> "_unit" ^ string_of_int index
            | WT.FPNormal (_, identifier, _, _, _, _, _) -> identifier.WT.name)
          fn.WT.parameters
      in
      if List.length parameterNames <> List.length signature.parameters then
        Error ("Function '" ^ name ^ "' has mismatched parameter metadata")
      else
        let counts =
          List.fold_left
            (fun counts name ->
              M.add name
                (1 + Option.value (M.find_opt name counts) ~default:0)
                counts)
            M.empty parameterNames
        in
        match
          List.find_opt (fun name -> M.find name counts > 1) parameterNames
        with
        | Some duplicate -> Error ("Duplicate parameter '" ^ duplicate ^ "'")
        | None -> (
            let locals, reversed, symbols =
              List.fold_left
                (fun (locals, reversed, symbols) (name, typ) ->
                  let id, symbols = C.allocateBinding name symbols in
                  ( M.add name (typ, id) locals,
                    (id, C.checkedType typ) :: reversed,
                    symbols ))
                (M.empty, [], symbols)
                (List.combine parameterNames signature.parameters)
            in
            match NonEmptyList.tryFromList (List.rev reversed) with
            | None -> Error ("Function '" ^ name ^ "' requires a parameter")
            | Some params ->
                map
                  (fun (_, body, symbols) ->
                    ( ({
                         C.id = signature.id;
                         name;
                         typeParams = signature.typeParams;
                         params;
                         returnType = C.checkedType signature.return;
                         body;
                         recursion = None;
                       }
                        : C.functionDef),
                      symbols ))
                  (checkExpression
                     {
                       globals with
                       modulePath = path;
                       typeParams = S.of_list signature.typeParams;
                       currentFunction =
                         Some (signature.id, name, signature.typeParams);
                     }
                     locals symbols (Some signature.return) fn.WT.body)))

let[@warning "-4"] attachRecursiveGroups program =
  let symbols = C.programSymbols program
  and topLevels = C.programTopLevels program in
  let functions =
    List.filter_map
      (function C.FunctionDef func -> Some func | _ -> None)
      topLevels
  in
  let names =
    S.of_list (List.map (fun (func : C.functionDef) -> func.C.name) functions)
  in
  let graph =
    M.of_list
      (List.map
         (fun (func : C.functionDef) ->
           let dependencies =
             SpecializationIdentity.directDependencies func.C.body
             |> SpecializationIdentity.FunctionSet.elements
             |> List.filter_map (fun id -> C.functionName id symbols)
             |> S.of_list |> S.inter names
           in
           (func.C.name, dependencies))
         functions)
  in
  let required key inventory =
    match M.find_opt key inventory with
    | Some value -> value
    | None -> Crash.crash ("Missing recursive function '" ^ key ^ "'")
  in
  (* Mutual reachability defines an SCC, but materializing every root's closure
    and partitioning the entire remaining catalog for each function is quadratic.
    Classify once, then restore source order for both groups and their members:
    graph traversal order must never change semantic identities. *)
  let indices =
    M.of_list
      (List.mapi
         (fun index (func : C.functionDef) -> (func.C.name, index))
         functions)
  in
  let edges =
    Array.of_list
      (List.map
         (fun (func : C.functionDef) ->
           S.elements (required func.C.name graph)
           |> List.map (fun name -> required name indices))
         functions)
  in
  let components = StronglyConnectedComponents.classify edges in
  let members = Array.make (Array.length components) [] in
  List.iteri
    (fun index func ->
      let component = components.(index) in
      members.(component) <- func :: members.(component))
    functions;
  let emitted = Array.make (Array.length components) false in
  let orderedGroups =
    List.mapi (fun index _ -> components.(index)) functions
    |> List.filter_map (fun component ->
        if emitted.(component) then None
        else (
          emitted.(component) <- true;
          Some (List.rev members.(component))))
  in
  let sourceOrdinals =
    M.of_list
      (List.filter_map
         (function
           | index, C.FunctionDef definition ->
               Some (definition.C.name, index + 1)
           | _ -> None)
         (List.mapi (fun index value -> (index, value)) topLevels))
  in
  let rec groups ordinal remaining resolved =
    match remaining with
    | [] -> resolved
    | [] :: _ -> Crash.crash "Recursive component has no members"
    | ((first : C.functionDef) :: sameGroup) :: later ->
        let availability =
          if sameGroup <> [] then AST.MutualRecursiveMember
          else if S.mem first.C.name (required first.C.name graph) then
            AST.SelfRecursiveMember
          else AST.CompletedGroupMember
        in
        let resolved =
          List.fold_left
            (fun resolved (groupIndex, (definition : C.functionDef)) ->
              let sourceOrdinal = required definition.C.name sourceOrdinals in
              let parsed : AST.parsedRecursiveMember =
                {
                  AST.binding =
                    AST.namedBindingId sourceOrdinal definition.C.name;
                  boundary = AST.scopeBoundaryId 0;
                  member = AST.recursiveMemberId sourceOrdinal;
                  sourceName = definition.C.name;
                  kind = AST.TopLevelFunctionMember;
                }
              in
              let resolvedMember : AST.resolvedRecursiveMember =
                {
                  AST.parsed;
                  group = AST.topLevelRecursiveGroupId ordinal;
                  groupIndex;
                  availability;
                }
              in
              let functionType =
                AST.TFunction
                  ( List.map snd
                      (NonEmptyList.toList
                         (C.functionParameterTypes definition)),
                    C.functionReturnType definition )
              in
              let checkedMember : C.recursiveMember =
                {
                  C.resolved = resolvedMember;
                  monomorphicType = C.checkedType functionType;
                }
              in
              M.add definition.C.name checkedMember resolved)
            resolved
            (List.mapi (fun index value -> (index, value)) (first :: sameGroup))
        in
        groups (ordinal + 1) later resolved
  in
  let resolved = groups 0 orderedGroups M.empty in
  C.programFromCheckedParts
    ( symbols,
      List.map
        (function
          | C.FunctionDef definition ->
              C.FunctionDef
                {
                  definition with
                  C.recursion = M.find_opt definition.C.name resolved;
                }
          | other -> other)
        topLevels )

(* Check annotated, nongeneric functions and sequential values directly from
   WrittenTypes. The production entry point is switched only after declaration
   catalogs, recursion, matches, and generic checking are included. *)
let checkItems (checkExpression : expressionChecker) baseEnvironment
    allowInternal requireEntry items =
  bind (predeclareTypes items) (fun localTypes ->
      let baseGlobals, baseSymbols =
        match baseEnvironment with
        | Some (Environment (globals, symbols)) -> (globals, symbols)
        | None -> (emptyGlobals, C.emptySymbols ())
      in
      let types = M.fold M.add localTypes baseGlobals.types in
      bind (predeclareFunctions allowInternal types items baseSymbols)
        (fun (localFunctions, symbols) ->
          let functions = M.fold M.add localFunctions baseGlobals.functions in
          let colliding = collidingCaseNames types in
          let initialGlobals =
            {
              baseGlobals with
              functions;
              types;
              collidingCases = colliding;
              allowInternal;
              modulePath = [];
              typeParams = S.empty;
              currentFunction = None;
            }
          in
          let result =
            List.fold_left
              (fun result item ->
                bind result (fun (globals, symbols, reversed, entryType) ->
                    let itemName =
                      match item with
                      | WS.Function (path, fn) ->
                          String.concat "." (path @ [ fn.WT.name.WT.name ])
                      | WS.Value (path, value) ->
                          String.concat "." (path @ [ value.WT.name.WT.name ])
                      | WS.Type (path, declaration) ->
                          String.concat "."
                            (path @ [ declaration.WT.name.WT.name ])
                      | WS.Expression (path, _) ->
                          String.concat "." (path @ [ "<entry>" ])
                    in
                    let checked =
                      match item with
                      | WS.Function (path, fn) ->
                          map
                            (fun (definition, symbols) ->
                              ( globals,
                                symbols,
                                C.FunctionDef definition :: reversed,
                                entryType ))
                            (checkFunction checkExpression globals symbols path
                               fn)
                      | WS.Value (path, value) -> (
                          let name =
                            String.concat "." (path @ [ value.WT.name.WT.name ])
                          in
                          let[@warning "-4"] genericAlias =
                            let source =
                              match value.WT.body with
                              | WT.EVariable (_, name) -> Some [ name ]
                              | WT.EFnName (_, name) ->
                                  Some (qualifiedFnName name)
                              | _ -> None
                            in
                            Option.bind source (fun source ->
                                match
                                  resolveFunction
                                    { globals with modulePath = path }
                                    source
                                with
                                | Some signature when signature.typeParams <> []
                                  ->
                                    Some signature
                                | _ -> None)
                          in
                          match genericAlias with
                          | Some signature ->
                              (* A pure polymorphic function alias has no single runtime
                                 closure. Resolve each use to the same function identity
                                 and specialize it there, preserving alias equality. *)
                              Ok
                                ( {
                                    globals with
                                    functions =
                                      M.add name signature globals.functions;
                                  },
                                  symbols,
                                  reversed,
                                  entryType )
                          | None ->
                              map
                                (fun (typ, body, symbols) ->
                                  let id, symbols =
                                    C.internValue name symbols
                                  in
                                  let definition : C.valueDef =
                                    {
                                      C.id;
                                      name;
                                      typ = C.checkedType typ;
                                      body;
                                    }
                                  in
                                  ( {
                                      globals with
                                      values =
                                        M.add name (typ, id) globals.values;
                                    },
                                    symbols,
                                    C.ValueDef definition :: reversed,
                                    entryType ))
                                (checkExpression
                                   { globals with modulePath = path }
                                   M.empty symbols None value.WT.body))
                      | WS.Expression (path, expr) -> (
                          match entryType with
                          | Some _ -> Error "Multiple program entry expressions"
                          | None ->
                              map
                                (fun (typ, body, symbols) ->
                                  ( globals,
                                    symbols,
                                    C.Expression body :: reversed,
                                    Some typ ))
                                (checkExpression
                                   { globals with modulePath = path }
                                   M.empty symbols None expr))
                      | WS.Type (path, declaration) ->
                          map
                            (fun (definition, symbols) ->
                              ( globals,
                                symbols,
                                definition :: reversed,
                                entryType ))
                            (checkTypeDeclaration globals.allowInternal
                               globals.types globals.collidingCases symbols path
                               declaration)
                    in
                    Result.map_error
                      (fun error -> itemName ^ ": " ^ error)
                      checked))
              (Ok (initialGlobals, symbols, [], None))
              items
          in
          bind result (fun (finalGlobals, finalSymbols, reversed, entryType) ->
              match (requireEntry, entryType) with
              | true, None -> Error "Program requires an entry expression"
              | _, typ ->
                  Ok
                    ( Option.value typ ~default:AST.TUnit,
                      attachRecursiveGroups
                        (C.programFromCheckedParts
                           (finalSymbols, List.rev reversed)),
                      Environment (finalGlobals, finalSymbols) ))))
