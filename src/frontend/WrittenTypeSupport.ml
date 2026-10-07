(* Shared type inventories and structural-record checking from WrittenTypeSupport.ml. *)
module WT = WrittenTypes
module M = StringOrder.Map
module S = StringOrder.Set

let bind = Result.bind
let map = Result.map

type locals = (AST.semanticType * AST.bindingId) M.t
type checkedExpression = AST.semanticType * CheckedAST.expr * CheckedAST.symbols

type functionSignature = {
  id : AST.functionId;
  typeParams : string list;
  parameters : AST.semanticType list;
  return : AST.semanticType;
}

type typeKind = RecordKind | SumKind | AliasKind

type typeEntry = {
  kind : typeKind;
  params : string list;
  path : string list;
  definition : WT.typeDefinition;
}

type typeInventory = typeEntry M.t

type globals = {
  functions : functionSignature M.t;
  values : locals;
  types : typeInventory;
  collidingCases : S.t;
  allowInternal : bool;
  typeParams : S.t;
  modulePath : string list;
  currentFunction : (AST.functionId * string * string list) option;
}

let emptyGlobals =
  {
    functions = M.empty;
    values = M.empty;
    types = M.empty;
    collidingCases = S.empty;
    allowInternal = false;
    typeParams = S.empty;
    modulePath = [];
    currentFunction = None;
  }

let[@warning "-4"] collidingCaseNames types =
  let byName =
    M.fold
      (fun owner entry grouped ->
        match entry.definition with
        | WT.TDEnum cases ->
            List.fold_left
              (fun grouped (_, (item : WT.enumCaseSyntax)) ->
                let name = snd item.WT.name in
                M.add name
                  (owner :: Option.value (M.find_opt name grouped) ~default:[])
                  grouped)
              grouped cases
        | _ -> grouped)
      types M.empty
  in
  M.fold
    (fun name owners result ->
      if S.cardinal (S.of_list owners) > 1 then S.add name result else result)
    byName S.empty

let caseTag colliding owner caseName ordinal =
  if S.mem caseName colliding then AST.constructorRuntimeIdentity owner caseName
  else ordinal

let qualifiedFnName (name : WT.qualifiedFnIdentifier) =
  List.map
    (fun ((identifier : WT.identifier), _) -> identifier.WT.name)
    name.WT.modules
  @ [ name.WT.fn.WT.name ]

let restrictedIdentifier allowInternal segments =
  (not allowInternal)
  && List.exists
       (fun segment ->
         String.length segment >= 2 && String.sub segment 0 2 = "__")
       segments

let resolveFunction globals segments =
  List.find_map
    (fun candidate -> M.find_opt candidate globals.functions)
    (NameResolution.candidateSpellings NameResolution.Callable
       globals.modulePath
       (String.concat "." segments))

let resolveValue globals segments =
  List.find_map
    (fun candidate -> M.find_opt candidate globals.values)
    (NameResolution.candidateSpellings NameResolution.Value globals.modulePath
       (String.concat "." segments))

let requireType expected actual =
  match expected with
  | Some typ when Option.is_none (Unification.reconcileTypes None typ actual) ->
      Error
        ("Expected "
        ^ StructuralFormat.semanticType typ
        ^ ", got "
        ^ StructuralFormat.semanticType actual)
  | _ -> Ok ()

let checkedLiteral expected symbols typ expression =
  map
    (fun () ->
      let resolved =
        if typ = AST.TNever then AST.TNever
        else
          Option.value
            (Option.bind expected (fun wanted ->
                 Unification.reconcileTypes None wanted typ))
            ~default:typ
      in
      (resolved, expression, symbols))
    (requireType expected typ)

(* Resolve the type syntax while retaining the compiler's nominal registry as
   the authority for custom names. No source AST type is constructed here. *)
let rec typeReference resolveCustom typeParams reference =
  let convert = typeReference resolveCustom typeParams in
  let convertMany references = ResultList.traverse convert references in
  match reference with
  | WT.TUnit _ -> Ok AST.TUnit
  | WT.TBool _ -> Ok AST.TBool
  | WT.TInt _ -> Ok AST.TInt
  | WT.TInt8 _ -> Ok AST.TInt8
  | WT.TUInt8 _ -> Ok AST.TUInt8
  | WT.TInt16 _ -> Ok AST.TInt16
  | WT.TUInt16 _ -> Ok AST.TUInt16
  | WT.TInt32 _ -> Ok AST.TInt32
  | WT.TUInt32 _ -> Ok AST.TUInt32
  | WT.TInt64 _ -> Ok AST.TInt64
  | WT.TUInt64 _ -> Ok AST.TUInt64
  | WT.TInt128 _ -> Ok AST.TInt128
  | WT.TUInt128 _ -> Ok AST.TUInt128
  | WT.TFloat _ -> Ok AST.TFloat64
  | WT.TChar _ -> Ok AST.TChar
  | WT.TString _ -> Ok AST.TString
  | WT.TDateTime _ -> Ok AST.TDateTime
  | WT.TUuid _ -> resolveCustom [] "Uuid" []
  | WT.TBlob _ -> Ok AST.TBlob
  | WT.TList (_, _, _, inner, _) ->
      map (fun typ -> AST.TList typ) (convert inner)
  | WT.TDict (_, _, _, key, _, value, _) ->
      bind (convert key) (fun key ->
          map (fun value -> AST.TDict (key, value)) (convert value))
  | WT.TTuple (_, first, _, second, rest, _, _) ->
      map
        (fun types -> AST.TTuple types)
        (convertMany (first :: second :: List.map snd rest))
  | WT.TFn (_, arguments, ret) ->
      bind
        (convertMany (List.map fst arguments))
        (fun arguments ->
          map (fun ret -> AST.TFunction (arguments, ret)) (convert ret))
  | WT.TVariable (_, _, (_, name)) ->
      if S.mem name typeParams then Ok (AST.TVar name)
      else Error ("Undeclared type parameter '" ^ name ^ "'")
  | WT.TCustom name ->
      bind (convertMany name.WT.typeArgs) (fun args ->
          resolveCustom
            (List.map
               (fun ((identifier : WT.identifier), _) -> identifier.WT.name)
               name.WT.modules)
            name.WT.typ.WT.name args)

(* Function annotations may introduce type parameters without listing them
   after the function name. Keep their first-seen order for positional calls. *)
let rec collectWrittenTypeParams found reference =
  let collect = collectWrittenTypeParams in
  let collectMany items = List.fold_left collect found items in
  match reference with
  | WT.TVariable (_, _, (_, name)) ->
      if List.mem name found then found else found @ [ name ]
  | WT.TList (_, _, _, inner, _) -> collect found inner
  | WT.TDict (_, _, _, key, _, value, _) -> collect (collect found key) value
  | WT.TTuple (_, first, _, second, rest, _, _) ->
      collectMany (first :: second :: List.map snd rest)
  | WT.TFn (_, arguments, ret) ->
      collect (collectMany (List.map fst arguments)) ret
  | WT.TCustom name -> collectMany name.WT.typeArgs
  | WT.TUnit _ | WT.TBool _ | WT.TInt _ | WT.TInt8 _ | WT.TUInt8 _ | WT.TInt16 _
  | WT.TUInt16 _ | WT.TInt32 _ | WT.TUInt32 _ | WT.TInt64 _ | WT.TUInt64 _
  | WT.TInt128 _ | WT.TUInt128 _ | WT.TFloat _ | WT.TChar _ | WT.TString _
  | WT.TDateTime _ | WT.TUuid _ | WT.TBlob _ ->
      found

let[@warning "-4"] resolveWrittenType allowInternal types modulePath typeParams
    reference =
  let rec convert seen path parameters syntax =
    typeReference (resolveCustom seen path) parameters syntax
  and resolveCustom seen path modules name args =
    match (allowInternal, modules, name, args) with
    | true, [], "RawPtr", [] -> Ok AST.TInternalRawPtr
    | false, [], "RawPtr", [] ->
        Error "RawPtr is reserved for compiler-internal source"
    | _, [], "Stream", [ element ] -> Ok (AST.TStream element)
    | _ -> (
        let spelling = String.concat "." (modules @ [ name ]) in
        match
          List.find_map
            (fun candidate ->
              Option.map
                (fun metadata -> (candidate, metadata))
                (M.find_opt candidate types))
            (NameResolution.candidateSpellings NameResolution.Type path spelling)
        with
        | None -> Error ("Unknown type '" ^ spelling ^ "'")
        | Some (canonical, entry)
          when List.length entry.params <> List.length args ->
            Error
              ("Type '" ^ canonical ^ "' expects "
              ^ string_of_int (List.length entry.params)
              ^ " arguments, got "
              ^ string_of_int (List.length args))
        | Some (canonical, entry) -> (
            match (entry.kind, entry.definition) with
            | RecordKind, _ -> Ok (AST.TRecord (canonical, args))
            | SumKind, _ -> Ok (AST.TSum (canonical, args))
            | AliasKind, WT.TDAlias target ->
                if S.mem canonical seen then
                  Error ("Cyclic type alias '" ^ canonical ^ "'")
                else
                  map
                    (Types.applySubst
                       (M.of_list (List.combine entry.params args)))
                    (convert (S.add canonical seen) entry.path
                       (S.of_list entry.params) target)
            | AliasKind, _ ->
                Crash.crash "Alias type entry has no alias definition"))
  in
  convert S.empty modulePath typeParams reference

let findNamedType globals (name : WT.qualifiedTypeIdentifier) =
  let spelling =
    String.concat "."
      (List.map
         (fun ((identifier : WT.identifier), _) -> identifier.WT.name)
         name.WT.modules
      @ [ name.WT.typ.WT.name ])
  in
  match
    List.find_map
      (fun canonical ->
        Option.map
          (fun entry -> (canonical, entry))
          (M.find_opt canonical globals.types))
      (NameResolution.candidateSpellings NameResolution.Type globals.modulePath
         spelling)
  with
  | Some found -> Ok found
  | None -> Error ("Unknown type '" ^ spelling ^ "'")

let[@warning "-4"] resolveNamedType globals expected
    (name : WT.qualifiedTypeIdentifier) inferredArgs =
  bind (findNamedType globals name) (fun (canonical, entry) ->
      bind
        (ResultList.traverse
           (resolveWrittenType globals.allowInternal globals.types
              globals.modulePath S.empty)
           name.WT.typeArgs)
        (fun givenArgs ->
          let args =
            match (givenArgs, expected, inferredArgs) with
            | [], Some wanted, Some inferred
              when Unification.containsTVar wanted ->
                inferred
            | [], Some (AST.TRecord (_, wantedArgs)), Some inferred
            | [], Some (AST.TSum (_, wantedArgs)), Some inferred
              when List.length wantedArgs <> List.length entry.params ->
                inferred
            | [], Some (AST.TRecord (wanted, inferred)), _
            | [], Some (AST.TSum (wanted, inferred)), _
              when wanted = canonical ->
                inferred
            | [], _, Some inferred -> inferred
            | _ -> givenArgs
          in
          if List.length args <> List.length entry.params then
            Error
              ("Type '" ^ canonical ^ "' expects "
              ^ string_of_int (List.length entry.params)
              ^ " arguments, got "
              ^ string_of_int (List.length args))
          else
            match entry.definition with
            | WT.TDAlias target ->
                bind
                  (map
                     (Types.applySubst
                        (M.of_list (List.combine entry.params args)))
                     (resolveWrittenType globals.allowInternal globals.types
                        entry.path (S.of_list entry.params) target))
                  (function
                    | AST.TRecord (targetName, targetArgs)
                    | AST.TSum (targetName, targetArgs) -> (
                        match M.find_opt targetName globals.types with
                        | Some targetEntry ->
                            Ok (targetName, targetEntry, targetArgs)
                        | None -> Error ("Unknown type '" ^ targetName ^ "'"))
                    | _ -> Ok (canonical, entry, args))
            | _ -> Ok (canonical, entry, args)))

let[@warning "-4"] recordFields allowInternal types entry args =
  match entry.definition with
  | WT.TDRecord fields ->
      let substitution = M.of_list (List.combine entry.params args) in
      ResultList.traverse
        (fun ((field : WT.recordFieldSyntax), _) ->
          map
            (fun typ -> (snd field.WT.name, Types.applySubst substitution typ))
            (resolveWrittenType allowInternal types entry.path
               (S.of_list entry.params) field.WT.typ))
        fields
  | _ -> Error "Expected a record type"

module TypePairSet = Set.Make (struct
  type t = AST.semanticType * AST.semanticType

  let compare (a, b) (c, d) =
    let order = AST.compareSemanticType a c in
    if order = 0 then AST.compareSemanticType b d else order
end)

(* Equality and dictionary keys use the field layout, even when source record
   declarations have different names. Keep ordinary assignments nominal. *)
let[@warning "-4"] structuralEqualityCompatible globals left right =
  let rec compatible seen left right =
    if Option.is_some (Unification.reconcileTypes None left right) then true
    else if TypePairSet.mem (left, right) seen then true
    else
      let seen = TypePairSet.add (left, right) seen in
      match (left, right) with
      | AST.TRecord (leftName, leftArgs), AST.TRecord (rightName, rightArgs)
        -> (
          let fields name args =
            Option.bind (M.find_opt name globals.types) (fun entry ->
                Result.to_option
                  (recordFields globals.allowInternal globals.types entry args))
          in
          match (fields leftName leftArgs, fields rightName rightArgs) with
          | Some leftFields, Some rightFields
            when List.length leftFields = List.length rightFields ->
              let rightByName = M.of_list rightFields in
              List.for_all
                (fun (name, typ) ->
                  Option.fold ~none:false ~some:(compatible seen typ)
                    (M.find_opt name rightByName))
                leftFields
          | _ -> false)
      | _ -> false
  in
  compatible TypePairSet.empty left right

let[@warning "-4"] convertStructuralRecord globals targetType actualType
    expression symbols =
  let rec convert target actual value symbols =
    if Option.is_some (Unification.reconcileTypes None target actual) then
      Ok (value, symbols)
    else
      match (target, actual) with
      | ( AST.TRecord (targetName, targetArgs),
          AST.TRecord (actualName, actualArgs) ) -> (
          match
            ( M.find_opt targetName globals.types,
              M.find_opt actualName globals.types )
          with
          | Some targetEntry, Some actualEntry ->
              bind
                (recordFields globals.allowInternal globals.types targetEntry
                   targetArgs) (fun targetFields ->
                  bind
                    (recordFields globals.allowInternal globals.types
                       actualEntry actualArgs) (fun actualFields ->
                      let actualByName =
                        M.of_list
                          (List.mapi
                             (fun index (name, typ) -> (name, (index, typ)))
                             actualFields)
                      in
                      let binding, afterBinding =
                        CheckedAST.allocateBinding "__structural_record" symbols
                      in
                      let typeId, afterType =
                        CheckedAST.internType targetName afterBinding
                      in
                      let result =
                        List.fold_left
                          (fun state (targetIndex, (fieldName, fieldType)) ->
                            bind state (fun (reversed, currentSymbols) ->
                                match M.find_opt fieldName actualByName with
                                | None ->
                                    Error
                                      ("Record field '" ^ fieldName
                                     ^ "' is missing")
                                | Some (actualIndex, actualFieldType) ->
                                    let sourceField, afterSource =
                                      CheckedAST.internField actualName
                                        fieldName actualIndex currentSymbols
                                    in
                                    bind
                                      (convert fieldType actualFieldType
                                         (CheckedAST.RecordAccess
                                            ( CheckedAST.Local binding,
                                              sourceField ))
                                         afterSource)
                                      (fun (converted, afterValue) ->
                                        let targetField, afterTarget =
                                          CheckedAST.internField targetName
                                            fieldName targetIndex afterValue
                                        in
                                        Ok
                                          ( (targetField, converted) :: reversed,
                                            afterTarget ))))
                          (Ok ([], afterType))
                          (List.mapi
                             (fun index value -> (index, value))
                             targetFields)
                      in
                      bind result (fun (reversed, finalSymbols) ->
                          map
                            (fun fields ->
                              let reference : CheckedAST.recordReference =
                                {
                                  CheckedAST.typeId;
                                  typeArgs =
                                    List.map CheckedAST.checkedType targetArgs;
                                }
                              in
                              ( CheckedAST.Let
                                  ( CheckedAST.LPVariable binding,
                                    value,
                                    CheckedAST.RecordLiteral (reference, fields)
                                  ),
                                finalSymbols ))
                            (CheckedAST.completeRecordFields typeId
                               (List.length targetFields) (List.rev reversed)))))
          | _ -> Error "Unknown structural record type")
      | _ ->
          Error
            ("Cannot structurally convert "
            ^ StructuralFormat.semanticType actual
            ^ " to "
            ^ StructuralFormat.semanticType target)
  in
  convert targetType actualType expression symbols
