(* Binding, match-pattern checking, and exhaustiveness from WrittenPatternSupport.ml. *)
open WrittenTypeSupport
module WT = WrittenTypes
module C = CheckedAST
module M = StringOrder.Map
module S = StringOrder.Set

let bind = Result.bind
let map = Result.map

let floatLiteral negative whole fraction =
  let text = (if negative then "-" else "") ^ whole ^ "." ^ fraction in
  let length = String.length text in
  let whitespace = function
    | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' -> true
    | _ -> false
  in
  let rec spaces index =
    if index < length && whitespace text.[index] then spaces (index + 1)
    else index
  in
  let sign index =
    if index < length && (text.[index] = '+' || text.[index] = '-') then
      index + 1
    else index
  in
  let rec digits index =
    if index < length && text.[index] >= '0' && text.[index] <= '9' then
      digits (index + 1)
    else index
  in
  let first = sign (spaces 0) in
  let afterWhole = digits first in
  let afterFraction =
    if afterWhole < length && text.[afterWhole] = '.' then
      digits (afterWhole + 1)
    else afterWhole
  in
  let hasDigits = afterWhole > first || afterFraction > afterWhole + 1 in
  let afterExponent =
    if
      afterFraction < length
      && (text.[afterFraction] = 'e' || text.[afterFraction] = 'E')
    then
      let firstDigit = sign (afterFraction + 1) in
      let lastDigit = digits firstDigit in
      if lastDigit = firstDigit then None else Some lastDigit
    else Some afterFraction
  in
  match afterExponent with
  | Some index when hasDigits && spaces index = length ->
      float_of_string_opt (String.trim text)
  | _ -> None

let rec checkLetPattern pattern typ symbols =
  match pattern with
  | WT.LPUnit _ ->
      map
        (fun () -> (C.LPUnit, M.empty, symbols))
        (requireType (Some AST.TUnit) typ)
  | WT.LPWildcard _ -> Ok (C.LPWildcard, M.empty, symbols)
  | WT.LPVariable (_, name) ->
      let id, symbols = WrittenCheckingState.allocateBinding name symbols in
      Ok (C.LPVariable id, M.singleton name (typ, id), symbols)
  | WT.LPTuple (_, first, _, second, rest, _, _) -> (
      let patterns = first :: second :: List.map snd rest in
      match typ with
      | AST.TTuple types when List.length patterns = List.length types ->
          let result =
            List.fold_left
              (fun result (item, typ) ->
                bind result (fun (checked, locals, symbols) ->
                    bind (checkLetPattern item typ symbols)
                      (fun (item, itemLocals, symbols) ->
                        match
                          List.find_opt
                            (fun name -> M.mem name locals)
                            (List.map fst (M.bindings itemLocals))
                        with
                        | Some name ->
                            Error
                              ("Duplicate binding '" ^ name
                             ^ "' in tuple pattern")
                        | None ->
                            Ok
                              ( item :: checked,
                                M.fold M.add itemLocals locals,
                                symbols ))))
              (Ok ([], M.empty, symbols))
              (List.combine patterns types)
          in
          bind result (fun (reversed, locals, symbols) ->
              match List.rev reversed with
              | first :: second :: rest ->
                  Ok (C.LPTuple (first, second, rest), locals, symbols)
              | _ -> Error "Tuple pattern requires at least two elements")
      | _ -> Error "Tuple pattern does not match the value type")
[@@warning "-4"]

let mergePatternBindings left right =
  match
    List.find_opt
      (fun name -> M.mem name left)
      (List.map fst (M.bindings right))
  with
  | Some name -> Error ("Duplicate binding '" ^ name ^ "' in match pattern")
  | None -> Ok (M.fold M.add right left)

let[@warning "-4"] rec checkMatchPattern globals symbols prebound expected
    pattern =
  let literal typ result =
    map (fun () -> (result, M.empty, symbols)) (requireType (Some typ) expected)
  in
  let rec children symbols bindings reversed remaining =
    match remaining with
    | [] -> Ok (List.rev reversed, bindings, symbols)
    | (child, typ) :: tail ->
        bind (checkMatchPattern globals symbols prebound typ child)
          (fun (child, childBindings, symbols) ->
            bind (mergePatternBindings bindings childBindings) (fun bindings ->
                children symbols bindings (child :: reversed) tail))
  in
  match pattern with
  | WT.MPVariable (_, "_") -> Ok (C.PWildcard, M.empty, symbols)
  | WT.MPVariable (_, name) ->
      let idResult =
        match prebound with
        | Some bindings -> (
            match M.find_opt name bindings with
            | Some (typ, id)
              when Option.is_some (Unification.reconcileTypes None typ expected)
              ->
                Ok (id, symbols)
            | Some _ ->
                Error
                  ("Or-pattern binding '" ^ name ^ "' has inconsistent types")
            | None ->
                Error
                  ("Or-pattern binding '" ^ name
                 ^ "' is missing in another branch"))
        | None -> Ok (WrittenCheckingState.allocateBinding name symbols)
      in
      map
        (fun (id, symbols) ->
          let bindingType =
            Option.value
              (Option.bind prebound (fun bindings ->
                   Option.bind (M.find_opt name bindings) (fun (typ, _) ->
                       Unification.reconcileTypes None typ expected)))
              ~default:expected
          in
          (C.PVariable id, M.singleton name (bindingType, id), symbols))
        idResult
  | WT.MPUnit _ -> literal AST.TUnit C.PUnit
  | WT.MPBool (_, value) -> literal AST.TBool (C.PBool value)
  | WT.MPInt (_, (_, value)) -> literal AST.TInt (C.PBigInt value)
  | WT.MPInt64 (_, (_, value), _) -> literal AST.TInt64 (C.PInt64 value)
  | WT.MPInt8 (_, (_, value), _) -> literal AST.TInt8 (C.PInt8Literal value)
  | WT.MPUInt8 (_, (_, value), _) -> literal AST.TUInt8 (C.PUInt8Literal value)
  | WT.MPInt16 (_, (_, value), _) -> literal AST.TInt16 (C.PInt16Literal value)
  | WT.MPUInt16 (_, (_, value), _) ->
      literal AST.TUInt16 (C.PUInt16Literal value)
  | WT.MPInt32 (_, (_, value), _) -> literal AST.TInt32 (C.PInt32Literal value)
  | WT.MPUInt32 (_, (_, value), _) ->
      literal AST.TUInt32 (C.PUInt32Literal value)
  | WT.MPUInt64 (_, (_, value), _) ->
      literal AST.TUInt64 (C.PUInt64Literal value)
  | WT.MPInt128 (_, (_, value), _) ->
      literal AST.TInt128 (C.PInt128Literal value)
  | WT.MPUInt128 (_, (_, value), _) ->
      literal AST.TUInt128 (C.PUInt128Literal value)
  | WT.MPString (_, contents, _, _) ->
      literal AST.TString (C.PString (Option.fold ~none:"" ~some:snd contents))
  | WT.MPChar (_, contents, _, _) ->
      literal AST.TChar (C.PChar (Option.fold ~none:"" ~some:snd contents))
  | WT.MPFloat (_, negative, whole, fraction) -> (
      match floatLiteral negative whole fraction with
      | Some value -> literal AST.TFloat64 (C.PFloat value)
      | None ->
          Error
            ("Invalid Float pattern '"
            ^ (if negative then "-" else "")
            ^ whole ^ "." ^ fraction ^ "'"))
  | WT.MPTuple (_, first, _, second, rest, _, _) -> (
      let patterns = first :: second :: List.map snd rest in
      let types =
        match expected with
        | AST.TTuple types when List.length patterns = List.length types ->
            Some types
        | AST.TVar name ->
            Some
              (List.mapi
                 (fun index _ ->
                   AST.TVar ("__tuple_elem_" ^ name ^ "_" ^ string_of_int index))
                 patterns)
        | AST.TInferenceVar (_, identity) ->
            Some
              (List.mapi
                 (fun index _ ->
                   let name = "tuple_element_" ^ string_of_int index in
                   AST.TInferenceVar (name, identity ^ "/" ^ name))
                 patterns)
        | _ -> None
      in
      match types with
      | Some types ->
          map
            (fun (patterns, bindings, symbols) ->
              (C.PTuple patterns, bindings, symbols))
            (children symbols M.empty [] (List.combine patterns types))
      | None -> Error "Tuple pattern does not match the scrutinee type")
  | WT.MPList (_, contents, _, _) -> (
      let elementType =
        match expected with
        | AST.TList typ -> Some typ
        | AST.TVar name -> Some (AST.TVar ("__list_elem_" ^ name))
        | AST.TInferenceVar (_, identity) ->
            Some
              (AST.TInferenceVar ("list_element", identity ^ "/list_element"))
        | _ -> None
      in
      match elementType with
      | Some typ ->
          map
            (fun (patterns, bindings, symbols) ->
              (C.PList patterns, bindings, symbols))
            (children symbols M.empty []
               (List.map (fun (item, _) -> (item, typ)) contents))
      | None -> Error "List pattern requires a list scrutinee")
  | WT.MPListCons (_, head, tail, _) -> (
      let elementType =
        match expected with
        | AST.TList typ -> Some typ
        | AST.TVar name -> Some (AST.TVar ("__list_elem_" ^ name))
        | AST.TInferenceVar (_, identity) ->
            Some
              (AST.TInferenceVar ("list_element", identity ^ "/list_element"))
        | _ -> None
      in
      match elementType with
      | Some typ ->
          bind (checkMatchPattern globals symbols prebound typ head)
            (fun (head, headBindings, symbols) ->
              bind
                (checkMatchPattern globals symbols prebound (AST.TList typ) tail)
                (fun (tail, tailBindings, symbols) ->
                  map
                    (fun bindings ->
                      (C.PListCons ([ head ], tail), bindings, symbols))
                    (mergePatternBindings headBindings tailBindings)))
      | None -> Error "List cons pattern requires a list scrutinee")
  | WT.MPEnum (_, (_, caseName), fields) -> (
      match expected with
      | AST.TSum (canonical, args) -> (
          match M.find_opt canonical globals.types with
          | None -> Error ("Unknown enum type '" ^ canonical ^ "'")
          | Some entry -> (
              match entry.definition with
              | WT.TDEnum cases -> (
                  match
                    List.find_opt
                      (fun (_, (_, (item : WT.enumCaseSyntax))) ->
                        snd item.WT.name = caseName)
                      (List.mapi (fun index value -> (index, value)) cases)
                  with
                  | None ->
                      Error
                        ("Unknown constructor '" ^ canonical ^ "." ^ caseName
                       ^ "' in pattern")
                  | Some (_, (_, item))
                    when List.length fields <> List.length item.WT.fields ->
                      Error
                        ("Constructor pattern '" ^ canonical ^ "." ^ caseName
                       ^ "' has wrong field count")
                  | Some (ordinal, (_, item)) ->
                      let substitution =
                        M.of_list (List.combine entry.params args)
                      in
                      bind
                        (ResultList.traverse
                           (fun (field : WT.enumFieldSyntax) ->
                             map
                               (Types.applySubst substitution)
                               (resolveWrittenType globals.allowInternal
                                  globals.types entry.path
                                  (S.of_list entry.params) field.WT.typ))
                           item.WT.fields)
                        (fun types ->
                          map
                            (fun (fields, bindings, symbols) ->
                              let tag =
                                caseTag globals.collidingCases canonical
                                  caseName ordinal
                              in
                              let id, symbols =
                                WrittenCheckingState.internConstructor canonical
                                  caseName tag symbols
                              in
                              (C.PConstructor (id, fields), bindings, symbols))
                            (children symbols M.empty []
                               (List.combine fields types))))
              | _ -> Error ("Type '" ^ canonical ^ "' is not an enum")))
      | _ -> Error "Constructor pattern requires an enum scrutinee")
  | WT.MPOr (_, alternatives) -> (
      match alternatives with
      | [] -> Error "Or-pattern requires at least one alternative"
      | first :: rest ->
          bind (checkMatchPattern globals symbols prebound expected first)
            (fun (firstPattern, bindings, symbols) ->
              let result =
                List.fold_left
                  (fun result alternative ->
                    bind result (fun (reversed, commonBindings, symbols) ->
                        bind
                          (checkMatchPattern globals symbols
                             (Some commonBindings) expected alternative)
                          (fun (alternative, otherBindings, symbols) ->
                            if
                              S.of_list
                                (List.map fst (M.bindings otherBindings))
                              <> S.of_list
                                   (List.map fst (M.bindings commonBindings))
                            then
                              Error
                                "Every branch of an or-pattern must bind the \
                                 same names"
                            else
                              map
                                (fun merged ->
                                  (alternative :: reversed, merged, symbols))
                                (M.fold
                                   (fun name (otherType, _) result ->
                                     bind result (fun merged ->
                                         match M.find_opt name merged with
                                         | Some (priorType, id) -> (
                                             match
                                               Unification.reconcileTypes None
                                                 priorType otherType
                                             with
                                             | Some commonType ->
                                                 Ok
                                                   (M.add name (commonType, id)
                                                      merged)
                                             | None ->
                                                 Error
                                                   ("Or-pattern binding '"
                                                  ^ name
                                                  ^ "' has inconsistent types"))
                                         | None ->
                                             Error
                                               ("Or-pattern binding '" ^ name
                                              ^ "' is missing in another branch"
                                               )))
                                   otherBindings (Ok commonBindings)))))
                  (Ok ([ firstPattern ], bindings, symbols))
                  rest
              in
              bind result (fun (reversed, commonBindings, symbols) ->
                  match NonEmptyList.tryFromList (List.rev reversed) with
                  | Some patterns -> Ok (C.POr patterns, commonBindings, symbols)
                  | None -> Error "Or-pattern requires an alternative")))
  | WT.MPError _ -> Error "Invalid recovery pattern in validated source"

let[@warning "-4"] rec patternAlternatives = function
  | C.POr alternatives ->
      List.concat_map patternAlternatives (NonEmptyList.toList alternatives)
  | other -> [ other ]

let[@warning "-4"] patternCoversAny = function
  | C.PWildcard | C.PVariable _ -> true
  | _ -> false

let[@warning "-4"] rec patternCoversLiteral value pattern =
  if patternCoversAny pattern then true
  else
    match (value, pattern) with
    | C.UnitLiteral, C.PUnit -> true
    | C.BoolLiteral left, C.PBool right -> left = right
    | C.Int64Literal left, C.PInt64 right -> left = right
    | C.BigIntLiteral left, C.PBigInt right -> Z.equal left right
    | C.StringLiteral left, C.PString right -> left = right
    | C.CharLiteral left, C.PChar right -> left = right
    | C.FloatLiteral left, C.PFloat right -> left = right
    | C.TupleLiteral tuple, C.PTuple patterns ->
        let elements = C.tupleElementsToList tuple in
        List.length elements = List.length patterns
        && List.for_all2 patternCoversLiteral elements patterns
    | C.ListLiteral elements, C.PList patterns ->
        List.length elements = List.length patterns
        && List.for_all2 patternCoversLiteral elements patterns
    | _ -> false

let[@warning "-4"] matchIsExhaustive globals symbols scrutineeType scrutinee
    cases =
  let patterns =
    List.concat_map
      (fun (arm : C.matchCase) ->
        match arm.C.guard with
        | Some _ -> []
        | None ->
            List.concat_map patternAlternatives
              (NonEmptyList.toList arm.C.patterns))
      cases
  in
  let contains predicate = List.exists predicate patterns in
  let rec witnesses typ =
    match typ with
    | AST.TBool -> [ C.PBool true; C.PBool false ]
    | AST.TUnit -> [ C.PUnit ]
    | AST.TList elementType ->
        C.PList []
        :: List.concat_map
             (fun head ->
               [
                 C.PList [ head ];
                 C.PListCons ([ head ], C.PListCons ([ head ], C.PWildcard));
               ])
             (witnesses elementType)
    | AST.TSum (canonical, _) -> (
        match M.find_opt canonical globals.types with
        | Some entry -> (
            match entry.definition with
            | WT.TDEnum variants ->
                List.mapi
                  (fun ordinal (_, (variant : WT.enumCaseSyntax)) ->
                    let name = snd variant.WT.name in
                    let tag =
                      caseTag globals.collidingCases canonical name ordinal
                    in
                    let id, _ =
                      WrittenCheckingState.internConstructor canonical name tag
                        symbols
                    in
                    C.PConstructor
                      ( id,
                        List.init (List.length variant.WT.fields) (fun _ ->
                            C.PWildcard) ))
                  variants
            | _ -> [ C.PWildcard ])
        | None -> [ C.PWildcard ])
    | AST.TTuple types ->
        List.map
          (fun product -> C.PTuple product)
          (List.fold_left
             (fun products typ ->
               List.concat_map
                 (fun product ->
                   List.map
                     (fun witness -> product @ [ witness ])
                     (witnesses typ))
                 products)
             [ [] ] types)
    | _ -> [ C.PWildcard ]
  in
  let rec coversWitness pattern witness =
    if patternCoversAny pattern then true
    else
      match (pattern, witness) with
      | C.PBool left, C.PBool right -> left = right
      | C.PUnit, C.PUnit -> true
      | C.PList [], C.PList [] -> true
      | C.PList left, C.PList right when List.length left = List.length right ->
          List.for_all2 coversWitness left right
      | C.PListCons ([ head ], tail), C.PList [ single ] ->
          coversWitness head single && coversWitness tail (C.PList [])
      | C.PListCons (leftHeads, leftTail), C.PListCons (rightHeads, rightTail)
        when List.length leftHeads = List.length rightHeads ->
          List.for_all2 coversWitness leftHeads rightHeads
          && coversWitness leftTail rightTail
      | ( C.PConstructor (leftId, leftFields),
          C.PConstructor (rightId, rightFields) )
        when leftId = rightId
             && List.length leftFields = List.length rightFields ->
          List.for_all2 coversWitness leftFields rightFields
      | C.PTuple left, C.PTuple right when List.length left = List.length right
        ->
          List.for_all2 coversWitness left right
      | C.POr alternatives, _ ->
          List.exists
            (fun alternative -> coversWitness alternative witness)
            (NonEmptyList.toList alternatives)
      | _ -> false
  in
  if contains patternCoversAny || contains (patternCoversLiteral scrutinee) then
    true
  else
    match scrutineeType with
    | AST.TUnit -> contains (function C.PUnit -> true | _ -> false)
    | AST.TBool ->
        contains (function C.PBool true -> true | _ -> false)
        && contains (function C.PBool false -> true | _ -> false)
    | AST.TTuple _ | AST.TList _ ->
        List.for_all
          (fun witness ->
            List.exists (fun pattern -> coversWitness pattern witness) patterns)
          (witnesses scrutineeType)
    | AST.TSum (canonical, args) -> (
        match M.find_opt canonical globals.types with
        | Some entry -> (
            match entry.definition with
            | WT.TDEnum variants ->
                List.for_all
                  (fun (_, (variant : WT.enumCaseSyntax)) ->
                    let name = snd variant.WT.name in
                    let substitution =
                      M.of_list (List.combine entry.params args)
                    in
                    match
                      ResultList.traverse
                        (fun (field : WT.enumFieldSyntax) ->
                          map
                            (Types.applySubst substitution)
                            (resolveWrittenType globals.allowInternal
                               globals.types entry.path (S.of_list entry.params)
                               field.WT.typ))
                        variant.WT.fields
                    with
                    | Error _ -> false
                    | Ok types ->
                        let products =
                          List.fold_left
                            (fun products typ ->
                              List.concat_map
                                (fun product ->
                                  List.map
                                    (fun witness -> product @ [ witness ])
                                    (witnesses typ))
                                products)
                            [ [] ] types
                        in
                        List.for_all
                          (fun fields ->
                            contains (function
                              | C.PConstructor (id, patterns)
                                when List.length patterns = List.length fields
                                     && WrittenCheckingState.constructorInfo id
                                          symbols
                                        = Some (canonical, name) ->
                                  List.for_all2 coversWitness patterns fields
                              | _ -> false))
                          products)
                  variants
            | _ -> false)
        | None -> false)
    | _ -> false
