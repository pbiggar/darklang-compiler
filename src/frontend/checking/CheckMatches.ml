(*
   CheckMatches.ml - Check Match expressions while preserving source diagnostics and order.
*)
(* CheckMatches.ml - Check Match expressions while preserving source diagnostics and order. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
module I = Set.Make (Int)
module U = Unification
module T = Types
(*
   Type check the scrutinee first
   Extract bindings from a pattern based on scrutinee type
   Runtime error scrutinees are bottom-like: allow typechecking to proceed
   so evaluation order preserves the runtime failure at execution time.
   Leave unresolved pattern literals flexible until concrete type information arrives.
   This is important for patterns like `match [] with | [1L] -> ...`.
   See ensureLiteralType: preserve runtime-error propagation by not
   rejecting pattern type checks on known failing scrutinees.
   Get type arguments from scrutinee type to substitute into payload type
   Build substitution from type params to type args
   Resolve type alias before matching (e.g., Pair<Int64> -> (Int64, Int64))
   Tuple arity mismatch in pattern should be treated as a non-match.
   Match lowering emits a false condition for this pattern shape.
   Preserve runtime error propagation for known failing scrutinees.
   Keep non-binding behavior for tuple patterns that would otherwise
   introduce guard/body variables on an incompatible scrutinee type.
   Each element pattern binds variables of the list's element type
   Head patterns bind to element type
   Tail pattern binds to List<elemType>
   Exhaustiveness is an AOT property: lowering must never need a
   synthetic runtime match-failure arm. A guarded case cannot cover
   any value because its guard may be false.
   Constructor payload types can retain their declaration-level
   generic shape in the variant registry. Once type checking has
   accepted a payload pattern, a variable/wildcard tuple payload is
   irrefutable regardless of that unresolved representation.
   A module may share its type's name: the variant registry
   records `Stdlib.Result`, while the concrete type is
   `Stdlib.Result.Result`.
   Tuple matches are decision matrices. Split each finite head type
   into its public constructors, then prove that the remaining
   columns cover every resulting row. This covers, for example,
   Result.map2's (Ok, Ok), (Error, _), (_, Error) matrix.
   Coverage composes through constructor payloads. In particular,
   `Ok(Linux) | Ok(MacOS) | ...` covers `Ok(OS)` when the nested
   OS constructors are complete; requiring one `Ok(_)` arm loses
   that information and rejects valid interpreter programs.
   A literal scrutinee is safe when at least one arm definitely
   matches it. Earlier unknown arms do not invalidate that proof:
   they either select a body themselves or fall through to the
   definitely matching arm.
   Type check each case and ensure they all return the same type
   Returns (resultType, transformedCases)
   Specialized checked functions can re-enter checking. Reopen their
   resolved patterns for validation, then resolve them again at this
   checked boundary so all ordinary pattern rules remain centralized.
   Extract bindings from first pattern after validation.
   Type check guard if present (must be Bool)
   Retry unconstrained so we can report mismatch against
   the case result type ("match body"), not literal context.
   Use reconcileTypes to handle type variables and type aliases
   Update resultType to the reconciled (concrete) type
   Pass expectedType to first case so empty lists, None, etc. get the right type
   Use reconcileTypes for expected type check too
*)
let check checkExpr sumNames sums env registry lookup generic warnings modules aliases expected scrutinee cases =
 let ( let* ) = Result.bind in
 let recurse value env expected = checkExpr value env registry lookup generic warnings modules aliases expected in
 let scrutineeExpected = match scrutinee with ListLiteral [] -> Some (TList (TVar U.emptyListElementVar)) | _ -> None in
 let* scrutineeType, scrutinee = recurse scrutinee env scrutineeExpected in
 let combineBindings results = List.fold_left (fun acc value -> match acc, value with Ok acc, Ok value -> Ok (acc @ value) | Error error, _ | _, Error error -> Error error) (Ok []) results in
 let rec extract pattern typ allowLengthMismatch =
  let literal expected = let resolved = T.resolveType aliases typ in
   match resolved with
   | resolved when isNeverType resolved || resolved = expected -> Ok []
   | TVar _ | TInferenceVar _ -> Ok []
   | _ -> Error (GenericError (formatPatternMismatchError scrutinee resolved expected None)) in
  let many patterns types = combineBindings (List.map2 (fun pattern typ -> extract pattern typ allowLengthMismatch) patterns types) in
  let listType typ = match T.resolveType aliases typ with
   | TList typ -> Some typ | TVar name -> Some (TVar ("__list_elem_" ^ name))
   | TInferenceVar (_, identity) -> Some (TInferenceVar ("list_element", identity ^ "/list_element"))
   | TNever -> Some (TVar "__list_elem_runtime_error") | _ -> None in
  let listMismatch () = let resolved = T.resolveType aliases typ in
   Error (GenericError ("Cannot match " ^ typeToString resolved ^ " value " ^ Option.value (formatPatternMismatchValue scrutinee) ~default:"<unknown>" ^ " with a List pattern")) in
  match pattern with
  | POr alternatives ->
    let* values = ResultList.traverse (fun pattern -> extract pattern typ allowLengthMismatch) (NonEmptyList.toList alternatives) in
    let values = NonEmptyList.fromList values in let first = NonEmptyList.head values in
    let names = S.of_list (List.map fst first) in
    if List.for_all (fun bindings -> S.equal names (S.of_list (List.map fst bindings))) (NonEmptyList.toList values) then Ok first
    else Error (GenericError "Every branch of an or-pattern must bind the same names")
  | PUnit -> literal TUnit | PWildcard -> Ok [] | PVar name -> Ok [name, typ]
  | PInt64 _ -> literal TInt64 | PBigInt _ -> literal TInt | PInt128Literal _ -> literal TInt128
  | PInt8Literal _ -> literal TInt8 | PInt16Literal _ -> literal TInt16 | PInt32Literal _ -> literal TInt32
  | PUInt8Literal _ -> literal TUInt8 | PUInt16Literal _ -> literal TUInt16 | PUInt32Literal _ -> literal TUInt32 | PUInt64Literal _ -> literal TUInt64 | PUInt128Literal _ -> literal TUInt128
  | PBool _ -> literal TBool | PChar _ -> literal TChar | PFloat _ -> literal TFloat64
  | PString _ -> let resolved = T.resolveType aliases typ in
    (match resolved with TString | TChar | TVar _ | TInferenceVar _ -> Ok [] | resolved when isNeverType resolved -> Ok [] | _ -> Error (GenericError (formatPatternMismatchError scrutinee resolved TString None)))
  | PConstructor (variant, patterns) ->
    (match M.find_opt variant lookup with None -> Error (GenericError ("Unknown variant in pattern: " ^ variant))
     | Some (name, params, _, types) ->
       let args = match T.resolveType aliases typ with TSum (_, args) -> args | _ -> [] in
       let subst = if List.length params = List.length args then M.of_list (List.combine params args) else M.empty in
       if List.length patterns <> List.length types then Error (GenericError ("Expected " ^ string_of_int (List.length types) ^ " fields in " ^ name ^ ".`" ^ variant ^ "` pattern, but got " ^ string_of_int (List.length patterns))) else
       many patterns (List.map (fun typ -> T.canonicalizeBareSumTypeRefsWithNames sumNames (T.applySubst subst typ)) types))
  | PResolvedConstructor _ -> Crash.crash "Resolved constructor pattern re-entered match checking"
  | PTuple patterns ->
    let rec containsBinding = function PVar _ -> true | PConstructor (_, fields) | PResolvedConstructor (_, _, _, fields) | PTuple fields | PList fields -> List.exists containsBinding fields | PListCons (heads, tail) -> List.exists containsBinding heads || containsBinding tail | _ -> false in
    let resolved = T.resolveType aliases typ in
    (match resolved with
     | TTuple types when List.length patterns = List.length types -> many patterns types
     | TVar name -> many patterns (List.mapi (fun index _ -> TVar ("__tuple_elem_" ^ name ^ "_" ^ string_of_int index)) patterns)
     | TInferenceVar (_, identity) -> many patterns (List.mapi (fun index _ -> let name = "tuple_element_" ^ string_of_int index in TInferenceVar (name, identity ^ "/" ^ name)) patterns)
     | TTuple _ -> Ok []
     | _ -> if isNeverType resolved || List.exists containsBinding patterns then Ok [] else
       Error (GenericError ("Cannot match " ^ typeToString resolved ^ " value " ^ Option.value (formatPatternMismatchValue scrutinee) ~default:"<unknown>" ^ " with a Tuple pattern")))
  | PList patterns ->
    (match listType typ with None -> listMismatch () | Some element ->
      let resolved = T.resolveType aliases typ in
      let definiteMismatch = match resolved with
       | TList element -> let resolved = T.resolveType aliases element in
         let mismatch expected = match resolved with TVar _ | TInferenceVar _ -> false | _ -> resolved <> expected in
         List.exists (function PUnit -> mismatch TUnit | PInt64 _ -> mismatch TInt64 | PBigInt _ -> mismatch TInt | PInt128Literal _ -> mismatch TInt128 | PInt8Literal _ -> mismatch TInt8 | PInt16Literal _ -> mismatch TInt16 | PInt32Literal _ -> mismatch TInt32 | PUInt8Literal _ -> mismatch TUInt8 | PUInt16Literal _ -> mismatch TUInt16 | PUInt32Literal _ -> mismatch TUInt32 | PUInt64Literal _ -> mismatch TUInt64 | PUInt128Literal _ -> mismatch TUInt128 | PBool _ -> mismatch TBool | PChar _ -> mismatch TChar | PFloat _ -> mismatch TFloat64 | PString _ -> (match resolved with TString | TChar | TVar _ | TInferenceVar _ -> false | _ -> true) | _ -> false) patterns
       | _ -> false in
      match scrutinee with
      | ListLiteral values when allowLengthMismatch && definiteMismatch && List.length values <> List.length patterns -> Error (GenericError ("No match for " ^ formatListLiteralForNoMatch values))
      | _ -> many patterns (List.map (fun _ -> element) patterns))
  | PListCons (heads, tail) ->
    (match listType typ with None -> listMismatch () | Some element ->
     let heads = many heads (List.map (fun _ -> element) heads) in
     let tail = extract tail (TList element) allowLengthMismatch in combineBindings [heads; tail]) in
 let rec bindingNames = function
  | PVar name -> [name] | PConstructor (_, fields) | PResolvedConstructor (_, _, _, fields) | PTuple fields | PList fields -> List.concat_map bindingNames fields
  | PListCons (heads, tail) -> List.concat_map bindingNames heads @ bindingNames tail
  | POr alternatives -> bindingNames (NonEmptyList.head alternatives)
  | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _ | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _ | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _ | PString _ | PChar _ | PFloat _ -> [] in
 let _duplicatePatternBindings pattern =
  let counts = List.fold_left (fun acc name -> M.add name (1 + Option.value (M.find_opt name acc) ~default:0) acc) M.empty (bindingNames pattern) in M.bindings counts |> List.filter_map (fun (name, count) -> if count > 1 then Some name else None) in
 let validateGroup patterns =
  let rec loop remaining expected = match remaining with [] -> Ok () | pattern :: rest ->
   let* _ = Result.map_error (fun error -> GenericError error) (AST.validateBinders (MatchBinderPattern pattern)) in
   let bindings = FreeVariables.collectPatternBindings pattern |> S.filter (fun name -> name <> "" && not (String.starts_with ~prefix:"_" name)) in
   match expected with None -> loop rest (Some bindings) | Some expected when S.equal expected bindings -> loop rest (Some expected)
    | Some expected -> Error (GenericError ("Pattern matches require all branches to provide the same variables - expected [" ^ String.concat ", " (S.elements expected) ^ "], got [" ^ String.concat ", " (S.elements bindings) ^ "]")) in
  loop (NonEmptyList.toList patterns) None in
 let rec always pattern typ = match pattern, T.resolveType aliases typ with PWildcard, _ | PVar _, _ | PUnit, TUnit -> true
  | PTuple patterns, TTuple types when List.length patterns = List.length types -> List.for_all2 always patterns types | _ -> false in
 let namesMatch left right = left = right || String.ends_with ~suffix:("." ^ right) left || String.ends_with ~suffix:("." ^ left) right in
 let combineStatuses statuses = if List.mem (Some false) statuses then Some false else if List.for_all ((=) (Some true)) statuses then Some true else None in
 let rec definitely pattern value =
  let pair patterns values = if List.length patterns <> List.length values then Some false else combineStatuses (List.map2 definitely patterns values) in
  match pattern, value with
  | PWildcard, _ | PVar _, _ | PUnit, UnitLiteral -> Some true
  | PInt64 a, Int64Literal b -> Some (a = b) | PBigInt a, BigIntLiteral b | PInt128Literal a, Int128Literal b | PUInt128Literal a, UInt128Literal b -> Some (Z.equal a b)
  | PInt8Literal a, Int8Literal b | PInt16Literal a, Int16Literal b | PUInt8Literal a, UInt8Literal b | PUInt16Literal a, UInt16Literal b -> Some (a = b)
  | PInt32Literal a, Int32Literal b -> Some (a = b) | PUInt32Literal a, UInt32Literal b | PUInt64Literal a, UInt64Literal b -> Some (a = b)
  | PBool a, BoolLiteral b -> Some (a = b) | PString a, StringLiteral b | PChar a, CharLiteral b -> Some (a = b) | PFloat a, FloatLiteral b -> Some (a = b)
  | PTuple patterns, TupleLiteral values | PList patterns, ListLiteral values -> pair patterns values
  | PListCons (heads, tail), ListLiteral values -> if List.length values < List.length heads then Some false else
    combineStatuses (List.map2 definitely heads (List.take (List.length heads) values) @ [definitely tail (ListLiteral (List.drop (List.length heads) values))])
  | PConstructor (name, fields), Constructor (_, variant, values) -> if not (namesMatch name variant) then Some false else pair fields values
  | _ -> None in
 let knownCase (case : AST.matchCase) = match case.guard with Some _ -> None | None ->
  let statuses = List.map (fun pattern -> if always pattern scrutineeType then Some true else definitely pattern scrutinee) (NonEmptyList.toList case.patterns) in
  if List.mem (Some true) statuses then Some true else if List.for_all ((=) (Some false)) statuses then Some false else None in
 let rec binderOnly = function PVar _ | PWildcard -> true | PTuple fields | PList fields -> List.for_all binderOnly fields | PListCons (heads, tail) -> List.for_all binderOnly heads && binderOnly tail | _ -> false in
 let _caseCanShortCircuit (case : AST.matchCase) = Option.is_none case.guard && List.for_all binderOnly (NonEmptyList.toList case.patterns) in
 let rec covers pattern typ = match pattern, T.resolveType aliases typ with
  | PVar _, _ | PWildcard, _ | PUnit, TUnit -> true
  | PTuple patterns, TTuple types when List.length patterns = List.length types -> List.for_all2 covers patterns types
  | PListCons ([head], tail), TList element -> covers head element && covers tail (TList element)
  | _ -> false in
 let rec irrefutable = function PVar _ | PWildcard | PUnit -> true | PTuple elements -> List.for_all irrefutable elements | _ -> false in
 let payloadCovers pattern typ = covers pattern typ || irrefutable pattern in
 let sumNamesMatch left right =
  let last name = List.hd (List.rev (String.split_on_char '.' name)) in
  namesMatch left right || right = left ^ "." ^ last left || left = right ^ "." ^ last right in
 let variants name args =
  let info = match M.find_opt name sums with Some info -> Some info | None -> List.find_map (fun (owner, info) -> if sumNamesMatch owner name then Some info else None) (M.bindings sums) in
  match info with None -> [] | Some (info : T.sumTypeInfo) ->
  let subst = if List.length info.T.typeParams = List.length args then M.of_list (List.combine info.T.typeParams args) else M.empty in
  List.map (fun (variant : T.sumVariantInfo) -> variant.T.name, List.map (fun typ -> T.canonicalizeBareSumTypeRefsWithNames sumNames (T.applySubst subst typ)) variant.T.fields) info.T.variants in
 let rec matrix types rows = match types with [] -> rows <> [] | typ :: rest ->
  let resolved = T.resolveType aliases typ in
  let covering = List.filter_map (function pattern :: rest when covers pattern resolved -> Some rest | _ -> None) rows in
  match resolved with
  | TBool ->
    let rowsFor value = List.filter_map (function (PWildcard | PVar _) :: rest -> Some rest | PBool actual :: rest when actual = value -> Some rest | _ -> None) rows in
    matrix rest (rowsFor true) && matrix rest (rowsFor false)
  | TSum (name, args) -> let variants = variants name args in variants <> [] && List.for_all (fun (name, fields) ->
    let rows = List.filter_map (function (PWildcard | PVar _) :: rest -> Some rest | PConstructor (variant, patterns) :: rest when namesMatch variant name ->
      if List.length patterns = List.length fields && List.for_all2 payloadCovers patterns fields then Some rest else None | _ -> None) rows in matrix rest rows) variants
  | _ -> matrix rest covering in
 let tupleExhaustive types patterns =
  let rows = List.filter_map (function PTuple elements when List.length elements = List.length types -> Some elements | PWildcard | PVar _ -> Some (List.map (fun _ -> PWildcard) types) | _ -> None) patterns in matrix types rows in
 let rec listCoverage element pattern = match pattern with
  | PList elements when List.for_all (fun pattern -> covers pattern element) elements -> I.singleton (List.length elements), None
  | PListCons (heads, tail) when List.for_all (fun pattern -> covers pattern element) heads ->
    let count = List.length heads in (match tail with PWildcard | PVar _ -> I.empty, Some count | _ ->
      let lengths, minimum = listCoverage element tail in I.fold (fun length acc -> I.add (count + length) acc) lengths I.empty, Option.map ((+) count) minimum)
  | _ -> I.empty, None in
 let listExhaustive element patterns =
  let lengths, minimums = List.fold_left (fun (all, minimums) pattern -> let lengths, minimum = listCoverage element pattern in I.union all lengths, Option.to_list minimum @ minimums) (I.empty, []) patterns in
  match List.sort Int.compare minimums with [] -> false | minimum :: _ ->
   let rec check length = length >= minimum || I.mem length lengths && check (length + 1) in check 0 in
 let _listPatternsCoverAllLengths = listExhaustive in
 let patternsCover typ patterns = if List.exists (fun pattern -> covers pattern typ) patterns then true else
  match T.resolveType aliases typ with
  | TBool -> List.exists ((=) (PBool true)) patterns && List.exists ((=) (PBool false)) patterns
  | TList element -> listExhaustive element patterns
  | TTuple types -> tupleExhaustive types patterns
  | TSum (name, args) -> let variants = variants name args in variants <> [] && List.for_all (fun (name, fields) ->
    let rows = List.filter_map (function PConstructor (variant, patterns) when namesMatch variant name -> Some patterns | _ -> None) patterns in
    List.exists (fun patterns -> List.length patterns = List.length fields && List.for_all2 payloadCovers patterns fields) rows || matrix fields rows) variants
  | _ -> false in
 let exhaustive cases =
  let patterns = List.concat_map (fun (case : AST.matchCase) -> match case.guard with Some _ -> [] | None -> NonEmptyList.toList case.patterns) cases in
  List.exists (fun case -> knownCase case = Some true) cases || patternsCover scrutineeType patterns in
 let rec resolveConstructors typ pattern =
  let fields params types patterns =
   let args = match T.resolveType aliases typ with TSum (_, args) -> args | _ -> [] in
   let subst = if List.length params = List.length args then M.of_list (List.combine params args) else M.empty in
   let types = List.map (T.applySubst subst) types in
   if List.length patterns = List.length types then List.map2 resolveConstructors types patterns else List.map (resolveConstructors TNever) patterns in
  match pattern with
  | PConstructor (variant, patterns) ->
    let resolved = match T.resolveType aliases typ with TSum (name, _) -> (match M.find_opt (name ^ "." ^ variant) lookup with Some value -> Some value | None -> M.find_opt variant lookup) | _ -> M.find_opt variant lookup in
    (match resolved with None -> Crash.crash ("Validated constructor pattern '" ^ variant ^ "' was not resolved") | Some (name, params, tag, types) ->
     let prefix = name ^ "." in let variant = if String.starts_with ~prefix variant then String.sub variant (String.length prefix) (String.length variant - String.length prefix) else variant in
     PResolvedConstructor (name, variant, tag, fields params types patterns))
  | PResolvedConstructor (name, variant, tag, patterns) ->
    (match M.find_opt (name ^ "." ^ variant) lookup with Some (_, params, _, types) -> PResolvedConstructor (name, variant, tag, fields params types patterns) | None -> Crash.crash ("Resolved constructor pattern '" ^ name ^ "." ^ variant ^ "' was not found"))
  | PTuple patterns -> (match T.resolveType aliases typ with TTuple types when List.length types = List.length patterns -> PTuple (List.map2 resolveConstructors types patterns) | _ -> PTuple (List.map (resolveConstructors TNever) patterns))
  | PList patterns -> (match T.resolveType aliases typ with TList element -> PList (List.map (resolveConstructors element) patterns) | _ -> PList (List.map (resolveConstructors TNever) patterns))
  | PListCons (heads, tail) -> (match T.resolveType aliases typ with TList element -> PListCons (List.map (resolveConstructors element) heads, resolveConstructors typ tail) | _ -> PListCons (List.map (resolveConstructors TNever) heads, resolveConstructors TNever tail))
  | POr alternatives -> POr (NonEmptyList.map (resolveConstructors typ) alternatives)
  | _ -> pattern in
 let rec reopen = function
  | PResolvedConstructor (_, variant, _, fields) | PConstructor (variant, fields) -> PConstructor (variant, List.map reopen fields)
  | PTuple patterns -> PTuple (List.map reopen patterns) | PList patterns -> PList (List.map reopen patterns)
  | PListCons (heads, tail) -> PListCons (List.map reopen heads, reopen tail) | POr alternatives -> POr (NonEmptyList.map reopen alternatives) | pattern -> pattern in
 let checkingCases = List.map (fun (case : AST.matchCase) -> {case with patterns = NonEmptyList.map reopen case.patterns}) cases in
 let rec checkCases remaining result acc firstHasVars = match remaining with
  | [] -> (match result with Some typ -> Ok (typ, List.rev acc, firstHasVars) | None -> Error (GenericError "Match expression must have at least one case"))
  | (case : AST.matchCase) :: rest ->
    let* () = validateGroup case.patterns in
    let allowLength = List.length cases = 1 && case.patterns.NonEmptyList.tail = [] in
    let* bindings = extract (NonEmptyList.head case.patterns) scrutineeType allowLength in
    let patterns = NonEmptyList.map (resolveConstructors scrutineeType) case.patterns in
    let env = List.fold_left (fun env (name, typ) -> M.add name typ env) env bindings in
    let guardResult = match case.guard with None -> Ok None | Some guard ->
     let checked = match recurse guard env (Some TBool) with Error (UndefinedVariable name) -> Error (UndefinedCallTarget name) | other -> other in
     let* typ, guard = checked in if typ = TBool then Ok (Some guard) else Error (TypeMismatch (TBool, typ, "guard clause")) in
    let* guard = guardResult in
    let checked = recurse case.body env result in
    let checked = match result, checked with
     | Some expected, Error (TypeMismatch (contextType, _, "boolean literal")) when expected = contextType -> recurse case.body env None
     | _ -> checked in
    let* bodyType, body = checked in
    let case = {AST.patterns; guard; body} in
    match result with None -> checkCases rest (Some bodyType) (case :: acc) (U.containsTVar bodyType)
     | Some expected -> match U.reconcileTypes (Some aliases) expected bodyType with Some typ -> checkCases rest (Some typ) (case :: acc) firstHasVars | None -> Error (TypeMismatch (expected, bodyType, "match body")) in
 let* typ, checkedCases, firstHasVars = checkCases checkingCases expected [] false in
 let* checkedCases = if firstHasVars && not (U.containsTVar typ) then Result.map (fun (_, cases, _) -> cases) (checkCases checkingCases (Some typ) [] false) else Ok checkedCases in
 if not (exhaustive checkingCases) then Error (GenericError ("Non-exhaustive match expression for " ^ typeToString scrutineeType)) else
 match expected with None -> Ok (typ, Match (scrutinee, checkedCases)) | Some expected ->
  match U.reconcileTypes (Some aliases) expected typ with Some typ -> Ok (typ, Match (scrutinee, checkedCases)) | None -> Error (TypeMismatch (expected, typ, "match expression"))
[@@warning "-4"]
