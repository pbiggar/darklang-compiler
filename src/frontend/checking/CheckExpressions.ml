(*
   CheckExpressions.ml - Dispatch expression checking and propagate contextual expectations.
*)
(* Expressions.ml - Dispatch expression checking and propagate contextual expectations. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module T = Types
module U = Unification
module C = ComparisonPlanning
module ExprSet = Set.Make (struct type t = AST.expr let compare = Stdlib.compare end)
(*
   Check expression type top-down, potentially transforming the expression.
   Parameters:
   - expr: Expression to type-check
   - env: SemanticType environment (variable name -> type mappings)
   - typeReg: SemanticType registry (record type name -> field definitions)
   - variantLookup: Maps variant names to (type name, tag index)
   - genericFuncReg: Registry of generic functions (function name -> type params)
   - expectedType: Optional expected type from context (for checking)
   Returns: Result<SemanticType * Expr, TypeError>
   - Type: The type of the expression
   - Expr: The (possibly transformed) expression
   Unit literal is always TUnit
   Handle type variables (e.g., when expected is TVar "t")
   A type variable or an alias of Int, as for the sized literals.
   Boolean literals are always TBool
   String literals are always TString
   Char literals are always TChar (single Extended Grapheme Cluster)
   Interpolated strings are always TString
   Check that all expression parts are strings
   Float literals are always TFloat64
   Negation works on integer and float numeric types
   Boolean not works on booleans and returns booleans
   Bitwise NOT works on integer types and preserves the operand type
   Checking a long sequence through recursive Result.bind calls retains
   one host stack frame per binding. Large generated programs combine
   that depth with their top-level functions, so walk consecutive lets
   iteratively and rebuild their typed form after checking the tail.
   The RHS is checked in the incoming environment. Only a completely
   validated and type-compatible pattern extends the continuation.
   Variable reference: look up in environment
   Check if it's a module function (e.g., Stdlib.Int64.add)
   If expression: condition must be bool, branches must have same type
   If an outer context already provides an expected type, keep using it.
   Otherwise, use the then-branch type to type-check the else-branch.
   This lets bottom-like runtime-failing expressions (e.g. unwrap None)
   inhabit the enclosing branch type.
   When no outer expected type exists, we type-check else with then-type context.
   If that fails due to contextual mismatch, re-check else unconstrained so the
   final diagnostic can report branch-vs-branch mismatch, not literal mismatch.
   The interpreter checks this at the statement boundary. The compiler
   enforces the same Unit contract statically and uses only the final
   expression to determine the sequence's result type.
   Specialized programs can be checked again by the E2E preamble
   planner. Keep this compiler-internal plan well typed without
   exposing its marker as a source-level function.
   Generic function call with explicit type arguments: func<Type1, Type2>(args)
   1. Look up the canonical function identity
   2. Look up type parameters
   3. Build substitution from type params to type args
   4. Apply substitution to get concrete types
   5. Check argument count - allow partial application
   Partial application with explicit type args
   Type-check the provided arguments
   Use typesCompatible to allow type variables to unify with concrete types
   Create unique parameter names for the remaining parameters
   Create the lambda body: TypeApp call with all args (using resolved name)
   Create the lambda
   The resulting type is a function from remaining params to return type
   6. Type check each argument and collect transformed args
   7. Return the concrete return type (using resolved name)
   Check if it's a generic module function (e.g., __raw_get<v>)
   Build substitution from type params to type args
   Apply substitution to get concrete types
   Check argument count - allow partial application
   Type check each argument and collect transformed args
   Type-check each element and build tuple type
   Tuple elements that are known runtime failures should make the whole
   tuple expression runtime-fail (bottom-like behavior), preserving the
   left-to-right first failure.
   Resolve type aliases first, then check compatibility for type variables
   This allows Pair<Int64> to match (Int64, Int64) when Pair<a> = (a, a)
   and (a, b) to match (Int64, Int64) when using generic functions
   Check the tuple expression
   Check the record expression to get its type
   Update expressions remain in source order. Duplicate names
   are intentionally accepted and the lowering uses the last value.
   Check the record expression
   Look up the variant to find its type and expected payload
   Type-check elements and infer element type from first element
   Empty list: use expected list type or keep a type variable
   A bare type variable (a generic parameter not yet bound, as the seed
   of a fold) takes the list; the element stays open for the other
   arguments to fix. `Stdlib.List.fold xs [] (fun acc x -> [x])` was a
   mismatch reported as the enclosing function's return value.
   Use expected list element type for the first element when available, so
   lambda/list literals in expected contexts reconcile type variables consistently.
   A bare type variable must be inferred from the element. Passing it into
   expression checking would hide concrete requirements such as arithmetic.
   Structured generic types still carry useful constraints. In particular,
   checking (String, a) contextually preserves the precise error at a bad key.
   Check remaining elements match the inferred type
   Use reconcileTypes to allow type variables to unify and resolve type aliases
   Type-check the function expression
   Partial application of lambda/function value
   Create fresh parameter names for the remaining parameters
   Use "lambda" as identifier since we're applying a function value, not a named function
   Create the lambda body: apply the original function with all args
   Create the lambda: fun p0 p1 ... -> func(providedArgs, p0, p1, ...)
   Check each argument against expected param type
   Closure: function with captured values
   The closure has the same type as the underlying function (minus closure param)
   For now, just check the captures and return function type
   Look up closure function type
   The closure type is the function type without the closure param
*)
let rec checkExprWithParamNamesAndSumTypeNames paramNames sumNames sums expr env registry lookup generic warnings modules aliases expected =
 let ( let* ) = Result.bind in
 let checkExpr = checkExprWithParamNamesAndSumTypeNames paramNames sumNames sums in
 let recurse value expected = checkExpr value env registry lookup generic warnings modules aliases expected in
 let canonical = T.canonicalizeBareSumTypeRefsWithNames sumNames in
 let expected = Option.map canonical expected in
 let contextExpected name body = ContextInference.tryFindFunctionValueExpectation recurse env registry lookup modules aliases name body in
 let callExpected name count body = Option.bind (ContextInference.tryFindCallArguments name body) (ContextInference.inferFunctionExpectationFromArguments recurse count) in
 let reconcileResult typ expr context = match expected with None -> Ok (typ, expr) | Some expected ->
  match U.reconcileTypes (Some aliases) expected typ with Some typ -> Ok (typ, expr) | None -> Error (TypeMismatch (expected, typ, context)) in
 let simple compatible typ context = match expected with Some expected when not (compatible expected typ) -> Error (TypeMismatch (expected, typ, context)) | Some _ | None -> Ok (typ, expr) in
 let fixed typ context = match expected with None -> Ok (typ, expr) | Some expected when expected = typ -> Ok (typ, expr) | Some other ->
  match U.reconcileTypes (Some aliases) other typ with Some resolved when resolved = typ -> Ok (typ, expr) | _ -> Error (TypeMismatch (other, typ, context)) in
 let orElse = ContextInference.orElse in
 match expr with
 | BoundaryRender (renderer, value) -> Result.map (fun (_, value) -> TString, BoundaryRender (renderer, value)) (recurse value None)
 | RuntimeError message -> Ok (TNever, RuntimeError message)
 | UnitLiteral -> simple U.typesCompatible TUnit "unit literal"
 | Int64Literal _ -> fixed TInt64 "integer literal" | Int128Literal _ -> fixed TInt128 "integer literal" | BigIntLiteral _ -> fixed TInt "Int literal"
 | Int8Literal _ -> simple U.typesCompatible TInt8 "Int8 literal" | Int16Literal _ -> simple U.typesCompatible TInt16 "Int16 literal" | Int32Literal _ -> simple U.typesCompatible TInt32 "Int32 literal"
 | UInt8Literal _ -> simple U.typesCompatible TUInt8 "UInt8 literal" | UInt16Literal _ -> simple U.typesCompatible TUInt16 "UInt16 literal" | UInt32Literal _ -> simple U.typesCompatible TUInt32 "UInt32 literal" | UInt64Literal _ -> simple U.typesCompatible TUInt64 "UInt64 literal" | UInt128Literal _ -> simple U.typesCompatible TUInt128 "UInt128 literal"
 | BoolLiteral _ -> simple U.typesCompatible TBool "boolean literal" | StringLiteral _ -> simple (U.typesCompatibleWithAliases aliases) TString "string literal" | CharLiteral _ -> simple U.typesCompatible TChar "char literal" | FloatLiteral _ -> simple U.typesCompatible TFloat64 "float literal"
 | InterpolatedString parts ->
   let normalize = function UndefinedVariable name -> UndefinedCallTarget name | other -> other in
   let rec loop remaining acc = match remaining with [] -> Ok (List.rev acc) | StringText text :: rest -> loop rest (StringText text :: acc)
    | StringExpr value :: rest ->
      let checked = match recurse value (Some TString) with
       | Error (UndefinedVariable name) -> Error (UndefinedCallTarget name)
       | Error (TypeMismatch (TString, _, _)) -> let* typ, value = Result.map_error normalize (recurse value None) in Error (GenericError (interpolationTypeMismatchMessage value typ))
       | other -> other in
      let* typ, value = checked in if typ = TString || typ = TChar then loop rest (StringExpr value :: acc) else Error (GenericError (interpolationTypeMismatchMessage value typ)) in
   let* parts = loop parts [] in
   (match expected with Some TString | None -> Ok (TString, InterpolatedString parts) | Some other -> Error (TypeMismatch (other, TString, "interpolated string")))
 | BinOp (op, left, right) -> CheckBinaryOperations.check checkExpr sums env registry lookup generic warnings modules aliases expected op left right
 | UnaryOp (op, value) ->
   let integer = function TInt8 | TInt16 | TInt32 | TInt64 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true | _ -> false in
   (match op with
    | Neg -> let* typ, value = recurse value None in
      if integer typ || typ = TFloat64 then (match expected with Some other when other <> typ -> Error (TypeMismatch (other, typ, "result of negation")) | Some _ | None -> Ok (typ, UnaryOp (op, value))) else Error (InvalidOperation ("-", [typ]))
    | Not -> let* typ, value = recurse value (Some TBool) in
      if typ <> TBool then Error (TypeMismatch (TBool, typ, "operand of !")) else (match expected with Some TBool | None -> Ok (TBool, UnaryOp (op, value)) | Some other -> Error (TypeMismatch (other, TBool, "result of !")))
    | BitNot -> let* typ, value = recurse value None in
      if not (integer typ) then Error (InvalidOperation ("~~~", [typ])) else (match expected with Some other when other <> typ -> Error (TypeMismatch (other, typ, "result of ~~~")) | Some _ | None -> Ok (typ, UnaryOp (op, value))))
 | RecursiveLet (recursion, value, body) ->
   let name = AST.recursiveBindingName recursion and availability = Option.value (AST.recursiveBindingAvailability recursion) ~default:SelfRecursiveMember in
   let continuation = match value with Lambda (params, _, _) -> orElse (contextExpected name body) (fun () -> callExpected name (NonEmptyList.length params) body) | _ -> None in
   let provisional = match value with
    | Lambda (params, annotation, _) ->
      let expectedParams, expectedRet = match continuation with Some (TFunction (params, ret)) -> params, Some ret | _ -> [], None in
      let params = NonEmptyList.toList params |> List.mapi (fun index (param : AST.lambdaParameter) ->
       Option.value (orElse param.inferredType (fun () -> orElse param.sourceAnnotation (fun () -> List.nth_opt expectedParams index))) ~default:(TVar ("recursiveParameter" ^ string_of_int index))) in
      TFunction (params, Option.value (orElse annotation (fun () -> expectedRet)) ~default:(TVar "recursiveReturn"))
    | _ -> Crash.crash "RecursiveLet must contain a lambda value" in
   let valueEnv = match availability with SelfRecursiveMember | MutualRecursiveMember -> M.add name provisional env | OrdinaryBinding | CompletedGroupMember | ImportedGroupMember -> env in
   let* typ, value = checkExpr value valueEnv registry lookup generic warnings modules aliases (Some provisional) in
   let typ = match typ, value with TFunction (_, ret), Lambda (params, _, _) ->
     let types = NonEmptyList.toList params |> List.map (fun (param : AST.lambdaParameter) -> Option.value (orElse param.inferredType (fun () -> param.sourceAnnotation)) ~default:(TVar "underdeterminedRecursiveParameter")) in TFunction (types, ret) | _ -> typ in
   let recursion = match recursion with ResolvedRecursiveBinding resolved -> TypedRecursiveBinding {AST.resolved; monomorphicType = typ} | TypedRecursiveBinding typed -> TypedRecursiveBinding {typed with monomorphicType = typ}
    | RecursiveBindingCandidate _ | ParsedRecursiveBinding _ -> Crash.crash "Recursive binding reached type checking before name resolution" in
   let* bodyType, body = checkExpr body (M.add name typ env) registry lookup generic warnings modules aliases expected in Ok (bodyType, RecursiveLet (recursion, value, body))
 | Let (pattern, value, body) ->
   let rebuild checked body = List.fold_left (fun body (pattern, value) -> Let (pattern, value, body)) body checked in
   let rec chain currentEnv checked pattern value body =
    let valueExpected = match pattern, value with
     | LPVariable name, Lambda (params, _, _) -> orElse (contextExpected name body) (fun () -> callExpected name (NonEmptyList.length params) body)
     | LPVariable name, Constructor (_, _, []) -> ContextInference.filter (fun typ -> not (U.containsTVar typ)) (contextExpected name body)
     | _, ListLiteral [] -> Some (TList (TVar U.emptyListElementVar)) | _ -> None in
    let* typ, value = checkExpr value currentEnv registry lookup generic warnings modules aliases valueExpected in
    let typ = canonical typ in
    let* _ = Result.map_error (fun error -> GenericError error) (AST.validateBinders (LetBinderPatterns [pattern])) in
    match bindLetPatternTypes pattern typ with
    | None -> let rendered = Option.value (tryFormatLiteralValue value) ~default:("<" ^ typeToString typ ^ ">") in
      let message = "Could not deconstruct value " ^ rendered ^ " into pattern " ^ formatLetDeconstructionPattern pattern in
      Ok (TNever, rebuild checked (Let (pattern, value, RuntimeError message)))
    | Some bindings ->
      let env = List.fold_left (fun env (name, typ) -> M.add name typ env) currentEnv bindings in
      let body = match pattern, value with LPVariable name, (FloatLiteral _ | Int64Literal _) -> substituteInterpolationLiteral name value body | _ -> body in
      let checked = (pattern, value) :: checked in
      match body with Let (pattern, value, body) -> chain env checked pattern value body | _ ->
       let* typ, body = checkExpr body env registry lookup generic warnings modules aliases expected in Ok (typ, rebuild checked body) in
   chain env [] pattern value body
 | Var name ->
   let builtin typ canonicalName = reconcileResult typ (Var canonicalName) ("variable " ^ name) in
   if isBuiltinTestNanName name then builtin TFloat64 "Builtin.testNan"
   else if isBuiltinTestInfinityName name then builtin TFloat64 "Builtin.testInfinity"
   else if isBuiltinBlobEmptyName name then builtin TBlob "Builtin.blobEmpty" else
   (match U.tryLookupResolved name env with Some (typ, resolved) -> reconcileResult (canonical typ) (Var resolved) ("variable " ^ name) | None ->
     match DarkStdlib.tryGetFunction modules name with Some (func, resolved) -> reconcileResult (DarkStdlib.getFunctionType func) (Var resolved) ("variable " ^ name) | None -> Error (UndefinedVariable name))
 | If (condition, yes, no) ->
   let originalCondition = condition and originalYes = yes and originalNo = no in
   let* conditionType, condition = recurse condition None in
   let* condition = if conditionType = TBool then Ok condition else
    let known value = isKnownUnwrapFailureExpr M.empty value || isKnownTestRuntimeErrorExpr M.empty value in
    if known originalCondition || known condition then let* typ, value = recurse originalCondition (Some TBool) in
      if typ = TBool then Ok value else Error (GenericError (ifConditionTypeMismatchMessage value typ))
    else Error (GenericError (ifConditionTypeMismatchMessage condition conditionType)) in
   let* yesType, yes = recurse yes expected in
   let noExpected = match expected with Some _ -> expected | None -> Some yesType in
   let noChecked = match recurse no noExpected with
    | Ok _ as result -> result | Error (TypeMismatch (typ, _, _)) when expected = None && typ = yesType -> recurse no None | Error error -> Error error in
   let* noType, no = noChecked in
   let reconciled = match U.reconcileTypes (Some aliases) yesType noType with Some _ as typ -> typ | None ->
    let yesFails = isKnownUnwrapFailureExpr M.empty originalYes || isKnownUnwrapFailureExpr M.empty yes in
    let noFails = isKnownUnwrapFailureExpr M.empty originalNo || isKnownUnwrapFailureExpr M.empty no in
    if yesFails && not noFails then Some noType else if noFails && not yesFails then Some yesType else None in
   (match reconciled with None -> Error (IfBranchTypeMismatch (yesType, noType)) | Some typ ->
    let* yes = if U.containsTVar yesType && not (U.containsTVar typ) then Result.map snd (recurse originalYes (Some typ)) else Ok yes in
    reconcileResult typ (If (condition, yes, no)) "if expression")
 | Sequence (first, next) -> let* _, first = recurse first (Some TUnit) in let* typ, next = recurse next expected in Ok (typ, Sequence (first, next))
 | Apply (Var name, [], args) -> CheckCalls.check checkExpr paramNames sums env registry lookup generic warnings modules aliases expected name args
 | Apply (Var name, [target], {NonEmptyList.head = left; tail = [right]}) when name = C.internalTypeAppMarkerName C.EqHelperDispatch ->
   let* _, left = recurse left (Some target) in let* _, right = recurse right (Some target) in
   (match expected with Some expected when not (U.typesCompatible expected TBool) -> Error (TypeMismatch (expected, TBool, "comparison result"))
    | Some _ | None -> Ok (TBool, C.makeInternalTypeApp (C.EqHelperDispatchTypeApp (target, left, right))))
 | Apply (Var name, typeArgs, args) -> ExplicitCalls.check checkExpr paramNames sumNames sums env registry lookup generic warnings modules aliases expected name typeArgs args
 | TupleLiteral elements ->
   let expectedTypes = match Option.map (T.resolveType aliases) expected with Some (TTuple types) when List.length types = List.length elements -> List.map Option.some types | _ -> List.map (fun _ -> None) elements in
   let rec loop elements types accTypes accExprs = match elements, types with
    | value :: rest, expected :: types -> let* typ, value = recurse value expected in loop rest types (typ :: accTypes) (value :: accExprs)
    | _ -> Ok (List.rev accTypes, List.rev accExprs) in
   let* types, elements = loop elements expectedTypes [] [] in
   (match List.find_opt (isKnownTestRuntimeErrorExpr M.empty) elements with
    | Some failure ->
      let failure = match failure with Apply (Var name, [], {NonEmptyList.head = arg; tail = []}) when isBuiltinTestRuntimeErrorName name -> AST.applyNamed "Builtin.testRuntimeError" (NonEmptyList.singleton arg)
       | _ -> let message = Option.value (tryExtractKnownTestRuntimeErrorMessage M.empty failure) ~default:"<runtime error>" in AST.applyNamed "Builtin.testRuntimeError" (NonEmptyList.singleton (StringLiteral message)) in
      Ok (Option.value expected ~default:TNever, failure)
    | None -> let typ = TTuple types in
      match expected with Some expected when not (U.typesCompatible (T.resolveType aliases expected) typ) -> Error (TypeMismatch (expected, typ, "tuple literal")) | Some _ | None -> Ok (typ, TupleLiteral elements))
 | TupleAccess (value, index) -> let* typ, value = recurse value None in
   (match typ with TTuple types -> if index < 0 || index >= List.length types then Error (GenericError ("Tuple index " ^ string_of_int index ^ " out of bounds (tuple has " ^ string_of_int (List.length types) ^ " elements)")) else
     let typ = List.nth types index in (match expected with Some expected when expected <> typ -> Error (TypeMismatch (expected, typ, "tuple access ." ^ string_of_int index)) | Some _ | None -> Ok (typ, TupleAccess (value, index)))
    | other -> Error (GenericError ("Cannot access ." ^ string_of_int index ^ " on non-tuple type " ^ typeToString other)))
 | RecordLiteral (reference, fields) -> CheckRecordLiterals.check checkExpr env registry lookup generic warnings modules aliases expected reference fields
 | RecordUpdate (record, updates) ->
   let* typ, record = recurse record None in
   (match T.resolveAliasTargetType aliases typ with
    | TRecord (name, args) ->
      (match M.find_opt name registry with None -> Error (GenericError ("Unknown record type: " ^ name)) | Some (info : T.recordTypeInfo) ->
       let updates = List.map (fun ((reference : AST.recordFieldReference), value) -> (if reference.sourceFieldName = "___" then "" else reference.sourceFieldName), value) updates in
       if List.exists (fun (name, _) -> name = "") updates then Error (GenericError "Empty key in record update") else
       let* subst = T.buildRecordFieldSubstitutionFromParams info.T.typeParams args |> Result.map_error (fun message -> GenericError message) in
       let unknown = List.filter_map (fun (name, _) -> if M.mem name info.T.fieldTypes then None else Some name) updates in
       if unknown <> [] then Error (GenericError ("Unknown fields in record update: " ^ String.concat ", " unknown)) else
       let rec loop remaining acc = match remaining with [] -> Ok (List.rev acc) | (field, value) :: rest ->
        match M.find_opt field info.T.fieldTypes with
        | None -> Crash.crash ("Validated record update field '" ^ field ^ "' disappeared")
        | Some pattern -> let expectedField = T.applyTypeArguments subst pattern in let* actual, value = recurse value (Some expectedField) in
          if not (U.typesCompatibleWithAliases aliases expectedField actual) then Error (TypeMismatch (expectedField, actual, "field " ^ field ^ " in record update")) else
          let rec index ordinal = function [] -> Crash.crash ("Validated record field '" ^ field ^ "' has no declaration slot") | (name, _) :: rest -> if name = field then ordinal else index (ordinal + 1) rest in
          loop rest ((AST.resolvedRecordFieldReference name field (index 0 info.T.fields), value) :: acc) in
       let* updates = loop updates [] in Ok (TRecord (name, args), RecordUpdate (record, updates)))
    | other -> Error (GenericError ("Cannot use record update syntax on non-record type " ^ typeToString other)))
 | RecordAccess (record, reference) ->
   let field = if reference.sourceFieldName = "___" then "" else reference.sourceFieldName in
   if field = "" then Error (GenericError "Field name is empty") else
   let* typ, record = recurse record None in
   (match T.resolveAliasTargetType aliases typ with
    | TRecord (name, args) ->
      (match M.find_opt name registry with None -> Error (GenericError ("Unknown record type: " ^ name)) | Some (info : T.recordTypeInfo) ->
       match M.find_opt field info.T.fieldTypes with None -> Error (GenericError ("Tried to access field '" ^ field ^ "' but record type " ^ name ^ " has no such field"))
        | Some pattern ->
          let rec index ordinal = function [] -> Crash.crash ("Validated record field '" ^ field ^ "' has no declaration slot") | (name, _) :: rest -> if name = field then ordinal else index (ordinal + 1) rest in
          let index = index 0 info.T.fields in
          let* subst = T.buildRecordFieldSubstitutionFromParams info.T.typeParams args |> Result.map_error (fun message -> GenericError message) in
          let typ = T.applyTypeArguments subst pattern in
          match expected with Some expected when not (U.typesCompatibleWithAliases aliases expected typ) -> Error (TypeMismatch (expected, typ, "field access ." ^ field))
           | Some _ | None -> Ok (typ, RecordAccess (record, AST.resolvedRecordFieldReference name field index)))
    | other -> Error (GenericError ("Attempting to access field '" ^ field ^ "' on non-record type " ^ typeToString other)))
 | Constructor (reference, variant, fields) ->
   let expectedVariant = match reference, expected with UnresolvedConstructor None, Some (TSum (name, _)) -> T.tryFindVariant (AST.resolvedConstructorReference (T.resolveTypeName aliases name)) variant lookup | _ -> None in
   if reference = UnresolvedConstructor None && T.unqualifiedVariantOwnerCount variant lookup > 1 && Option.is_none expectedVariant then
    let identities = M.bindings lookup |> List.filter_map (fun (key, (name, _, _, _)) -> if key = name ^ "." ^ variant then Some (NameResolution.ConstructorSymbol (name, variant)) else None)
      |> List.sort_uniq (fun a b -> StringOrder.compare (NameResolution.symbolIdentityToString a) (NameResolution.symbolIdentityToString b)) in
    (match NameResolution.tryQualifiedName variant with Some name -> Error (ResolutionFailure (NameResolution.AmbiguousReference (name, NameResolution.Constructor, identities))) | None -> Error (GenericError ("Ambiguous constructor: " ^ variant)))
   else
   let resolved = orElse expectedVariant (fun () -> orElse (T.tryFindVariant reference variant lookup) (fun () -> if generic.T.requireExplicitTypeArgsForBareCalls then None else M.find_opt variant lookup)) in
   (match resolved with None -> Error (GenericError ("Unknown constructor: " ^ variant)) | Some (name, params, _tag, fieldTypes) ->
    if List.length fieldTypes <> List.length fields then Error (GenericError ("Expected " ^ string_of_int (List.length fieldTypes) ^ " fields in " ^ name ^ ".`" ^ variant ^ "`, but got " ^ string_of_int (List.length fields))) else
    let args = match Option.map (T.resolveType aliases) expected with Some (TSum (expected, args)) when expected = name && List.length args = List.length params -> args | _ -> List.map (fun name -> TVar name) params in
    let initial = List.combine params args |> List.filter (fun (_, typ) -> match typ with TVar _ | TInferenceVar _ -> false | _ -> true) |> M.of_list in
    let rec loop types fields subst acc = match types, fields with
     | [], [] -> Ok (subst, List.rev acc)
     | typ :: types, value :: fields ->
       let expectedField = canonical (T.applySubst subst typ) in
       let context = match expectedField with TVar _ | TInferenceVar _ -> None | _ -> Some expectedField in
       let* actual, value = recurse value context in let actual = canonical actual in
       let* newSubst = U.unifyTypes expectedField actual |> Result.map_error (fun message -> GenericError ("Type mismatch in " ^ variant ^ " field: " ^ message)) in
       let* subst = U.consolidateBindings (M.bindings subst @ M.bindings newSubst) |> Result.map_error (fun message -> GenericError message) in
       loop types fields subst (value :: acc)
     | _ -> Crash.crash "Constructor field arity was validated before field checking" in
    let* subst, fields = loop fieldTypes fields initial [] in
    let args = List.map2 (fun param expected -> let inferred = T.applySubst subst (TVar param) in if inferred = TVar param then expected else inferred) params args in
    let typ = TSum (name, args) in
    let reference = AST.resolvedConstructorReferenceWithTypeArgs name args in
    reconcileResult typ (Constructor (reference, variant, fields)) ("constructor " ^ variant))
 | Match (scrutinee, cases) -> CheckMatches.check checkExpr sumNames sums env registry lookup generic warnings modules aliases expected scrutinee cases
 | DictLiteral (_, _, entries) ->
   let _, duplicate = List.fold_left (fun (seen, duplicate) (key, _) -> match duplicate with Some _ -> seen, duplicate | None when ExprSet.mem key seen -> seen, Some key | None -> ExprSet.add key seen, None) (ExprSet.empty, None) entries in
   (match duplicate with Some key -> Error (GenericError ("Cannot add two dictionary entries with the same key " ^ Option.value (tryFormatLiteralValue key) ~default:"<computed key>")) | None ->
    let expectedKey, expectedValue = match Option.map (T.resolveType aliases) expected with Some (TDict (key, value)) -> Some key, Some value | _ -> None, None in
    let finish key value entries = let typ = TDict (key, value) in
     if not (C.dictKeyAdmissibleType aliases registry sums key) then Error (GenericError ("Type " ^ typeToString (T.resolveType aliases key) ^ " cannot be used as a Dict key")) else reconcileResult typ (DictLiteral (key, value, entries)) "Dict literal" in
    match entries with [] -> finish (Option.value expectedKey ~default:(TVar "dictKey")) (Option.value expectedValue ~default:(TVar "dictValue")) []
     | (key, value) :: rest -> let* keyType, key = recurse key expectedKey in let* valueType, value = recurse value expectedValue in
       let rec loop remaining acc = match remaining with [] -> Ok (List.rev acc) | (key, value) :: rest ->
        let* actualKey, key = recurse key (Some keyType) in let* actualValue, value = recurse value None in
        match U.reconcileTypes (Some aliases) keyType actualKey, U.reconcileTypes (Some aliases) valueType actualValue with
         | Some _, Some _ -> loop rest ((key, value) :: acc)
         | None, _ -> Error (GenericError ("dict keys must have one type: got " ^ typeToString actualKey ^ ", expected " ^ typeToString keyType))
         | _, None -> Error (GenericError ("dict values must have one type: got " ^ typeToString actualValue ^ ", expected " ^ typeToString valueType)) in
       let* entries = loop rest [key, value] in finish keyType valueType entries)
 | ListLiteral elements ->
   (match elements with
    | [] -> (match Option.map (T.resolveType aliases) expected with Some (TList typ) -> Ok (TList typ, ListLiteral []) | Some (TVar _ | TInferenceVar _) | None -> Ok (TList (TVar U.emptyListElementVar), ListLiteral []) | Some other -> Error (TypeMismatch (other, TList (TVar U.emptyListElementVar), "empty list")))
    | first :: rest ->
      let context = match Option.map (T.resolveType aliases) expected with Some (TList (TVar _ | TInferenceVar _)) -> None | Some (TList typ) -> Some typ | _ -> None in
      let* elementType, first = recurse first context in
      let rec loop remaining acc = match remaining with [] -> Ok (List.rev acc) | value :: rest -> let* typ, value = recurse value (Some elementType) in
       if typ = elementType then loop rest (value :: acc) else Error (TypeMismatch (elementType, typ, "list element")) in
      let* elements = loop rest [first] in reconcileResult (TList elementType) (ListLiteral elements) "list literal")
 | Lambda (params, annotation, body) -> CheckLambdas.check checkExpr env registry lookup generic warnings modules aliases expected params annotation body
 | Apply (func, [], args) ->
   let args = NonEmptyList.toList args in
   let context = match func with Lambda (params, _, _) -> ContextInference.inferFunctionExpectationFromArguments recurse (NonEmptyList.length params) args | _ -> None in
   let* typ, func = recurse func context in
   (match typ with
    | TFunction (params, ret) ->
      let count = List.length params in let args = normalizeNullaryCallArgs count args in let supplied = List.length args in
      if supplied > count then Error (GenericError ("Expected " ^ string_of_int count ^ " arguments, got " ^ string_of_int supplied)) else
      let rec loop arguments params acc = match arguments, params with [] , [] -> Ok (List.rev acc) | value :: rest, typ :: params ->
       let* actual, value = recurse value (Some typ) in if T.typesEqual aliases actual typ then loop rest params (value :: acc) else Error (TypeMismatch (typ, actual, "function argument")) | _ -> Error (GenericError "Argument count mismatch") in
      if supplied < count then
       let* args = loop args (List.take supplied params) [] in let types = List.drop supplied params in let remaining = makePartialParams "lambda" types in
       let body = Apply (func, [], toCallArgs (args @ List.map (fun (name, _) -> Var name) remaining)) in
       let typ = TFunction (types, ret) in
       (match expected with Some expected when not (T.typesEqual aliases expected typ) -> Error (TypeMismatch (expected, typ, "partial application")) | Some _ | None -> Ok (typ, Lambda (toLambdaParams remaining, None, body)))
      else
       let* args = loop args params [] in
       (match expected with Some expected when not (T.typesEqual aliases expected ret) -> Error (TypeMismatch (expected, ret, "function application result")) | Some _ | None -> Ok (ret, Apply (func, [], toCallArgs args)))
    | other -> Error (GenericError ("Cannot apply non-function type: " ^ typeToString other)))
 | Apply (_, _ :: _, _) -> Error (GenericError "Explicit type arguments require a named function")
 | IndirectApply _ -> Crash.crash "IndirectApply is compiler-generated after expression type checking"
 | Closure (name, captures) ->
   let* captures = ResultList.traverse (fun value -> Result.map snd (recurse value None)) captures in
   (match M.find_opt name env with
    | Some (TFunction (_ :: params, ret)) -> let typ = TFunction (params, ret) in
      (match expected with Some expected when expected <> typ -> Error (TypeMismatch (expected, typ, "closure " ^ name)) | Some _ | None -> Ok (typ, Closure (name, captures)))
    | Some typ -> Ok (typ, Closure (name, captures)) | None -> Error (UndefinedVariable name))
[@@warning "-4"]
