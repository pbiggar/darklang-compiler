(*
   ResolveDeclarations.fs - Resolve declaration identities, recursive groups, and source names.
*)
(* ResolveDeclarations.ml - Resolve declaration identities, recursive groups, and source names. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
module N = NameResolution
(*
   Registries derivable directly from a program's top-level declarations.
*)
type topLevelDeclarationSummary = {
 typeReg : Types.typeRegistry; recordTypeParams : string list M.t;
 aliasReg : Types.aliasRegistry; variantLookup : Types.variantLookup;
 funcSigs : (AST.semanticType list * AST.semanticType) M.t;
 funcParamNames : Types.funcParamNameRegistry; genericFuncs : string list M.t
}
let splitDeclaredName name = match N.tryQualifiedName name with
 | None -> N.RootNamespace, name
 | Some qualified -> (match List.rev (N.qualifiedNameSegments qualified) with
   | terminal :: rest -> (match NonEmptyList.tryFromList (List.rev rest) with Some path -> N.ModuleNamespace path, terminal | None -> N.RootNamespace, terminal)
   | [] -> Crash.crash "Qualified name contained no segments")
let requiredCandidate visible identity provenance = match N.candidate visible identity provenance with
 | Some value -> value | None -> Crash.crash ("Invalid compiler declaration name entered resolution inventory: " ^ visible)
let declarationResolutionEnvironment topLevels moduleRegistry includeIntrinsicCatalog =
 let sourceNames = List.filter_map (function FunctionDef (definition : AST.functionDef) -> Some definition.name | _ -> None) topLevels |> S.of_list in
 let registered = M.fold (fun name _ acc -> S.add name acc) moduleRegistry sourceNames in
 let visibleNames name = let versioned = name ^ "_v0" in if String.ends_with ~suffix:"_v0" name || S.mem versioned registered then [name] else [name; versioned] in
 let key = function FunctionDef definition -> Some ("function", definition.name) | ValueDef value -> Some ("value", AST.valueDefName value)
  | TypeDef (RecordDef (name, _, _) | SumTypeDef (name, _, _) | TypeAlias (name, _, _)) -> Some ("type", name) | Expression _ -> None in
 let module K = Map.Make (struct type t = string * string let compare (a,b) (c,d) = let result = StringOrder.compare a c in if result = 0 then StringOrder.compare b d else result end) in
 let indexed = List.mapi (fun index value -> index, value) topLevels in
 let winning = List.fold_left (fun acc (index, value) -> match key value with None -> acc | Some name -> K.add name index acc) K.empty indexed in
 let source = List.filter (fun (index, value) -> match key value with None -> true | Some name -> K.find_opt name winning = Some index) indexed
  |> List.concat_map (fun (index, value) -> match value with
   | FunctionDef definition -> let namespace, terminal = splitDeclaredName definition.name in
     let identity = N.ModuleFunction (namespace, terminal, "source:" ^ string_of_int index ^ ":" ^ definition.name) in
     List.map (fun visible -> requiredCandidate visible identity (N.SourceDeclaration definition.name)) (visibleNames definition.name)
   | ValueDef value -> let name = AST.valueDefName value in let namespace, terminal = splitDeclaredName name in
     [requiredCandidate name (N.ModuleValue (namespace, terminal)) (N.SourceDeclaration name)]
   | TypeDef definition ->
     let name, variants = match definition with RecordDef (name, _, _) | TypeAlias (name, _, _) -> name, [] | SumTypeDef (name, _, variants) -> name, variants in
     requiredCandidate name (N.UserType name) (N.SourceDeclaration name) :: List.concat_map (fun (variant : AST.variant) ->
      let qualified = name ^ "." ^ variant.name in let identity = N.ConstructorSymbol (name, variant.name) in
      [requiredCandidate variant.name identity (N.SourceDeclaration qualified); requiredCandidate qualified identity (N.SourceDeclaration qualified)]) variants
   | Expression _ -> []) in
 let intrinsic = M.bindings moduleRegistry |> List.filter (fun (name, _) -> includeIntrinsicCatalog && not (S.mem name sourceNames))
  |> List.concat_map (fun (name, _) -> let namespace, terminal = splitDeclaredName name in
   List.map (fun visible -> requiredCandidate visible (N.ModuleFunction (namespace, terminal, "intrinsic:" ^ name)) (N.CompilerExtension name)) (visibleNames name)) in
 let builtin name identity provenance = requiredCandidate ("Builtin." ^ name) identity (N.BuiltinRegistration ("Builtin." ^ provenance)) in
 let builtins = List.map (fun name -> builtin name (N.BuiltinFunction (name, 0)) name) ["unwrap"; "testRuntimeError"; "crash"] @
  List.concat_map (fun name -> [builtin name (N.BuiltinValue (name, 0)) name; builtin (name ^ "_v0") (N.BuiltinValue (name, 0)) name]) ["testNan"; "testInfinity"] @
  [builtin "blobEmpty" (N.BuiltinValue ("blobEmpty", 0)) "blobEmpty"] in
 N.addCandidates (source @ intrinsic @ builtins) N.empty
[@@warning "-4"]
let unionMany sets = List.fold_left S.union S.empty sets
(*
   Collect resolved callable dependencies for declaration grouping. Local
   availability has already been decided by name resolution, so only canonical
   package names can become declaration-graph edges here.
*)
let rec collectDeclarationCalls expr =
 let combine values = List.map collectDeclarationCalls values |> unionMany in
 match expr with
 | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | RuntimeError _ -> S.empty
 | BoundaryRender (_, value) | UnaryOp (_, value) | TupleAccess (value, _) | RecordAccess (value, _) -> collectDeclarationCalls value
 | BinOp (_, left, right) | Sequence (left, right) | Let (_, left, right) | RecursiveLet (_, left, right) -> combine [left; right]
 | If (condition, yes, no) -> combine [condition; yes; no]
 | Apply (Var name, _, args) -> S.add name (combine (NonEmptyList.toList args))
 | TupleLiteral values | ListLiteral values -> combine values
 | DictLiteral (_, _, entries) -> combine (List.concat_map (fun (key, value) -> [key; value]) entries)
 | RecordLiteral (_, entries) -> combine (List.map snd entries)
 | RecordUpdate (record, fields) -> combine (record :: List.map snd fields)
 | Constructor (_, _, fields) -> combine fields
 | Match (scrutinee, cases) -> S.union (collectDeclarationCalls scrutinee) (combine (List.concat_map (fun (case : AST.matchCase) -> case.body :: Option.to_list case.guard) cases))
 | Lambda (_, _, body) -> collectDeclarationCalls body
 | Apply (func, _, args) | IndirectApply (func, args) -> combine (func :: NonEmptyList.toList args)
 | Closure (name, captures) -> S.add name (combine captures)
 | InterpolatedString parts -> combine (List.filter_map (function StringExpr value -> Some value | StringText _ -> None) parts)
(*
   Partition one declaration boundary into deterministic strongly connected
   components. The implementation uses mutual reachability instead of a
   mutable Tarjan stack; source order determines group and member order.
*)
let resolveRecursiveDeclarationGroups topLevels =
 let functions : AST.functionDef list = List.filter_map (function FunctionDef definition -> Some definition | _ -> None) topLevels in
 let names = S.of_list (List.map (fun (definition : AST.functionDef) -> definition.name) functions) in
 let graph = M.of_list (List.map (fun (definition : AST.functionDef) -> definition.name, S.inter names (collectDeclarationCalls definition.body)) functions) in
 let reachable root =
  let rec visit pending visited = match pending with [] -> visited | name :: rest when S.mem name visited -> visit rest visited
   | name :: rest -> visit (S.elements (Option.value (M.find_opt name graph) ~default:S.empty) @ rest) (S.add name visited) in
  visit (S.elements (Option.value (M.find_opt root graph) ~default:S.empty)) S.empty in
 let reachability = M.of_list (List.map (fun (definition : AST.functionDef) -> definition.name, reachable definition.name) functions) in
 let mutually left right = S.mem right (M.find left reachability) && S.mem left (M.find right reachability) in
 let rec groups ordinal remaining acc = match remaining with
  | [] -> List.rev acc
  | (first : AST.functionDef) :: rest ->
    let same, later = List.partition (fun (candidate : AST.functionDef) -> mutually first.name candidate.name) rest in
    let group = AST.topLevelRecursiveGroupId ordinal in
    let availability = if same <> [] then MutualRecursiveMember else if S.mem first.name (M.find first.name graph) then SelfRecursiveMember else CompletedGroupMember in
    let members = List.mapi (fun groupIndex (definition : AST.functionDef) -> match definition.recursion with
     | Some (ParsedRecursiveBinding parsed) -> Some {AST.parsed; group; groupIndex; availability}
     | Some (ResolvedRecursiveBinding resolved) -> Some resolved | Some (TypedRecursiveBinding typed) -> Some typed.resolved
     | Some (RecursiveBindingCandidate _) | None -> None) (first :: same) |> List.filter_map Fun.id in
    let acc = match NonEmptyList.tryFromList members with Some members -> ({AST.group; members} : AST.resolvedRecursiveGroup) :: acc | None -> acc in
    groups (ordinal + 1) later acc in
 let resolved = groups 0 functions [] |> List.concat_map (fun (group : AST.resolvedRecursiveGroup) -> NonEmptyList.toList group.members |> List.map (fun (member : AST.resolvedRecursiveMember) -> member.parsed.sourceName, member)) |> M.of_list in
 List.map (function FunctionDef definition -> (match M.find_opt definition.name resolved with Some recursion -> FunctionDef {definition with recursion = Some (ResolvedRecursiveBinding recursion)} | None -> FunctionDef definition) | other -> other) topLevels
[@@warning "-4"]
(*
   `self` is the enclosing top-level function as (bare name, declared name):
   inside its own body a function is in scope by its bare name, as in the
   interpreter, where a module's declarations see each other unqualified.
   Lexical values have the highest precedence in both applicable
   contexts, and their canonical spelling is the source spelling.
   A lexical candidate's visible name is exactly its binding name.
   If none matched above, adding every in-scope binding cannot affect
   this lookup.
   Json planning needs these aliases' semantic identity.
   Pattern constructor identity is selected against the scrutinee's
   sum type by the pattern checker; equal case names in other types
   are therefore not an ambiguity at this syntax-only traversal.
   Preserve a genuine ambiguity until the expected sum type
   is available during type checking. Key's broad case names
   do not shadow a single pre-existing constructor identity.
*)
let resolveProgramNames resolutionEnv aliases recordNames (Program topLevels) =
 let ( let* ) = Result.bind in
 let map = Result.map in
 let traverse = ResultList.traverse in
 let resolveName currentModule self context locals spelling =
  let applicable = match context with N.Value | N.Callable -> true | N.Constructor | N.Type -> false in
  if applicable && S.mem spelling locals then Ok spelling else
  match self with
  | Some (bare, declared) when applicable && bare = spelling && bare <> "" -> Ok declared
  | Some _ | None -> N.resolveInModule context currentModule spelling resolutionEnv |> map (fun (value : N.successfulResolution) -> N.canonicalSpelling value.N.identity) |> Result.map_error (fun error -> ResolutionFailure error) in
 let rec resolveType currentModule typ =
  let recurse = resolveType currentModule in
  let named make name args =
   let* name = resolveName currentModule None N.Type S.empty name in let* args = traverse recurse args in
   match name, M.find_opt name aliases with
   | ("DateTime" | "Uuid"), _ -> Ok (make name args)
   | _, Some (params, target) when List.length params = List.length args -> recurse (Types.applySubst (M.of_list (List.combine params args)) target)
   | _ -> Ok (make name args) in
  match typ with
  | TRecord (name, args) -> named (fun name args -> TRecord (name, args)) name args
  | TSum (name, args) -> named (fun name args -> if S.mem name recordNames then TRecord (name, args) else TSum (name, args)) name args
  | TFunction (params, ret) -> let* params = traverse recurse params in let* ret = recurse ret in Ok (TFunction (params, ret))
  | TTuple values -> map (fun values -> TTuple values) (traverse recurse values)
  | TList value -> map (fun value -> TList value) (recurse value) | TStream value -> map (fun value -> TStream value) (recurse value)
  | TDict (key, value) -> let* key = recurse key in let* value = recurse value in Ok (TDict (key, value))
  | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr -> Ok typ in
 let rec patternNames = function
  | PVar name -> S.singleton name
  | PConstructor (_, fields) | PResolvedConstructor (_, _, _, fields) | PTuple fields | PList fields -> unionMany (List.map patternNames fields)
  | PListCons (heads, tail) -> S.union (unionMany (List.map patternNames heads)) (patternNames tail)
  | POr alternatives -> patternNames (NonEmptyList.head alternatives)
  | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _ | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _ | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _ | PString _ | PChar _ | PFloat _ -> S.empty in
 let rec resolvePattern locals pattern =
  let recurse = resolvePattern locals in
  match pattern with
  | PConstructor (name, fields) -> map (fun fields -> PConstructor (name, fields)) (traverse recurse fields)
  | PTuple fields -> map (fun fields -> PTuple fields) (traverse recurse fields)
  | PList fields -> map (fun fields -> PList fields) (traverse recurse fields)
  | PListCons (heads, tail) -> let* heads = traverse recurse heads in let* tail = recurse tail in Ok (PListCons (heads, tail))
  | POr alternatives -> map (fun fields -> POr (NonEmptyList.fromList fields)) (traverse recurse (NonEmptyList.toList alternatives))
  | _ -> Ok pattern [@warning "-4"] in
 let rec resolveExpr currentModule self locals expr =
  let recurse = resolveExpr currentModule self locals in
  let resolveArgs args = map NonEmptyList.fromList (traverse recurse (NonEmptyList.toList args)) in
  match expr with
  | Var name -> map (fun name -> Var name) (resolveName currentModule self N.Value locals name)
  | Apply (Var name, args, values) -> let* name = resolveName currentModule self N.Callable locals name in let* args = traverse (resolveType currentModule) args in let* values = resolveArgs values in Ok (Apply (Var name, args, values))
  | Constructor (reference, variant, fields) ->
    let resolvedConstructor name = match List.rev (String.split_on_char '.' name) with
     | case :: rest -> map (fun fields -> Constructor (AST.resolvedConstructorReference (String.concat "." (List.rev rest)), case, fields)) (traverse recurse fields)
     | [] -> Error (GenericError "Resolved constructor name contained no segments") in
    let rec owner name = match M.find_opt name aliases with Some (_, TRecord (target, _) | _, TSum (target, _)) -> owner target | _ -> name in
    let spelling = match AST.constructorReferenceTypeName reference with None -> variant | Some name -> owner name ^ "." ^ variant in
    (match resolveName currentModule self N.Constructor locals spelling with
     | Ok name -> resolvedConstructor name
     | Error (ResolutionFailure (N.AmbiguousReference (_, _, identities))) when reference = UnresolvedConstructor None ->
       let identities = List.filter (function N.ConstructorSymbol ("Darklang.Stdlib.Cli.Stdin.Key.Key", _) -> false | _ -> true) identities in
       (match identities with [identity] -> resolvedConstructor (N.canonicalSpelling identity) | _ -> map (fun fields -> Constructor (reference, variant, fields)) (traverse recurse fields))
     | Error error -> Error error) [@warning "-4"]
  | Let (pattern, value, body) -> let* value = recurse value in let* body = resolveExpr currentModule self (S.union locals (S.of_list (AST.letPatternBindings pattern))) body in Ok (Let (pattern, value, body))
  | RecursiveLet (recursion, value, body) ->
    let name = AST.recursiveBindingName recursion and kind = AST.recursiveBindingKind recursion in
    let parsed = match recursion with ParsedRecursiveBinding parsed -> parsed | ResolvedRecursiveBinding resolved -> resolved.parsed | TypedRecursiveBinding typed -> typed.resolved.parsed | RecursiveBindingCandidate _ -> Crash.crash "Recursive candidate was not assigned a parsed identity" in
    let parameterShadows = match value with Lambda (params, _, _) -> List.exists (fun (param : AST.lambdaParameter) -> List.mem name (AST.letPatternBindings param.pattern)) (NonEmptyList.toList params) | _ -> false in
    let collision = S.mem name locals in
    let packageCollision = match N.resolveInModule N.Callable currentModule name resolutionEnv with Ok _ -> true | Error _ -> false in
    if kind = NamedLocalFunctionMember && (collision || packageCollision) then Error (GenericError ("Nested function name '" ^ name ^ "' is ambiguous with an existing function or value")) else
    let availability = if collision || parameterShadows then OrdinaryBinding else SelfRecursiveMember in
    let valueLocals = match availability with SelfRecursiveMember -> S.add name locals | OrdinaryBinding -> locals | MutualRecursiveMember | CompletedGroupMember | ImportedGroupMember -> Crash.crash "Local recursive candidate received a non-local availability" in
    let* value = resolveExpr currentModule self valueLocals value in let* body = resolveExpr currentModule self (S.add name locals) body in
    Ok (RecursiveLet (ResolvedRecursiveBinding {AST.parsed; group = AST.singletonRecursiveGroupId parsed.member; groupIndex = 0; availability}, value, body))
  | Lambda (params, annotation, body) ->
    let optional = function None -> Ok None | Some typ -> map Option.some (resolveType currentModule typ) in
    let* params = traverse (fun (param : AST.lambdaParameter) -> let* sourceAnnotation = optional param.sourceAnnotation in let* inferredType = optional param.inferredType in Ok {param with sourceAnnotation; inferredType}) (NonEmptyList.toList params) in
    let names = S.of_list (List.concat_map (fun (param : AST.lambdaParameter) -> AST.letPatternBindings param.pattern) params) in
    let* annotation = optional annotation in let* body = resolveExpr currentModule self (S.union locals names) body in Ok (Lambda (NonEmptyList.fromList params, annotation, body))
  | Match (scrutinee, cases) -> let* scrutinee = recurse scrutinee in
    let* cases = traverse (fun (case : AST.matchCase) -> let patterns = NonEmptyList.toList case.patterns in
     let* resolvedPatterns = traverse (resolvePattern locals) patterns in let names = S.union locals (unionMany (List.map patternNames patterns)) in
     let* guard = ResultList.sequenceOption (Option.map (resolveExpr currentModule self names) case.guard) in
     let* body = resolveExpr currentModule self names case.body in Ok {AST.patterns = NonEmptyList.fromList resolvedPatterns; guard; body}) cases in Ok (Match (scrutinee, cases))
  | RecordLiteral (reference, fields) ->
    let* name = resolveName currentModule None N.Type locals reference.sourceTypeName in let* typeArgs = traverse (resolveType currentModule) reference.typeArgs in
    let* fields = traverse (fun (field, value) -> map (fun value -> field, value) (recurse value)) fields in
    Ok (RecordLiteral ({AST.sourceTypeName = name; resolvedTypeName = name; typeArgs}, fields))
  | DictLiteral (key, value, entries) -> let* entries = traverse (fun (key, value) -> let* key = recurse key in let* value = recurse value in Ok (key, value)) entries in Ok (DictLiteral (key, value, entries))
  | BoundaryRender (name, value) -> map (fun value -> BoundaryRender (name, value)) (recurse value)
  | BinOp (op, left, right) -> let* left = recurse left in let* right = recurse right in Ok (BinOp (op, left, right))
  | UnaryOp (op, value) -> map (fun value -> UnaryOp (op, value)) (recurse value)
  | If (condition, yes, no) -> let* condition = recurse condition in let* yes = recurse yes in let* no = recurse no in Ok (If (condition, yes, no))
  | Sequence (first, next) -> let* first = recurse first in let* next = recurse next in Ok (Sequence (first, next))
  | InterpolatedString parts -> map (fun parts -> InterpolatedString parts) (traverse (function StringText text -> Ok (StringText text) | StringExpr value -> map (fun value -> StringExpr value) (recurse value)) parts)
  | TupleLiteral values -> map (fun values -> TupleLiteral values) (traverse recurse values)
  | TupleAccess (value, index) -> map (fun value -> TupleAccess (value, index)) (recurse value)
  | RecordUpdate (record, fields) -> let* record = recurse record in let* fields = traverse (fun (field, value) -> map (fun value -> field, value) (recurse value)) fields in Ok (RecordUpdate (record, fields))
  | RecordAccess (record, field) -> map (fun record -> RecordAccess (record, field)) (recurse record)
  | ListLiteral values -> map (fun values -> ListLiteral values) (traverse recurse values)
  | Apply (func, [], args) -> let* func = recurse func in let* args = resolveArgs args in Ok (Apply (func, [], args))
  | Apply (_, _ :: _, _) -> Error (GenericError "Explicit type arguments require a named function")
  | IndirectApply (func, args) -> let* func = recurse func in let* args = resolveArgs args in Ok (IndirectApply (func, args))
  | Closure (name, captures) -> let* name = resolveName currentModule self N.Callable locals name in map (fun captures -> Closure (name, captures)) (traverse recurse captures)
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | RuntimeError _ -> Ok expr in
 let resolveTypeDef currentModule = function
  | RecordDef (name, params, fields) -> map (fun fields -> RecordDef (name, params, fields)) (traverse (fun (field, typ) -> map (fun typ -> field, typ) (resolveType currentModule typ)) fields)
  | SumTypeDef (name, params, variants) -> map (fun variants -> SumTypeDef (name, params, variants)) (traverse (fun (variant : AST.variant) -> map (fun fields -> {variant with fields}) (traverse (resolveType currentModule) variant.fields)) variants)
  | TypeAlias (name, params, target) -> map (fun target -> TypeAlias (name, params, target)) (resolveType currentModule target) in
 let declaredModule name = match N.tryQualifiedName name with None -> [] | Some qualified -> List.rev (List.tl (List.rev (N.qualifiedNameSegments qualified))) in
 let resolveTopLevel = function
  | FunctionDef definition ->
    let currentModule = declaredModule definition.name in
    let* params = traverse (fun (name, typ) -> map (fun typ -> name, typ) (resolveType currentModule typ)) (NonEmptyList.toList definition.params) in
    let* returnType = resolveType currentModule definition.returnType in
    let* body = resolveExpr currentModule (Some (snd (splitDeclaredName definition.name), definition.name)) (S.of_list (List.map fst params)) definition.body in
    let recursion = match definition.recursion with Some (ParsedRecursiveBinding parsed) -> Some (ParsedRecursiveBinding {parsed with sourceName = definition.name}) | other -> other in
    Ok (FunctionDef {definition with params = NonEmptyList.fromList params; returnType; body; recursion})
  | TypeDef definition -> let name = match definition with RecordDef (name, _, _) | SumTypeDef (name, _, _) | TypeAlias (name, _, _) -> name in map (fun definition -> TypeDef definition) (resolveTypeDef (declaredModule name) definition)
  | ValueDef value -> map (fun body -> match value with UncheckedValueDef (name, _) -> ValueDef (UncheckedValueDef (name, body)) | CheckedValueDef (name, typ, _) -> ValueDef (CheckedValueDef (name, typ, body))) (resolveExpr (declaredModule (AST.valueDefName value)) None S.empty (AST.valueDefBody value))
  | Expression (path, expr) -> map (fun expr -> Expression (path, expr)) (resolveExpr path None S.empty expr) in
 map (fun values -> Program values) (traverse resolveTopLevel topLevels)

[@@warning "-4"]
