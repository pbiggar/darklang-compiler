// ResolveDeclarations.fs - Resolve declaration identities, recursive groups, and source names.

module ResolveDeclarations

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open CheckedFreeVariables

/// Registries derivable directly from a program's top-level declarations.
type internal TopLevelDeclarationSummary = {
    TypeReg: TypeRegistry
    RecordTypeParams: Map<string, string list>
    AliasReg: AliasRegistry
    VariantLookup: VariantLookup
    FuncSigs: Map<string, Type list * Type>
    FuncParamNames: FuncParamNameRegistry
    GenericFuncs: Map<string, string list>
}

let private splitDeclaredName (name: string) : NameResolution.NamespaceIdentity * string =
    match NameResolution.tryQualifiedName name with
    | None -> (NameResolution.RootNamespace, name)
    | Some qualifiedName ->
        match NameResolution.qualifiedNameSegments qualifiedName |> List.rev with
        | terminal :: reversedNamespace ->
            match List.rev reversedNamespace |> NonEmptyList.tryFromList with
            | Some path -> (NameResolution.ModuleNamespace path, terminal)
            | None -> (NameResolution.RootNamespace, terminal)
        | [] -> Crash.crash "Qualified name contained no segments"

let private requiredCandidate visibleName identity provenance : NameResolution.Candidate =
    match NameResolution.candidate visibleName identity provenance with
    | Some candidate -> candidate
    | None -> Crash.crash $"Invalid compiler declaration name entered resolution inventory: {visibleName}"

let internal declarationResolutionEnvironment
    (topLevels: TopLevel list)
    (moduleRegistry: ModuleRegistry)
    (includeIntrinsicCatalog: bool)
    : NameResolution.ResolutionEnvironment =
    let sourceFunctionNames =
        topLevels
        |> List.choose (function FunctionDef funcDef -> Some funcDef.Name | _ -> None)
        |> Set.ofList
    let registeredFunctionNames =
        Set.union sourceFunctionNames (moduleRegistry |> Map.keys |> Set.ofSeq)
    let visibleFunctionNames (qualifiedName: string) =
        let versionedName = $"{qualifiedName}_v0"
        if qualifiedName.EndsWith("_v0") || Set.contains versionedName registeredFunctionNames then
            [qualifiedName]
        else
            [qualifiedName; versionedName]
    let declarationKey topLevel =
        match topLevel with
        | FunctionDef funcDef -> Some ("function", funcDef.Name)
        | ValueDef valueDef -> Some ("value", valueDefName valueDef)
        | TypeDef (RecordDef (name, _, _))
        | TypeDef (SumTypeDef (name, _, _))
        | TypeDef (TypeAlias (name, _, _)) -> Some ("type", name)
        | Expression _ -> None
    let winningDeclarationIndices =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, topLevel) ->
            declarationKey topLevel |> Option.map (fun key -> (key, index)))
        |> Map.ofList
    let sourceCandidates =
        topLevels
        |> List.indexed
        |> List.filter (fun (index, topLevel) ->
            declarationKey topLevel
            |> Option.map (fun key -> Map.tryFind key winningDeclarationIndices = Some index)
            |> Option.defaultValue true)
        |> List.collect (fun (declarationIndex, topLevel) ->
            let declarationId name = $"source:{declarationIndex}:{name}"
            match topLevel with
            | FunctionDef funcDef ->
                let (namespaceIdentity, terminal) = splitDeclaredName funcDef.Name
                let identity =
                    NameResolution.ModuleFunction (
                        namespaceIdentity,
                        terminal,
                        declarationId funcDef.Name
                    )
                visibleFunctionNames funcDef.Name
                |> List.map (fun visibleName ->
                    requiredCandidate
                        visibleName
                        identity
                        (NameResolution.SourceDeclaration funcDef.Name))
            | ValueDef valueDef ->
                let name = valueDefName valueDef
                let (namespaceIdentity, terminal) = splitDeclaredName name
                [ requiredCandidate
                    name
                    (NameResolution.ModuleValue (namespaceIdentity, terminal))
                    (NameResolution.SourceDeclaration name) ]
            | TypeDef typeDef ->
                let (typeName, variants) =
                    match typeDef with
                    | RecordDef (name, _, _) -> (name, [])
                    | TypeAlias (name, _, _) -> (name, [])
                    | SumTypeDef (name, _, variants) -> (name, variants)
                let typeCandidate =
                    requiredCandidate
                        typeName
                        (NameResolution.UserType typeName)
                        (NameResolution.SourceDeclaration typeName)
                let constructorCandidates =
                    variants
                    |> List.collect (fun variant ->
                        let identity =
                            NameResolution.ConstructorSymbol (typeName, variant.Name)
                        [ requiredCandidate
                            variant.Name
                            identity
                            (NameResolution.SourceDeclaration $"{typeName}.{variant.Name}")
                          requiredCandidate
                            $"{typeName}.{variant.Name}"
                            identity
                            (NameResolution.SourceDeclaration $"{typeName}.{variant.Name}") ])
                typeCandidate :: constructorCandidates
            | Expression _ -> [])

    let intrinsicCandidates =
        moduleRegistry
        |> Map.toList
        |> List.filter (fun (qualifiedName, _) ->
            includeIntrinsicCatalog && not (Set.contains qualifiedName sourceFunctionNames))
        |> List.collect (fun (qualifiedName, _) ->
            let (namespaceIdentity, terminal) = splitDeclaredName qualifiedName
            let identity =
                NameResolution.ModuleFunction (
                    namespaceIdentity,
                    terminal,
                    $"intrinsic:{qualifiedName}"
                )
            visibleFunctionNames qualifiedName
            |> List.map (fun visibleName ->
                requiredCandidate
                    visibleName
                    identity
                    (NameResolution.CompilerExtension qualifiedName)))

    let builtinCandidates =
        [ requiredCandidate
            "Builtin.unwrap"
            (NameResolution.BuiltinFunction ("unwrap", 0))
            (NameResolution.BuiltinRegistration "Builtin.unwrap")
          requiredCandidate
            "Builtin.testRuntimeError"
            (NameResolution.BuiltinFunction ("testRuntimeError", 0))
            (NameResolution.BuiltinRegistration "Builtin.testRuntimeError")
          requiredCandidate
            "Builtin.crash"
            (NameResolution.BuiltinFunction ("crash", 0))
            (NameResolution.BuiltinRegistration "Builtin.crash")
          requiredCandidate
            "Builtin.testNan"
            (NameResolution.BuiltinValue ("testNan", 0))
            (NameResolution.BuiltinRegistration "Builtin.testNan")
          requiredCandidate
            "Builtin.testNan_v0"
            (NameResolution.BuiltinValue ("testNan", 0))
            (NameResolution.BuiltinRegistration "Builtin.testNan")
          requiredCandidate
            "Builtin.testInfinity"
            (NameResolution.BuiltinValue ("testInfinity", 0))
            (NameResolution.BuiltinRegistration "Builtin.testInfinity")
          requiredCandidate
            "Builtin.testInfinity_v0"
            (NameResolution.BuiltinValue ("testInfinity", 0))
            (NameResolution.BuiltinRegistration "Builtin.testInfinity")
          requiredCandidate
            "Builtin.blobEmpty"
            (NameResolution.BuiltinValue ("blobEmpty", 0))
            (NameResolution.BuiltinRegistration "Builtin.blobEmpty") ]

    NameResolution.empty
    |> NameResolution.addCandidates (sourceCandidates @ intrinsicCandidates @ builtinCandidates)

/// Collect resolved callable dependencies for declaration grouping. Local
/// availability has already been decided by name resolution, so only canonical
/// package names can become declaration-graph edges here.
let rec private collectDeclarationCalls (expr: Expr) : Set<string> =
    let combine expressions =
        expressions
        |> List.map collectDeclarationCalls
        |> List.fold Set.union Set.empty
    match expr with
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
    | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
    | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
    | Var _ | FuncRef _ | RuntimeError _ -> Set.empty
    | BoundaryRender (_, value) | UnaryOp (_, value) | TupleAccess (value, _)
    | RecordAccess (value, _) -> collectDeclarationCalls value
    | BinOp (_, left, right) | Sequence (left, right)
    | Let (_, left, right) | RecursiveLet (_, left, right) -> combine [left; right]
    | If (condition, thenBranch, elseBranch) -> combine [condition; thenBranch; elseBranch]
    | Call (name, args) | TypeApp (name, _, args) ->
        Set.add name (combine (NonEmptyList.toList args))
    | TupleLiteral values | ListLiteral values -> combine values
    | DictLiteral (_, entries) | RecordLiteral (_, entries) -> entries |> List.map snd |> combine
    | RecordUpdate (record, fields) -> combine (record :: (fields |> List.map snd))
    | Constructor (_, _, payload) ->
        payload |> Option.map collectDeclarationCalls |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let caseCalls =
            cases
            |> List.collect (fun case -> case.Body :: (case.Guard |> Option.toList))
            |> combine
        Set.union (collectDeclarationCalls scrutinee) caseCalls
    | Lambda (_, _, body) -> collectDeclarationCalls body
    | Apply (func, args) | IndirectApply (func, args) ->
        combine (func :: NonEmptyList.toList args)
    | Closure (name, captures) -> Set.add name (combine captures)
    | InterpolatedString parts ->
        parts
        |> List.choose (function StringExpr value -> Some value | StringText _ -> None)
        |> combine

/// Partition one declaration boundary into deterministic strongly connected
/// components. The implementation uses mutual reachability instead of a
/// mutable Tarjan stack; source order determines group and member order.
let internal resolveRecursiveDeclarationGroups (topLevels: TopLevel list) : TopLevel list =
    let functions : FunctionDef list =
        topLevels
        |> List.choose (function FunctionDef func -> Some func | _ -> None)
    let functionNames = functions |> List.map _.Name |> Set.ofList
    let graph =
        functions
        |> List.map (fun func ->
            (func.Name, Set.intersect functionNames (collectDeclarationCalls func.Body)))
        |> Map.ofList

    let reachableFrom root =
        let rec visit pending visited =
            match pending with
            | [] -> visited
            | name :: rest when Set.contains name visited -> visit rest visited
            | name :: rest ->
                let next = Map.tryFind name graph |> Option.defaultValue Set.empty |> Set.toList
                visit (next @ rest) (Set.add name visited)
        let first = Map.tryFind root graph |> Option.defaultValue Set.empty |> Set.toList
        visit first Set.empty

    let reachability =
        functions
        |> List.map (fun func -> (func.Name, reachableFrom func.Name))
        |> Map.ofList

    let mutuallyReachable left right =
        Map.find left reachability |> Set.contains right
        && Map.find right reachability |> Set.contains left

    let rec formGroups
        (ordinal: int)
        (remaining: FunctionDef list)
        (acc: ResolvedRecursiveGroup list)
        : ResolvedRecursiveGroup list =
        match remaining with
        | [] -> List.rev acc
        | first :: rest ->
            let (sameGroup, laterGroups) =
                rest |> List.partition (fun candidate -> mutuallyReachable first.Name candidate.Name)
            let members = first :: sameGroup
            let groupId = recursiveGroupId [0; ordinal]
            let availability =
                if not (List.isEmpty sameGroup) then MutualRecursiveMember
                elif Map.find first.Name graph |> Set.contains first.Name then SelfRecursiveMember
                else CompletedGroupMember
            let resolvedMembers : ResolvedRecursiveMember list =
                members
                |> List.indexed
                |> List.choose (fun (groupIndex, func) ->
                    match func.Recursion with
                    | Some (ParsedRecursiveBinding parsed) ->
                        Some {
                            Parsed = parsed
                            Group = groupId
                            GroupIndex = groupIndex
                            Availability = availability
                        }
                    | Some (ResolvedRecursiveBinding resolved) -> Some resolved
                    | Some (TypedRecursiveBinding typed) -> Some typed.Resolved
                    | Some (RecursiveBindingCandidate _) | None -> None)
            match NonEmptyList.tryFromList resolvedMembers with
            | Some nonempty ->
                let group : ResolvedRecursiveGroup = { Group = groupId; Members = nonempty }
                formGroups (ordinal + 1) laterGroups (group :: acc)
            | None -> formGroups (ordinal + 1) laterGroups acc

    let groups : ResolvedRecursiveGroup list = formGroups 0 functions []
    let resolvedByName : Map<string, ResolvedRecursiveMember> =
        groups
        |> List.collect (fun group ->
            group.Members
            |> NonEmptyList.toList
            |> List.map (fun groupMember -> (groupMember.Parsed.SourceName, groupMember)))
        |> Map.ofList

    topLevels
    |> List.map (function
        | FunctionDef func ->
            match Map.tryFind func.Name resolvedByName with
            | Some recursion -> FunctionDef { func with Recursion = Some (ResolvedRecursiveBinding recursion) }
            | None -> FunctionDef func
        | other -> other)

let internal resolveProgramNames
    (resolutionEnv: NameResolution.ResolutionEnvironment)
    (aliasReg: AliasRegistry)
    (recordTypeNames: Set<string>)
    (program: Program)
    : Result<Program, TypeError> =
    // `self` is the enclosing top-level function as (bare name, declared name):
    // inside its own body a function is in scope by its bare name, as in the
    // interpreter, where a module's declarations see each other unqualified.
    let resolveName (self: (string * string) option) context localNames spelling =
        let lexicalHit =
            match context with
            | NameResolution.ResolutionContext.Value
            | NameResolution.ResolutionContext.Callable -> Set.contains spelling localNames
            | NameResolution.ResolutionContext.Constructor
            | NameResolution.ResolutionContext.Type -> false
        let selfHit =
            match self, context with
            | Some (bare, _), (NameResolution.ResolutionContext.Value | NameResolution.ResolutionContext.Callable) ->
                bare = spelling && bare <> ""
            | _ -> false
        if lexicalHit then
            // Lexical values have the highest precedence in both applicable
            // contexts, and their canonical spelling is the source spelling.
            Ok spelling
        elif selfHit then
            match self with
            | Some (_, declared) -> Ok declared
            | None -> Ok spelling
        else
            // A lexical candidate's visible name is exactly its binding name.
            // If none matched above, adding every in-scope binding cannot affect
            // this lookup.
            NameResolution.resolve context spelling resolutionEnv
            |> Result.map (fun resolution -> NameResolution.canonicalSpelling resolution.Identity)
            |> Result.mapError ResolutionFailure

    let rec resolveTypeRefs typ =
        let recurse = resolveTypeRefs
        let resolveNamed makeType name typeArgs =
            resolveName None NameResolution.ResolutionContext.Type Set.empty name
            |> Result.bind (fun resolvedName ->
                ResultList.traverse recurse typeArgs
                |> Result.bind (fun resolvedArgs ->
                    match resolvedName, Map.tryFind resolvedName aliasReg with
                    // Json planning needs these aliases' semantic identity.
                    | ("DateTime" | "Uuid"), _ -> Ok (makeType resolvedName resolvedArgs)
                    | _, Some (typeParams, target) when List.length typeParams = List.length resolvedArgs ->
                        let subst = List.zip typeParams resolvedArgs |> Map.ofList
                        target |> applySubst subst |> recurse
                    | _ -> Ok (makeType resolvedName resolvedArgs)))
        match typ with
        | TRecord (name, typeArgs) -> resolveNamed (fun n args -> TRecord (n, args)) name typeArgs
        | TSum (name, typeArgs) ->
            resolveNamed
                (fun n args ->
                    if Set.contains n recordTypeNames then TRecord (n, args)
                    else TSum (n, args))
                name
                typeArgs
        | TFunction (parameterTypes, returnType) ->
            ResultList.traverse recurse parameterTypes
            |> Result.bind (fun parameters' -> recurse returnType |> Result.map (fun ret -> TFunction (parameters', ret)))
        | TTuple elementTypes -> ResultList.traverse recurse elementTypes |> Result.map TTuple
        | TEnumFields fieldTypes -> ResultList.traverse recurse fieldTypes |> Result.map TEnumFields
        | TList elementType -> recurse elementType |> Result.map TList
        | TStream elementType -> recurse elementType |> Result.map TStream
        | TDict (keyType, valueType) ->
            recurse keyType
            |> Result.bind (fun key' -> recurse valueType |> Result.map (fun value' -> TDict (key', value')))
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> Ok typ

    let rec patternBoundNames pattern =
        match pattern with
        | PVar name -> Set.singleton name
        | PConstructor (_, payload) -> payload |> Option.map patternBoundNames |> Option.defaultValue Set.empty
        | PTuple patterns | PList patterns -> patterns |> List.map patternBoundNames |> Set.unionMany
        | PListCons (heads, tail) -> Set.union (heads |> List.map patternBoundNames |> Set.unionMany) (patternBoundNames tail)
        | POr alternatives -> alternatives |> NonEmptyList.head |> patternBoundNames
        | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _ | PInt16Literal _
        | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _ | PUInt32Literal _ | PUInt64Literal _
        | PUInt128Literal _ | PBool _ | PString _ | PChar _ | PFloat _ -> Set.empty

    let rec resolvePattern localNames pattern =
        let recurse = resolvePattern localNames
        match pattern with
        | PConstructor (name, payload) ->
            // Pattern constructor identity is selected against the scrutinee's
            // sum type by the pattern checker; equal case names in other types
            // are therefore not an ambiguity at this syntax-only traversal.
            payload
            |> Option.map recurse
            |> ResultList.sequenceOption
            |> Result.map (fun payload' -> PConstructor (name, payload'))
        | PTuple patterns -> ResultList.traverse recurse patterns |> Result.map PTuple
        | PList patterns -> ResultList.traverse recurse patterns |> Result.map PList
        | PListCons (heads, tail) ->
            ResultList.traverse recurse heads
            |> Result.bind (fun heads' -> recurse tail |> Result.map (fun tail' -> PListCons (heads', tail')))
        | POr alternatives ->
            alternatives
            |> NonEmptyList.toList
            |> ResultList.traverse recurse
            |> Result.map (NonEmptyList.fromList >> POr)
        | _ -> Ok pattern

    let rec resolveExpr self localNames expr =
        let recurse = resolveExpr self localNames
        let resolveArgs args = ResultList.traverse recurse (NonEmptyList.toList args) |> Result.map NonEmptyList.fromList
        match expr with
        | Var name ->
            resolveName self NameResolution.ResolutionContext.Value localNames name
            |> Result.map Var
        | Call (name, args) ->
            resolveName self NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName -> resolveArgs args |> Result.map (fun args' -> Call (resolvedName, args')))
        | TypeApp (name, typeArgs, args) ->
            resolveName self NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName ->
                ResultList.traverse resolveTypeRefs typeArgs
                |> Result.bind (fun types' -> resolveArgs args |> Result.map (fun args' -> TypeApp (resolvedName, types', args'))))
        | FuncRef name -> resolveName self NameResolution.ResolutionContext.Callable localNames name |> Result.map FuncRef
        | Constructor (constructorReference, variantName, payload) ->
            let resolvedConstructor (resolvedName: string) =
                let segments = resolvedName.Split('.') |> Array.toList
                match List.rev segments with
                | caseName :: reversedTypeName ->
                    let resolvedTypeName = reversedTypeName |> List.rev |> String.concat "."
                    payload
                    |> Option.map recurse
                    |> ResultList.sequenceOption
                    |> Result.map (fun payload' ->
                        Constructor (resolvedConstructorReference resolvedTypeName, caseName, payload'))
                | [] -> Error (GenericError "Resolved constructor name contained no segments")
            let spelling =
                match constructorReferenceTypeName constructorReference with
                | None -> variantName
                | Some typeName ->
                    let rec canonicalOwner owner =
                        match Map.tryFind owner aliasReg with
                        | Some ([], TRecord (target, []))
                        | Some ([], TSum (target, [])) -> canonicalOwner target
                        | _ -> owner
                    $"{canonicalOwner typeName}.{variantName}"
            match resolveName self NameResolution.ResolutionContext.Constructor localNames spelling with
            | Ok resolvedName -> resolvedConstructor resolvedName
            | Error (ResolutionFailure (NameResolution.AmbiguousReference (_, _, identities)))
                when constructorReference = UnresolvedConstructor None ->
                let nonKeyIdentities =
                    identities
                    |> List.filter (function
                        | NameResolution.ConstructorSymbol ("Stdlib.Cli.Stdin.Key.Key", _) -> false
                        | _ -> true)
                match nonKeyIdentities with
                | [identity] ->
                    identity
                    |> NameResolution.canonicalSpelling
                    |> resolvedConstructor
                | _ ->
                    // Preserve a genuine ambiguity until the expected sum type
                    // is available during type checking. Key's broad case names
                    // do not shadow a single pre-existing constructor identity.
                    payload
                    |> Option.map recurse
                    |> ResultList.sequenceOption
                    |> Result.map (fun payload' -> Constructor (constructorReference, variantName, payload'))
            | Error error -> Error error
        | Let (pattern, value, body) ->
            recurse value
            |> Result.bind (fun value' ->
                let bindings = letPatternBindings pattern |> Set.ofList
                resolveExpr self (Set.union localNames bindings) body
                |> Result.map (fun body' -> Let (pattern, value', body')))
        | RecursiveLet (recursion, value, body) ->
            let name = recursiveBindingName recursion
            let kind = recursiveBindingKind recursion
            let parsed =
                match recursion with
                | ParsedRecursiveBinding parsed -> parsed
                | ResolvedRecursiveBinding resolved -> resolved.Parsed
                | TypedRecursiveBinding typed -> typed.Resolved.Parsed
                | RecursiveBindingCandidate _ ->
                    Crash.crash "Recursive candidate was not assigned a parsed identity"
            let parameterShadowsSelf =
                match value with
                | Lambda (parameters, _, _) ->
                    parameters
                    |> NonEmptyList.toList
                    |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                    |> List.contains name
                | _ -> false
            let outerLocalCollision = Set.contains name localNames
            let packageCollision =
                match NameResolution.resolve NameResolution.ResolutionContext.Callable name resolutionEnv with
                | Ok _ -> true
                | Error _ -> false
            if kind = NamedLocalFunctionMember && (outerLocalCollision || packageCollision) then
                Error (GenericError $"Nested function name '{name}' is ambiguous with an existing function or value")
            else
                let availability =
                    if outerLocalCollision || parameterShadowsSelf then OrdinaryBinding
                    else SelfRecursiveMember
                let valueLocals =
                    match availability with
                    | SelfRecursiveMember -> Set.add name localNames
                    | OrdinaryBinding -> localNames
                    | MutualRecursiveMember | CompletedGroupMember | ImportedGroupMember ->
                        Crash.crash "Local recursive candidate received a non-local availability"
                resolveExpr self valueLocals value
                |> Result.bind (fun value' ->
                    resolveExpr self (Set.add name localNames) body
                    |> Result.map (fun body' ->
                        let resolved = {
                            Parsed = parsed
                            Group = singletonRecursiveGroupId parsed.Member
                            GroupIndex = 0
                            Availability = availability
                        }
                        RecursiveLet (ResolvedRecursiveBinding resolved, value', body')))
        | Lambda (parameters, returnAnnotation, body) ->
            let resolveOptionalType = function
                | None -> Ok None
                | Some typ -> resolveTypeRefs typ |> Result.map Some
            parameters
            |> NonEmptyList.toList
            |> ResultList.traverse (fun parameter ->
                resolveOptionalType parameter.SourceAnnotation
                |> Result.bind (fun sourceAnnotation ->
                    resolveOptionalType parameter.InferredType
                    |> Result.map (fun inferredType ->
                        { parameter with SourceAnnotation = sourceAnnotation; InferredType = inferredType })))
            |> Result.bind (fun parameters' ->
                let parameterNames =
                    parameters'
                    |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                    |> Set.ofList
                resolveOptionalType returnAnnotation
                |> Result.bind (fun returnAnnotation' ->
                    resolveExpr self (Set.union localNames parameterNames) body
                    |> Result.map (fun body' -> Lambda (NonEmptyList.fromList parameters', returnAnnotation', body'))))
        | Match (scrutinee, cases) ->
            recurse scrutinee
            |> Result.bind (fun scrutinee' ->
                cases
                |> ResultList.traverse (fun matchCase ->
                    let patterns = NonEmptyList.toList matchCase.Patterns
                    ResultList.traverse (resolvePattern localNames) patterns
                    |> Result.bind (fun patterns' ->
                        let bindings = patterns |> List.map patternBoundNames |> Set.unionMany
                        let caseLocals = Set.union localNames bindings
                        matchCase.Guard
                        |> Option.map (resolveExpr self caseLocals)
                        |> ResultList.sequenceOption
                        |> Result.bind (fun guard' ->
                            resolveExpr self caseLocals matchCase.Body
                            |> Result.map (fun body' ->
                                { Patterns = NonEmptyList.fromList patterns'; Guard = guard'; Body = body' }))))
                |> Result.map (fun cases' -> Match (scrutinee', cases')))
        | RecordLiteral (reference, fields) ->
            resolveName None NameResolution.ResolutionContext.Type localNames reference.SourceTypeName
            |> Result.bind (fun resolvedTypeName ->
                ResultList.traverse resolveTypeRefs reference.TypeArgs
                |> Result.bind (fun typeArgs ->
                    fields
                    |> ResultList.traverse (fun (field, value) -> recurse value |> Result.map (fun value' -> (field, value')))
                    |> Result.map (fun fields' ->
                        RecordLiteral (
                            { SourceTypeName = resolvedTypeName
                              ResolvedTypeName = resolvedTypeName
                              TypeArgs = typeArgs },
                            fields'
                        ))))
        | DictLiteral (valueType, entries) ->
            entries
            |> ResultList.traverse (fun (key, value) -> recurse value |> Result.map (fun value' -> (key, value')))
            |> Result.map (fun entries' -> DictLiteral (valueType, entries'))
        | BoundaryRender (renderer, value) -> recurse value |> Result.map (fun value' -> BoundaryRender (renderer, value'))
        | BinOp (op, left, right) -> recurse left |> Result.bind (fun l -> recurse right |> Result.map (fun r -> BinOp (op, l, r)))
        | UnaryOp (op, inner) -> recurse inner |> Result.map (fun inner' -> UnaryOp (op, inner'))
        | If (condition, thenBranch, elseBranch) ->
            recurse condition |> Result.bind (fun c -> recurse thenBranch |> Result.bind (fun t -> recurse elseBranch |> Result.map (fun e -> If (c, t, e))))
        | Sequence (first, next) ->
            recurse first
            |> Result.bind (fun first' -> recurse next |> Result.map (fun next' -> Sequence (first', next')))
        | InterpolatedString parts ->
            parts
            |> ResultList.traverse (function StringText text -> Ok (StringText text) | StringExpr e -> recurse e |> Result.map StringExpr)
            |> Result.map InterpolatedString
        | TupleLiteral elements -> ResultList.traverse recurse elements |> Result.map TupleLiteral
        | TupleAccess (tuple, index) -> recurse tuple |> Result.map (fun tuple' -> TupleAccess (tuple', index))
        | RecordUpdate (record, updates) ->
            recurse record
            |> Result.bind (fun record' -> updates |> ResultList.traverse (fun (field, value) -> recurse value |> Result.map (fun value' -> (field, value'))) |> Result.map (fun updates' -> RecordUpdate (record', updates')))
        | RecordAccess (record, fieldName) -> recurse record |> Result.map (fun record' -> RecordAccess (record', fieldName))
        | ListLiteral elements -> ResultList.traverse recurse elements |> Result.map ListLiteral
        | Apply (func, args) -> recurse func |> Result.bind (fun func' -> resolveArgs args |> Result.map (fun args' -> Apply (func', args')))
        | IndirectApply (func, args) ->
            recurse func
            |> Result.bind (fun func' -> resolveArgs args |> Result.map (fun args' -> IndirectApply (func', args')))
        | Closure (name, captures) ->
            resolveName self NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName -> ResultList.traverse recurse captures |> Result.map (fun captures' -> Closure (resolvedName, captures')))
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
        | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | RuntimeError _ -> Ok expr

    let resolveTypeDef typeDef =
        match typeDef with
        | RecordDef (name, typeParams, fields) ->
            fields |> ResultList.traverse (fun (field, typ) -> resolveTypeRefs typ |> Result.map (fun typ' -> (field, typ'))) |> Result.map (fun fields' -> RecordDef (name, typeParams, fields'))
        | SumTypeDef (name, typeParams, variants) ->
            variants |> ResultList.traverse (fun variant -> variant.Payload |> Option.map resolveTypeRefs |> ResultList.sequenceOption |> Result.map (fun payload -> { variant with Payload = payload })) |> Result.map (fun variants' -> SumTypeDef (name, typeParams, variants'))
        | TypeAlias (name, typeParams, targetType) -> resolveTypeRefs targetType |> Result.map (fun target' -> TypeAlias (name, typeParams, target'))

    let resolveTopLevel topLevel =
        match topLevel with
        | FunctionDef funcDef ->
            let parameters = NonEmptyList.toList funcDef.Params
            parameters
            |> ResultList.traverse (fun (name, typ) -> resolveTypeRefs typ |> Result.map (fun typ' -> (name, typ')))
            |> Result.bind (fun parameters' ->
                resolveTypeRefs funcDef.ReturnType
                |> Result.bind (fun returnType' ->
                    let locals = parameters' |> List.map fst |> Set.ofList
                    let self = Some (snd (splitDeclaredName funcDef.Name), funcDef.Name)
                    resolveExpr self locals funcDef.Body
                    |> Result.map (fun body' ->
                        let recursion' =
                            match funcDef.Recursion with
                            | Some (ParsedRecursiveBinding parsed) ->
                                Some (ParsedRecursiveBinding { parsed with SourceName = funcDef.Name })
                            | other -> other
                        FunctionDef
                            { funcDef with
                                Params = NonEmptyList.fromList parameters'
                                ReturnType = returnType'
                                Body = body'
                                Recursion = recursion' })))
        | TypeDef typeDef -> resolveTypeDef typeDef |> Result.map TypeDef
        | ValueDef valueDef ->
            resolveExpr None Set.empty (valueDefBody valueDef)
            |> Result.map (fun body ->
                match valueDef with
                | UncheckedValueDef (name, _) -> ValueDef (UncheckedValueDef (name, body))
                | CheckedValueDef (name, typ, _) -> ValueDef (CheckedValueDef (name, typ, body)))
        | Expression expr -> resolveExpr None Set.empty expr |> Result.map Expression

    let (Program topLevels) = program
    ResultList.traverse resolveTopLevel topLevels
    |> Result.map Program
