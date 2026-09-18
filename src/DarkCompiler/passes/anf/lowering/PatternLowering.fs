// PatternLowering.fs - Lower ordered match alternatives and typed pattern projections.

module PatternLowering

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open TypeSubstitution
open Monomorphization
open ClosureAnalysis
open LiftExpressions
open LiftFunctions
open LoweringTypeInference
open ANFContinuations
open LoweringCallbacks

let lowerMatch (toANFCore: ExpressionLowerer) (toAtomCore: AtomLowerer) (toANFBoundAtomCore: BoundAtomLowerer) (sumTypeNames: Set<string>) (inertScopes: Set<string>) (scrutinee: AST.Expr) (cases: AST.MatchCase list) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    // Infer scrutinee type to pass to pattern extraction for correct typing
    let typeEnv = typeEnvFromVarEnv env
    match inferTypeCore sumTypeNames scrutinee typeEnv typeReg variantLookup funcReg moduleRegistry with
    | Error msg -> Error $"Match scrutinee type inference failed: {msg}"
    | Ok scrutType ->
    // Compile match to if-else chain
    // First convert scrutinee to a bound atom. This supports effectful/complex
    // scrutinees such as Builtin.testRuntimeError(...) that cannot be lowered via toAtom.
    toANFBoundAtomCore sumTypeNames inertScopes scrutinee varGen env typeReg variantLookup funcReg moduleRegistry
    |> Result.bind (fun (scrutineeExpr, scrutineeAtom, varGen1) ->
        // Check if any pattern needs to access list structure
        // If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
        let hasNonEmptyListPattern =
            cases |> List.exists (fun mc ->
                mc.Patterns |> AST.NonEmptyList.toList |> List.exists (fun pat ->
                    match pat with
                    | AST.PList (_ :: _) -> true
                    | AST.PListCons (_ :: _, _) -> true  // h :: t also needs list access
                    | _ -> false))

        // If there are non-empty list patterns, bind the scrutinee to a variable
        let (scrutineeAtom', scrutineePostBindings, varGen1') =
            match scrutineeAtom with
            | ANF.Var _ -> (scrutineeAtom, [], varGen1)
            | _ when hasNonEmptyListPattern ->
                let (tempVar, vg) = ANF.freshVar varGen1
                (ANF.Var tempVar, [(tempVar, ANF.Atom scrutineeAtom)], vg)
            | _ -> (scrutineeAtom, [], varGen1)

        // Check if the TYPE that a variant belongs to has any variant with a payload
        // This determines if values are heap-allocated or simple integers
        let tryPatternVariant variantName =
            tryFindVariantForType variantName scrutType variantLookup

        let typeHasAnyPayload (variantName: string) : bool =
            match tryPatternVariant variantName with
            | Some (typeName, _, _, _) ->
                variantLookup
                |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)
            | None -> false

        // Check if pattern always matches (wildcard or variable)
        let rec patternAlwaysMatches (pattern: AST.Pattern) : bool =
            match pattern with
            | AST.PUnit -> true
            | AST.PWildcard -> true
            | AST.PVar _ -> true
            | _ -> false

        // Constructor coverage is usable only when the payload pattern cannot
        // reject a value; literal and nested patterns therefore remain partial.
        let constructorPatternCoverage (pattern: AST.Pattern) : int option =
            match scrutType, pattern with
            | AST.TSum (typeName, _), AST.PConstructor (constructorName, payloadPattern) ->
                match tryFindVariant (AST.resolvedConstructorReference typeName) constructorName variantLookup, payloadPattern with
                | Some (variantTypeName, _, tag, None), None when variantTypeName = typeName ->
                    Some tag
                | Some (variantTypeName, _, tag, Some _), Some innerPattern
                    when variantTypeName = typeName && patternAlwaysMatches innerPattern ->
                    Some tag
                | _ ->
                    None
            | _ ->
                None

        let constructorMatchIsExhaustive (matchCases: AST.MatchCase list) : bool =
            let coveredConstructors =
                matchCases
                |> List.fold (fun coveredOpt mc ->
                    match coveredOpt, mc.Guard with
                    | Some covered, None ->
                        mc.Patterns
                        |> AST.NonEmptyList.toList
                        |> List.fold (fun caseCoveredOpt pattern ->
                            match caseCoveredOpt, constructorPatternCoverage pattern with
                            | Some caseCovered, Some tag ->
                                Some (Set.add tag caseCovered)
                            | _ ->
                                None)
                            (Some covered)
                    | _ ->
                        None)
                    (Some Set.empty)

            match scrutType, coveredConstructors with
            | AST.TSum (typeName, _), Some coveredConstructors ->
                // The lookup intentionally contains both qualified and bare
                // aliases. Tags are the canonical per-type constructor identity,
                // so collecting them also avoids counting those aliases twice.
                let allConstructorsForType =
                    variantLookup
                    |> Map.fold (fun constructors _ (variantTypeName, _, tag, _) ->
                        if variantTypeName = typeName then
                            Set.add tag constructors
                        else
                            constructors)
                        Set.empty
                not (Set.isEmpty allConstructorsForType)
                && coveredConstructors = allConstructorsForType
            | _ ->
                false

        // Extract pattern bindings and compile body with extended environment
        // scrutType is the type of the scrutinee, used to determine correct types for pattern variables
        // The list-pattern compilers take variables, wildcards, numeric literals and
        // tuples of those as heads; a string, a constructor or a nested list there
        // ("Unsupported head pattern in list cons") goes through the stages.
        let rec headNeedsStages (pattern: AST.Pattern) : bool =
            match pattern with
            | AST.PString _ | AST.PChar _ | AST.PConstructor _ | AST.PList _ | AST.PListCons _ -> true
            | AST.PTuple elements -> List.exists headNeedsStages elements
            | AST.POr alternatives -> AST.NonEmptyList.toList alternatives |> List.exists headNeedsStages
            | _ -> false
        let listArmNeedsStages (pattern: AST.Pattern) : bool =
            match pattern with
            | AST.PList elements -> List.exists headNeedsStages elements
            | AST.PListCons (heads, _) -> List.exists headNeedsStages heads
            | _ -> false

        let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
            // Recursively collect all variable bindings from a pattern
            // Returns: updated env, list of bindings, updated vargen
            // sourceType is the type of the source being matched, used to get correct element types
            let rec collectPatternBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match pat with
                | AST.POr alternatives ->
                    collectPatternBindings (AST.NonEmptyList.head alternatives) sourceAtom sourceType env bindings vg
                | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                | AST.PInt8Literal _
                | AST.PInt16Literal _
                | AST.PInt32Literal _
                | AST.PUInt8Literal _
                | AST.PUInt16Literal _
                | AST.PUInt32Literal _
                | AST.PUInt64Literal _ | AST.PUInt128Literal _
                | AST.PUnit
                | AST.PWildcard
                | AST.PBool _
                | AST.PString _
                | AST.PChar _
                | AST.PFloat _ ->
                    // No variable bindings
                    Ok (env, bindings, vg)
                | AST.PVar name ->
                    // Bind the source to a variable with the correct type
                    // Use TypedAtom to preserve the semantic type (e.g., tuple element type)
                    // even when the source comes from a function with generic return type
                    let (tempId, vg1) = ANF.freshVar vg
                    let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                    let newEnv = Map.add name (tempId, sourceType) env
                    Ok (newEnv, binding :: bindings, vg1)
                | AST.PTuple innerPatterns ->
                    let unknownElemTypes =
                        innerPatterns
                        |> List.mapi (fun idx _ -> AST.TVar $"__tuple_elem_{idx}")

                    // Extract each element and recursively collect bindings
                    let rec collectFromTuple (pats: AST.Pattern list) (types: AST.Type list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                        match pats, types with
                        | [], _ -> Ok (env, bindings, vg)
                        | p :: rest, t :: restTypes ->
                            // Extract raw element with TupleGet
                            let (rawElemVar, vg1) = ANF.freshVar vg
                            let rawElemExpr = ANF.TupleGet (sourceAtom, idx)
                            let rawElemBinding = (rawElemVar, rawElemExpr)
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (elemVar, vg1') = ANF.freshVar vg1
                            let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, t)
                            let elemBinding = (elemVar, elemExpr)
                            // Recursively collect bindings from this element's pattern with correct type
                            collectPatternBindings p (ANF.Var elemVar) t env (elemBinding :: rawElemBinding :: bindings) vg1'
                            |> Result.bind (fun (env', bindings', vg') ->
                                collectFromTuple rest restTypes (idx + 1) env' bindings' vg')
                        | _ ->
                            Error "Tuple pattern element/type mismatch"

                    let elemTypes =
                        match sourceType with
                        | AST.TTuple types when List.length types = List.length innerPatterns -> types
                        | AST.TEnumFields types when List.length types = List.length innerPatterns -> types
                        | _ -> unknownElemTypes

                    collectFromTuple innerPatterns elemTypes 0 env bindings vg
                | AST.PConstructor (constructorName, payloadPattern) ->
                    let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
                        match typ with
                        | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
                        | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
                        | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
                        | AST.TList elem -> AST.TList (substituteType subst elem)
                        | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
                        | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
                        | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
                        | _ -> typ

                    let resolvePayloadType (constructorName: string) (scrutineeType: AST.Type) : Result<AST.Type option, string> =
                        match tryFindVariantForType constructorName scrutineeType variantLookup with
                        | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                            let payloadType =
                                match scrutineeType with
                                | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                    let subst = List.zip typeParams typeArgs |> Map.ofList
                                    substituteType subst payloadTypeTemplate
                                | _ -> payloadTypeTemplate
                            Ok (Some payloadType)
                        | Some (_, _, _, None) ->
                            Ok None
                        | None ->
                            Error $"Unknown constructor '{constructorName}' in pattern"

                    match payloadPattern with
                    | None -> Ok (env, bindings, vg)
                    | Some innerPat ->
                        resolvePayloadType constructorName sourceType
                        |> Result.bind (fun payloadType ->
                            match payloadType with
                            | None ->
                                // Constructor arity mismatch should not bind payload.
                                Ok (env, bindings, vg)
                            | Some concretePayloadType ->
                                // Extract payload (at index 1) and recursively collect
                                let (payloadVar, vg1) = ANF.freshVar vg
                                let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                                let payloadBinding = (payloadVar, payloadExpr)
                                collectPatternBindings
                                    innerPat
                                    (ANF.Var payloadVar)
                                    concretePayloadType
                                    env
                                    (payloadBinding :: bindings)
                                    vg1)
                | AST.PList innerPatterns ->
                    // Extract element type from list type
                    let elemType =
                        match sourceType with
                        | AST.TList t -> t
                        | _ -> AST.TVar "__list_elem_unknown"
                    // For list patterns, extract head elements using SkewList operations
                    // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                    // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                    let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                        match pats with
                        | [] -> Ok (env, bindings, vg)
                        | p :: rest ->
                            // Lists are SkewLists - use headUnsafe/tail to extract
                            let (headVar, vg1) = ANF.freshVar vg
                            let headExpr = listHeadUnsafeExpr funcReg elemType currentList
                            let headBinding = (headVar, headExpr)
                            collectPatternBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                            |> Result.bind (fun (env', bindings', vg') ->
                                if List.isEmpty rest then
                                    Ok (env', bindings', vg')
                                else
                                    // Get tail for next iteration
                                    let (tailVar, vg2) = ANF.freshVar vg'
                                    let tailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                                    let tailBinding = (tailVar, tailExpr)
                                    collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                    collectFromList innerPatterns sourceAtom env bindings vg
                | AST.PListCons (headPatterns, tailPattern) ->
                    // Extract element type from list type
                    let elemType =
                        match sourceType with
                        | AST.TList t -> t
                        | _ -> AST.TVar "__list_elem_unknown"
                    // Extract head elements then bind tail using SkewList operations
                    // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                    // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                    let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                        match pats with
                        | [] ->
                            // Bind the remaining list to tail pattern (tail has same type as source)
                            collectPatternBindings tailPattern currentList sourceType env bindings vg
                        | p :: rest ->
                            // Lists are SkewLists - use headUnsafe/tail to extract
                            let (rawHeadVar, vg1) = ANF.freshVar vg
                            let rawHeadExpr = listHeadUnsafeExpr funcReg elemType currentList
                            let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (headVar, vg1') = ANF.freshVar vg1
                            let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                            let headBinding = (headVar, headExpr)
                            collectPatternBindings p (ANF.Var headVar) elemType env (headBinding :: rawHeadBinding :: bindings) vg1'
                            |> Result.bind (fun (env', bindings', vg') ->
                                let (rawTailVar, vg2) = ANF.freshVar vg'
                                let rawTailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                                let rawTailBinding = (rawTailVar, rawTailExpr)
                                // Wrap tail with TypedAtom to preserve list type
                                let (tailVar, vg2') = ANF.freshVar vg2
                                let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, sourceType)
                                let tailBinding = (tailVar, tailExpr)
                                collectHeads rest (ANF.Var tailVar) env' (tailBinding :: rawTailBinding :: bindings') vg2')
                    collectHeads headPatterns sourceAtom env bindings vg

            match pattern with
            | AST.PList _ | AST.PListCons _ when listArmNeedsStages pattern ->
                // Heads that are strings, constructors or lists: the specialized
                // list compilers below do not take them, the general collector does.
                collectPatternBindings pattern scrutAtom scrutType currentEnv [] vg
                |> Result.bind (fun (newEnv, bindings, vg1) ->
                    toANFCore sumTypeNames inertScopes body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        (wrapBindings (List.rev bindings) bodyExpr, vg2)))
            | AST.POr alternatives ->
                extractAndCompileBody (AST.NonEmptyList.head alternatives) body scrutAtom scrutType currentEnv vg
            | AST.PUnit -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PWildcard -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
            | AST.PInt8Literal _
            | AST.PInt16Literal _
            | AST.PInt32Literal _
            | AST.PUInt8Literal _
            | AST.PUInt16Literal _
            | AST.PUInt32Literal _
            | AST.PUInt64Literal _ | AST.PUInt128Literal _ ->
                toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PBool _ -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PString _ -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PChar _ -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PFloat _ -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
            | AST.PVar name ->
                // Bind scrutinee to variable name with the correct type
                let (tempId, vg1) = ANF.freshVar vg
                let env' = Map.add name (tempId, scrutType) currentEnv
                toANFCore sumTypeNames inertScopes body vg1 env' typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (bodyExpr, vg2) ->
                    let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                    (expr, vg2))
            | AST.PConstructor (constructorName, payloadPattern) ->
                match payloadPattern with
                | None -> toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | Some innerPattern ->
                    match tryFindVariantForType constructorName scrutType variantLookup with
                    | Some (_, _, _, None) ->
                        // Constructor arity mismatch behaves as non-matching.
                        // Do not introduce payload bindings in this branch body.
                        toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                        // Extract payload from heap-allocated variant
                        // Variant layout: [tag:8][payload:8], so payload is at index 1
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let (typedPayloadVar, vg2) = ANF.freshVar vg1
                        let payloadExpr = ANF.TupleGet (scrutAtom, 1)
                        // Apply type substitution if scrutType has type args
                        let payloadType =
                            match scrutType with
                            | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                let subst = List.zip typeParams typeArgs |> Map.ofList
                                let rec substitute t =
                                    match t with
                                    | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue t
                                    | AST.TTuple elems -> AST.TTuple (List.map substitute elems)
                                    | AST.TList elem -> AST.TList (substitute elem)
                                    | AST.TDict (k, v) -> AST.TDict (substitute k, substitute v)
                                    | AST.TSum (name, args) -> AST.TSum (name, List.map substitute args)
                                    | AST.TFunction (args, ret) -> AST.TFunction (List.map substitute args, substitute ret)
                                    | _ -> t
                                substitute payloadTypeTemplate
                            | _ -> payloadTypeTemplate
                            |> canonicalizeBareSumTypeRefs variantLookup

                        let typedPayloadExpr = ANF.TypedAtom (ANF.Var payloadVar, payloadType)
                        extractAndCompileBody innerPattern body (ANF.Var typedPayloadVar) payloadType currentEnv vg2
                        |> Result.map (fun (innerExpr, vg3) ->
                            let expr = ANF.Let (payloadVar, payloadExpr, ANF.Let (typedPayloadVar, typedPayloadExpr, innerExpr))
                            (expr, vg3))
                    | None ->
                        Error $"Constructor '{constructorName}' not found in variant lookup"
            | AST.PTuple patterns ->
                // Collect all bindings from the tuple pattern, then compile body
                collectPatternBindings (AST.PTuple patterns) scrutAtom scrutType currentEnv [] vg
                |> Result.bind (fun (newEnv, bindings, vg1) ->
                    toANFCore sumTypeNames inertScopes body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let finalExpr = wrapBindings (List.rev bindings) bodyExpr
                        (finalExpr, vg2)))
            | AST.PList patterns ->
                // Extract list elements from SkewList structure
                // SkewList layout:
                // SINGLE (tag 1): [node:8] where node is LEAF-tagged
                // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
                // LEAF (tag 5): [value:8]

                // Get element type from list type
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | AST.TVar scrutTypeVar -> AST.TVar $"__list_elem_{scrutTypeVar}"
                    | AST.TRuntimeError -> AST.TVar "__list_elem_runtime_error"
                    | _ -> AST.TVar "__list_elem_unknown"

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), valueType)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements from a value
                // tupleType is the type of the tuple being destructured
                let rec collectTupleBindings (tupPats: AST.Pattern list) (tupleAtom: ANF.Atom) (tupleType: AST.Type) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    let tupleElemTypesResult =
                        match tupleType with
                        | AST.TTuple types when List.length types >= List.length tupPats -> Ok types
                        | AST.TEnumFields types when List.length types >= List.length tupPats -> Ok types
                        | AST.TTuple types ->
                            Error $"Tuple pattern expects {List.length tupPats} elements but got {List.length types}"
                        | AST.TEnumFields types ->
                            Error $"Tuple pattern expects {List.length tupPats} elements but got {List.length types}"
                        | _ ->
                            Error $"Tuple pattern expects tuple elements, got {typeToString tupleType}"
                    match tupleElemTypesResult with
                    | Error err ->
                        Error err
                    | Ok tupleElemTypes ->
                        match tupPats with
                        | [] -> Ok (env, bindings, vg)
                        | tupPat :: tupRest ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (tupleAtom, idx)
                            let elemBinding = (elemVar, elemExpr)
                            let elemT = List.item idx tupleElemTypes
                            match tupPat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (elemVar, elemT) env
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [elemBinding]) vg1
                            | AST.PWildcard ->
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) env bindings vg1
                            | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _ | AST.PUInt128Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PChar _ | AST.PFloat _ | AST.PTuple _
                            | AST.PList _ | AST.PListCons _ | AST.POr _ ->
                                Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                let patternLen = List.length patterns
                if patternLen = 0 then
                    // Empty pattern - no bindings needed
                    toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                elif patternLen = 1 then
                    // SINGLE node: extract the single element
                    // Untag to get pointer to SINGLE structure
                    let (ptrVar, vg1) = ANF.freshVar vg
                    let ptrExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    // Get the complete-tree root from the digit.
                    let (nodeVar, vg2) = ANF.freshVar vg1
                    let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), None)
                    // Leaf and internal tree nodes both store their value at offset 0.
                    let (rawValueAtom, rawValueVar, rawBindings, vg3) = unwrapLeaf (ANF.Var nodeVar) vg2 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                    // Wrap with TypedAtom to preserve element type in TypeMap
                    let (typedValueVar, vg3') = ANF.freshVar vg3
                    let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                    let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                    let valueVar = typedValueVar
                    let valueAtom = ANF.Var typedValueVar
                    // Bind the pattern
                    match List.head patterns with
                    | AST.PVar name ->
                        let newEnv = Map.add name (valueVar, elemType) currentEnv
                        toANFCore sumTypeNames inertScopes body vg3' newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg4) ->
                            (wrapBindings bindings bodyExpr, vg4))
                    | AST.PWildcard ->
                        toANFCore sumTypeNames inertScopes body vg3' currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg4) ->
                            (wrapBindings bindings bodyExpr, vg4))
                    | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                    | AST.PInt8Literal _
                    | AST.PInt16Literal _
                    | AST.PInt32Literal _
                    | AST.PUInt8Literal _
                    | AST.PUInt16Literal _
                    | AST.PUInt32Literal _
                    | AST.PUInt64Literal _ | AST.PUInt128Literal _ ->
                        toANFCore sumTypeNames inertScopes body vg3' currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg4) ->
                            (wrapBindings bindings bodyExpr, vg4))
                    | AST.PTuple innerPatterns ->
                        // elemType is the list element type, use it as tuple type
                        collectTupleBindings innerPatterns valueAtom elemType 0 currentEnv bindings vg3'
                        |> Result.bind (fun (newEnv, newBindings, vg4) ->
                            toANFCore sumTypeNames inertScopes body vg4 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg5) ->
                                (wrapBindings newBindings bodyExpr, vg5)))
                    | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                        Error "Nested pattern in list element not yet supported"
                    | _ ->
                        Error $"Unsupported pattern in single-element list: {List.head patterns}"
                else
                    // Traverse exact-list patterns through the representation API.
                    let listType = AST.TList elemType
                    let rec extractElements (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, bindings, vg)
                        | pat :: rest ->
                            let (rawValueVar, vg1) = ANF.freshVar vg
                            let rawValueExpr = listHeadUnsafeExpr funcReg elemType currentList
                            let (typedValueVar, vg2) = ANF.freshVar vg1
                            let typedValueExpr = ANF.TypedAtom (ANF.Var rawValueVar, elemType)
                            let (rawTailVar, vg3) = ANF.freshVar vg2
                            let rawTailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                            let (typedTailVar, vg4) = ANF.freshVar vg3
                            let typedTailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)
                            let newBindings =
                                bindings
                                @ [
                                    rawValueVar, rawValueExpr
                                    typedValueVar, typedValueExpr
                                    rawTailVar, rawTailExpr
                                    typedTailVar, typedTailExpr
                                  ]
                            let valueVar = typedValueVar
                            let valueAtom = ANF.Var typedValueVar

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (valueVar, elemType) env
                                extractElements rest (ANF.Var typedTailVar) newEnv newBindings vg4
                            | AST.PWildcard ->
                                extractElements rest (ANF.Var typedTailVar) env newBindings vg4
                            | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _ | AST.PUInt128Literal _ ->
                                extractElements rest (ANF.Var typedTailVar) env newBindings vg4
                            | AST.PTuple innerPatterns ->
                                // elemType is the list element type, use it as tuple type
                                collectTupleBindings innerPatterns valueAtom elemType 0 env newBindings vg4
                                |> Result.bind (fun (tupEnv, tupBindings, vg3) ->
                                    extractElements rest (ANF.Var typedTailVar) tupEnv tupBindings vg3)
                            | _ ->
                                Error $"Unsupported pattern in list element: {pat}"

                    extractElements patterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg2) ->
                        toANFCore sumTypeNames inertScopes body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg3) ->
                            (wrapBindings bindings bodyExpr, vg3)))
            | AST.PListCons (headPatterns, tailPattern) ->
                // Get element type from list type
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | _ -> Crash.crash $"PListCons pattern expects TList scrutinee in extractAndCompileBody, got {scrutType}"
                // Extract head elements and bind tail using SkewList operations
                // Lists are SkewLists, use headUnsafe_i64/tail_i64 for extraction
                let rec collectListConsBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen, string> =
                    match pats with
                    | [] -> Ok (env, List.rev bindings, listAtom, vg)
                    | pat :: rest ->
                        // Extract head using SkewList.headUnsafe_i64
                        let (rawHeadVar, vg1) = ANF.freshVar vg
                        let rawHeadExpr = listHeadUnsafeExpr funcReg elemType listAtom
                        let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                        // Wrap with TypedAtom to preserve correct element type in TypeMap
                        let (headVar, vg1') = ANF.freshVar vg1
                        let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                        let headBinding = (headVar, headExpr)
                        // Extract tail using SkewList.tail_i64
                        let (rawTailVar, vg2) = ANF.freshVar vg1'
                        let rawTailExpr = ANF.Call ("Stdlib.List.__tail_i64", [listAtom])
                        let rawTailBinding = (rawTailVar, rawTailExpr)
                        // Wrap with TypedAtom to preserve list type for tail
                        let listType = AST.TList elemType
                        let (tailVar, vg2') = ANF.freshVar vg2
                        let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)
                        let tailBinding = (tailVar, tailExpr)
                        // All bindings including raw extractions
                        // Order: typedBindings first (will be reversed at line 3923), so after reversal raw bindings come before typed
                        let allBaseBindings = tailBinding :: rawTailBinding :: headBinding :: rawHeadBinding :: bindings
                        match pat with
                        | AST.PVar name ->
                            let newEnv = Map.add name (headVar, elemType) env
                            collectListConsBindings rest (ANF.Var tailVar) newEnv allBaseBindings vg2'
                        | AST.PWildcard ->
                            collectListConsBindings rest (ANF.Var tailVar) env allBaseBindings vg2'
                        | AST.PTuple innerPatterns ->
                            // For tuple patterns inside list cons, extract each tuple element and bind variables
                            // elemType is the tuple type (since list elements are tuples)
                            let tupleElemTypes =
                                match elemType with
                                | AST.TTuple types -> types
                                | AST.TVar tupleTypeVar ->
                                    innerPatterns
                                    |> List.mapi (fun idx _ ->
                                        AST.TVar $"__tuple_elem_{tupleTypeVar}_{idx}")
                                | AST.TRuntimeError ->
                                    innerPatterns
                                    |> List.mapi (fun idx _ ->
                                        AST.TVar $"__tuple_elem_runtime_error_{idx}")
                                | _ ->
                                    innerPatterns
                                    |> List.mapi (fun idx _ ->
                                        AST.TVar $"__tuple_elem_unknown_{idx}")
                            let rec collectTupleBindings (tupPats: AST.Pattern list) (types: AST.Type list) (tupleAtom: ANF.Atom) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                                match tupPats with
                                | [] -> Ok (env, bindings, vg)
                                | tupPat :: tupRest ->
                                    // Extract raw element with TupleGet
                                    let (rawElemVar, vg1) = ANF.freshVar vg
                                    let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                                    let rawElemBinding = (rawElemVar, rawElemExpr)
                                    let elemT =
                                        if idx < List.length types then
                                            List.item idx types
                                        else
                                            AST.TVar $"__tuple_elem_missing_{idx}"
                                    // Wrap with TypedAtom to preserve correct element type
                                    let (elemVar, vg1') = ANF.freshVar vg1
                                    let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                                    let elemBinding = (elemVar, elemExpr)
                                    match tupPat with
                                    | AST.PVar name ->
                                        let newEnv = Map.add name (elemVar, elemT) env
                                        collectTupleBindings tupRest types tupleAtom (idx + 1) newEnv (elemBinding :: rawElemBinding :: bindings) vg1'
                                    | AST.PWildcard ->
                                        collectTupleBindings tupRest types tupleAtom (idx + 1) env (rawElemBinding :: bindings) vg1
                                    | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                                    | AST.PInt8Literal _
                                    | AST.PInt16Literal _
                                    | AST.PInt32Literal _
                                    | AST.PUInt8Literal _
                                    | AST.PUInt16Literal _
                                    | AST.PUInt32Literal _
                                    | AST.PUInt64Literal _ | AST.PUInt128Literal _
                                    | AST.PUnit
                                    | AST.PConstructor _
                                    | AST.PBool _
                                    | AST.PString _ | AST.PChar _ | AST.PFloat _ | AST.PTuple _
                                    | AST.PList _ | AST.PListCons _ | AST.POr _ ->
                                        Error $"Nested pattern in tuple element not yet supported: {tupPat}"
                            collectTupleBindings innerPatterns tupleElemTypes (ANF.Var headVar) 0 env allBaseBindings vg2'
                            |> Result.bind (fun (newEnv, newBindings, vg3) ->
                                collectListConsBindings rest (ANF.Var tailVar) newEnv newBindings vg3)
                        | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                        | AST.PInt8Literal _
                        | AST.PInt16Literal _
                        | AST.PInt32Literal _
                        | AST.PUInt8Literal _
                        | AST.PUInt16Literal _
                        | AST.PUInt32Literal _
                        | AST.PUInt64Literal _ | AST.PUInt128Literal _ ->
                            collectListConsBindings rest (ANF.Var tailVar) env allBaseBindings vg2'
                        | AST.PUnit
                        | AST.PConstructor _
                        | AST.PBool _
                        | AST.PString _ | AST.PChar _ | AST.PFloat _
                        | AST.PList _ | AST.PListCons _ | AST.POr _ ->
                            Error $"Nested pattern in list cons element not yet supported: {pat}"
                collectListConsBindings headPatterns scrutAtom currentEnv [] vg
                |> Result.bind (fun (newEnv, bindings, tailAtom, vg1) ->
                    // Bind tail pattern
                    match tailPattern with
                    | AST.PVar name ->
                        let (tailVar, vg2) = ANF.freshVar vg1
                        // Tail has the same list type as the scrutinee
                        let newEnv' = Map.add name (tailVar, scrutType) newEnv
                        toANFCore sumTypeNames inertScopes body vg2 newEnv' typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg3) ->
                            let tailBinding = (tailVar, ANF.TypedAtom (tailAtom, scrutType))
                            let allBindings = bindings @ [tailBinding]
                            let finalExpr = wrapBindings allBindings bodyExpr
                            (finalExpr, vg3))
                    | AST.PWildcard ->
                        toANFCore sumTypeNames inertScopes body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2))
                    | _ -> Error "Tail pattern in list cons must be variable or wildcard")

        // Extract pattern bindings, check guard, and compile body
        // Returns: if guard is true, execute body; otherwise execute elseExpr
        // scrutType is the type of the scrutinee for correct pattern variable typing
        and extractAndCompileBodyWithGuard (pattern: AST.Pattern) (guardExpr: AST.Expr) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) (elseExpr: ANF.AExpr) : Result<ANF.AExpr * ANF.VarGen, string> =
            // First, we need to extract bindings from the pattern
            // Then compile the guard with those bindings in scope
            // Then compile the body with those bindings in scope
            // Finally, generate: let <bindings> in if <guard> then <body> else <elseExpr>

            // Return true only when we can prove a pattern can never match this type.
            // Used to preserve "fall through" semantics for guarded patterns that should not bind.
            let rec patternDefinitelyCannotMatchType (pat: AST.Pattern) (patType: AST.Type) : bool =
                match pat with
                | AST.PTuple innerPatterns ->
                    match patType with
                    | AST.TTuple elemTypes
                    | AST.TEnumFields elemTypes ->
                        List.length innerPatterns <> List.length elemTypes
                        || List.exists2 patternDefinitelyCannotMatchType innerPatterns elemTypes
                    | AST.TVar _ -> false
                    | _ -> true
                | AST.PList innerPatterns ->
                    match patType with
                    | AST.TList elemType ->
                        innerPatterns
                        |> List.exists (fun innerPat ->
                            patternDefinitelyCannotMatchType innerPat elemType)
                    | AST.TVar _ -> false
                    | _ -> true
                | AST.PListCons (headPatterns, tailPattern) ->
                    match patType with
                    | AST.TList elemType ->
                        (headPatterns
                         |> List.exists (fun headPat ->
                             patternDefinitelyCannotMatchType headPat elemType))
                        || patternDefinitelyCannotMatchType tailPattern patType
                    | AST.TVar _ -> false
                    | _ -> true
                | AST.POr alternatives ->
                    alternatives
                    |> AST.NonEmptyList.toList
                    |> List.forall (fun alternative ->
                        patternDefinitelyCannotMatchType alternative patType)
                | _ -> false

            // Helper to collect pattern variable bindings (simplified version for common patterns)
            // sourceType is the type of the source being matched
            let rec collectBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match pat with
                | AST.POr alternatives ->
                    collectBindings (AST.NonEmptyList.head alternatives) sourceAtom sourceType env bindings vg
                | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                | AST.PInt8Literal _
                | AST.PInt16Literal _
                | AST.PInt32Literal _
                | AST.PUInt8Literal _
                | AST.PUInt16Literal _
                | AST.PUInt32Literal _
                | AST.PUInt64Literal _ | AST.PUInt128Literal _
                | AST.PUnit
                | AST.PWildcard
                | AST.PBool _
                | AST.PString _
                | AST.PChar _
                | AST.PFloat _ ->
                    Ok (env, bindings, vg)
                | AST.PVar name ->
                    let (tempId, vg1) = ANF.freshVar vg
                    // Use TypedAtom to preserve the correct type in TypeMap
                    let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                    let newEnv = Map.add name (tempId, sourceType) env
                    Ok (newEnv, binding :: bindings, vg1)
                | AST.PTuple innerPatterns ->
                    let elemTypes =
                        match sourceType with
                        | AST.TTuple types when List.length types = List.length innerPatterns -> types
                        | AST.TEnumFields types when List.length types = List.length innerPatterns -> types
                        | AST.TTuple types ->
                            Crash.crash
                                $"collectBindings(PTuple): expected {List.length innerPatterns} tuple elements, got {List.length types}"
                        | AST.TEnumFields types ->
                            Crash.crash
                                $"collectBindings(PTuple): expected {List.length innerPatterns} tuple elements, got {List.length types}"
                        | _ ->
                            Crash.crash
                                $"collectBindings(PTuple): expected tuple source type, got {typeToString sourceType}"
                    let rec collectFromTuple pats types idx env bindings vg =
                        match pats, types with
                        | [], _ -> Ok (env, bindings, vg)
                        | p :: rest, t :: restTypes ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (sourceAtom, idx)
                            collectBindings p (ANF.Var elemVar) t env ((elemVar, elemExpr) :: bindings) vg1
                            |> Result.bind (fun (env', bindings', vg') ->
                                collectFromTuple rest restTypes (idx + 1) env' bindings' vg')
                        | p :: rest, [] ->
                            let remaining = List.length (p :: rest)
                            Crash.crash
                                $"collectBindings(PTuple): missing tuple element type at index {idx}; {remaining} pattern elements remain"
                    collectFromTuple innerPatterns elemTypes 0 env bindings vg
                | AST.PConstructor (constructorName, payloadPattern) ->
                    let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
                        match typ with
                        | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
                        | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
                        | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
                        | AST.TList elem -> AST.TList (substituteType subst elem)
                        | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
                        | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
                        | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
                        | _ -> typ

                    let resolvePayloadType (constructorName: string) (scrutineeType: AST.Type) : Result<AST.Type option, string> =
                        match tryFindVariantForType constructorName scrutineeType variantLookup with
                        | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                            let payloadType =
                                match scrutineeType with
                                | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                    let subst = List.zip typeParams typeArgs |> Map.ofList
                                    substituteType subst payloadTypeTemplate
                                | _ -> payloadTypeTemplate
                                |> canonicalizeBareSumTypeRefs variantLookup
                                |> function
                                    | AST.TEnumFields fieldTypes -> AST.TTuple fieldTypes
                                    | other -> other

                            Ok (Some payloadType)
                        | Some (_, _, _, None) ->
                            Ok None
                        | None ->
                            Error $"Unknown constructor '{constructorName}' in pattern"

                    match payloadPattern with
                    | None -> Ok (env, bindings, vg)
                    | Some innerPat ->
                        resolvePayloadType constructorName sourceType
                        |> Result.bind (fun payloadType ->
                            match payloadType with
                            | None ->
                                // Constructor arity mismatch should not bind payload.
                                Ok (env, bindings, vg)
                            | Some concretePayloadType ->
                                let (payloadVar, vg1) = ANF.freshVar vg
                                let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                                collectBindings
                                    innerPat
                                    (ANF.Var payloadVar)
                                    concretePayloadType
                                    env
                                    ((payloadVar, payloadExpr) :: bindings)
                                    vg1)
                | AST.PList innerPatterns ->
                    let elemTypeResult =
                        match sourceType with
                        | AST.TList t -> Ok t
                        | AST.TVar _
                        | AST.TRuntimeError -> Ok (AST.TVar "__list_elem_unknown")
                        | _ ->
                            Error
                                $"collectBindings(PList): expected list-compatible source type, got {typeToString sourceType}"
                    elemTypeResult
                    |> Result.bind (fun elemType ->
                        // For list patterns, extract head elements using SkewList operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectFromList
                            (pats: AST.Pattern list)
                            (currentList: ANF.Atom)
                            (env: VarEnv)
                            (bindings: (ANF.TempId * ANF.CExpr) list)
                            (vg: ANF.VarGen)
                            =
                            match pats with
                            | [] -> Ok (env, bindings, vg)
                            | p :: rest ->
                                // Lists are SkewLists - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = listHeadUnsafeExpr funcReg elemType currentList
                                let headBinding = (headVar, headExpr)
                                collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    if List.isEmpty rest then
                                        Ok (env', bindings', vg')
                                    else
                                        // Get tail for next iteration
                                        let (tailVar, vg2) = ANF.freshVar vg'
                                        let tailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                                        let tailBinding = (tailVar, tailExpr)
                                        collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                        collectFromList innerPatterns sourceAtom env bindings vg)
                | AST.PListCons (headPatterns, tailPattern) ->
                    let elemTypeResult =
                        match sourceType with
                        | AST.TList t -> Ok t
                        | AST.TVar _
                        | AST.TRuntimeError -> Ok (AST.TVar "__list_elem_unknown")
                        | _ ->
                            Error
                                $"collectBindings(PListCons): expected list-compatible source type, got {typeToString sourceType}"
                    elemTypeResult
                    |> Result.bind (fun elemType ->
                        // Extract head elements then bind tail using SkewList operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectHeads
                            (pats: AST.Pattern list)
                            (currentList: ANF.Atom)
                            (env: VarEnv)
                            (bindings: (ANF.TempId * ANF.CExpr) list)
                            (vg: ANF.VarGen)
                            =
                            match pats with
                            | [] ->
                                // Bind the remaining list to tail pattern (tail has same type as source)
                                collectBindings tailPattern currentList sourceType env bindings vg
                            | p :: rest ->
                                // Lists are SkewLists - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = listHeadUnsafeExpr funcReg elemType currentList
                                let headBinding = (headVar, headExpr)
                                collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    let (tailVar, vg2) = ANF.freshVar vg'
                                    let tailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                                    let tailBinding = (tailVar, tailExpr)
                                    collectHeads rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                        collectHeads headPatterns sourceAtom env bindings vg)

            if patternDefinitelyCannotMatchType pattern scrutType then
                Ok (elseExpr, vg)
            else
                collectBindings pattern scrutAtom scrutType currentEnv [] vg
                |> Result.bind (fun (newEnv, bindings, vg1) ->
                    // Compile guard expression in the extended environment
                    toAtomCore sumTypeNames inertScopes guardExpr vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (guardAtom, guardBindings, vg2) ->
                        // Compile body expression in the extended environment
                        toANFCore sumTypeNames inertScopes body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg3) ->
                            // Build: if guard then body else elseExpr
                            let ifExpr = ANF.If (guardAtom, bodyExpr, elseExpr)
                            // Wrap guard bindings
                            let withGuardBindings = wrapBindings guardBindings ifExpr
                            // Wrap pattern bindings (in reverse order since we accumulated in reverse)
                            let finalExpr = wrapBindings (List.rev bindings) withGuardBindings
                            (finalExpr, vg3))))

        // Build comparison expression for a pattern
        // A constructor pattern's variant is looked up in the type of the value it
        // tests, which for a nested pattern is the enclosing variant's payload, not
        // the match scrutinee. Looked up by bare name, `| Some(String "2.0")` on an
        // Option<Json> found whatever type registered a `String` variant last, with
        // that type's tag, and then compared the payload as a string: wrong arm on
        // a Number, SIGSEGV on a Float payload.
        let rec substituteTypeParams (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
            match typ with
            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
            | AST.TTuple elems -> AST.TTuple (List.map (substituteTypeParams subst) elems)
            | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteTypeParams subst) args)
            | AST.TList elem -> AST.TList (substituteTypeParams subst elem)
            | AST.TDict (k, v) -> AST.TDict (substituteTypeParams subst k, substituteTypeParams subst v)
            | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteTypeParams subst) args)
            | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteTypeParams subst) args, substituteTypeParams subst ret)
            | _ -> typ

        /// `patType` is the static type of the value `scrutAtom` holds, when known;
        /// None falls back to the match scrutinee's type.
        let rec buildPatternComparison (pattern: AST.Pattern) (scrutAtom: ANF.Atom) (patType: AST.Type option) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
            let testedType = defaultArg patType scrutType
            let variantHere variantName = tryFindVariantForType variantName testedType variantLookup
            let typeHasAnyPayloadHere (variantName: string) : bool =
                match variantHere variantName with
                | Some (typeName, _, _, _) ->
                    variantLookup
                    |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)
                | None -> false
            match pattern with
            | AST.POr alternatives ->
                buildPatternComparison (AST.NonEmptyList.head alternatives) scrutAtom patType vg
            | AST.PUnit -> Ok None  // Unit pattern always matches unit type
            | AST.PWildcard -> Ok None
            | AST.PVar _ -> Ok None
            | AST.PInt64 n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PBigInt n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Call ("Stdlib.Int.__equals", [scrutAtom; ANF.StringLiteral (n.ToString())])
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PInt128Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = int128LiteralComparison scrutAtom n
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PInt8Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int8 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PInt16Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int16 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PInt32Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int32 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PUInt8Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt8 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PUInt16Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt16 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PUInt32Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt32 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PUInt64Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt64 n))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PUInt128Literal n ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = uint128LiteralComparison scrutAtom n
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PBool b ->
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.BoolLiteral b)
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PString s ->
                // String patterns must use byte-wise equality, not pointer equality.
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.CanonicalBufferEq (MemoryModel.Utf8String, scrutAtom, ANF.StringLiteral (s.Normalize(System.Text.NormalizationForm.FormC)))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PChar c ->
                // Char values are represented as single-EGC strings at runtime.
                let (cmpVar, vg1) = ANF.freshVar vg
                let cmpExpr = ANF.CanonicalBufferEq (MemoryModel.GraphemeCluster, scrutAtom, ANF.StringLiteral (c.Normalize(System.Text.NormalizationForm.FormC)))
                Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PFloat f ->
                if f = 0.0 then
                    // Distinguish -0.0 from 0.0 using reciprocal sign.
                    let patternBits = System.BitConverter.DoubleToInt64Bits(f)
                    let reciprocalTarget =
                        if patternBits < 0L then
                            System.Double.NegativeInfinity
                        else
                            System.Double.PositiveInfinity
                    let (zeroCmpVar, vg1) = ANF.freshVar vg
                    let zeroCmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.FloatLiteral 0.0)
                    let (reciprocalVar, vg2) = ANF.freshVar vg1
                    let reciprocalExpr = ANF.Prim (ANF.Div, ANF.FloatLiteral 1.0, scrutAtom)
                    let (reciprocalCmpVar, vg3) = ANF.freshVar vg2
                    let reciprocalCmpExpr =
                        ANF.Prim (ANF.Eq, ANF.Var reciprocalVar, ANF.FloatLiteral reciprocalTarget)
                    let (andVar, vg4) = ANF.freshVar vg3
                    let andExpr = ANF.Prim (ANF.And, ANF.Var zeroCmpVar, ANF.Var reciprocalCmpVar)
                    let bindings =
                        [ (zeroCmpVar, zeroCmpExpr)
                          (reciprocalVar, reciprocalExpr)
                          (reciprocalCmpVar, reciprocalCmpExpr)
                          (andVar, andExpr) ]
                    Ok (Some (ANF.Var andVar, bindings, vg4))
                else
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.FloatLiteral f)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
            | AST.PConstructor (variantName, payloadPattern) ->
                match variantHere variantName with
                | Some (_, typeParams, tag, variantPayloadType) ->
                    let arityMismatch =
                        match payloadPattern, variantPayloadType with
                        | None, None -> false
                        | Some _, Some _ -> false
                        | _ -> true
                    // The payload's type, with the tested type's arguments substituted.
                    let payloadType =
                        match variantPayloadType, testedType with
                        | Some template, AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                            Some (substituteTypeParams (List.zip typeParams typeArgs |> Map.ofList) template)
                        | Some template, _ -> Some template
                        | None, _ -> None

                    if arityMismatch then
                        // Constructor arity mismatch in pattern should not match.
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Atom (ANF.BoolLiteral false)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    elif typeHasAnyPayloadHere variantName then
                        // Mixed or payload-carrying sum type: tag is stored in heap at index 0.
                        let (tagVar, vg1) = ANF.freshVar vg
                        let tagLoadExpr = ANF.TupleGet (scrutAtom, 0)
                        let (tagCmpVar, vg2) = ANF.freshVar vg1
                        let tagCmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 tag)))

                        match payloadPattern, variantPayloadType with
                        | Some innerPattern, Some _ ->
                            // Extract payload and check inner pattern if needed.
                            let (payloadVar, vg3) = ANF.freshVar vg2
                            let payloadLoadExpr = ANF.TupleGet (scrutAtom, 1)
                            buildPatternComparison innerPattern (ANF.Var payloadVar) payloadType vg3
                            |> Result.map (fun innerResult ->
                                match innerResult with
                                | None ->
                                    // Inner pattern is variable/wildcard, only need tag check.
                                    Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg3)
                                | Some (innerCond, innerBindings, vg4) ->
                                    let (andVar, vg5) = ANF.freshVar vg4
                                    let andExpr = ANF.Prim (ANF.And, ANF.Var tagCmpVar, innerCond)
                                    let allBindings =
                                        [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr); (payloadVar, payloadLoadExpr)]
                                        @ innerBindings
                                        @ [(andVar, andExpr)]
                                    Some (ANF.Var andVar, allBindings, vg5))
                        | None, None ->
                            // Nullary variant in a payload-mixed sum type: only check tag.
                            Ok (Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg2))
                        | _ ->
                            // Already handled by arityMismatch guard above.
                            Error "Internal error: inconsistent constructor arity handling"
                    else
                        // Simple enum (no payload variants in the type): scrutinee IS the tag.
                        match payloadPattern with
                        | Some _ ->
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Atom (ANF.BoolLiteral false)
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                        | None ->
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 (int64 tag)))
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | None -> Error $"Unknown constructor in pattern: {variantName}"
            | AST.PTuple innerPatterns ->
                // Tuple patterns with literals need to compare each element
                let rec buildTupleComparisons (patterns: AST.Pattern list) (index: int) (vg: ANF.VarGen) (accBindings: (ANF.TempId * ANF.CExpr) list) (accConditions: ANF.Atom list) =
                    match patterns with
                    | [] ->
                        if List.isEmpty accConditions then
                            Ok None  // All variables/wildcards, no comparison needed
                        else
                            // AND together all conditions
                            let rec andAll (conds: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                                match conds with
                                | [] -> Error "Empty conditions list"
                                | [single] -> Ok (single, bindings, vg)
                                | first :: rest ->
                                    andAll rest vg bindings
                                    |> Result.map (fun (restResult, restBindings, vg1) ->
                                        let (andVar, vg2) = ANF.freshVar vg1
                                        let andExpr = ANF.Prim (ANF.And, first, restResult)
                                        (ANF.Var andVar, restBindings @ [(andVar, andExpr)], vg2))
                            andAll accConditions vg accBindings
                            |> Result.map (fun (result, bindings, vg') -> Some (result, bindings, vg'))
                    | p :: rest ->
                        // Extract element at index
                        let (elemVar, vg1) = ANF.freshVar vg
                        let elemLoad = ANF.TupleGet (scrutAtom, index)
                        let newBindings = accBindings @ [(elemVar, elemLoad)]
                        let elemPatType =
                            match testedType with
                            | AST.TTuple elemTypes -> List.tryItem index elemTypes
                            | _ -> None
                        // Check if this pattern needs comparison
                        buildPatternComparison p (ANF.Var elemVar) elemPatType vg1
                        |> Result.bind (fun compResult ->
                            match compResult with
                            | None ->
                                // This element pattern doesn't need comparison (var/wildcard)
                                buildTupleComparisons rest (index + 1) vg1 newBindings accConditions
                            | Some (cond, condBindings, vg2) ->
                                // Add this comparison
                                buildTupleComparisons rest (index + 1) vg2 (newBindings @ condBindings) (accConditions @ [cond]))
                buildTupleComparisons innerPatterns 0 vg [] []
            | AST.PList patterns ->
                // Exact list patterns compare the cached skew-list length.
                let patternLen = List.length patterns
                if patternLen = 0 then
                    // Empty list pattern: check scrutinee == 0
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 0L))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                elif patternLen = 1 then
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthExpr = ANF.Call ("Stdlib.List.__length_i64", [scrutAtom])
                    let (cmpVar, vg2) = ANF.freshVar vg1
                    let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 1L))
                    Ok (Some (ANF.Var cmpVar, [(lengthVar, lengthExpr); (cmpVar, cmpExpr)], vg2))
                else
                    // Multiple elements: check length == patternLen
                    // Use Stdlib.List.__length which handles EMPTY/SINGLE/DEEP safely
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthExpr = ANF.Call ("Stdlib.List.__length_i64", [scrutAtom])
                    let (cmpVar, vg2) = ANF.freshVar vg1
                    let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                    Ok (Some (ANF.Var cmpVar, [(lengthVar, lengthExpr); (cmpVar, cmpExpr)], vg2))
            | AST.PListCons (headPatterns, _) ->
                // A cons pattern needs one element per normalized head before binding its tail;
                // a :: b :: t needs at least two, etc.
                let minLength = List.length headPatterns
                if minLength = 0 then
                    Ok None
                else
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthExpr = ANF.Call ("Stdlib.List.__length_i64", [scrutAtom])
                    let (cmpVar, vg2) = ANF.freshVar vg1
                    let cmpExpr = ANF.Prim (ANF.Gte, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 minLength)))
                    Ok (Some (ANF.Var cmpVar, [(lengthVar, lengthExpr); (cmpVar, cmpExpr)], vg2))

        let rec patternBindsVariables (pattern: AST.Pattern) : bool =
            match pattern with
            | AST.PVar _ -> true
            | AST.PTuple patterns ->
                patterns |> List.exists patternBindsVariables
            | AST.PConstructor (_, payloadPattern) ->
                payloadPattern |> Option.exists patternBindsVariables
            | AST.PList patterns ->
                patterns |> List.exists patternBindsVariables
            | AST.PListCons (headPatterns, tailPattern) ->
                (headPatterns |> List.exists patternBindsVariables) || patternBindsVariables tailPattern
            | _ -> false

        let rec substituteTypeForStaticPatternCheck (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
            match typ with
            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
            | AST.TTuple elems -> AST.TTuple (List.map (substituteTypeForStaticPatternCheck subst) elems)
            | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteTypeForStaticPatternCheck subst) args)
            | AST.TList elem -> AST.TList (substituteTypeForStaticPatternCheck subst elem)
            | AST.TDict (k, v) ->
                AST.TDict (substituteTypeForStaticPatternCheck subst k, substituteTypeForStaticPatternCheck subst v)
            | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteTypeForStaticPatternCheck subst) args)
            | AST.TFunction (args, ret) ->
                AST.TFunction (
                    List.map (substituteTypeForStaticPatternCheck subst) args,
                    substituteTypeForStaticPatternCheck subst ret
                )
            | _ -> typ

        let resolvePayloadTypeForStaticPatternCheck
            (constructorName: string)
            (scrutineeType: AST.Type)
            : AST.Type option option =
            match tryFindVariantForType constructorName scrutineeType variantLookup with
            | None -> None
            | Some (sumTypeName, typeParams, _, payloadTypeTemplateOpt) ->
                let payloadTypeOpt =
                    match payloadTypeTemplateOpt with
                    | None -> None
                    | Some payloadTypeTemplate ->
                        let payloadType =
                            match scrutineeType with
                            | AST.TSum (scrutineeSumTypeName, typeArgs)
                                when scrutineeSumTypeName = sumTypeName
                                     && List.length typeParams = List.length typeArgs ->
                                let subst = List.zip typeParams typeArgs |> Map.ofList
                                substituteTypeForStaticPatternCheck subst payloadTypeTemplate
                            | _ ->
                                payloadTypeTemplate
                        Some payloadType
                Some payloadTypeOpt

        let rec patternStaticallyCannotMatchType (pattern: AST.Pattern) (sourceType: AST.Type) : bool =
            match pattern with
            | AST.PTuple innerPatterns ->
                match sourceType with
                | AST.TTuple elemTypes
                | AST.TEnumFields elemTypes ->
                    if List.length elemTypes <> List.length innerPatterns then
                        true
                    else
                        List.zip innerPatterns elemTypes
                        |> List.exists (fun (innerPattern, elemType) ->
                            patternStaticallyCannotMatchType innerPattern elemType)
                | AST.TVar _
                | AST.TRuntimeError -> false
                | _ -> true
            | AST.PConstructor (constructorName, payloadPatternOpt) ->
                match sourceType with
                | AST.TVar _
                | AST.TRuntimeError -> false
                | AST.TSum (_, typeArgs)
                | AST.TRecord (_, typeArgs) ->
                    match tryFindVariantForType constructorName sourceType variantLookup with
                    | None -> true
                    | Some (_, typeParams, _, payloadTypeOpt) ->
                        match payloadPatternOpt, payloadTypeOpt with
                        | None, None -> false
                        | Some innerPattern, Some payloadType ->
                            let subst =
                                if List.length typeParams = List.length typeArgs then
                                    List.zip typeParams typeArgs |> Map.ofList
                                else
                                    Map.empty
                            let concretePayloadType =
                                payloadType
                                |> applySubstToType subst
                                |> canonicalizeBareSumTypeRefs variantLookup
                                |> function
                                    | AST.TEnumFields fieldTypes -> AST.TTuple fieldTypes
                                    | other -> other
                            patternStaticallyCannotMatchType innerPattern concretePayloadType
                        | _ -> true
                | _ -> true
            | AST.PList innerPatterns ->
                match sourceType with
                | AST.TList elemType ->
                    innerPatterns
                    |> List.exists (fun innerPattern ->
                        patternStaticallyCannotMatchType innerPattern elemType)
                | AST.TVar _
                | AST.TRuntimeError -> false
                | _ -> true
            | AST.PListCons (headPatterns, tailPattern) ->
                match sourceType with
                | AST.TList elemType ->
                    (headPatterns
                     |> List.exists (fun headPattern ->
                        patternStaticallyCannotMatchType headPattern elemType))
                    || patternStaticallyCannotMatchType tailPattern sourceType
                | AST.TVar _
                | AST.TRuntimeError -> false
                | _ -> true
            | _ ->
                false

        let makeFalsePatternCondition
            (vg: ANF.VarGen)
            : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
            let (cmpVar, vg1) = ANF.freshVar vg
            let cmpExpr = ANF.Atom (ANF.BoolLiteral false)
            (ANF.Var cmpVar, [ (cmpVar, cmpExpr) ], vg1)

        // Collect variable bindings for nested patterns under a value that is already known to match.
        // This is used by list/list-cons lowering where structural checks are emitted separately.
        let rec collectNestedPatternBindings
            (pattern: AST.Pattern)
            (sourceAtom: ANF.Atom)
            (sourceType: AST.Type)
            (env: VarEnv)
            (bindings: (ANF.TempId * ANF.CExpr) list)
            (vg: ANF.VarGen)
            : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match pattern with
            | AST.POr alternatives ->
                collectNestedPatternBindings
                    (AST.NonEmptyList.head alternatives)
                    sourceAtom
                    sourceType
                    env
                    bindings
                    vg
            | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
            | AST.PInt8Literal _
            | AST.PInt16Literal _
            | AST.PInt32Literal _
            | AST.PUInt8Literal _
            | AST.PUInt16Literal _
            | AST.PUInt32Literal _
            | AST.PUInt64Literal _ | AST.PUInt128Literal _
            | AST.PUnit
            | AST.PWildcard
            | AST.PBool _
            | AST.PString _
            | AST.PChar _
            | AST.PFloat _ ->
                Ok (env, bindings, vg)
            | AST.PVar name ->
                let (tempId, vg1) = ANF.freshVar vg
                let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                Ok (Map.add name (tempId, sourceType) env, bindings @ [binding], vg1)
            | AST.PTuple patterns ->
                let tupleElemTypesOpt =
                    match sourceType with
                    | AST.TTuple types when List.length types = List.length patterns ->
                        Some types
                    | AST.TEnumFields types when List.length types = List.length patterns ->
                        Some types
                    | AST.TVar sourceTypeVar ->
                        Some (
                            patterns
                            |> List.mapi (fun idx _ ->
                                AST.TVar $"__tuple_elem_{sourceTypeVar}_{idx}")
                        )
                    | AST.TRuntimeError ->
                        Some (
                            patterns
                            |> List.mapi (fun idx _ ->
                                AST.TVar $"__tuple_elem_runtime_error_{idx}")
                        )
                    | _ ->
                        None

                match tupleElemTypesOpt with
                | None ->
                    Ok (env, bindings, vg)
                | Some elemTypes ->
                    let rec loop
                        (remaining: (AST.Pattern * AST.Type) list)
                        (idx: int)
                        (currentEnv: VarEnv)
                        (currentBindings: (ANF.TempId * ANF.CExpr) list)
                        (currentVg: ANF.VarGen)
                        : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match remaining with
                        | [] ->
                            Ok (currentEnv, currentBindings, currentVg)
                        | (pat, elemType) :: rest ->
                            let (elemVar, vg1) = ANF.freshVar currentVg
                            let elemExpr = ANF.TupleGet (sourceAtom, idx)
                            collectNestedPatternBindings pat (ANF.Var elemVar) elemType currentEnv (currentBindings @ [ (elemVar, elemExpr) ]) vg1
                            |> Result.bind (fun (env', bindings', vg') ->
                                loop rest (idx + 1) env' bindings' vg')

                    loop (List.zip patterns elemTypes) 0 env bindings vg
            | AST.PConstructor (constructorName, payloadPattern) ->
                let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
                    match typ with
                    | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
                    | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
                    | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
                    | AST.TList elem -> AST.TList (substituteType subst elem)
                    | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
                    | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
                    | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
                    | _ -> typ
                let resolvePayloadType (constructorName: string) (scrutineeType: AST.Type) : Result<AST.Type option, string> =
                    match tryFindVariantForType constructorName scrutineeType variantLookup with
                    | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                        let payloadType =
                            match scrutineeType with
                            | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                let subst = List.zip typeParams typeArgs |> Map.ofList
                                substituteType subst payloadTypeTemplate
                            | _ -> payloadTypeTemplate
                            |> canonicalizeBareSumTypeRefs variantLookup

                        Ok (Some payloadType)
                    | Some (_, _, _, None) ->
                        Ok None
                    | None ->
                        Error $"Unknown constructor '{constructorName}' in pattern"
                match payloadPattern with
                | None -> Ok (env, bindings, vg)
                | Some innerPattern ->
                    resolvePayloadType constructorName sourceType
                    |> Result.bind (fun payloadType ->
                        match payloadType with
                        | None ->
                            // Constructor arity mismatch behaves as a non-match and
                            // contributes no payload bindings.
                            Ok (env, bindings, vg)
                        | Some concretePayloadType ->
                            let (payloadVar, vg1) = ANF.freshVar vg
                            let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                            collectNestedPatternBindings
                                innerPattern
                                (ANF.Var payloadVar)
                                concretePayloadType
                                env
                                (bindings @ [ (payloadVar, payloadExpr) ])
                                vg1)
            | AST.PList patterns ->
                let elemTypeResult =
                    match sourceType with
                    | AST.TList t -> Ok t
                    | AST.TVar sourceTypeVar ->
                        Ok (AST.TVar $"__list_elem_{sourceTypeVar}")
                    | AST.TRuntimeError ->
                        Ok (AST.TVar "__list_elem_runtime_error")
                    | _ ->
                        Error $"PList nested binding expects list source type, got {typeToString sourceType}"
                elemTypeResult
                |> Result.bind (fun elemType ->
                    let rec loop
                        (remaining: AST.Pattern list)
                        (currentList: ANF.Atom)
                        (currentEnv: VarEnv)
                        (currentBindings: (ANF.TempId * ANF.CExpr) list)
                        (currentVg: ANF.VarGen)
                        : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match remaining with
                        | [] -> Ok (currentEnv, currentBindings, currentVg)
                        | pat :: rest ->
                            let (headVar, vg1) = ANF.freshVar currentVg
                            let headExpr = listHeadUnsafeExpr funcReg elemType currentList
                            collectNestedPatternBindings pat (ANF.Var headVar) elemType currentEnv (currentBindings @ [(headVar, headExpr)]) vg1
                            |> Result.bind (fun (env', bindings', vg') ->
                                if List.isEmpty rest then
                                    Ok (env', bindings', vg')
                                else
                                    let (tailVar, vg2) = ANF.freshVar vg'
                                    let tailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                                    loop rest (ANF.Var tailVar) env' (bindings' @ [(tailVar, tailExpr)]) vg2)
                    loop patterns sourceAtom env bindings vg)
            | AST.PListCons (headPatterns, tailPattern) ->
                let elemTypeResult =
                    match sourceType with
                    | AST.TList t -> Ok t
                    | AST.TVar sourceTypeVar ->
                        Ok (AST.TVar $"__list_elem_{sourceTypeVar}")
                    | AST.TRuntimeError ->
                        Ok (AST.TVar "__list_elem_runtime_error")
                    | _ ->
                        Error $"PListCons nested binding expects list source type, got {typeToString sourceType}"
                elemTypeResult
                |> Result.bind (fun elemType ->
                    let rec collectHeads
                        (remaining: AST.Pattern list)
                        (currentList: ANF.Atom)
                        (currentEnv: VarEnv)
                        (currentBindings: (ANF.TempId * ANF.CExpr) list)
                        (currentVg: ANF.VarGen)
                        : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen, string> =
                        match remaining with
                        | [] -> Ok (currentEnv, currentBindings, currentList, currentVg)
                        | pat :: rest ->
                            let (headVar, vg1) = ANF.freshVar currentVg
                            let headExpr = listHeadUnsafeExpr funcReg elemType currentList
                            let (tailVar, vg2) = ANF.freshVar vg1
                            let tailExpr = ANF.Call ("Stdlib.List.__tail_i64", [currentList])
                            collectNestedPatternBindings pat (ANF.Var headVar) elemType currentEnv (currentBindings @ [(headVar, headExpr); (tailVar, tailExpr)]) vg2
                            |> Result.bind (fun (env', bindings', vg') ->
                                collectHeads rest (ANF.Var tailVar) env' bindings' vg')
                    collectHeads headPatterns sourceAtom env bindings vg
                    |> Result.bind (fun (envAfterHeads, bindingsAfterHeads, tailAtom, vg1) ->
                        collectNestedPatternBindings tailPattern tailAtom sourceType envAfterHeads bindingsAfterHeads vg1))

        // Compile a list pattern for SkewList with proper length validation.
        // SkewList layout:
        // SINGLE (tag 1): [node:8] where node is LEAF-tagged
        // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
        // LEAF (tag 5): [value:8]
        // listType is the list type (TList elemType) for correct pattern variable typing
        let compileListPatternWithChecks
            (patterns: AST.Pattern list)
            (listAtom: ANF.Atom)
            (listType: AST.Type)
            (currentEnv: VarEnv)
            (body: AST.Expr)
            (elseExpr: ANF.AExpr)
            (vg: ANF.VarGen)
            : Result<ANF.AExpr * ANF.VarGen, string> =

            // List patterns on non-list scrutinees are definite non-matches.
            // This can happen after grouped-pattern desugaring where non-first alternatives
            // were not type-checked against the scrutinee shape.
            match listType with
            | AST.TList elemType ->
                let patternLen = List.length patterns

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), valueType)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements from a value
                // tupleType is the type of the tuple being matched (TTuple elemTypes)
                let rec extractTupleBindings
                    (tupPats: AST.Pattern list)
                    (tupleAtom: ANF.Atom)
                    (tupleType: AST.Type)
                    (idx: int)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    let tupleElemTypesResult =
                        match tupleType with
                        | AST.TTuple types when List.length types >= List.length tupPats -> Ok types
                        | AST.TEnumFields types when List.length types >= List.length tupPats -> Ok types
                        | AST.TTuple types ->
                            Error $"Tuple pattern expects {List.length tupPats} elements but got {List.length types}"
                        | AST.TEnumFields types ->
                            Error $"Tuple pattern expects {List.length tupPats} elements but got {List.length types}"
                        | _ ->
                            Error $"Tuple pattern expects tuple elements, got {typeToString tupleType}"
                    match tupleElemTypesResult with
                    | Error err -> Error err
                    | Ok tupleElemTypes ->
                        match tupPats with
                        | [] -> Ok (env, bindings, vg)
                        | tupPat :: tupRest ->
                            let (rawElemVar, vg1) = ANF.freshVar vg
                            let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                            let rawElemBinding = (rawElemVar, rawElemExpr)
                            let elemT = List.item idx tupleElemTypes
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (elemVar, vg1') = ANF.freshVar vg1
                            let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                            let elemBinding = (elemVar, elemExpr)
                            match tupPat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (elemVar, elemT) env  // Use correct element type
                                extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                            | AST.PWildcard ->
                                extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                            | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _ | AST.PUInt128Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PChar _ | AST.PFloat _ | AST.PTuple _
                            | AST.PList _ | AST.PListCons _ | AST.POr _ ->
                                Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                if patternLen = 0 then
                    // Empty list: check scrutinee == 0 (EMPTY)
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L))
                    toANFCore sumTypeNames inertScopes body vg1 currentEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let ifExpr = ANF.If (ANF.Var checkVar, bodyExpr, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg2))
                elif patternLen = 1 then
                    // A singleton has one digit whose tree pointer is at offset 16.
                    let (tagVar, vg1) = ANF.freshVar vg
                    let tagExpr = ANF.Call ("Stdlib.List.__length_i64", [listAtom])
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))

                    // Untag to get pointer to SINGLE structure
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    // Get the complete-tree root from the digit.
                    let (nodeVar, vg4) = ANF.freshVar vg3
                    let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), None)
                    // Leaf and internal tree nodes both store their value at offset 0.
                    let (rawValueAtom, rawValueVar, rawBindings, vg5) = unwrapLeaf (ANF.Var nodeVar) vg4 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                    // Wrap with TypedAtom to preserve element type in TypeMap
                    let (typedValueVar, vg5') = ANF.freshVar vg5
                    let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                    let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                    let valueVar = typedValueVar
                    let valueAtom = ANF.Var typedValueVar

                    // Bind the pattern
                    let pat = List.head patterns
                    let compileLiteralPattern (literal: ANF.SizedInt) =
                        // Literal pattern: check tag==SINGLE, extract value, check value==literal
                        // Important: bindings must come BEFORE the literal check since they define valueVar
                        let (litCheckVar, vg6) = ANF.freshVar vg5'
                        let litCheckExpr = ANF.Prim (ANF.Eq, valueAtom, ANF.IntLiteral literal)
                        toANFCore sumTypeNames inertScopes body vg6 currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg7) ->
                            // Structure: check tag -> extract value (bindings) -> check literal -> if match then body else else
                            // Note: We use two nested Ifs because the tag check guards the memory access in bindings
                            let ifLitExpr = ANF.If (ANF.Var litCheckVar, bodyExpr, elseExpr)
                            let withLitBinding = ANF.Let (litCheckVar, litCheckExpr, ifLitExpr)
                            // bindings must be OUTSIDE the inner If to define valueVar before litCheckExpr uses it
                            let withBindings = wrapBindings bindings withLitBinding
                            let withTagCheck = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, withTagCheck)), vg7))

                    let compileWideLiteralPattern (litCheckExpr: ANF.CExpr) =
                        let (litCheckVar, vg6) = ANF.freshVar vg5'
                        toANFCore sumTypeNames inertScopes body vg6 currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg7) ->
                            let ifLitExpr = ANF.If (ANF.Var litCheckVar, bodyExpr, elseExpr)
                            let withLitBinding = ANF.Let (litCheckVar, litCheckExpr, ifLitExpr)
                            let withBindings = wrapBindings bindings withLitBinding
                            let withTagCheck = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, withTagCheck)), vg7))

                    match pat with
                    | AST.PVar name ->
                        let newEnv = Map.add name (valueVar, elemType) currentEnv  // Use element type
                        toANFCore sumTypeNames inertScopes body vg5' newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | AST.PWildcard ->
                        toANFCore sumTypeNames inertScopes body vg5' currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | (AST.PTuple _ as nestedPattern)
                    | (AST.PConstructor _ as nestedPattern)
                    | (AST.PList _ as nestedPattern)
                    | (AST.PListCons _ as nestedPattern) ->
                        // Structural comparison must run before extracting binders so a
                        // failed nested pattern cannot leak its bindings into this arm.
                        buildPatternComparison nestedPattern valueAtom (Some elemType) vg5'
                        |> Result.bind (fun comparison ->
                            let (conditionOpt, comparisonBindings, vg6) =
                                match comparison with
                                | None -> (None, [], vg5')
                                | Some (condition, bindings', vg') -> (Some condition, bindings', vg')
                            collectNestedPatternBindings nestedPattern valueAtom elemType currentEnv [] vg6
                            |> Result.bind (fun (newEnv, nestedBindings, vg7) ->
                                toANFCore sumTypeNames inertScopes body vg7 newEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg8) ->
                                    let extractedBody = wrapBindings nestedBindings bodyExpr
                                    let matchedBody =
                                        match conditionOpt with
                                        | None -> extractedBody
                                        | Some condition -> ANF.If (condition, extractedBody, elseExpr)
                                    let withBindings = wrapBindings (bindings @ comparisonBindings) matchedBody
                                    let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                                    (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg8))))
                    | AST.PInt64 n -> compileLiteralPattern (ANF.Int64 n)
                    | AST.PInt128Literal n -> compileWideLiteralPattern (int128LiteralComparison valueAtom n)
                    | AST.PInt8Literal n -> compileLiteralPattern (ANF.Int8 n)
                    | AST.PInt16Literal n -> compileLiteralPattern (ANF.Int16 n)
                    | AST.PInt32Literal n -> compileLiteralPattern (ANF.Int32 n)
                    | AST.PUInt8Literal n -> compileLiteralPattern (ANF.UInt8 n)
                    | AST.PUInt16Literal n -> compileLiteralPattern (ANF.UInt16 n)
                    | AST.PUInt32Literal n -> compileLiteralPattern (ANF.UInt32 n)
                    | AST.PUInt64Literal n -> compileLiteralPattern (ANF.UInt64 n)
                    | AST.PUInt128Literal n -> compileWideLiteralPattern (uint128LiteralComparison valueAtom n)
                    | _ ->
                        Error $"Unsupported pattern in single-element list: {pat}"
                else
                    // Multiple elements: check length == patternLen (safe for all list types)
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthName =
                        match elemType with
                        | AST.TFloat64 -> "Stdlib.List.__lengthFloat"
                        | _ -> "Stdlib.List.__length_i64"
                    let lengthExpr = ANF.Call (lengthName, [listAtom])
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                    // Untag to get pointer (only used in then-branch after length check passes)
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                    // Note: lengthExpr and checkExpr are safe (length handles EMPTY)
                    // ptrExpr just does bitwise and, doesn't dereference
                    let headerBindings = [(lengthVar, lengthExpr); (checkVar, checkExpr); (ptrVar, ptrExpr)]
                    let vg6 = vg3  // Keep consistent naming for the rest of the code

                    // Extract elements using getAt (handles varying prefix/suffix layouts)
                    // Returns: (env, bindings, conditionAtoms, vg)
                    let rec extractElements
                        (pats: AST.Pattern list)
                        (idx: int)
                        (env: VarEnv)
                        (bindings: (ANF.TempId * ANF.CExpr) list)
                        (condAtoms: ANF.Atom list)
                        (vg: ANF.VarGen)
                        : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, bindings, condAtoms, vg)
                        | pat :: rest ->
                            // Use getAt to retrieve element at this index
                            // getAt returns Option, but we know length == patternLen so it's always Some
                            // Select a type-specific wrapper to avoid defaulting to Int64 for floats.
                            // Monomorphization happens at AST level, so we must use a non-generic wrapper here.
                            let (optVar, vg1) = ANF.freshVar vg
                            let getAtName =
                                match elemType with
                                | AST.TFloat64 -> "Stdlib.List.__getAtFloat"
                                | _ -> "Stdlib.List.__getAtInt64"
                            let getAtExpr = ANF.Call (getAtName, [listAtom; ANF.IntLiteral (ANF.Int64 (int64 idx))])
                            // Unwrap the Some - getAt returns tagged value with tag 1 for Some
                            let (rawValueVar, vg2) = ANF.freshVar vg1
                            let valueType =
                                match elemType with
                                | AST.TFloat64 -> Some AST.TFloat64
                                | _ -> None
                            let rawValueExpr = ANF.RawGet (ANF.Var optVar, ANF.IntLiteral (ANF.Int64 8L), valueType)  // Some payload at offset 8
                            // Wrap with TypedAtom to preserve element type in TypeMap
                            let (typedValueVar, vg2') = ANF.freshVar vg2
                            let typedValueExpr = ANF.TypedAtom (ANF.Var rawValueVar, elemType)
                            let newBindings = bindings @ [(optVar, getAtExpr); (rawValueVar, rawValueExpr); (typedValueVar, typedValueExpr)]
                            let valueVar = typedValueVar

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (valueVar, elemType) env  // Use element type
                                extractElements rest (idx + 1) newEnv newBindings condAtoms vg2'
                            | AST.PWildcard ->
                                extractElements rest (idx + 1) env newBindings condAtoms vg2'
                            | (AST.PTuple _ as nestedPattern)
                            | (AST.PList _ as nestedPattern)
                            | (AST.PListCons _ as nestedPattern)
                            | (AST.PConstructor _ as nestedPattern) ->
                                let staticallyCannotMatch = patternStaticallyCannotMatchType nestedPattern elemType
                                let comparisonResult =
                                    if staticallyCannotMatch then
                                        let (condition, bindings', vg3) = makeFalsePatternCondition vg2'
                                        Ok (Some (condition, bindings', vg3))
                                    else
                                        buildPatternComparison nestedPattern (ANF.Var valueVar) (Some elemType) vg2'
                                comparisonResult
                                |> Result.bind (fun comparison ->
                                    let (conditionOpt, comparisonBindings, vg3) =
                                        match comparison with
                                        | None -> (None, [], vg2')
                                        | Some (condition, bindings', vg') -> (Some condition, bindings', vg')
                                    let nestedBindingsResult =
                                        if patternBindsVariables nestedPattern && not staticallyCannotMatch then
                                            collectNestedPatternBindings nestedPattern (ANF.Var valueVar) elemType env [] vg3
                                        else
                                            Ok (env, [], vg3)
                                    nestedBindingsResult
                                    |> Result.bind (fun (envAfterPattern, nestedBindings, vg4) ->
                                        let nextConditions =
                                            match conditionOpt with
                                            | None -> condAtoms
                                            | Some condition -> condAtoms @ [condition]
                                        extractElements
                                            rest
                                            (idx + 1)
                                            envAfterPattern
                                            (newBindings @ comparisonBindings @ nestedBindings)
                                            nextConditions
                                            vg4))
                            | (AST.PInt64 _ as pat)
                            | (AST.PInt8Literal _ as pat)
                            | (AST.PInt16Literal _ as pat)
                            | (AST.PInt32Literal _ as pat)
                            | (AST.PUInt8Literal _ as pat)
                            | (AST.PUInt16Literal _ as pat)
                            | (AST.PUInt32Literal _ as pat)
                            | (AST.PUInt64Literal _ as pat) ->
                                let literal =
                                    match patternLiteralToSizedInt pat with
                                    | Some value -> value
                                    | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                                let (litCheckVar, vg3) = ANF.freshVar vg2'
                                let litCheckExpr = ANF.Prim (ANF.Eq, ANF.Var valueVar, ANF.IntLiteral literal)
                                let bindingsWithLiteral = newBindings @ [(litCheckVar, litCheckExpr)]
                                extractElements rest (idx + 1) env bindingsWithLiteral (condAtoms @ [ANF.Var litCheckVar]) vg3
                            | AST.PInt128Literal n ->
                                let (litCheckVar, vg3) = ANF.freshVar vg2'
                                let litCheckExpr =
                                    int128LiteralComparison (ANF.Var valueVar) n
                                let bindingsWithLiteral = newBindings @ [(litCheckVar, litCheckExpr)]
                                extractElements rest (idx + 1) env bindingsWithLiteral (condAtoms @ [ANF.Var litCheckVar]) vg3
                            | AST.PUInt128Literal n ->
                                let (litCheckVar, vg3) = ANF.freshVar vg2'
                                let litCheckExpr =
                                    uint128LiteralComparison (ANF.Var valueVar) n
                                let bindingsWithLiteral = newBindings @ [(litCheckVar, litCheckExpr)]
                                extractElements rest (idx + 1) env bindingsWithLiteral (condAtoms @ [ANF.Var litCheckVar]) vg3
                            | _ ->
                                Error $"Unsupported pattern in list element: {pat}"

                    extractElements patterns 0 currentEnv [] [] vg6
                    |> Result.bind (fun (newEnv, elemBindings, condAtoms, vg7) ->
                        toANFCore sumTypeNames inertScopes body vg7 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg8) ->
                            // Build the inner expression based on whether we have extra conditions
                            let (innerExpr, vg9) =
                                match condAtoms with
                                | [] ->
                                    // No extra conditions - just return body
                                    (bodyExpr, vg8)
                                | checks ->
                                    // AND condition atoms together (length check is handled separately by checkVar)
                                    let rec buildCombinedChecks
                                        (remaining: ANF.Atom list)
                                        (accBindings: (ANF.TempId * ANF.CExpr) list)
                                        (prevCond: ANF.Atom option)
                                        (vg: ANF.VarGen)
                                        : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
                                        match remaining with
                                        | [] ->
                                            match prevCond with
                                            | Some cond -> (cond, accBindings, vg)
                                            | None -> (ANF.BoolLiteral true, accBindings, vg)
                                        | condAtom :: rest ->
                                            match prevCond with
                                            | None ->
                                                buildCombinedChecks rest accBindings (Some condAtom) vg
                                            | Some prevCondAtom ->
                                                let (combinedVar, vg1) = ANF.freshVar vg
                                                let combinedExpr = ANF.Prim (ANF.And, prevCondAtom, condAtom)
                                                buildCombinedChecks rest (accBindings @ [(combinedVar, combinedExpr)]) (Some (ANF.Var combinedVar)) vg1
                                    let (combinedCondAtom, condBindings, vg9') = buildCombinedChecks checks [] None vg8
                                    let checkedBody = ANF.If (combinedCondAtom, bodyExpr, elseExpr)
                                    let withCondBindings = wrapBindings condBindings checkedBody
                                    (withCondBindings, vg9')
                            // Wrap with element bindings (inside length check)
                            let withElemBindings = wrapBindings elemBindings innerExpr
                            // Wrap with length check
                            let ifExpr = ANF.If (ANF.Var checkVar, withElemBindings, elseExpr)
                            let withHeader = wrapBindings headerBindings ifExpr
                            (withHeader, vg9)))
            | _ ->
                Ok (elseExpr, vg)

        // Compile a list cons pattern [h, ...t] for SkewList
        // This pattern extracts head element(s) and binds the rest to tail
        // For SkewList:
        // - SINGLE (tag 1): head is the element, tail is EMPTY
        // - DEEP (tag 2): head is prefix[0], tail requires calling SkewList.tail
        // listType is the list type (TList elemType) for correct pattern variable typing
        let rec compileListConsPatternWithChecks
            (headPatterns: AST.Pattern list)
            (tailPattern: AST.Pattern)
            (listAtom: ANF.Atom)
            (listType: AST.Type)
            (currentEnv: VarEnv)
            (body: AST.Expr)
            (elseExpr: ANF.AExpr)
            (vg: ANF.VarGen)
            : Result<ANF.AExpr * ANF.VarGen, string> =

            // Extract element type from list type
            let elemTypeResult : Result<AST.Type, string> =
                match listType with
                | AST.TList t -> Ok t
                | _ ->
                    Error $"List cons pattern expects TList scrutinee, got {typeToString listType}"

            elemTypeResult
            |> Result.bind (fun elemType ->
                // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                // The correct element type is tracked in the VarEnv/TypeMap, not in the function name

            // Helper to unwrap a LEAF node and get the value
            let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                let (leafPtrVar, vg1) = ANF.freshVar vg
                let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                let (valueVar, vg2) = ANF.freshVar vg1
                let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), valueType)
                let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                (ANF.Var valueVar, valueVar, newBindings, vg2)

            // Helper to extract tuple elements
            // tupleType is the type of the tuple being matched (TTuple elemTypes)
            let rec extractTupleBindings
                (tupPats: AST.Pattern list)
                (tupleAtom: ANF.Atom)
                (tupleType: AST.Type)
                (idx: int)
                (env: VarEnv)
                (bindings: (ANF.TempId * ANF.CExpr) list)
                (vg: ANF.VarGen)
                : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                // Extract element types from tuple type
                let elemTypes =
                    match tupleType with
                    | AST.TTuple types -> types
                    | _ -> Crash.crash $"Tuple head pattern expects tuple element type, got {typeToString tupleType}"
                match tupPats with
                | [] -> Ok (env, bindings, vg)
                | tupPat :: tupRest ->
                    let (rawElemVar, vg1) = ANF.freshVar vg
                    let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                    let rawElemBinding = (rawElemVar, rawElemExpr)
                    let elemT =
                        if idx < List.length elemTypes then
                            List.item idx elemTypes
                        else
                            Crash.crash
                                $"Tuple head pattern arity mismatch: requested index {idx}, tuple has {List.length elemTypes} elements"
                    // Wrap with TypedAtom to preserve correct element type in TypeMap
                    let (elemVar, vg1') = ANF.freshVar vg1
                    let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                    let elemBinding = (elemVar, elemExpr)
                    match tupPat with
                    | AST.PVar name ->
                        let newEnv = Map.add name (elemVar, elemT) env
                        extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                    | AST.PWildcard ->
                        // Even for wildcard, we need to extract the element (for proper tuple access)
                        // but don't bind it to a name. Just add the raw binding and continue.
                        extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                    | _ ->
                        Error $"Nested pattern in tuple element not yet supported: {tupPat}"

            let tupleHeadPatternType
                (candidateElemType: AST.Type)
                (patterns: AST.Pattern list)
                : AST.Type option =
                match candidateElemType with
                | AST.TTuple elemTypes when List.length elemTypes = List.length patterns ->
                    Some candidateElemType
                | AST.TVar tupleTypeVar ->
                    let unresolvedElemTypes =
                        patterns
                        |> List.mapi (fun idx _ ->
                            AST.TVar $"__tuple_elem_{tupleTypeVar}_{idx}")
                    Some (AST.TTuple unresolvedElemTypes)
                | AST.TRuntimeError ->
                    let unresolvedElemTypes =
                        patterns
                        |> List.mapi (fun idx _ ->
                            AST.TVar $"__tuple_elem_runtime_error_{idx}")
                    Some (AST.TTuple unresolvedElemTypes)
                | _ ->
                    None

            match headPatterns with
            | [] ->
                // All head elements extracted - bind tail and compile body
                match tailPattern with
                | AST.PVar name ->
                    let (tailVar, vg1) = ANF.freshVar vg
                    let newEnv = Map.add name (tailVar, listType) currentEnv  // Use actual list type
                    toANFCore sumTypeNames inertScopes body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let withTail = ANF.Let (tailVar, ANF.Atom listAtom, bodyExpr)
                        (withTail, vg2))
                | AST.PWildcard ->
                    toANFCore sumTypeNames inertScopes body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | _ -> Error "Tail pattern in list cons must be variable or wildcard"

            | [singleHeadPattern]
                when (match singleHeadPattern with
                      | AST.PList _ | AST.PListCons _ | AST.PConstructor _ -> false
                      | _ -> true)
                     && (match tailPattern with
                         | AST.PVar _ | AST.PWildcard -> true
                         | _ -> false) ->
                // Single head pattern [h, ...t] - most common case
                // Use branching based on tag to handle SINGLE vs DEEP nodes

                // Check list is not empty
                let (notEmptyVar, vg1) = ANF.freshVar vg
                let notEmptyExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral (ANF.Int64 0L))

                // Get tag
                let (tagVar, vg2) = ANF.freshVar vg1
                let tagExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L))

                // Untag to get pointer
                let (ptrVar, vg3) = ANF.freshVar vg2
                let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                // The old single/deep distinction no longer exists. Route every
                // non-empty digit through the generic tree-root branch below.
                let (isSingleVar, vg4) = ANF.freshVar vg3
                let isSingleExpr = ANF.Atom (ANF.BoolLiteral false)

                // notEmptyVar must be bound OUTSIDE the If since it's used as the condition
                let condBindings = [(notEmptyVar, notEmptyExpr)]
                let innerBindings = [(tagVar, tagExpr); (ptrVar, ptrExpr); (isSingleVar, isSingleExpr)]

                // Compile the SINGLE branch: node at offset 0, tail = EMPTY
                let compileSingleBranch vg =
                    let (singleNodeVar, vg1) = ANF.freshVar vg
                    let singleNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var singleNodeVar) vg1 [(singleNodeVar, singleNodeExpr)]
                    // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                    let (typedHeadVar, vg2') = ANF.freshVar vg2
                    let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                    let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                    let headBindingsWithType = headBindings @ [typedHeadBinding]
                    let typedHeadAtom = ANF.Var typedHeadVar
                    // Tail is empty list (0 = EMPTY sentinel) - wrap with TypedAtom to preserve list type
                    let (rawTailVar, vg3) = ANF.freshVar vg2'
                    let rawTailExpr = ANF.Atom (ANF.IntLiteral (ANF.Int64 0L))  // EMPTY
                    let (tailVar, vg3') = ANF.freshVar vg3
                    let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)

                    // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                    // guardOpt is Some(var, expr) for literal patterns that need comparison
                    let headEnvResult =
                        match singleHeadPattern with
                        | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                        | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                        | (AST.PTuple _ as nestedPattern)
                        | (AST.PConstructor _ as nestedPattern)
                        | (AST.PList _ as nestedPattern)
                        | (AST.PListCons _ as nestedPattern) ->
                            let staticallyCannotMatch = patternStaticallyCannotMatchType nestedPattern elemType
                            let comparisonResult =
                                if staticallyCannotMatch then
                                    let (condition, bindings, vg4) = makeFalsePatternCondition vg3'
                                    Ok (Some (condition, bindings, vg4))
                                else
                                    buildPatternComparison nestedPattern typedHeadAtom (Some elemType) vg3'
                            comparisonResult
                            |> Result.bind (fun comparison ->
                                let (guardOpt, comparisonBindings, vg4) =
                                    match comparison with
                                    | None -> (None, [], vg3')
                                    | Some (condition, bindings, vg') ->
                                        let (guardVar, vg'') = ANF.freshVar vg'
                                        (Some (guardVar, ANF.Atom condition), bindings, vg'')
                                let nestedBindingsResult =
                                    if patternBindsVariables nestedPattern && not staticallyCannotMatch then
                                        collectNestedPatternBindings nestedPattern typedHeadAtom elemType currentEnv [] vg4
                                    else
                                        Ok (currentEnv, [], vg4)
                                nestedBindingsResult
                                |> Result.map (fun (env, nestedBindings, vg5) ->
                                    (env, comparisonBindings @ nestedBindings, vg5, guardOpt)))
                        | (AST.PInt64 _ as pat)
                        | (AST.PInt8Literal _ as pat)
                        | (AST.PInt16Literal _ as pat)
                        | (AST.PInt32Literal _ as pat)
                        | (AST.PUInt8Literal _ as pat)
                        | (AST.PUInt16Literal _ as pat)
                        | (AST.PUInt32Literal _ as pat)
                        | (AST.PUInt64Literal _ as pat) ->
                            // Compare head value to literal - guard check
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let literal =
                                match patternLiteralToSizedInt pat with
                                | Some value -> value
                                | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                            let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral literal)
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | AST.PInt128Literal n ->
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let guardExpr =
                                int128LiteralComparison (ANF.Var typedHeadVar) n
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | AST.PUInt128Literal n ->
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let guardExpr =
                                uint128LiteralComparison (ANF.Var typedHeadVar) n
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                    headEnvResult
                    |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                        let tailEnvResult =
                            match tailPattern with
                            | AST.PVar name -> Ok (Map.add name (tailVar, listType) envWithHead, vg4)  // Use actual list type
                            | AST.PWildcard -> Ok (envWithHead, vg4)
                            | _ -> Error "Tail pattern must be variable or wildcard"

                        tailEnvResult
                        |> Result.bind (fun (finalEnv, vg5) ->
                            toANFCore sumTypeNames inertScopes body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg6) ->
                                let withTupleBindings = bodyExpr
                                let withTypedTail = ANF.Let (tailVar, tailExpr, withTupleBindings)
                                let withTail = ANF.Let (rawTailVar, rawTailExpr, withTypedTail)
                                // If there's a guard (literal pattern), add check AFTER head bindings
                                // because guardExpr uses headVar which is defined in headBindings
                                let withGuard =
                                    match guardOpt with
                                    | Some (guardVar, guardExpr) ->
                                        // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                        let ifGuard = ANF.If (ANF.Var guardVar, withTail, elseExpr)
                                        let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                        wrapBindings headBindingsWithType (wrapBindings tupleBindings withGuardBinding)
                                    | None -> wrapBindings headBindingsWithType (wrapBindings tupleBindings withTail)
                                (withGuard, vg6))))

                // Compile the DEEP branch: node at offset 16 (prefix[0])
                // For tail, call Stdlib.List.__tail to properly compute the tail
                let compileDeepBranch vg =
                    let (deepNodeVar, vg1) = ANF.freshVar vg
                    let deepNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), None)
                    let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var deepNodeVar) vg1 [(deepNodeVar, deepNodeExpr)]
                    // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                    let (typedHeadVar, vg2') = ANF.freshVar vg2
                    let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                    let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                    let headBindingsWithType = headBindings @ [typedHeadBinding]
                    let typedHeadAtom = ANF.Var typedHeadVar

                    // Call Stdlib.List.__tail to get the tail
                    let (tailResultVar, vg3) = ANF.freshVar vg2'
                    let tailCallExpr = ANF.Call ("Stdlib.List.__tail_i64", [listAtom])
                    // Wrap with TypedAtom to preserve correct list type in TypeMap
                    let (typedTailVar, vg3') = ANF.freshVar vg3
                    let typedTailExpr = ANF.TypedAtom (ANF.Var tailResultVar, listType)

                    let tailBindings = [(tailResultVar, tailCallExpr); (typedTailVar, typedTailExpr)]

                    // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                    let headEnvResult =
                        match singleHeadPattern with
                        | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                        | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                        | (AST.PTuple _ as nestedPattern)
                        | (AST.PConstructor _ as nestedPattern)
                        | (AST.PList _ as nestedPattern)
                        | (AST.PListCons _ as nestedPattern) ->
                            let staticallyCannotMatch = patternStaticallyCannotMatchType nestedPattern elemType
                            let comparisonResult =
                                if staticallyCannotMatch then
                                    let (condition, bindings, vg4) = makeFalsePatternCondition vg3'
                                    Ok (Some (condition, bindings, vg4))
                                else
                                    buildPatternComparison nestedPattern typedHeadAtom (Some elemType) vg3'
                            comparisonResult
                            |> Result.bind (fun comparison ->
                                let (guardOpt, comparisonBindings, vg4) =
                                    match comparison with
                                    | None -> (None, [], vg3')
                                    | Some (condition, bindings, vg') ->
                                        let (guardVar, vg'') = ANF.freshVar vg'
                                        (Some (guardVar, ANF.Atom condition), bindings, vg'')
                                let nestedBindingsResult =
                                    if patternBindsVariables nestedPattern && not staticallyCannotMatch then
                                        collectNestedPatternBindings nestedPattern typedHeadAtom elemType currentEnv [] vg4
                                    else
                                        Ok (currentEnv, [], vg4)
                                nestedBindingsResult
                                |> Result.map (fun (env, nestedBindings, vg5) ->
                                    (env, comparisonBindings @ nestedBindings, vg5, guardOpt)))
                        | (AST.PInt64 _ as pat)
                        | (AST.PInt8Literal _ as pat)
                        | (AST.PInt16Literal _ as pat)
                        | (AST.PInt32Literal _ as pat)
                        | (AST.PUInt8Literal _ as pat)
                        | (AST.PUInt16Literal _ as pat)
                        | (AST.PUInt32Literal _ as pat)
                        | (AST.PUInt64Literal _ as pat) ->
                            // Compare head value to literal - guard check
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let literal =
                                match patternLiteralToSizedInt pat with
                                | Some value -> value
                                | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                            let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral literal)
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | AST.PInt128Literal n ->
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let guardExpr =
                                int128LiteralComparison (ANF.Var typedHeadVar) n
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | AST.PUInt128Literal n ->
                            let (guardVar, vg4) = ANF.freshVar vg3'
                            let guardExpr =
                                uint128LiteralComparison (ANF.Var typedHeadVar) n
                            Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                        | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                    headEnvResult
                    |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                        let tailEnvResult =
                            match tailPattern with
                            | AST.PVar name -> Ok (Map.add name (typedTailVar, listType) envWithHead, vg4)  // Use typed tail var with correct list type
                            | AST.PWildcard -> Ok (envWithHead, vg4)
                            | _ -> Error "Tail pattern must be variable or wildcard"

                        tailEnvResult
                        |> Result.bind (fun (finalEnv, vg5) ->
                            toANFCore sumTypeNames inertScopes body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg6) ->
                                let withTupleBindings = bodyExpr
                                let withTailBinding = wrapBindings tailBindings withTupleBindings
                                // If there's a guard (literal pattern), add check AFTER head bindings
                                // because guardExpr uses headVar which is defined in headBindingsWithType
                                let withGuard =
                                    match guardOpt with
                                    | Some (guardVar, guardExpr) ->
                                        // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                        let ifGuard = ANF.If (ANF.Var guardVar, withTailBinding, elseExpr)
                                        let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                        wrapBindings headBindingsWithType (wrapBindings tupleBindings withGuardBinding)
                                    | None -> wrapBindings headBindingsWithType (wrapBindings tupleBindings withTailBinding)
                                (withGuard, vg6))))

                // Build the combined expression with branching
                compileSingleBranch vg4
                |> Result.bind (fun (singleBranchExpr, vg5) ->
                    compileDeepBranch vg5
                    |> Result.map (fun (deepBranchExpr, vg6) ->
                        // If SINGLE then singleBranch else deepBranch
                        let tagBranchExpr = ANF.If (ANF.Var isSingleVar, singleBranchExpr, deepBranchExpr)
                        // Wrap inner bindings (tag, ptr, isSingle) around the tag branch
                        let withInnerBindings = wrapBindings innerBindings tagBranchExpr
                        // If not empty then execute inner bindings + branch else elseExpr
                        let ifExpr = ANF.If (ANF.Var notEmptyVar, withInnerBindings, elseExpr)
                        // Bind notEmptyVar BEFORE the If
                        let finalExpr = wrapBindings condBindings ifExpr
                        (finalExpr, vg6)))

            | _ ->
                // Multiple head patterns [a, b, ...t]
                // Check length >= number of head patterns
                let numHeads = List.length headPatterns
                let (lengthVar, vg1) = ANF.freshVar vg
                let lengthName =
                    match elemType with
                    | AST.TFloat64 -> "Stdlib.List.__lengthFloat"
                    | _ -> "Stdlib.List.__length_i64"
                let lengthExpr = ANF.Call (lengthName, [listAtom])
                let (lengthCheckVar, vg2) = ANF.freshVar vg1
                let lengthCheckExpr = ANF.Prim (ANF.Gte, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 numHeads)))

                // Extract head elements and final tail using head/tail calls
                // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                let rec extractElements
                    (pats: AST.Pattern list)
                    (currentListVar: ANF.TempId)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (condAtoms: ANF.Atom list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.TempId * ANF.Atom list * ANF.VarGen, string> =
                    match pats with
                    | [] ->
                        // No more head patterns, currentListVar is the tail
                        Ok (env, bindings, currentListVar, condAtoms, vg)
                    | pat :: rest ->
                        // Call head to get current element
                        let (headResultVar, vg1) = ANF.freshVar vg
                        let headCallExpr =
                            listHeadUnsafeExpr funcReg elemType (ANF.Var currentListVar)
                        // Call tail to get rest
                        let (tailResultVar, vg2) = ANF.freshVar vg1
                        let tailCallExpr = ANF.Call ("Stdlib.List.__tail_i64", [ANF.Var currentListVar])
                        // Preserve type information for both head and tail values.
                        let (typedHeadVar, vg2') = ANF.freshVar vg2
                        let typedHeadExpr = ANF.TypedAtom (ANF.Var headResultVar, elemType)
                        let (typedTailVar, vg2'') = ANF.freshVar vg2'
                        let typedTailExpr = ANF.TypedAtom (ANF.Var tailResultVar, listType)
                        let newBindings =
                            bindings
                            @ [
                                (headResultVar, headCallExpr)
                                (typedHeadVar, typedHeadExpr)
                                (tailResultVar, tailCallExpr)
                                (typedTailVar, typedTailExpr)
                              ]

                        match pat with
                        | AST.PVar name ->
                            let newEnv = Map.add name (typedHeadVar, elemType) env
                            extractElements rest typedTailVar newEnv newBindings condAtoms vg2''
                        | AST.PWildcard ->
                            extractElements rest typedTailVar env newBindings condAtoms vg2''
                        | (AST.PInt64 _ as litPat)
                        | (AST.PInt8Literal _ as litPat)
                        | (AST.PInt16Literal _ as litPat)
                        | (AST.PInt32Literal _ as litPat)
                        | (AST.PUInt8Literal _ as litPat)
                        | (AST.PUInt16Literal _ as litPat)
                        | (AST.PUInt32Literal _ as litPat)
                        | (AST.PUInt64Literal _ as litPat) ->
                            let literal =
                                match patternLiteralToSizedInt litPat with
                                | Some value -> value
                                | None -> Crash.crash $"Expected integer literal pattern, got {litPat}"
                            let (litCheckVar, vg3) = ANF.freshVar vg2''
                            let litCheckExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral literal)
                            let bindingsWithCheck = newBindings @ [(litCheckVar, litCheckExpr)]
                            extractElements rest typedTailVar env bindingsWithCheck (condAtoms @ [ANF.Var litCheckVar]) vg3
                        | AST.PInt128Literal n ->
                            let (litCheckVar, vg3) = ANF.freshVar vg2''
                            let litCheckExpr =
                                int128LiteralComparison (ANF.Var typedHeadVar) n
                            let bindingsWithCheck = newBindings @ [(litCheckVar, litCheckExpr)]
                            extractElements rest typedTailVar env bindingsWithCheck (condAtoms @ [ANF.Var litCheckVar]) vg3
                        | AST.PUInt128Literal n ->
                            let (litCheckVar, vg3) = ANF.freshVar vg2''
                            let litCheckExpr =
                                uint128LiteralComparison (ANF.Var typedHeadVar) n
                            let bindingsWithCheck = newBindings @ [(litCheckVar, litCheckExpr)]
                            extractElements rest typedTailVar env bindingsWithCheck (condAtoms @ [ANF.Var litCheckVar]) vg3
                        | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                            let staticallyCannotMatch = patternStaticallyCannotMatchType pat elemType
                            let cmpResult =
                                if staticallyCannotMatch then
                                    let (condAtom, bindings', vg3) = makeFalsePatternCondition vg2''
                                    Ok (Some (condAtom, bindings', vg3))
                                else
                                    buildPatternComparison pat (ANF.Var typedHeadVar) (Some elemType) vg2''
                            cmpResult
                            |> Result.bind (fun cmpOpt ->
                                let (cmpCondOpt, cmpBindings, vg3) =
                                    match cmpOpt with
                                    | None -> (None, [], vg2'')
                                    | Some (condAtom, bindings', vg') -> (Some condAtom, bindings', vg')
                                let nestedBindingsResult =
                                    if patternBindsVariables pat && not staticallyCannotMatch then
                                        collectNestedPatternBindings pat (ANF.Var typedHeadVar) elemType env [] vg3
                                    else
                                        Ok (env, [], vg3)
                                nestedBindingsResult
                                |> Result.bind (fun (envAfterPat, nestedBindings, vg4) ->
                                    let condAtoms' =
                                        match cmpCondOpt with
                                        | None -> condAtoms
                                        | Some condAtom -> condAtoms @ [condAtom]
                                    let bindingsWithPat = newBindings @ cmpBindings @ nestedBindings
                                    extractElements rest typedTailVar envAfterPat bindingsWithPat condAtoms' vg4))
                        | _ ->
                            Error $"Unsupported head pattern in multi-element list cons: {pat}"

                // Get initial list variable
                let (initialListVar, vg3) = ANF.freshVar vg2
                let initialListExpr = ANF.Atom listAtom

                extractElements headPatterns initialListVar currentEnv [(initialListVar, initialListExpr)] [] vg3
                |> Result.bind (fun (envAfterHeads, headBindings, finalTailVar, headCondAtoms, vg4) ->
                    // Compile the tail after extracting the chained heads. Exact-list and
                    // further-cons tails need their full checked lowering: a comparison-only
                    // PList condition validates length but does not compare its elements.
                    let tailBodyResult : Result<ANF.AExpr * (ANF.TempId * ANF.CExpr) list * ANF.Atom list * ANF.VarGen, string> =
                        match tailPattern with
                        | AST.PList patterns ->
                            compileListPatternWithChecks
                                patterns
                                (ANF.Var finalTailVar)
                                listType
                                envAfterHeads
                                body
                                elseExpr
                                vg4
                            |> Result.map (fun (tailBody, vg5) -> (tailBody, [], [], vg5))
                        | AST.PListCons (nestedHeads, nestedTail) ->
                            compileListConsPatternWithChecks
                                nestedHeads
                                nestedTail
                                (ANF.Var finalTailVar)
                                listType
                                envAfterHeads
                                body
                                elseExpr
                                vg4
                            |> Result.map (fun (tailBody, vg5) -> (tailBody, [], [], vg5))
                        | AST.PVar name ->
                            toANFCore sumTypeNames inertScopes
                                body
                                vg4
                                (Map.add name (finalTailVar, listType) envAfterHeads)
                                typeReg
                                variantLookup
                                funcReg
                                moduleRegistry
                            |> Result.map (fun (tailBody, vg5) -> (tailBody, [], [], vg5))
                        | AST.PWildcard ->
                            toANFCore sumTypeNames inertScopes body vg4 envAfterHeads typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (tailBody, vg5) -> (tailBody, [], [], vg5))
                        | _ ->
                            let staticallyCannotMatch =
                                patternStaticallyCannotMatchType tailPattern listType
                            let comparisonResult =
                                if staticallyCannotMatch then
                                    let (condAtom, bindings, vg5) = makeFalsePatternCondition vg4
                                    Ok (Some (condAtom, bindings, vg5))
                                else
                                    buildPatternComparison tailPattern (ANF.Var finalTailVar) (Some (AST.TList elemType)) vg4
                            comparisonResult
                            |> Result.bind (fun comparison ->
                                let (conditionAtoms, comparisonBindings, vg5) =
                                    match comparison with
                                    | None -> ([], [], vg4)
                                    | Some (conditionAtom, bindings, nextVg) ->
                                        ([conditionAtom], bindings, nextVg)
                                let nestedBindingsResult =
                                    if patternBindsVariables tailPattern && not staticallyCannotMatch then
                                        collectNestedPatternBindings
                                            tailPattern
                                            (ANF.Var finalTailVar)
                                            listType
                                            envAfterHeads
                                            []
                                            vg5
                                    else
                                        Ok (envAfterHeads, [], vg5)
                                nestedBindingsResult
                                |> Result.bind (fun (tailEnv, nestedBindings, vg6) ->
                                    toANFCore sumTypeNames inertScopes body vg6 tailEnv typeReg variantLookup funcReg moduleRegistry
                                    |> Result.map (fun (tailBody, vg7) ->
                                        (tailBody, comparisonBindings @ nestedBindings, conditionAtoms, vg7))))

                    tailBodyResult
                    |> Result.map (fun (tailBody, tailBindings, tailCondAtoms, vg5) ->
                        let allCondAtoms = headCondAtoms @ tailCondAtoms
                        // Build pattern condition checks first; these checks may depend on
                        // vars extracted by headBindings/tailBindings, so extraction wraps outside.
                        let (guardedBody, vg6) =
                            match allCondAtoms with
                            | [] ->
                                (tailBody, vg5)
                            | checks ->
                                let rec buildCombinedChecks
                                    (remaining: ANF.Atom list)
                                    (accBindings: (ANF.TempId * ANF.CExpr) list)
                                    (prevCond: ANF.Atom option)
                                    (vg: ANF.VarGen)
                                    : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
                                    match remaining with
                                    | [] ->
                                        match prevCond with
                                        | Some cond -> (cond, accBindings, vg)
                                        | None -> (ANF.BoolLiteral true, accBindings, vg)
                                    | condAtom :: rest ->
                                        match prevCond with
                                        | None ->
                                            buildCombinedChecks rest accBindings (Some condAtom) vg
                                        | Some prevCondAtom ->
                                            let (combinedVar, vg1) = ANF.freshVar vg
                                            let combinedExpr = ANF.Prim (ANF.And, prevCondAtom, condAtom)
                                            buildCombinedChecks rest (accBindings @ [(combinedVar, combinedExpr)]) (Some (ANF.Var combinedVar)) vg1
                                let (combinedCondAtom, condBindings, vg6') = buildCombinedChecks checks [] None vg5
                                let checkedBody = ANF.If (combinedCondAtom, tailBody, elseExpr)
                                (wrapBindings condBindings checkedBody, vg6')

                        // Apply tail/head extraction bindings (including comparison inputs) outside guard checks.
                        let withExtractBindings = wrapBindings (headBindings @ tailBindings) guardedBody

                        // Check length condition first
                        let ifExpr = ANF.If (ANF.Var lengthCheckVar, withExtractBindings, elseExpr)
                        let withLengthCheck = ANF.Let (lengthCheckVar, lengthCheckExpr, ifExpr)
                        let finalExpr = ANF.Let (lengthVar, lengthExpr, withLengthCheck)
                        (finalExpr, vg6))))

        // Build OR of multiple pattern conditions for pattern grouping
        // Returns: combined condition atom, all bindings, updated vargen
        let patternStaticallyCannotMatchScrutinee (pattern: AST.Pattern) : bool =
            patternStaticallyCannotMatchType pattern scrutType

        let makeFalseCondition (vg: ANF.VarGen) : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
            let (cmpVar, vg1) = ANF.freshVar vg
            let cmpExpr = ANF.Atom (ANF.BoolLiteral false)
            (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1)

        // The tests a pattern makes on a value, in the order they must run. A
        // stage's bindings are only safe once every earlier stage's condition
        // held: a variant's payload slot is a payload only when its tag matched,
        // and past a smaller variant it is whatever the heap holds there. The
        // flat comparison above runs every load and test at once, so a nested
        // pattern that dereferences the payload (a constructor's tag load, a
        // string compare) reads through garbage: `| Some((_, String "2.0"))` on a
        // None or on a Some(Number) was a SIGSEGV. An arm compiled from stages
        // nests one `If` per stage instead. The else branch is repeated per
        // stage, which is what the list-pattern compilers already do.
        let rec buildPatternStages (pattern: AST.Pattern) (scrutAtom: ANF.Atom) (patType: AST.Type option) (vg: ANF.VarGen) : Result<((ANF.TempId * ANF.CExpr) list * ANF.Atom) list * ANF.VarGen, string> =
            let testedType = defaultArg patType scrutType
            let prependBindings (bindings: (ANF.TempId * ANF.CExpr) list) (stages: ((ANF.TempId * ANF.CExpr) list * ANF.Atom) list) =
                match stages with
                | [] -> []
                | (firstBindings, firstCond) :: rest -> (bindings @ firstBindings, firstCond) :: rest
            match pattern with
            | AST.PConstructor (variantName, Some innerPattern) when not (patternAlwaysMatches innerPattern) ->
                match tryFindVariantForType variantName testedType variantLookup with
                | Some (typeName, typeParams, tag, Some payloadTemplate)
                    when variantLookup |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome) ->
                    let payloadType =
                        match testedType with
                        | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                            substituteTypeParams (List.zip typeParams typeArgs |> Map.ofList) payloadTemplate
                        | _ -> payloadTemplate
                    let (tagVar, vg1) = ANF.freshVar vg
                    let (tagCmpVar, vg2) = ANF.freshVar vg1
                    let (payloadVar, vg3) = ANF.freshVar vg2
                    let tagStage =
                        ([(tagVar, ANF.TupleGet (scrutAtom, 0))
                          (tagCmpVar, ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 tag))))],
                         ANF.Var tagCmpVar)
                    buildPatternStages innerPattern (ANF.Var payloadVar) (Some payloadType) vg3
                    |> Result.map (fun (innerStages, vg4) ->
                        (tagStage :: prependBindings [(payloadVar, ANF.TupleGet (scrutAtom, 1))] innerStages, vg4))
                | _ ->
                    // Nullary, enum-only or unknown: the flat comparison is one stage.
                    buildPatternComparison pattern scrutAtom patType vg
                    |> Result.map (function
                        | None -> ([], vg)
                        | Some (cond, bindings, vg') -> ([(bindings, cond)], vg'))
            | AST.PTuple innerPatterns ->
                let rec elements (patterns: AST.Pattern list) (index: int) (vg: ANF.VarGen) (acc: ((ANF.TempId * ANF.CExpr) list * ANF.Atom) list) =
                    match patterns with
                    | [] -> Ok (acc, vg)
                    | p :: rest ->
                        let elemType =
                            match testedType with
                            | AST.TTuple elemTypes -> List.tryItem index elemTypes
                            | _ -> None
                        let (elemVar, vg1) = ANF.freshVar vg
                        buildPatternStages p (ANF.Var elemVar) elemType vg1
                        |> Result.bind (fun (elemStages, vg2) ->
                            let staged = prependBindings [(elemVar, ANF.TupleGet (scrutAtom, index))] elemStages
                            elements rest (index + 1) vg2 (acc @ staged))
                elements innerPatterns 0 vg []
            | AST.PList _ | AST.PListCons _ ->
                // A list pattern below the top of an arm (in a tuple, a payload) was
                // one flat length test, so `("Stdlib" :: _, x)` matched every
                // non-empty list. The length is one stage, then each head is taken
                // and tested in turn, then the tail.
                let (headPatterns, tailPattern, exact) =
                    match pattern with
                    | AST.PList elements -> (elements, None, true)
                    | AST.PListCons (heads, tail) -> (heads, Some tail, false)
                    | _ -> ([], None, false)
                let elemType =
                    match testedType with
                    | AST.TList t -> t
                    | _ -> AST.TVar "__list_elem_unknown"
                let listType = AST.TList elemType
                let count = List.length headPatterns
                let (lengthVar, vg1) = ANF.freshVar vg
                let (lengthCmpVar, vg2) = ANF.freshVar vg1
                let lengthStage =
                    ([(lengthVar, ANF.Call ("Stdlib.List.__length_i64", [scrutAtom]))
                      (lengthCmpVar, ANF.Prim ((if exact then ANF.Eq else ANF.Gte), ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 count))))],
                     ANF.Var lengthCmpVar)
                let rec heads (patterns: AST.Pattern list) (current: ANF.Atom) (vg: ANF.VarGen) (acc: ((ANF.TempId * ANF.CExpr) list * ANF.Atom) list) =
                    match patterns with
                    | [] ->
                        match tailPattern with
                        | Some tail when not (patternAlwaysMatches tail) ->
                            buildPatternStages tail current (Some listType) vg
                            |> Result.map (fun (tailStages, vg') -> (acc @ tailStages, vg'))
                        | _ -> Ok (acc, vg)
                    | p :: rest ->
                        let (rawHeadVar, vg1) = ANF.freshVar vg
                        let (headVar, vg2) = ANF.freshVar vg1
                        let (rawTailVar, vg3) = ANF.freshVar vg2
                        let (tailVar, vg4) = ANF.freshVar vg3
                        let headLoads =
                            [(rawHeadVar, listHeadUnsafeExpr funcReg elemType current)
                             (headVar, ANF.TypedAtom (ANF.Var rawHeadVar, elemType))]
                        let tailLoads =
                            [(rawTailVar, ANF.Call ("Stdlib.List.__tail_i64", [current]))
                             (tailVar, ANF.TypedAtom (ANF.Var rawTailVar, listType))]
                        buildPatternStages p (ANF.Var headVar) (Some elemType) vg4
                        |> Result.bind (fun (headStages, vg5) ->
                            let staged = prependBindings headLoads headStages
                            let needsTail = not (List.isEmpty rest) || Option.exists (fun t -> not (patternAlwaysMatches t)) tailPattern
                            if needsTail then
                                heads rest (ANF.Var tailVar) vg5 (acc @ staged @ [(tailLoads, ANF.BoolLiteral true)])
                            else
                                heads rest (ANF.Var tailVar) vg5 (acc @ staged))
                if count = 0 && tailPattern.IsNone then
                    // `[]`: the length test alone.
                    Ok ([lengthStage], vg2)
                else
                    heads headPatterns scrutAtom vg2 [lengthStage]
            | _ ->
                buildPatternComparison pattern scrutAtom patType vg
                |> Result.map (function
                    | None -> ([], vg)
                    | Some (cond, bindings, vg') -> ([(bindings, cond)], vg'))

        /// `thenExpr` under every stage's condition, `elseExpr` when any fails,
        /// each used once. One stage is a plain `If`. Several become a Bool join:
        /// the entry runs the stages in order, jumping out with `false` at the
        /// first failed test and with the last condition otherwise, and the
        /// continuation is the `If` on that Bool. Nesting `If`s instead would
        /// repeat `elseExpr` per stage, and it is the rest of the match: a match
        /// with n such arms would copy its tail 2^n times.
        let stagesToIf (stages: ((ANF.TempId * ANF.CExpr) list * ANF.Atom) list) (thenExpr: ANF.AExpr) (elseExpr: ANF.AExpr) (vg: ANF.VarGen) : ANF.AExpr * ANF.VarGen =
            match stages with
            | [] -> (thenExpr, vg)
            | [ (bindings, cond) ] -> (wrapBindings bindings (ANF.If (cond, thenExpr, elseExpr)), vg)
            | _ ->
                let (resultVar, vg1) = ANF.freshVar vg
                let rec entry (remaining: ((ANF.TempId * ANF.CExpr) list * ANF.Atom) list) : ANF.AExpr =
                    match remaining with
                    | [] -> ANF.Jump (resultVar, ANF.BoolLiteral true)
                    | [ (bindings, cond) ] -> wrapBindings bindings (ANF.Jump (resultVar, cond))
                    | (bindings, cond) :: rest ->
                        wrapBindings bindings (ANF.If (cond, entry rest, ANF.Jump (resultVar, ANF.BoolLiteral false)))
                let join =
                    ANF.Join (
                        { ANF.TypedParam.Id = resultVar; ANF.TypedParam.Type = AST.TBool },
                        ANF.If (ANF.Var resultVar, thenExpr, elseExpr),
                        entry stages)
                (join, vg1)

        let buildPatternGroupComparison (patterns: AST.Pattern list) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
            match patterns with
            | [] -> Ok None
            | [single] ->
                if patternStaticallyCannotMatchScrutinee single then
                    let (condAtom, bindings, vg1) = makeFalseCondition vg
                    Ok (Some (condAtom, bindings, vg1))
                else
                    buildPatternComparison single scrutAtom (Some scrutType) vg
            | multiple ->
                // Build comparison for each pattern, then OR them together
                let rec buildOr (pats: AST.Pattern list) (accCondOpt: ANF.Atom option) (accBindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                    match pats with
                    | [] ->
                        match accCondOpt with
                        | None -> Ok None
                        | Some cond -> Ok (Some (cond, accBindings, vg))
                    | pat :: rest ->
                        let cmpResult =
                            if patternStaticallyCannotMatchScrutinee pat then
                                let (condAtom, bindings, vg1) = makeFalseCondition vg
                                Ok (Some (condAtom, bindings, vg1))
                            else
                                buildPatternComparison pat scrutAtom (Some scrutType) vg
                        cmpResult
                        |> Result.bind (fun cmpOpt ->
                            match cmpOpt with
                            | None ->
                                // Pattern always matches (wildcard/var) - the whole group always matches
                                Ok None
                            | Some (condAtom, bindings, vg1) ->
                                let (newCondOpt, newBindings, vg2) =
                                    match accCondOpt with
                                    | None ->
                                        // First condition
                                        (Some condAtom, bindings @ accBindings, vg1)
                                    | Some accCond ->
                                        // OR with previous conditions
                                        // Put bindings in dependency order: comparison bindings first, OR at end
                                        // (foldBack makes first binding outermost, so dependencies must come first)
                                        let (orVar, vg') = ANF.freshVar vg1
                                        let orExpr = ANF.Prim (ANF.Or, accCond, condAtom)
                                        (Some (ANF.Var orVar), accBindings @ bindings @ [(orVar, orExpr)], vg')
                                buildOr rest newCondOpt newBindings vg2)
                buildOr multiple None [] vg

        let escapeForRuntimeError (text: string) : string =
            text
            |> String.collect (fun ch ->
                match ch with
                | '\\' -> "\\\\"
                | '"' -> "\\\""
                | '\n' -> "\\n"
                | '\r' -> "\\r"
                | '\t' -> "\\t"
                | _ -> string ch)

        let rec formatMatchValueForError (expr: AST.Expr) : string option =
            let rec formatAll (expressions: AST.Expr list) (acc: string list) : string list option =
                match expressions with
                | [] -> Some (List.rev acc)
                | expression :: rest ->
                    formatMatchValueForError expression
                    |> Option.bind (fun formatted -> formatAll rest (formatted :: acc))

            match expr with
            | AST.Int64Literal n -> Some $"{n}"
            | AST.Int128Literal n -> Some (int128ToCanonicalString n)
            | AST.Int8Literal n -> Some $"{n}"
            | AST.Int16Literal n -> Some $"{n}"
            | AST.Int32Literal n -> Some $"{n}"
            | AST.UInt8Literal n -> Some $"{n}"
            | AST.UInt16Literal n -> Some $"{n}"
            | AST.UInt32Literal n -> Some $"{n}"
            | AST.UInt64Literal n -> Some $"{n}"
            | AST.UInt128Literal n -> Some (uint128ToCanonicalString n)
            | AST.BoolLiteral b -> Some (if b then "true" else "false")
            | AST.FloatLiteral f -> Some $"{f}"
            | AST.UnitLiteral -> Some "()"
            | AST.StringLiteral s -> Some $"\"{escapeForRuntimeError s}\""
            | AST.CharLiteral c -> Some $"'{escapeForRuntimeError c}'"
            | AST.TupleLiteral elements ->
                formatAll elements []
                |> Option.map (fun rendered ->
                    let joined = String.concat ", " rendered
                    $"({joined})")
            | AST.ListLiteral elements ->
                formatAll elements []
                |> Option.map (fun rendered ->
                    let joined = String.concat ", " rendered
                    $"[{joined}]")
            | AST.Constructor (constructorReference, variantName, payload) ->
                let fullName =
                    match AST.constructorReferenceTypeName constructorReference with
                    | None -> variantName
                    | Some typeName -> $"{typeName}.{variantName}"
                match payload with
                | None -> Some fullName
                | Some payloadExpr ->
                    formatMatchValueForError payloadExpr
                    |> Option.map (fun payloadText -> $"{fullName}({payloadText})")
            | _ -> None

        let makeNoMatchingCaseFallback (vg: ANF.VarGen) : ANF.AExpr * ANF.VarGen =
            let valueText =
                formatMatchValueForError scrutinee
                |> Option.defaultValue "<unknown>"
            let message = $"Non-exhaustive match: No matching case found for value {valueText} in match expression"
            let (errorVar, vg1) = ANF.freshVar vg
            let errorExpr = ANF.RuntimeError message
            (ANF.Let (errorVar, errorExpr, ANF.Return (ANF.Var errorVar)), vg1)

        // Build the if-else chain from cases
        let rec buildChain (remaining: AST.MatchCase list) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
            match remaining with
            | [] ->
                // No cases left - shouldn't happen if we have wildcard/var
                Error $"Non-exhaustive pattern match for {typeToString scrutType}"
            | mc :: rest when not (List.isEmpty mc.Patterns.Tail) ->
                // Desugar grouped patterns (`p1 | p2 -> body`) into sequential single-pattern
                // cases so bindings come from the pattern that actually matched.
                let expandedCases =
                    AST.NonEmptyList.toList mc.Patterns
                    |> List.filter (fun pattern -> not (patternStaticallyCannotMatchScrutinee pattern))
                    |> List.map (fun pattern ->
                        { mc with Patterns = AST.NonEmptyList.singleton pattern })
                if List.isEmpty expandedCases then
                    buildChain rest vg
                else
                    buildChain (expandedCases @ rest) vg
            | [mc] ->
                let pattern = AST.NonEmptyList.head mc.Patterns
                let body = mc.Body
                let (fallbackExpr, vg1) = makeNoMatchingCaseFallback vg
                let finalCaseIsKnownExhaustive =
                    Option.isNone mc.Guard
                    && Option.isSome (constructorPatternCoverage pattern)
                    && constructorMatchIsExhaustive cases
                let compileBodyWithGuard (vgBody: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                    match mc.Guard, pattern with
                    | None, AST.PList (_ :: _ as listPatterns) when not (listArmNeedsStages pattern) ->
                        compileListPatternWithChecks listPatterns scrutineeAtom' scrutType env body fallbackExpr vgBody
                    | None, AST.PListCons (headPatterns, tailPattern) when not (listArmNeedsStages pattern) ->
                        compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body fallbackExpr vgBody
                    | None, _ ->
                        extractAndCompileBody pattern body scrutineeAtom' scrutType env vgBody
                    | Some guardExpr, _ ->
                        extractAndCompileBodyWithGuard pattern guardExpr body scrutineeAtom' scrutType env vgBody fallbackExpr

                // The specialized list/list-cons compilers already emit complete
                // matching checks plus fallback, so a separate pre-comparison
                // condition here would duplicate work and code size.
                let canSkipPreComparison =
                    match mc.Guard, pattern with
                    | None, AST.PList (_ :: _)
                    | None, AST.PListCons _ -> not (listArmNeedsStages pattern)
                    | _ -> false

                if canSkipPreComparison then
                    compileBodyWithGuard vg1
                elif finalCaseIsKnownExhaustive then
                    extractAndCompileBody pattern body scrutineeAtom' scrutType env vg1
                elif patternStaticallyCannotMatchScrutinee pattern then
                    // Never taken, but the arm is still compiled: an unknown
                    // constructor or an ill-typed body must still be an error.
                    compileBodyWithGuard vg1 |> Result.map (fun (_, vg2) -> (fallbackExpr, vg2))
                else
                    buildPatternStages pattern scrutineeAtom' (Some scrutType) vg1
                    |> Result.bind (fun (stages, vg2) ->
                        match stages with
                        | [] ->
                            // Pattern always matches; guard (if any) decides between body/fallback.
                            compileBodyWithGuard vg1
                        | _ ->
                            compileBodyWithGuard vg2
                            |> Result.map (fun (thenExpr, vg3) ->
                                stagesToIf stages thenExpr fallbackExpr vg3))
            | mc :: rest ->
                // For pattern grouping, use first pattern for bindings but OR all patterns for comparison
                let firstPattern = AST.NonEmptyList.head mc.Patterns
                let body = mc.Body
                if patternAlwaysMatches firstPattern then
                    // Wildcard or var - matches everything, but may still need guard
                    match mc.Guard with
                    | None ->
                        extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                    | Some guardExpr ->
                        // Wildcard with guard - still need to check guard, fall through if false
                        buildChain rest vg
                        |> Result.bind (fun (elseExpr, vg1) ->
                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                else
                    // Non-empty list patterns need special handling with interleaved
                    // checks. The list compilers take the body as is, so an arm with a
                    // `when` guard goes through the stages below, which test the guard
                    // after the pattern and fall through to the rest when it is false.
                    match firstPattern with
                    | AST.PList (_ :: _ as listPatterns)
                        when not (listArmNeedsStages firstPattern) && Option.isNone mc.Guard ->
                        // Build the else branch first (rest of cases)
                        buildChain rest vg
                        |> Result.bind (fun (elseExpr, vg1) ->
                            // Use the new interleaved check-and-extract function
                            compileListPatternWithChecks listPatterns scrutineeAtom' scrutType env body elseExpr vg1)
                    | AST.PListCons (headPatterns, tailPattern)
                        when not (listArmNeedsStages firstPattern) && Option.isNone mc.Guard ->
                        // List cons pattern - needs interleaved checks
                        buildChain rest vg
                        |> Result.bind (fun (elseExpr, vg1) ->
                            compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body elseExpr vg1)
                    | _ when List.isEmpty mc.Patterns.Tail && not (patternStaticallyCannotMatchScrutinee firstPattern) ->
                        buildPatternStages firstPattern scrutineeAtom' (Some scrutType) vg
                        |> Result.bind (fun (stages, vg1) ->
                            match stages, mc.Guard with
                            | [], None ->
                                extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg1
                            | [], Some guardExpr ->
                                buildChain rest vg1
                                |> Result.bind (fun (elseExpr, vg2) ->
                                    extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg2 elseExpr)
                            | _, None ->
                                extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg1
                                |> Result.bind (fun (thenExpr, vg2) ->
                                    buildChain rest vg2
                                    |> Result.map (fun (elseExpr, vg3) ->
                                        stagesToIf stages thenExpr elseExpr vg3))
                            | _, Some guardExpr ->
                                buildChain rest vg1
                                |> Result.bind (fun (elseExpr, vg2) ->
                                    extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg2 elseExpr
                                    |> Result.map (fun (guardedBody, vg3) ->
                                        stagesToIf stages guardedBody elseExpr vg3)))
                    | _ ->
                        // Use pattern grouping: OR all patterns in the group
                        buildPatternGroupComparison (AST.NonEmptyList.toList mc.Patterns) scrutineeAtom' vg
                        |> Result.bind (fun cmpOpt ->
                            match cmpOpt with
                            | None ->
                                // Pattern always matches
                                match mc.Guard with
                                | None ->
                                    extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                                | Some guardExpr ->
                                    buildChain rest vg
                                    |> Result.bind (fun (elseExpr, vg1) ->
                                        extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                            | Some (condAtom, bindings, vg1) ->
                                match mc.Guard with
                                | None ->
                                    extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg1
                                    |> Result.bind (fun (thenExpr, vg2) ->
                                        buildChain rest vg2
                                        |> Result.map (fun (elseExpr, vg3) ->
                                            let ifExpr = ANF.If (condAtom, thenExpr, elseExpr)
                                            let finalExpr = wrapBindings bindings ifExpr
                                            (finalExpr, vg3)))
                                | Some guardExpr ->
                                    // Pattern match + guard: if pattern matches, bind, check guard
                                    buildChain rest vg1
                                    |> Result.bind (fun (elseExpr, vg2) ->
                                        extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg2 elseExpr
                                        |> Result.map (fun (guardedBody, vg3) ->
                                            let ifExpr = ANF.If (condAtom, guardedBody, elseExpr)
                                            let finalExpr = wrapBindings bindings ifExpr
                                            (finalExpr, vg3))))

        buildChain cases varGen1'
        |> Result.map (fun (chainExpr, varGen2) ->
            let chainWithPostBindings = wrapBindings scrutineePostBindings chainExpr
            let exprWithScrutinee = bindReturns scrutineeExpr (fun _ -> chainWithPostBindings)
            (exprWithScrutinee, varGen2)))
