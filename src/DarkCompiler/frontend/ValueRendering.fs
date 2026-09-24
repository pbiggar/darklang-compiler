// ValueRendering.fs - Interpreter-compatible result rendering
//
// Builds monomorphic Dark functions which render values at the eval boundary.
// Keeping recursion in ordinary Dark code gives tuples, lists, records, and sums
// one renderer on every native target instead of backend-specific shape switches.

module ValueRendering

open AST
open CheckedAST

type private SumVariant = CheckingTypes.SumVariantInfo
type private SumInfo = CheckingTypes.SumTypeInfo
type private RecordInfo = CheckingTypes.RecordTypeInfo

type private RenderEnv = {
    Records: Lazy<Map<string, RecordInfo>>
    Sums: Lazy<Map<string, SumInfo>>
}

type private RenderState = {
    Functions: Map<string, FunctionDef>
    Symbols: Symbols
}

let private freshBinding name state =
    let (id, symbols) = allocateBinding name state.Symbols
    (id, { state with Symbols = symbols })

let private constructorPattern typeName (variant: SumVariant) symbols fields =
    match tryFindConstructorId typeName variant.Name symbols with
    | Some id -> PConstructor (id, fields)
    | None -> Crash.crash $"Value renderer constructor was not interned: {typeName}.{variant.Name}"

let private args (values: Expr list) : NonEmptyList<Expr> =
    NonEmptyList.fromList values

let private resolveFunction symbols name =
    tryFindFunctionId name symbols
    |> Option.defaultWith (fun () ->
        Crash.crash $"Value renderer function was not interned: {name}")

let private call symbols (name: string) (values: Expr list) : Expr =
    Call (resolveFunction symbols name, args values)

let private concat symbols (parts: Expr list) : Expr =
    match parts with
    | [] -> StringLiteral ""
    | first :: rest ->
        // Every renderer fragment has an ASCII delimiter at each join: quotes,
        // punctuation, separators, or the edge of a canonical numeric value.
        // Those boundaries cannot compose under NFC, so retain the native raw
        // concat used before public StringConcat acquired normalization.
        List.fold (fun acc part -> call symbols "__string_concat_raw" [acc; part]) first rest

let private stableHash (value: string) : uint64 =
    value
    |> Seq.fold
        (fun hash ch -> (hash ^^^ uint64 (int ch)) * 1099511628211UL)
        14695981039346656037UL

let private rendererName (typ: SemanticType) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_value_{stableHash text:x16}"

let private listItemsRendererName (typ: SemanticType) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_list_items_{stableHash text:x16}"

let private dictItemsRendererName (typ: SemanticType) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_dict_items_{stableHash text:x16}"

let private runtimeFunctionNames =
    [ "__string_concat_raw"
      "Darklang.Stdlib.DateTime.toString"
      "Darklang.Stdlib.Dict.__renderKey"
      "Darklang.Stdlib.Dict.toList"
      "Darklang.Stdlib.Float.toString"
      "Darklang.Stdlib.Int.toString"
      "Darklang.Stdlib.Int128.toString"
      "Darklang.Stdlib.Int16.toString"
      "Darklang.Stdlib.Int32.toString"
      "Darklang.Stdlib.Int64.toString"
      "Darklang.Stdlib.Int8.toString"
      "Darklang.Stdlib.String.length"
      "Darklang.Stdlib.String.replaceAll"
      "Darklang.Stdlib.UInt128.toString"
      "Darklang.Stdlib.UInt16.toString"
      "Darklang.Stdlib.UInt32.toString"
      "Darklang.Stdlib.UInt64.toString"
      "Darklang.Stdlib.UInt8.toString"
      "Darklang.Stdlib.Uuid.toString" ]

let private applySubstitution (subst: Map<string, SemanticType>) (typ: SemanticType) : SemanticType =
    let rec apply typ =
        match typ with
        | TVar name
        | TInferenceVar (_, name) -> Map.tryFind name subst |> Option.defaultValue typ
        | TList elemType -> TList (apply elemType)
        | TStream elemType -> TStream (apply elemType)
        | TDict (keyType, valueType) -> TDict (apply keyType, apply valueType)
        | TFunction (paramTypes, returnType) -> TFunction (List.map apply paramTypes, apply returnType)
        | TTuple elemTypes -> TTuple (List.map apply elemTypes)
        | TRecord (name, typeArgs) -> TRecord (name, List.map apply typeArgs)
        | TSum (name, typeArgs) -> TSum (name, List.map apply typeArgs)
        | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit
        | TNever | TInternalRawPtr -> typ
    apply typ

let private typeSubstitution (typeParams: string list) (typeArgs: SemanticType list) : Map<string, SemanticType> =
    if List.length typeParams = List.length typeArgs then
        List.zip typeParams typeArgs |> Map.ofList
    else
        Crash.crash
            $"Value renderer type argument mismatch: params={List.length typeParams}, args={List.length typeArgs}"

let private escapedString symbols (quote: string) (value: Expr) : Expr =
    let replace oldValue newValue input =
        call symbols "Darklang.Stdlib.String.replaceAll" [input; StringLiteral oldValue; StringLiteral newValue]

    let escaped =
        value
        |> replace "\\" "\\\\"
        |> replace "\n" "\\n"
        |> replace "\r" "\\r"
        |> replace "\t" "\\t"
        |> replace quote ($"\\{quote}")

    concat symbols [StringLiteral quote; escaped; StringLiteral quote]

let private makeCase (pattern: Pattern) (body: Expr) : MatchCase =
    { Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }

let rec private canonicalRenderType (env: RenderEnv) (typ: SemanticType) : SemanticType =
    let canonical = canonicalRenderType env
    match typ with
    | TRecord (name, typeArgs) when Map.containsKey name env.Sums.Value ->
        TSum (name, List.map canonical typeArgs)
    | TRecord (name, typeArgs) -> TRecord (name, List.map canonical typeArgs)
    | TSum (name, typeArgs) -> TSum (name, List.map canonical typeArgs)
    | TTuple elementTypes -> TTuple (List.map canonical elementTypes)
    | TList elementType -> TList (canonical elementType)
    | TDict (keyType, valueType) -> TDict (canonical keyType, canonical valueType)
    | TFunction (parameterTypes, returnType) ->
        TFunction (List.map canonical parameterTypes, canonical returnType)
    | _ -> typ

let rec private ensureRenderer
    (env: RenderEnv)
    (typ: SemanticType)
    (state: RenderState)
    : string * RenderState =
    let typ = canonicalRenderType env typ
    let name = rendererName typ
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (functionId, symbols) = internFunction name state.Symbols
        let state = { state with Symbols = symbols }
        let (valueId, state) = freshBinding "__value" state
        // Reserve the name before descending so recursive sum types terminate.
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton (valueId, typ)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let (body, withDependencies) = renderBody env typ (Local valueId) reserved
        let completed = { placeholder with Body = body }
        (name, { withDependencies with Functions = Map.add name completed withDependencies.Functions })

and private renderCall
    (env: RenderEnv)
    (typ: SemanticType)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    let (name, nextState) = ensureRenderer env typ state
    (call nextState.Symbols name [value], nextState)

and private renderDelimited
    (env: RenderEnv)
    (items: (SemanticType * Expr) list)
    (state: RenderState)
    : Expr list * RenderState =
    let rec loop remaining currentState acc =
        match remaining with
        | [] -> (List.rev acc, currentState)
        | (itemType, itemExpr) :: rest ->
            let (rendered, nextState) = renderCall env itemType itemExpr currentState
            loop rest nextState (rendered :: acc)
    loop items state []

and private ensureListItemsRenderer
    (env: RenderEnv)
    (elemType: SemanticType)
    (state: RenderState)
    : string * RenderState =
    let listType = TList elemType
    let name = listItemsRendererName listType
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (functionId, symbols) = internFunction name state.Symbols
        let state = { state with Symbols = symbols }
        let (itemsId, state) = freshBinding "__items" state
        let (headId, state) = freshBinding "__head" state
        let (tailId, state) = freshBinding "__tail" state
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton (itemsId, listType)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let (renderedHead, withElemRenderer) = renderCall env elemType (Local headId) reserved
        let tailBody =
            Match (
                Local tailId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase PWildcard (concat reserved.Symbols [StringLiteral ", "; call reserved.Symbols name [Local tailId]]) ]
            )
        let body =
            Match (
                Local itemsId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVariable headId], PVariable tailId))
                      (concat withElemRenderer.Symbols [renderedHead; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withElemRenderer with Functions = Map.add name completed withElemRenderer.Functions })

and private ensureDictItemsRenderer
    (env: RenderEnv)
    (keyType: SemanticType)
    (valueType: SemanticType)
    (state: RenderState)
    : string * RenderState =
    let entryType = TTuple [keyType; valueType]
    let listType = TList entryType
    let name = dictItemsRendererName (TDict (keyType, valueType))
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (functionId, symbols) = internFunction name state.Symbols
        let state = { state with Symbols = symbols }
        let (entriesId, state) = freshBinding "__entries" state
        let (entryId, state) = freshBinding "__entry" state
        let (tailId, state) = freshBinding "__tail" state
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton (entriesId, listType)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let entryKey = TupleAccess (Local entryId, 0)
        let entryValue = TupleAccess (Local entryId, 1)
        let (renderedKey, withKeyRenderer) =
            match keyType with
            | TString -> (call reserved.Symbols "Darklang.Stdlib.Dict.__renderKey" [entryKey], reserved)
            | _ -> renderCall env keyType entryKey reserved
        let separator = if keyType = TString then " = " else ": "
        let (renderedValue, withValueRenderer) = renderCall env valueType entryValue withKeyRenderer
        let renderedEntry =
            concat withValueRenderer.Symbols [
                renderedKey
                StringLiteral separator
                renderedValue
            ]
        let tailBody =
            Match (
                Local tailId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase PWildcard (concat withValueRenderer.Symbols [StringLiteral "; "; call withValueRenderer.Symbols name [Local tailId]]) ]
            )
        let body =
            Match (
                Local entriesId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVariable entryId], PVariable tailId))
                      (concat withValueRenderer.Symbols [renderedEntry; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withValueRenderer with Functions = Map.add name completed withValueRenderer.Functions })

and private renderBody
    (env: RenderEnv)
    (typ: SemanticType)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    match typ with
    | TUnit -> (StringLiteral "()", state)
    | TBool -> (If (value, StringLiteral "true", StringLiteral "false"), state)
    | TInt8 -> (call state.Symbols "Darklang.Stdlib.Int8.toString" [value], state)
    | TInt16 -> (call state.Symbols "Darklang.Stdlib.Int16.toString" [value], state)
    | TInt32 -> (call state.Symbols "Darklang.Stdlib.Int32.toString" [value], state)
    | TInt64 -> (call state.Symbols "Darklang.Stdlib.Int64.toString" [value], state)
    | TInt -> (call state.Symbols "Darklang.Stdlib.Int.toString" [value], state)
    | TUInt8 -> (call state.Symbols "Darklang.Stdlib.UInt8.toString" [value], state)
    | TUInt16 -> (call state.Symbols "Darklang.Stdlib.UInt16.toString" [value], state)
    | TUInt32 -> (call state.Symbols "Darklang.Stdlib.UInt32.toString" [value], state)
    | TUInt64 -> (call state.Symbols "Darklang.Stdlib.UInt64.toString" [value], state)
    // Fixed-block 128-bit values cross the textual boundary through their
    // limb-based decimal formatters.
    | TInt128 -> (call state.Symbols "Darklang.Stdlib.Int128.toString" [value], state)
    | TUInt128 -> (call state.Symbols "Darklang.Stdlib.UInt128.toString" [value], state)
    | TFloat64 -> (call state.Symbols "Darklang.Stdlib.Float.toString" [value], state)
    | TString -> (escapedString state.Symbols "\"" value, state)
    | TChar -> (escapedString state.Symbols "'" value, state)
    | TDateTime -> (call state.Symbols "Darklang.Stdlib.DateTime.toString" [value], state)
    | TTuple elemTypes ->
        let items = elemTypes |> List.mapi (fun index elemType -> (elemType, TupleAccess (value, index)))
        let (rendered, nextState) = renderDelimited env items state
        let separated =
            rendered
            |> List.mapi (fun index expr -> if index = 0 then [expr] else [StringLiteral ", "; expr])
            |> List.concat
        (concat nextState.Symbols (StringLiteral "(" :: separated @ [StringLiteral ")"]), nextState)
    | TList elemType ->
        let (itemsName, nextState) = ensureListItemsRenderer env elemType state
        let typeName = CheckingDiagnostics.typeToString typ
        let body =
            Match (
                value,
                [ makeCase (PList []) (StringLiteral $"{typeName} []")
                  makeCase PWildcard (concat nextState.Symbols [StringLiteral "["; call nextState.Symbols itemsName [value]; StringLiteral "]"]) ]
            )
        (body, nextState)
    | TStream _ ->
        (StringLiteral "<stream>", state)
    // An unconstrained Dict value can only be the polymorphic empty literal;
    // no value renderer is needed because there are no entries to inspect.
    | TDict (TVar _, TVar _)
    | TDict (TInferenceVar _, TVar _)
    | TDict (TVar _, TInferenceVar _)
    | TDict (TInferenceVar _, TInferenceVar _) -> (StringLiteral "Dict { }", state)
    | TDict (keyType, valueType) ->
        let (itemsName, nextState) = ensureDictItemsRenderer env keyType valueType state
        let (entriesId, nextState) = freshBinding "__dict_entries" nextState
        let entries =
            TypeApp (
                resolveFunction nextState.Symbols "Darklang.Stdlib.Dict.toList",
                [keyType; valueType],
                NonEmptyList.singleton value
            )
        let body =
            Let (
                LPVariable entriesId,
                entries,
                Match (
                    Local entriesId,
                    [ makeCase (PList []) (StringLiteral "Dict { }")
                      makeCase
                          PWildcard
                          (concat nextState.Symbols
                              [ StringLiteral "Dict { "
                                call nextState.Symbols itemsName [Local entriesId]
                                StringLiteral " }" ]) ]
                )
            )
        (body, nextState)
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records.Value with
        | None ->
            Crash.crash $"Missing record metadata for value renderer: {typeName}"
        | Some recordInfo ->
            let fallbackTypeParams =
                recordInfo.Fields
                |> List.collect (fun (_, fieldType) ->
                    let rec collect typ =
                        match typ with
                        | TVar name
                        | TInferenceVar (_, name) -> [name]
                        | TList elem -> collect elem
                        | TDict (key, value) -> collect key @ collect value
                        | TFunction (parameters, result) -> List.collect collect parameters @ collect result
                        | TTuple elems -> List.collect collect elems
                        | TRecord (_, args) | TSum (_, args) -> List.collect collect args
                        | _ -> []
                    collect fieldType)
                |> List.distinct
            let typeParams =
                if List.isEmpty recordInfo.TypeParams then fallbackTypeParams
                else recordInfo.TypeParams
            let subst = typeSubstitution typeParams typeArgs
            let sortedFields =
                recordInfo.Fields
                |> List.mapi (fun index (name, typ) -> (index, name, typ))
                |> List.sortBy (fun (_, name, _) -> name)
            let rec renderFields remaining currentState acc =
                match remaining with
                | [] -> (List.rev acc, currentState)
                | (fieldIndex, fieldName, fieldType) :: rest ->
                    let concreteType = applySubstitution subst fieldType
                    let (fieldId, symbols) =
                        CheckedAST.internField typeName fieldName fieldIndex currentState.Symbols
                    let (rendered, nextState) =
                        renderCall
                            env
                            concreteType
                            (RecordAccess (value, fieldId))
                            { currentState with Symbols = symbols }
                    renderFields rest nextState ((fieldName, rendered) :: acc)
            let (renderedFields, nextState) = renderFields sortedFields state []
            let shortParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ", "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let typeText = CheckingDiagnostics.typeToString typ
            let short = concat nextState.Symbols (StringLiteral $"{typeText} {{ " :: shortParts @ [StringLiteral " }"])
            let longParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ",\n  "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let long = concat nextState.Symbols (StringLiteral $"{typeText} {{\n  " :: longParts @ [StringLiteral "\n}"])
            let shortName = "__record_short"
            let (shortId, nextState) = freshBinding shortName nextState
            (Let (
                LPVariable shortId,
                short,
                If (
                    BinOp (
                        Lte,
                        call nextState.Symbols "Darklang.Stdlib.String.length" [Local shortId],
                        BigIntLiteral (System.Numerics.BigInteger 80)
                    ),
                    Local shortId,
                    long
                )
             ), nextState)
    | TSum ("Uuid", []) -> (call state.Symbols "Darklang.Stdlib.Uuid.toString" [value], state)
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums.Value with
        | None -> Crash.crash $"Missing sum metadata for value renderer: {typeName}"
        | Some sumInfo ->
            let subst = typeSubstitution sumInfo.TypeParams typeArgs
            let typeText = CheckingDiagnostics.typeToString typ
            let rec buildCases (remaining: SumVariant list) currentState acc =
                match remaining with
                | [] -> (List.rev acc, currentState)
                | variant :: rest ->
                    match variant.Fields with
                    | [] ->
                        let case =
                            makeCase
                                (constructorPattern typeName variant currentState.Symbols [])
                                (StringLiteral $"{typeText}.{variant.Name}")
                        buildCases rest currentState (case :: acc)
                    | fieldTypes ->
                        let concreteTypes = fieldTypes |> List.map (applySubstitution subst)
                        let fieldNames = fieldTypes |> List.mapi (fun index _ -> $"__field_{variant.Tag}_{index}")
                        let (fieldIds, currentState) =
                            fieldNames
                            |> List.mapFold (fun state fieldName -> freshBinding fieldName state) currentState
                        let (renderedFields, nextState) =
                            List.zip concreteTypes fieldIds
                            |> List.map (fun (fieldType, fieldId) -> (fieldType, Local fieldId))
                            |> fun items -> renderDelimited env items currentState
                        let separated =
                            renderedFields
                            |> List.mapi (fun index rendered ->
                                if index = 0 then [rendered] else [StringLiteral ", "; rendered])
                            |> List.concat
                        let body =
                            concat nextState.Symbols (StringLiteral $"{typeText}.{variant.Name}(" :: separated @ [StringLiteral ")"])
                        let case =
                            makeCase
                                (constructorPattern typeName variant nextState.Symbols (List.map PVariable fieldIds))
                                body
                        buildCases rest nextState (case :: acc)
            let (cases, nextState) = buildCases (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
            (Match (value, cases), nextState)
    | TFunction _ -> (StringLiteral "(lambda)", state)
    | TBlob ->
        // The interpreter deliberately does not expose ephemeral Blob payloads
        // or process-local identities through value rendering.
        (StringLiteral "<Blob: ephemeral>", state)
    | TInternalRawPtr ->
        (call state.Symbols "Darklang.Stdlib.Int64.toString" [value], state)
    | TNever -> (StringLiteral "()", state)
    | TVar name -> Crash.crash $"Unresolved type variable in value renderer: {name}"
    | TInferenceVar (displayName, _) -> Crash.crash $"Unresolved inference variable in value renderer: {displayName}"

let rewriteProgram
    (recordMetadata: CheckingTypes.IndexedTypeRegistry)
    (sumMetadata: CheckingTypes.IndexedSumTypeRegistry)
    (programType: SemanticType)
    (Program (symbols, topLevels))
    : Program =
    let symbols =
        runtimeFunctionNames
        |> List.fold (fun current name -> internFunction name current |> snd) symbols
    // Type checking already built and overlaid these immutable indexes. Keep
    // them lazy so primitive renderers do not inspect declaration metadata.
    let records = lazy recordMetadata
    let sums = lazy sumMetadata

    let env = { Records = records; Sums = sums }
    let (renderName, state) =
        match programType with
        | TDateTime -> (None, { Functions = Map.empty; Symbols = symbols })
        | _ ->
            let (name, generatedState) =
                ensureRenderer env programType { Functions = Map.empty; Symbols = symbols }
            (Some name, generatedState)

    let tryNamedPartialName expr =
        match expr with
        | Lambda (parameters, returnAnnotation, body) ->
            let parameterIds =
                parameters
                |> NonEmptyList.toList
                |> List.choose (fun parameter ->
                    match parameter.Pattern with
                    | LPVariable id -> Some id
                    | _ -> None)
            let generatedPartial =
                List.length parameterIds = NonEmptyList.length parameters
                && (parameterIds
                    |> List.forall (fun id ->
                        bindingName id symbols
                        |> Option.map (fun name -> name.StartsWith "__partial_")
                        |> Option.defaultValue false))
            let callNameAndArgs =
                match body with
                | Call (name, callArgs) -> Some (name, NonEmptyList.toList callArgs)
                | TypeApp (name, _, callArgs) -> Some (name, NonEmptyList.toList callArgs)
                | _ -> None
            match generatedPartial, callNameAndArgs with
            | true, Some (name, callArgs) when List.length callArgs > List.length parameterIds ->
                let trailingArgs = callArgs |> List.skip (List.length callArgs - List.length parameterIds)
                if List.forall2 (fun arg parameterId -> arg = Local parameterId) trailingArgs parameterIds then
                    Some name
                else
                    None
            | _ -> None
        | _ -> None

    let rewriteExpression state expr =
        let rendered =
            match programType, expr, tryNamedPartialName expr with
            | TDateTime, _, _ ->
                (BoundaryRender (resolveFunction state.Symbols "Darklang.Stdlib.DateTime.toString", expr), state)
            | TFunction _, _, Some functionId ->
                let (id, next) = freshBinding "__rendered_named_partial" state
                let name =
                    functionName functionId state.Symbols
                    |> Option.defaultWith (fun () -> Crash.crash "Named partial function identity is absent from symbols")
                (Let (LPVariable id, expr, StringLiteral name), next)
            | TFunction _, FuncRef functionId, _ ->
                let (id, next) = freshBinding "__rendered_named_function" state
                let name =
                    functionName functionId state.Symbols
                    |> Option.defaultWith (fun () -> Crash.crash "Function identity is absent from symbols")
                (Let (LPVariable id, expr, StringLiteral name), next)
            | TFunction _, Lambda _, _ ->
                let (id, next) = freshBinding "__rendered_lambda" state
                (Let (LPVariable id, expr, StringLiteral "(lambda)"), next)
            | _ ->
                (BoundaryRender (
                    renderName
                    |> Option.map (resolveFunction state.Symbols)
                    |> Option.defaultWith (fun () -> Crash.crash "Missing boundary value renderer"),
                    expr
                 ), state)
        let (expression, next) = rendered
        (Expression expression, next)

    let (rewrittenTopLevels, finalState) =
        topLevels
        |> List.mapFold (fun currentState topLevel ->
            match topLevel with
            | Expression expr -> rewriteExpression currentState expr
            | other -> (other, currentState)) state
    let generatedFunctions = state.Functions |> Map.toList |> List.map (snd >> FunctionDef)
    let generatedTopLevels = generatedFunctions @ rewrittenTopLevels
    Program (finalState.Symbols, generatedTopLevels)
