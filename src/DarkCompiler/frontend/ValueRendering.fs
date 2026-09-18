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

let private args (values: Expr list) : NonEmptyList<Expr> =
    NonEmptyList.fromList values

let private call (name: string) (values: Expr list) : Expr =
    Call (name, args values)

let private concat (parts: Expr list) : Expr =
    match parts with
    | [] -> StringLiteral ""
    | first :: rest ->
        // Every renderer fragment has an ASCII delimiter at each join: quotes,
        // punctuation, separators, or the edge of a canonical numeric value.
        // Those boundaries cannot compose under NFC, so retain the native raw
        // concat used before public StringConcat acquired normalization.
        List.fold (fun acc part -> call "__string_concat_raw" [acc; part]) first rest

let private stableHash (value: string) : uint64 =
    value
    |> Seq.fold
        (fun hash ch -> (hash ^^^ uint64 (int ch)) * 1099511628211UL)
        14695981039346656037UL

let private rendererName (typ: Type) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_value_{stableHash text:x16}"

let private listItemsRendererName (typ: Type) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_list_items_{stableHash text:x16}"

let private dictItemsRendererName (typ: Type) : string =
    let text = CheckingDiagnostics.typeToString typ
    $"__dark_render_dict_items_{stableHash text:x16}"

let private applySubstitution (subst: Map<string, Type>) (typ: Type) : Type =
    let rec apply typ =
        match typ with
        | TVar name -> Map.tryFind name subst |> Option.defaultValue typ
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
        | TRuntimeError | TRawPtr -> typ
    apply typ

let private typeSubstitution (typeParams: string list) (typeArgs: Type list) : Map<string, Type> =
    if List.length typeParams = List.length typeArgs then
        List.zip typeParams typeArgs |> Map.ofList
    else
        Crash.crash
            $"Value renderer type argument mismatch: params={List.length typeParams}, args={List.length typeArgs}"

let private escapedString (quote: string) (value: Expr) : Expr =
    let replace oldValue newValue input =
        call "Darklang.Stdlib.String.replaceAll" [input; StringLiteral oldValue; StringLiteral newValue]

    let escaped =
        value
        |> replace "\\" "\\\\"
        |> replace "\n" "\\n"
        |> replace "\r" "\\r"
        |> replace "\t" "\\t"
        |> replace quote ($"\\{quote}")

    concat [StringLiteral quote; escaped; StringLiteral quote]

let private makeCase (pattern: Pattern) (body: Expr) : MatchCase =
    { Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }

let rec private canonicalRenderType (env: RenderEnv) (typ: Type) : Type =
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
    (typ: Type)
    (state: RenderState)
    : string * RenderState =
    let typ = canonicalRenderType env typ
    let name = rendererName typ
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (valueId, state) = freshBinding "__value" state
        // Reserve the name before descending so recursive sum types terminate.
        let placeholder = {
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
    (typ: Type)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    let (name, nextState) = ensureRenderer env typ state
    (call name [value], nextState)

and private renderDelimited
    (env: RenderEnv)
    (items: (Type * Expr) list)
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
    (elemType: Type)
    (state: RenderState)
    : string * RenderState =
    let listType = TList elemType
    let name = listItemsRendererName listType
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (itemsId, state) = freshBinding "__items" state
        let (headId, state) = freshBinding "__head" state
        let (tailId, state) = freshBinding "__tail" state
        let placeholder = {
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
                  makeCase PWildcard (concat [StringLiteral ", "; call name [Local tailId]]) ]
            )
        let body =
            Match (
                Local itemsId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVariable headId], PVariable tailId))
                      (concat [renderedHead; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withElemRenderer with Functions = Map.add name completed withElemRenderer.Functions })

and private ensureDictItemsRenderer
    (env: RenderEnv)
    (keyType: Type)
    (valueType: Type)
    (state: RenderState)
    : string * RenderState =
    let entryType = TTuple [keyType; valueType]
    let listType = TList entryType
    let name = dictItemsRendererName (TDict (keyType, valueType))
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let (entriesId, state) = freshBinding "__entries" state
        let (entryId, state) = freshBinding "__entry" state
        let (tailId, state) = freshBinding "__tail" state
        let placeholder = {
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
            | TString -> (call "Darklang.Stdlib.Dict.__renderKey" [entryKey], reserved)
            | _ -> renderCall env keyType entryKey reserved
        let separator = if keyType = TString then " = " else ": "
        let (renderedValue, withValueRenderer) = renderCall env valueType entryValue withKeyRenderer
        let renderedEntry =
            concat [
                renderedKey
                StringLiteral separator
                renderedValue
            ]
        let tailBody =
            Match (
                Local tailId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase PWildcard (concat [StringLiteral "; "; call name [Local tailId]]) ]
            )
        let body =
            Match (
                Local entriesId,
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVariable entryId], PVariable tailId))
                      (concat [renderedEntry; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withValueRenderer with Functions = Map.add name completed withValueRenderer.Functions })

and private renderBody
    (env: RenderEnv)
    (typ: Type)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    match typ with
    | TUnit -> (StringLiteral "()", state)
    | TBool -> (If (value, StringLiteral "true", StringLiteral "false"), state)
    | TInt8 -> (call "Darklang.Stdlib.Int8.toString" [value], state)
    | TInt16 -> (call "Darklang.Stdlib.Int16.toString" [value], state)
    | TInt32 -> (call "Darklang.Stdlib.Int32.toString" [value], state)
    | TInt64 -> (call "Darklang.Stdlib.Int64.toString" [value], state)
    | TInt -> (call "Darklang.Stdlib.Int.toString" [value], state)
    | TUInt8 -> (call "Darklang.Stdlib.UInt8.toString" [value], state)
    | TUInt16 -> (call "Darklang.Stdlib.UInt16.toString" [value], state)
    | TUInt32 -> (call "Darklang.Stdlib.UInt32.toString" [value], state)
    | TUInt64 -> (call "Darklang.Stdlib.UInt64.toString" [value], state)
    // Fixed-block 128-bit values cross the textual boundary through their
    // limb-based decimal formatters.
    | TInt128 -> (call "Darklang.Stdlib.Int128.toString" [value], state)
    | TUInt128 -> (call "Darklang.Stdlib.UInt128.toString" [value], state)
    | TFloat64 -> (call "Darklang.Stdlib.Float.toString" [value], state)
    | TString -> (escapedString "\"" value, state)
    | TChar -> (escapedString "'" value, state)
    | TDateTime -> (call "Darklang.Stdlib.DateTime.toString" [value], state)
    | TTuple elemTypes ->
        let items = elemTypes |> List.mapi (fun index elemType -> (elemType, TupleAccess (value, index)))
        let (rendered, nextState) = renderDelimited env items state
        let separated =
            rendered
            |> List.mapi (fun index expr -> if index = 0 then [expr] else [StringLiteral ", "; expr])
            |> List.concat
        (concat (StringLiteral "(" :: separated @ [StringLiteral ")"]), nextState)
    | TList elemType ->
        let (itemsName, nextState) = ensureListItemsRenderer env elemType state
        let typeName = CheckingDiagnostics.typeToString typ
        let body =
            Match (
                value,
                [ makeCase (PList []) (StringLiteral $"{typeName} []")
                  makeCase PWildcard (concat [StringLiteral "["; call itemsName [value]; StringLiteral "]"]) ]
            )
        (body, nextState)
    | TStream _ ->
        (StringLiteral "<stream>", state)
    // An unconstrained Dict value can only be the polymorphic empty literal;
    // no value renderer is needed because there are no entries to inspect.
    | TDict (TVar _, TVar _) -> (StringLiteral "Dict { }", state)
    | TDict (keyType, valueType) ->
        let (itemsName, nextState) = ensureDictItemsRenderer env keyType valueType state
        let (entriesId, nextState) = freshBinding "__dict_entries" nextState
        let entries = TypeApp ("Darklang.Stdlib.Dict.toList", [keyType; valueType], NonEmptyList.singleton value)
        let body =
            Let (
                LPVariable entriesId,
                entries,
                Match (
                    Local entriesId,
                    [ makeCase (PList []) (StringLiteral "Dict { }")
                      makeCase
                          PWildcard
                          (concat
                              [ StringLiteral "Dict { "
                                call itemsName [Local entriesId]
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
                        | TVar name -> [name]
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
            let sortedFields = recordInfo.Fields |> List.sortBy fst
            let rec renderFields remaining currentState acc =
                match remaining with
                | [] -> (List.rev acc, currentState)
                | (fieldName, fieldType) :: rest ->
                    let concreteType = applySubstitution subst fieldType
                    let (rendered, nextState) = renderCall env concreteType (RecordAccess (value, fieldName)) currentState
                    renderFields rest nextState ((fieldName, rendered) :: acc)
            let (renderedFields, nextState) = renderFields sortedFields state []
            let shortParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ", "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let typeText = CheckingDiagnostics.typeToString typ
            let short = concat (StringLiteral $"{typeText} {{ " :: shortParts @ [StringLiteral " }"])
            let longParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ",\n  "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let long = concat (StringLiteral $"{typeText} {{\n  " :: longParts @ [StringLiteral "\n}"])
            let shortName = "__record_short"
            let (shortId, nextState) = freshBinding shortName nextState
            (Let (
                LPVariable shortId,
                short,
                If (
                    BinOp (
                        Lte,
                        call "Darklang.Stdlib.String.length" [Local shortId],
                        BigIntLiteral (System.Numerics.BigInteger 80)
                    ),
                    Local shortId,
                    long
                )
             ), nextState)
    | TSum ("Uuid", []) -> (call "Darklang.Stdlib.Uuid.toString" [value], state)
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
                        let case = makeCase (PConstructor (variant.Name, [])) (StringLiteral $"{typeText}.{variant.Name}")
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
                            concat (StringLiteral $"{typeText}.{variant.Name}(" :: separated @ [StringLiteral ")"])
                        let case = makeCase (PConstructor (variant.Name, List.map PVariable fieldIds)) body
                        buildCases rest nextState (case :: acc)
            let (cases, nextState) = buildCases (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
            (Match (value, cases), nextState)
    | TFunction _ -> (StringLiteral "(lambda)", state)
    | TBlob ->
        // The interpreter deliberately does not expose ephemeral Blob payloads
        // or process-local identities through value rendering.
        (StringLiteral "<Blob: ephemeral>", state)
    | TRawPtr ->
        (call "Darklang.Stdlib.Int64.toString" [value], state)
    | TRuntimeError -> (StringLiteral "()", state)
    | TVar name -> Crash.crash $"Unresolved type variable in value renderer: {name}"

let rewriteProgram
    (recordMetadata: CheckingTypes.IndexedTypeRegistry)
    (sumMetadata: CheckingTypes.IndexedSumTypeRegistry)
    (baseFunctions: Map<string, Type>)
    (programType: Type)
    (Program (symbols, topLevels))
    : Program =
    // Type checking already built and overlaid these immutable indexes. Keep
    // them lazy so primitive renderers do not inspect declaration metadata.
    let records = lazy recordMetadata
    let sums = lazy sumMetadata

    let namedFunctions =
        lazy
            topLevels
            |> List.choose (function FunctionDef fn -> Some fn.Name | _ -> None)
            |> Set.ofList
            |> Set.union (baseFunctions |> Map.keys |> Set.ofSeq)
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
                (BoundaryRender ("Darklang.Stdlib.DateTime.toString", expr), state)
            | TFunction _, _, Some name ->
                let (id, next) = freshBinding "__rendered_named_partial" state
                (Let (LPVariable id, expr, StringLiteral name), next)
            | TFunction _, NamedValue name, _ when Set.contains name namedFunctions.Value ->
                let (id, next) = freshBinding "__rendered_named_function" state
                (Let (LPVariable id, expr, StringLiteral name), next)
            | TFunction _, FuncRef name, _ ->
                let (id, next) = freshBinding "__rendered_named_function" state
                (Let (LPVariable id, expr, StringLiteral name), next)
            | TFunction _, Lambda _, _ ->
                let (id, next) = freshBinding "__rendered_lambda" state
                (Let (LPVariable id, expr, StringLiteral "(lambda)"), next)
            | _ ->
                (BoundaryRender (
                    renderName |> Option.defaultWith (fun () -> Crash.crash "Missing boundary value renderer"),
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
    Program (finalState.Symbols, generatedFunctions @ rewrittenTopLevels)
