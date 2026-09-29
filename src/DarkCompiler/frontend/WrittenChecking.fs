// WrittenChecking.fs - Construct checked source from validated interpreter syntax.

module WrittenChecking

module WT = LibParser.WrittenTypes

type private Locals = Map<string, AST.SemanticType * AST.BindingId>
type private CheckedExpression = AST.SemanticType * CheckedAST.Expr * CheckedAST.Symbols

type private FunctionSignature = {
    Id: AST.FunctionId
    TypeParams: string list
    Parameters: AST.SemanticType list
    Return: AST.SemanticType
}

type private TypeKind = RecordKind | SumKind | AliasKind
type private TypeEntry = {
    Kind: TypeKind
    Params: string list
    Path: string list
    Definition: WT.TypeDefinition
}
type private TypeInventory = Map<string, TypeEntry>

type private Globals = {
    Functions: Map<string, FunctionSignature>
    Values: Locals
    Types: TypeInventory
    CollidingCases: Set<string>
    AllowInternal: bool
    TypeParams: Set<string>
    ModulePath: string list
    CurrentFunction: (AST.FunctionId * string * string list) option
}

/// Declarations retained by a checked source batch for separately checked
/// source units. The representation stays inside this direct checker.
type Environment = private Environment of Globals * CheckedAST.Symbols

let includeAllocatedFunctions (allocated: CheckedAST.Symbols) (Environment (globals, symbols)) : Environment =
    Environment (globals, CheckedAST.includeAllocatedFunctionNames allocated symbols)

let private emptyGlobals = {
    Functions = Map.empty
    Values = Map.empty
    Types = Map.empty
    CollidingCases = Set.empty
    AllowInternal = false
    TypeParams = Set.empty
    ModulePath = []
    CurrentFunction = None
}

let private collidingCaseNames (types: TypeInventory) =
    types
    |> Map.toList
    |> List.collect (fun (owner, entry) ->
        match entry.Definition with
        | WT.TDEnum cases -> cases |> List.map (fun (_, item) -> snd item.name, owner)
        | _ -> [])
    |> List.groupBy fst
    |> List.choose (fun (caseName, owners) ->
        if owners |> List.map snd |> List.distinct |> List.length > 1 then Some caseName
        else None)
    |> Set.ofList

let private caseTag colliding owner caseName ordinal =
    if Set.contains caseName colliding then AST.constructorRuntimeIdentity owner caseName
    else ordinal

let private qualifiedFnName (name: WT.QualifiedFnIdentifier) =
    (name.modules |> List.map (fun (identifier, _) -> identifier.name)) @ [name.fn.name]

let private restrictedIdentifier (allowInternal: bool) (segments: string list) =
    not allowInternal
    && (segments |> List.exists (fun segment -> segment.StartsWith "__"))

let private resolveFunction (globals: Globals) (segments: string list) =
    NameResolution.candidateSpellings
        NameResolution.ResolutionContext.Callable
        globals.ModulePath
        (String.concat "." segments)
    |> List.tryPick (fun candidate -> Map.tryFind candidate globals.Functions)

let private resolveValue (globals: Globals) (segments: string list) =
    NameResolution.candidateSpellings
        NameResolution.ResolutionContext.Value
        globals.ModulePath
        (String.concat "." segments)
    |> List.tryPick (fun candidate -> Map.tryFind candidate globals.Values)

let private requireType expected actual : Result<unit, string> =
    match expected with
    | Some typ when TypeUnification.reconcileTypes None typ actual |> Option.isNone ->
        Error $"Expected {typ}, got {actual}"
    | _ -> Ok ()

let private checkedLiteral expected symbols typ expression : Result<CheckedExpression, string> =
    requireType expected typ
    |> Result.map (fun () ->
        let resolvedType =
            if typ = AST.TNever then AST.TNever
            else
                expected
                |> Option.bind (fun wanted -> TypeUnification.reconcileTypes None wanted typ)
                |> Option.defaultValue typ
        resolvedType, expression, symbols)

/// Resolve the type syntax while retaining the compiler's nominal registry as
/// the authority for custom names. No source AST type is constructed here.
let rec typeReference
    (resolveCustom: string list -> string -> AST.SemanticType list -> Result<AST.SemanticType, string>)
    (typeParams: Set<string>)
    (reference: WT.TypeReference)
    : Result<AST.SemanticType, string> =
    let convert = typeReference resolveCustom typeParams
    let convertMany references = ResultList.traverse convert references
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
    | WT.TList (_, _, _, inner, _) -> convert inner |> Result.map AST.TList
    | WT.TDict (_, _, _, key, _, value, _) ->
        convert key
        |> Result.bind (fun keyType ->
            convert value
            |> Result.map (fun valueType -> AST.TDict (keyType, valueType)))
    | WT.TTuple (_, first, _, second, rest, _, _) ->
        first :: second :: (rest |> List.map snd)
        |> convertMany
        |> Result.map AST.TTuple
    | WT.TFn (_, arguments, ret) ->
        arguments
        |> List.map fst
        |> convertMany
        |> Result.bind (fun argumentTypes ->
            convert ret
            |> Result.map (fun returnType -> AST.TFunction (argumentTypes, returnType)))
    | WT.TVariable (_, _, (_, name)) ->
        if Set.contains name typeParams then Ok (AST.TVar name)
        else Error $"Undeclared type parameter '{name}'"
    | WT.TCustom name ->
        name.typeArgs
        |> convertMany
        |> Result.bind (fun args ->
            let modules = name.modules |> List.map (fun (identifier, _) -> identifier.name)
            resolveCustom modules name.typ.name args)

/// Function annotations may introduce type parameters without listing them
/// after the function name. Keep their first-seen order for positional calls.
let rec private collectWrittenTypeParams
    (found: string list)
    (reference: WT.TypeReference)
    : string list =
    let collect = collectWrittenTypeParams
    let collectMany items = List.fold collect found items
    match reference with
    | WT.TVariable (_, _, (_, name)) ->
        if List.contains name found then found else found @ [name]
    | WT.TList (_, _, _, inner, _) -> collect found inner
    | WT.TDict (_, _, _, key, _, value, _) ->
        collect (collect found key) value
    | WT.TTuple (_, first, _, second, rest, _, _) ->
        collectMany (first :: second :: (rest |> List.map snd))
    | WT.TFn (_, arguments, ret) ->
        collect (collectMany (arguments |> List.map fst)) ret
    | WT.TCustom name -> collectMany name.typeArgs
    | _ -> found

let private resolveWrittenType
    (allowInternal: bool)
    (types: TypeInventory)
    (modulePath: string list)
    (typeParams: Set<string>)
    (reference: WT.TypeReference)
    : Result<AST.SemanticType, string> =
    let rec convert seen path parameters syntax =
        typeReference (resolveCustom seen path) parameters syntax
    and resolveCustom seen path modules name args =
        match allowInternal, modules, name, args with
        | true, [], "RawPtr", [] -> Ok AST.TInternalRawPtr
        | false, [], "RawPtr", [] ->
            Error "RawPtr is reserved for compiler-internal source"
        | _, [], "Stream", [element] -> Ok (AST.TStream element)
        | _ ->
            let spelling = String.concat "." (modules @ [name])
            let candidates =
                NameResolution.candidateSpellings
                    NameResolution.ResolutionContext.Type path spelling
            match candidates |> List.tryPick (fun candidate ->
                Map.tryFind candidate types |> Option.map (fun metadata -> candidate, metadata)) with
            | None -> Error $"Unknown type '{spelling}'"
            | Some (canonical, entry) when List.length entry.Params <> List.length args ->
                Error $"Type '{canonical}' expects {List.length entry.Params} arguments, got {List.length args}"
            | Some (canonical, entry) ->
                match entry.Kind, entry.Definition with
                | RecordKind, _ -> Ok (AST.TRecord (canonical, args))
                | SumKind, _ -> Ok (AST.TSum (canonical, args))
                | AliasKind, WT.TDAlias target ->
                    if Set.contains canonical seen then Error $"Cyclic type alias '{canonical}'"
                    else
                        convert (Set.add canonical seen) entry.Path (Set.ofList entry.Params) target
                        |> Result.map (CheckingTypes.applySubst (Map.ofList (List.zip entry.Params args)))
                | AliasKind, _ -> Crash.crash "Alias type entry has no alias definition"
    convert Set.empty modulePath typeParams reference

let private findNamedType
    (globals: Globals)
    (name: WT.QualifiedTypeIdentifier)
    : Result<string * TypeEntry, string> =
    let modules = name.modules |> List.map (fun (identifier, _) -> identifier.name)
    let spelling = String.concat "." (modules @ [name.typ.name])
    NameResolution.candidateSpellings
        NameResolution.ResolutionContext.Type globals.ModulePath spelling
    |> List.tryPick (fun canonical ->
        Map.tryFind canonical globals.Types
        |> Option.map (fun entry -> canonical, entry))
    |> function
        | Some found -> Ok found
        | None -> Error $"Unknown type '{spelling}'"

let private resolveNamedType
    (globals: Globals)
    (expected: AST.SemanticType option)
    (name: WT.QualifiedTypeIdentifier)
    (inferredArgs: AST.SemanticType list option)
    : Result<string * TypeEntry * AST.SemanticType list, string> =
    findNamedType globals name
    |> Result.bind (fun (canonical, entry) ->
        name.typeArgs
        |> ResultList.traverse (resolveWrittenType globals.AllowInternal globals.Types globals.ModulePath Set.empty)
        |> Result.bind (fun givenArgs ->
            let args =
                match givenArgs, expected, inferredArgs with
                | [], Some wanted, Some inferred when TypeUnification.containsTVar wanted -> inferred
                | [], Some (AST.TRecord (_, wantedArgs)), Some inferred
                | [], Some (AST.TSum (_, wantedArgs)), Some inferred
                    when List.length wantedArgs <> List.length entry.Params -> inferred
                | [], Some (AST.TRecord (wanted, inferred)), _
                | [], Some (AST.TSum (wanted, inferred)), _ when wanted = canonical -> inferred
                | [], _, Some inferred -> inferred
                | _ -> givenArgs
            if List.length args <> List.length entry.Params then
                Error $"Type '{canonical}' expects {List.length entry.Params} arguments, got {List.length args}"
            else
                match entry.Definition with
                | WT.TDAlias target ->
                    resolveWrittenType globals.AllowInternal globals.Types
                        entry.Path (Set.ofList entry.Params) target
                    |> Result.map (CheckingTypes.applySubst (Map.ofList (List.zip entry.Params args)))
                    |> Result.bind (function
                        | AST.TRecord (targetName, targetArgs)
                        | AST.TSum (targetName, targetArgs) ->
                            match Map.tryFind targetName globals.Types with
                            | Some targetEntry -> Ok (targetName, targetEntry, targetArgs)
                            | None -> Error $"Unknown type '{targetName}'"
                        | _ -> Ok (canonical, entry, args))
                | _ -> Ok (canonical, entry, args)))

let private recordFields
    (allowInternal: bool)
    (types: TypeInventory)
    (entry: TypeEntry)
    (args: AST.SemanticType list)
    : Result<(string * AST.SemanticType) list, string> =
    match entry.Definition with
    | WT.TDRecord fields ->
        let substitution = Map.ofList (List.zip entry.Params args)
        fields
        |> ResultList.traverse (fun (field, _) ->
            resolveWrittenType allowInternal types entry.Path (Set.ofList entry.Params) field.typ
            |> Result.map (CheckingTypes.applySubst substitution)
            |> Result.map (fun typ -> snd field.name, typ))
    | _ -> Error "Expected a record type"

// Equality and dictionary keys use the field layout, even when source record
// declarations have different names. Keep ordinary assignments nominal.
let private structuralEqualityCompatible (globals: Globals) left right =
    let rec compatible seen left right =
        if TypeUnification.reconcileTypes None left right |> Option.isSome then true
        elif Set.contains (left, right) seen then true
        else
            let seen = Set.add (left, right) seen
            match left, right with
            | AST.TRecord (leftName, leftArgs), AST.TRecord (rightName, rightArgs) ->
                let fields name args =
                    Map.tryFind name globals.Types
                    |> Option.bind (fun entry ->
                        recordFields globals.AllowInternal globals.Types entry args
                        |> Result.toOption)
                match fields leftName leftArgs, fields rightName rightArgs with
                | Some leftFields, Some rightFields when List.length leftFields = List.length rightFields ->
                    let rightByName = Map.ofList rightFields
                    leftFields
                    |> List.forall (fun (fieldName, fieldType) ->
                        Map.tryFind fieldName rightByName
                        |> Option.exists (compatible seen fieldType))
                | _ -> false
            | _ -> false
    compatible Set.empty left right

let private convertStructuralRecord
    (globals: Globals)
    (targetType: AST.SemanticType)
    (actualType: AST.SemanticType)
    (expression: CheckedAST.Expr)
    (symbols: CheckedAST.Symbols)
    : Result<CheckedAST.Expr * CheckedAST.Symbols, string> =
    let rec convert target actual value symbols =
        if TypeUnification.reconcileTypes None target actual |> Option.isSome then
            Ok (value, symbols)
        else
            match target, actual with
            | AST.TRecord (targetName, targetArgs), AST.TRecord (actualName, actualArgs) ->
                match Map.tryFind targetName globals.Types, Map.tryFind actualName globals.Types with
                | Some targetEntry, Some actualEntry ->
                    recordFields globals.AllowInternal globals.Types targetEntry targetArgs
                    |> Result.bind (fun targetFields ->
                        recordFields globals.AllowInternal globals.Types actualEntry actualArgs
                        |> Result.bind (fun actualFields ->
                            let actualByName =
                                actualFields
                                |> List.indexed
                                |> List.map (fun (index, (name, typ)) -> name, (index, typ))
                                |> Map.ofList
                            let binding, afterBinding = CheckedAST.allocateBinding "__structural_record" symbols
                            let typeId, afterType = CheckedAST.internType targetName afterBinding
                            targetFields
                            |> List.indexed
                            |> List.fold (fun state (targetIndex, (fieldName, fieldType)) ->
                                state
                                |> Result.bind (fun (reversed, currentSymbols) ->
                                    match Map.tryFind fieldName actualByName with
                                    | None -> Error $"Record field '{fieldName}' is missing"
                                    | Some (actualIndex, actualFieldType) ->
                                        let sourceField, afterSource =
                                            CheckedAST.internField actualName fieldName actualIndex currentSymbols
                                        let source = CheckedAST.RecordAccess (CheckedAST.Local binding, sourceField)
                                        convert fieldType actualFieldType source afterSource
                                        |> Result.map (fun (converted, afterValue) ->
                                            let targetField, afterTarget =
                                                CheckedAST.internField targetName fieldName targetIndex afterValue
                                            (targetField, converted) :: reversed, afterTarget)))
                                (Ok ([], afterType))
                            |> Result.bind (fun (reversed, finalSymbols) ->
                                CheckedAST.completeRecordFields typeId targetFields.Length (List.rev reversed)
                                |> Result.map (fun fields ->
                                    let reference : CheckedAST.RecordReference =
                                        { TypeId = typeId
                                          TypeArgs = targetArgs |> List.map CheckedAST.checkedType }
                                    CheckedAST.Let (
                                        CheckedAST.LPVariable binding,
                                        value,
                                        CheckedAST.RecordLiteral (reference, fields)),
                                    finalSymbols))))
                | _ -> Error "Unknown structural record type"
            | _ -> Error $"Cannot structurally convert {actual} to {target}"
    convert targetType actualType expression symbols

let rec private checkLetPattern
    (pattern: WT.LetPattern)
    (typ: AST.SemanticType)
    (symbols: CheckedAST.Symbols)
    : Result<CheckedAST.LetPattern * Locals * CheckedAST.Symbols, string> =
    match pattern with
    | WT.LPUnit _ ->
        requireType (Some AST.TUnit) typ
        |> Result.map (fun () -> CheckedAST.LPUnit, Map.empty, symbols)
    | WT.LPWildcard _ -> Ok (CheckedAST.LPWildcard, Map.empty, symbols)
    | WT.LPVariable (_, name) ->
        let id, symbols' = CheckedAST.allocateBinding name symbols
        Ok (CheckedAST.LPVariable id, Map.ofList [name, (typ, id)], symbols')
    | WT.LPTuple (_, first, _, second, rest, _, _) ->
        let patterns = first :: second :: (rest |> List.map snd)
        match typ with
        | AST.TTuple types when List.length patterns = List.length types ->
            List.zip patterns types
            |> List.fold (fun result (item, itemType) ->
                result
                |> Result.bind (fun (checkedExpr, locals, currentSymbols) ->
                    checkLetPattern item itemType currentSymbols
                    |> Result.bind (fun (checkedItem, itemLocals, nextSymbols) ->
                        let duplicate =
                            itemLocals
                            |> Map.keys
                            |> Seq.tryFind (fun name -> Map.containsKey name locals)
                        match duplicate with
                        | Some name -> Error $"Duplicate binding '{name}' in tuple pattern"
                        | None ->
                            let merged = Map.fold (fun acc name value -> Map.add name value acc) locals itemLocals
                            Ok (checkedItem :: checkedExpr, merged, nextSymbols))))
                (Ok ([], Map.empty, symbols))
            |> Result.bind (fun (reversed, locals, nextSymbols) ->
                match List.rev reversed with
                | checkedFirst :: checkedSecond :: checkedRest ->
                    Ok (CheckedAST.LPTuple (checkedFirst, checkedSecond, checkedRest), locals, nextSymbols)
                | _ -> Error "Tuple pattern requires at least two elements")
        | _ -> Error "Tuple pattern does not match the value type"

let private mergePatternBindings (left: Locals) (right: Locals) : Result<Locals, string> =
    let duplicate = right |> Map.keys |> Seq.tryFind (fun name -> Map.containsKey name left)
    match duplicate with
    | Some name -> Error $"Duplicate binding '{name}' in match pattern"
    | None -> Ok (Map.fold (fun acc name binding -> Map.add name binding acc) left right)

let rec private checkMatchPattern
    (globals: Globals)
    (symbols: CheckedAST.Symbols)
    (prebound: Locals option)
    (expected: AST.SemanticType)
    (pattern: WT.MatchPattern)
    : Result<CheckedAST.Pattern * Locals * CheckedAST.Symbols, string> =
    let literal typ result =
        requireType (Some typ) expected
        |> Result.map (fun () -> result, Map.empty, symbols)
    let rec children currentSymbols bindings reversed remaining =
        match remaining with
        | [] -> Ok (List.rev reversed, bindings, currentSymbols)
        | (child, typ) :: tail ->
            checkMatchPattern globals currentSymbols prebound typ child
            |> Result.bind (fun (checkedPattern, childBindings, nextSymbols) ->
                mergePatternBindings bindings childBindings
                |> Result.bind (fun merged ->
                    children nextSymbols merged (checkedPattern :: reversed) tail))
    match pattern with
    | WT.MPVariable (_, "_") -> Ok (CheckedAST.PWildcard, Map.empty, symbols)
    | WT.MPVariable (_, name) ->
        let idResult =
            match prebound with
            | Some bindings ->
                match Map.tryFind name bindings with
                | Some (typ, id) when
                    TypeUnification.reconcileTypes None typ expected |> Option.isSome ->
                    Ok (id, symbols)
                | Some _ -> Error $"Or-pattern binding '{name}' has inconsistent types"
                | None -> Error $"Or-pattern binding '{name}' is missing in another branch"
            | None -> Ok (CheckedAST.allocateBinding name symbols)
        idResult
        |> Result.map (fun (id, nextSymbols) ->
            let bindingType =
                prebound
                |> Option.bind (Map.tryFind name)
                |> Option.bind (fun (typ, _) ->
                    TypeUnification.reconcileTypes None typ expected)
                |> Option.defaultValue expected
            CheckedAST.PVariable id, Map.ofList [name, (bindingType, id)], nextSymbols)
    | WT.MPUnit _ -> literal AST.TUnit CheckedAST.PUnit
    | WT.MPBool (_, value) -> literal AST.TBool (CheckedAST.PBool value)
    | WT.MPInt (_, (_, value)) -> literal AST.TInt (CheckedAST.PBigInt value)
    | WT.MPInt64 (_, (_, value), _) -> literal AST.TInt64 (CheckedAST.PInt64 value)
    | WT.MPInt8 (_, (_, value), _) -> literal AST.TInt8 (CheckedAST.PInt8Literal value)
    | WT.MPUInt8 (_, (_, value), _) -> literal AST.TUInt8 (CheckedAST.PUInt8Literal value)
    | WT.MPInt16 (_, (_, value), _) -> literal AST.TInt16 (CheckedAST.PInt16Literal value)
    | WT.MPUInt16 (_, (_, value), _) -> literal AST.TUInt16 (CheckedAST.PUInt16Literal value)
    | WT.MPInt32 (_, (_, value), _) -> literal AST.TInt32 (CheckedAST.PInt32Literal value)
    | WT.MPUInt32 (_, (_, value), _) -> literal AST.TUInt32 (CheckedAST.PUInt32Literal value)
    | WT.MPUInt64 (_, (_, value), _) -> literal AST.TUInt64 (CheckedAST.PUInt64Literal value)
    | WT.MPInt128 (_, (_, value), _) -> literal AST.TInt128 (CheckedAST.PInt128Literal value)
    | WT.MPUInt128 (_, (_, value), _) -> literal AST.TUInt128 (CheckedAST.PUInt128Literal value)
    | WT.MPString (_, contents, _, _) ->
        literal AST.TString (CheckedAST.PString (contents |> Option.map snd |> Option.defaultValue ""))
    | WT.MPChar (_, contents, _, _) ->
        literal AST.TChar (CheckedAST.PChar (contents |> Option.map snd |> Option.defaultValue ""))
    | WT.MPFloat (_, negative, whole, fraction) ->
        let text = (if negative then "-" else "") + whole + "." + fraction
        match System.Double.TryParse(
            text,
            System.Globalization.NumberStyles.Float,
            System.Globalization.CultureInfo.InvariantCulture) with
        | true, value -> literal AST.TFloat64 (CheckedAST.PFloat value)
        | false, _ -> Error $"Invalid Float pattern '{text}'"
    | WT.MPTuple (_, first, _, second, rest, _, _) ->
        let patterns = first :: second :: (rest |> List.map snd)
        let elementTypes =
            match expected with
            | AST.TTuple types when List.length patterns = List.length types -> Some types
            | AST.TVar name ->
                patterns
                |> List.mapi (fun index _ -> AST.TVar $"__tuple_elem_{name}_{index}")
                |> Some
            | AST.TInferenceVar (_, identity) ->
                patterns
                |> List.mapi (fun index _ ->
                    AST.TInferenceVar ($"tuple_element_{index}", $"{identity}/tuple_element_{index}"))
                |> Some
            | _ -> None
        match elementTypes with
        | Some types ->
            children symbols Map.empty [] (List.zip patterns types)
            |> Result.map (fun (checkedPatterns, bindings, nextSymbols) ->
                CheckedAST.PTuple checkedPatterns, bindings, nextSymbols)
        | None -> Error "Tuple pattern does not match the scrutinee type"
    | WT.MPList (_, contents, _, _) ->
        let elementType =
            match expected with
            | AST.TList typ -> Some typ
            | AST.TVar name -> Some (AST.TVar $"__list_elem_{name}")
            | AST.TInferenceVar (_, identity) ->
                Some (AST.TInferenceVar ("list_element", $"{identity}/list_element"))
            | _ -> None
        match elementType with
        | Some elementType ->
            contents
            |> List.map (fun (item, _) -> item, elementType)
            |> children symbols Map.empty []
            |> Result.map (fun (checkedPatterns, bindings, nextSymbols) ->
                CheckedAST.PList checkedPatterns, bindings, nextSymbols)
        | None -> Error "List pattern requires a list scrutinee"
    | WT.MPListCons (_, head, tail, _) ->
        let elementType =
            match expected with
            | AST.TList typ -> Some typ
            | AST.TVar name -> Some (AST.TVar $"__list_elem_{name}")
            | AST.TInferenceVar (_, identity) ->
                Some (AST.TInferenceVar ("list_element", $"{identity}/list_element"))
            | _ -> None
        match elementType with
        | Some elementType ->
            checkMatchPattern globals symbols prebound elementType head
            |> Result.bind (fun (checkedHead, headBindings, afterHead) ->
                checkMatchPattern globals afterHead prebound (AST.TList elementType) tail
                |> Result.bind (fun (checkedTail, tailBindings, afterTail) ->
                    mergePatternBindings headBindings tailBindings
                    |> Result.map (fun bindings ->
                        CheckedAST.PListCons ([checkedHead], checkedTail), bindings, afterTail)))
        | None -> Error "List cons pattern requires a list scrutinee"
    | WT.MPEnum (_, (_, caseName), fields) ->
        match expected with
        | AST.TSum (canonical, args) ->
            match Map.tryFind canonical globals.Types with
            | None -> Error $"Unknown enum type '{canonical}'"
            | Some entry ->
                match entry.Definition with
                | WT.TDEnum cases ->
                    match cases |> List.indexed |> List.tryFind (fun (_, (_, item)) -> snd item.name = caseName) with
                    | None -> Error $"Unknown constructor '{canonical}.{caseName}' in pattern"
                    | Some (ordinal, (_, item)) when List.length fields <> List.length item.fields ->
                        Error $"Constructor pattern '{canonical}.{caseName}' has wrong field count"
                    | Some (ordinal, (_, item)) ->
                        let substitution = Map.ofList (List.zip entry.Params args)
                        item.fields
                        |> ResultList.traverse (fun field ->
                            resolveWrittenType globals.AllowInternal globals.Types entry.Path (Set.ofList entry.Params) field.typ
                            |> Result.map (CheckingTypes.applySubst substitution))
                        |> Result.bind (fun fieldTypes ->
                            children symbols Map.empty [] (List.zip fields fieldTypes)
                            |> Result.map (fun (checkedFields, bindings, afterFields) ->
                                let tag = caseTag globals.CollidingCases canonical caseName ordinal
                                let constructorId, afterConstructor =
                                    CheckedAST.internConstructor canonical caseName tag afterFields
                                CheckedAST.PConstructor (constructorId, checkedFields), bindings, afterConstructor))
                | _ -> Error $"Type '{canonical}' is not an enum"
        | _ -> Error "Constructor pattern requires an enum scrutinee"
    | WT.MPOr (_, alternatives) ->
        match alternatives with
        | [] -> Error "Or-pattern requires at least one alternative"
        | first :: rest ->
            checkMatchPattern globals symbols prebound expected first
            |> Result.bind (fun (firstPattern, bindings, afterFirst) ->
                rest
                |> List.fold (fun result alternative ->
                    result
                    |> Result.bind (fun (reversed, commonBindings, currentSymbols) ->
                        checkMatchPattern globals currentSymbols (Some commonBindings) expected alternative
                        |> Result.bind (fun (checkedAlternative, otherBindings, nextSymbols) ->
                            if (otherBindings |> Map.keys |> Set.ofSeq)
                               <> (commonBindings |> Map.keys |> Set.ofSeq) then
                                Error "Every branch of an or-pattern must bind the same names"
                            else
                                otherBindings
                                |> Map.fold (fun result name (otherType, _) ->
                                    result
                                    |> Result.bind (fun merged ->
                                        match Map.tryFind name merged with
                                        | Some (priorType, id) ->
                                            match TypeUnification.reconcileTypes None priorType otherType with
                                            | Some commonType -> Ok (Map.add name (commonType, id) merged)
                                            | None -> Error $"Or-pattern binding '{name}' has inconsistent types"
                                        | None -> Error $"Or-pattern binding '{name}' is missing in another branch"))
                                    (Ok commonBindings)
                                |> Result.map (fun merged -> checkedAlternative :: reversed, merged, nextSymbols))))
                    (Ok ([firstPattern], bindings, afterFirst))
                |> Result.bind (fun (reversed, commonBindings, finalSymbols) ->
                    match AST.NonEmptyList.tryFromList (List.rev reversed) with
                    | Some patterns -> Ok (CheckedAST.POr patterns, commonBindings, finalSymbols)
                    | None -> Error "Or-pattern requires an alternative"))
    | WT.MPError _ -> Error "Invalid recovery pattern in validated source"

let rec private patternAlternatives pattern =
    match pattern with
    | CheckedAST.POr alternatives ->
        alternatives
        |> AST.NonEmptyList.toList
        |> List.collect patternAlternatives
    | other -> [other]

let private patternCoversAny = function
    | CheckedAST.PWildcard | CheckedAST.PVariable _ -> true
    | _ -> false

let rec private patternCoversLiteral (value: CheckedAST.Expr) (pattern: CheckedAST.Pattern) =
    if patternCoversAny pattern then true
    else
        match value, pattern with
        | CheckedAST.UnitLiteral, CheckedAST.PUnit -> true
        | CheckedAST.BoolLiteral left, CheckedAST.PBool right -> left = right
        | CheckedAST.Int64Literal left, CheckedAST.PInt64 right -> left = right
        | CheckedAST.BigIntLiteral left, CheckedAST.PBigInt right -> left = right
        | CheckedAST.StringLiteral left, CheckedAST.PString right -> left = right
        | CheckedAST.CharLiteral left, CheckedAST.PChar right -> left = right
        | CheckedAST.FloatLiteral left, CheckedAST.PFloat right -> left = right
        | CheckedAST.TupleLiteral tuple, CheckedAST.PTuple patterns ->
            let elements = CheckedAST.tupleElementsToList tuple
            List.length elements = List.length patterns
            && List.forall2 patternCoversLiteral elements patterns
        | CheckedAST.ListLiteral elements, CheckedAST.PList patterns ->
            List.length elements = List.length patterns
            && List.forall2 patternCoversLiteral elements patterns
        | _ -> false

let private matchIsExhaustive
    (globals: Globals)
    (symbols: CheckedAST.Symbols)
    (scrutineeType: AST.SemanticType)
    (scrutinee: CheckedAST.Expr)
    (cases: CheckedAST.MatchCase list)
    : bool =
    let patterns =
        cases
        |> List.collect (fun arm ->
            match arm.Guard with
            | Some _ -> []
            | None ->
                arm.Patterns
                |> AST.NonEmptyList.toList
                |> List.collect patternAlternatives)
    let contains predicate = List.exists predicate patterns
    let rec witnesses typ =
        match typ with
        | AST.TBool -> [CheckedAST.PBool true; CheckedAST.PBool false]
        | AST.TUnit -> [CheckedAST.PUnit]
        | AST.TList elementType ->
            CheckedAST.PList []
            :: (witnesses elementType
                |> List.collect (fun head ->
                    [ CheckedAST.PList [head]
                      CheckedAST.PListCons (
                          [head],
                          CheckedAST.PListCons ([head], CheckedAST.PWildcard)) ]))
        | AST.TSum (canonical, _) ->
            match Map.tryFind canonical globals.Types with
            | Some entry ->
                match entry.Definition with
                | WT.TDEnum variants ->
                    variants
                    |> List.indexed
                    |> List.map (fun (ordinal, (_, variant)) ->
                        let caseName = snd variant.name
                        let tag = caseTag globals.CollidingCases canonical caseName ordinal
                        let id, _ =
                            CheckedAST.internConstructor canonical caseName tag symbols
                        CheckedAST.PConstructor (
                            id,
                            List.replicate variant.fields.Length CheckedAST.PWildcard))
                | _ -> [CheckedAST.PWildcard]
            | None -> [CheckedAST.PWildcard]
        | AST.TTuple elementTypes ->
            elementTypes
            |> List.fold (fun products elementType ->
                [ for product in products do
                    for witness in witnesses elementType do
                        yield product @ [witness] ]) [[]]
            |> List.map CheckedAST.PTuple
        | _ -> [CheckedAST.PWildcard]
    let rec coversWitness pattern witness =
        if patternCoversAny pattern then true
        else
            match pattern, witness with
            | CheckedAST.PBool left, CheckedAST.PBool right -> left = right
            | CheckedAST.PUnit, CheckedAST.PUnit -> true
            | CheckedAST.PList [], CheckedAST.PList [] -> true
            | CheckedAST.PList left, CheckedAST.PList right
                when List.length left = List.length right ->
                List.forall2 coversWitness left right
            | CheckedAST.PListCons ([head], tail), CheckedAST.PList [single] ->
                coversWitness head single
                && coversWitness tail (CheckedAST.PList [])
            | CheckedAST.PListCons (leftHeads, leftTail),
              CheckedAST.PListCons (rightHeads, rightTail)
                when List.length leftHeads = List.length rightHeads ->
                List.forall2 coversWitness leftHeads rightHeads
                && coversWitness leftTail rightTail
            | CheckedAST.PConstructor (leftId, leftFields),
              CheckedAST.PConstructor (rightId, rightFields)
                when leftId = rightId && List.length leftFields = List.length rightFields ->
                List.forall2 coversWitness leftFields rightFields
            | CheckedAST.PTuple left, CheckedAST.PTuple right
                when List.length left = List.length right ->
                List.forall2 coversWitness left right
            | CheckedAST.POr alternatives, _ ->
                alternatives |> AST.NonEmptyList.toList
                |> List.exists (fun alternative -> coversWitness alternative witness)
            | _ -> false
    if contains patternCoversAny
       || contains (patternCoversLiteral scrutinee) then true
    else
        match scrutineeType with
        | AST.TUnit -> contains (function CheckedAST.PUnit -> true | _ -> false)
        | AST.TBool ->
            contains (function CheckedAST.PBool true -> true | _ -> false)
            && contains (function CheckedAST.PBool false -> true | _ -> false)
        | AST.TTuple _ ->
            witnesses scrutineeType
            |> List.forall (fun witness ->
                patterns |> List.exists (fun pattern -> coversWitness pattern witness))
        | AST.TSum (canonical, args) ->
            match Map.tryFind canonical globals.Types with
            | Some entry ->
                match entry.Definition with
                | WT.TDEnum variants ->
                    variants
                    |> List.forall (fun (_, variant) ->
                        let caseName = snd variant.name
                        let substitution = Map.ofList (List.zip entry.Params args)
                        let fieldTypes =
                            variant.fields
                            |> ResultList.traverse (fun field ->
                                resolveWrittenType globals.AllowInternal globals.Types
                                    entry.Path (Set.ofList entry.Params) field.typ
                                |> Result.map (CheckingTypes.applySubst substitution))
                        match fieldTypes with
                        | Error _ -> false
                        | Ok types ->
                            let fieldWitnesses =
                                types
                                |> List.fold (fun products fieldType ->
                                    [ for product in products do
                                        for witness in witnesses fieldType do
                                            yield product @ [witness] ]) [[]]
                            fieldWitnesses
                            |> List.forall (fun witnessFields ->
                                contains (function
                                    | CheckedAST.PConstructor (id, fields)
                                        when List.length fields = List.length witnessFields
                                          && CheckedAST.constructorInfo id symbols = Some (canonical, caseName) ->
                                        List.forall2 coversWitness fields witnessFields
                                    | _ -> false)))
                | _ -> false
            | None -> false
        | AST.TList _ ->
            witnesses scrutineeType
            |> List.forall (fun witness ->
                patterns |> List.exists (fun pattern -> coversWitness pattern witness))
        | _ -> false

let rec private checkExpression
    (globals: Globals)
    (locals: Locals)
    (symbols: CheckedAST.Symbols)
    (expected: AST.SemanticType option)
    (expression: WT.Expr)
    : Result<CheckedExpression, string> =
    let baseCheckedLiteral = checkedLiteral
    let checkedLiteral expected symbols typ checkedExpression =
        match expected, typ with
        | Some (AST.TVar wanted), AST.TVar actual
            when wanted <> actual
                 && Set.contains wanted globals.TypeParams
                 && Set.contains actual globals.TypeParams ->
            Error $"Expected type parameter '{wanted}', got '{actual}'"
        | _ -> baseCheckedLiteral expected symbols typ checkedExpression
    let literal = checkedLiteral expected symbols
    let check = checkExpression globals locals
    let rec checkMany
        (currentSymbols: CheckedAST.Symbols)
        (remaining: WT.Expr list)
        (expectedTypes: AST.SemanticType option list)
        (reversed: (AST.SemanticType * CheckedAST.Expr) list)
        : Result<(AST.SemanticType * CheckedAST.Expr) list * CheckedAST.Symbols, string> =
        match remaining with
        | [] -> Ok (List.rev reversed, currentSymbols)
        | item :: tail ->
            let itemExpected, restExpected =
                match expectedTypes with
                | first :: rest -> first, rest
                | [] -> None, []
            checkExpression globals locals currentSymbols itemExpected item
            |> Result.bind (fun (typ, checkedExpr, nextSymbols) ->
                checkMany nextSymbols tail restExpected ((typ, checkedExpr) :: reversed))
    match expression with
    | WT.EVariable (_, name) when restrictedIdentifier globals.AllowInternal [name] ->
        Error $"Internal identifier not allowed in user code: {name}"
    | WT.EFnName (_, name)
    | WT.EApply (_, WT.EFnName (_, name), _, _)
        when restrictedIdentifier globals.AllowInternal (qualifiedFnName name) ->
        let spelling = String.concat "." (qualifiedFnName name)
        Error $"Internal identifier not allowed in user code: {spelling}"
    | WT.EUnit _ -> literal AST.TUnit CheckedAST.UnitLiteral
    | WT.EBool (_, value) -> literal AST.TBool (CheckedAST.BoolLiteral value)
    | WT.EInt (_, (_, value)) -> literal AST.TInt (CheckedAST.BigIntLiteral value)
    | WT.EInt64 (_, (_, value), _) -> literal AST.TInt64 (CheckedAST.Int64Literal value)
    | WT.EInt8 (_, (_, value), _) -> literal AST.TInt8 (CheckedAST.Int8Literal value)
    | WT.EUInt8 (_, (_, value), _) -> literal AST.TUInt8 (CheckedAST.UInt8Literal value)
    | WT.EInt16 (_, (_, value), _) -> literal AST.TInt16 (CheckedAST.Int16Literal value)
    | WT.EUInt16 (_, (_, value), _) -> literal AST.TUInt16 (CheckedAST.UInt16Literal value)
    | WT.EInt32 (_, (_, value), _) -> literal AST.TInt32 (CheckedAST.Int32Literal value)
    | WT.EUInt32 (_, (_, value), _) -> literal AST.TUInt32 (CheckedAST.UInt32Literal value)
    | WT.EUInt64 (_, (_, value), _) -> literal AST.TUInt64 (CheckedAST.UInt64Literal value)
    | WT.EInt128 (_, (_, value), _) -> literal AST.TInt128 (CheckedAST.Int128Literal value)
    | WT.EUInt128 (_, (_, value), _) -> literal AST.TUInt128 (CheckedAST.UInt128Literal value)
    | WT.EFloat (_, negative, whole, fraction) ->
        let text = (if negative then "-" else "") + whole + "." + fraction
        match System.Double.TryParse(
            text,
            System.Globalization.NumberStyles.Float,
            System.Globalization.CultureInfo.InvariantCulture) with
        | true, value -> literal AST.TFloat64 (CheckedAST.FloatLiteral value)
        | false, _ -> Error $"Invalid Float literal '{text}'"
    | WT.EChar (_, Some (_, value), _, _) -> literal AST.TChar (CheckedAST.CharLiteral value)
    | WT.EChar _ -> Error "Empty Char literal"
    | WT.EString (_, _, segments, _, _) ->
        segments
        |> List.fold (fun result segment ->
            result
            |> Result.bind (fun (reversed, currentSymbols) ->
                match segment with
                | WT.StringText (_, value) ->
                    Ok (CheckedAST.StringText value :: reversed, currentSymbols)
                | WT.StringInterpolation (_, expr, _, _) ->
                    checkExpression globals locals currentSymbols None expr
                    |> Result.bind (fun (typ, checkedExpr, nextSymbols) ->
                        if TypeUnification.reconcileTypes None AST.TString typ |> Option.isSome then
                            Ok (CheckedAST.StringExpr checkedExpr :: reversed, nextSymbols)
                        else
                            Error $"Expected String in string interpolation, got {typ}")))
            (Ok ([], symbols))
        |> Result.bind (fun (reversed, nextSymbols) ->
            let parts = List.rev reversed
            let checkedExpr =
                match parts with
                | [] -> CheckedAST.StringLiteral ""
                | [CheckedAST.StringText value] -> CheckedAST.StringLiteral value
                | _ -> CheckedAST.InterpolatedString parts
            checkedLiteral expected nextSymbols AST.TString checkedExpr)
    | WT.EVariable (_, name) ->
        match Map.tryFind name locals |> Option.orElseWith (fun () -> resolveValue globals [name]) with
        | Some (typ, id) -> checkedLiteral expected symbols typ (CheckedAST.Local id)
        | None ->
            match resolveFunction globals [name] with
            | Some signature ->
                checkedLiteral expected symbols
                    (AST.TFunction (signature.Parameters, signature.Return))
                    (CheckedAST.FuncRef signature.Id)
            | None -> Error $"Unbound local variable '{name}'"
    | WT.ELambda (range, patterns, body, keywordFun, symbolArrow)
        when patterns
             |> List.exists (function WT.LPVariable (_, "") -> true | _ -> false) ->
        let effectivePatterns =
            patterns
            |> List.filter (function WT.LPVariable (_, "") -> false | _ -> true)
        match effectivePatterns with
        | [] -> check symbols expected body
        | _ -> check symbols expected (WT.ELambda (range, effectivePatterns, body, keywordFun, symbolArrow))
    | WT.ELambda (range, patterns, body, keywordFun, symbolArrow)
        when expected
             |> Option.exists (function
                 | AST.TFunction (argumentTypes, _) ->
                     not (List.isEmpty argumentTypes)
                     && List.length argumentTypes < List.length patterns
                 | _ -> false) ->
        match expected with
        | Some (AST.TFunction (argumentTypes, _)) ->
            let indexed = patterns |> List.indexed
            let outerPatterns =
                indexed
                |> List.choose (fun (index, pattern) ->
                    if index < List.length argumentTypes then Some pattern else None)
            let innerPatterns =
                indexed
                |> List.choose (fun (index, pattern) ->
                    if index >= List.length argumentTypes then Some pattern else None)
            let inner = WT.ELambda (range, innerPatterns, body, keywordFun, symbolArrow)
            check symbols expected
                (WT.ELambda (range, outerPatterns, inner, keywordFun, symbolArrow))
        | _ -> Crash.crash "Curried lambda guard lost its function expectation"
    | WT.ELambda (range, patterns, body, _, _) ->
        let knownParameterTypes =
            match expected with
            | Some (AST.TFunction (argumentTypes, _))
                when List.length argumentTypes = List.length patterns ->
                List.zip patterns argumentTypes
                |> List.choose (fun (pattern, typ) ->
                    match pattern with
                    | WT.LPVariable (_, name) when not (TypeUnification.containsTVar typ) ->
                        Some (name, typ)
                    | _ -> None)
                |> Map.ofList
            | _ -> Map.empty
        let numericOperandType expression =
            match expression with
            | WT.EInt _ -> Some AST.TInt
            | WT.EInt8 _ -> Some AST.TInt8
            | WT.EInt16 _ -> Some AST.TInt16
            | WT.EInt32 _ -> Some AST.TInt32
            | WT.EInt64 _ -> Some AST.TInt64
            | WT.EInt128 _ -> Some AST.TInt128
            | WT.EUInt8 _ -> Some AST.TUInt8
            | WT.EUInt16 _ -> Some AST.TUInt16
            | WT.EUInt32 _ -> Some AST.TUInt32
            | WT.EUInt64 _ -> Some AST.TUInt64
            | WT.EUInt128 _ -> Some AST.TUInt128
            | WT.EFloat _ -> Some AST.TFloat64
            | WT.EVariable (_, name) ->
                Map.tryFind name knownParameterTypes
                |> Option.orElseWith (fun () -> Map.tryFind name locals |> Option.map fst)
            | _ -> None
        let isNumericOperator = function
            | WT.InfixFnCall WT.ArithmeticPlus
            | WT.InfixFnCall WT.ArithmeticMinus
            | WT.InfixFnCall WT.ArithmeticMultiply
            | WT.InfixFnCall WT.ArithmeticDivide
            | WT.InfixFnCall WT.ArithmeticModulo
            | WT.InfixFnCall WT.ArithmeticPower
            | WT.InfixFnCall WT.ComparisonGreaterThan
            | WT.InfixFnCall WT.ComparisonGreaterThanOrEqual
            | WT.InfixFnCall WT.ComparisonLessThan
            | WT.InfixFnCall WT.ComparisonLessThanOrEqual -> true
            | _ -> false
        let rec inferParameterType name expression =
            match expression with
            | WT.EInfix (_, (_, op), WT.EVariable (_, leftName), right)
                when leftName = name && isNumericOperator op ->
                numericOperandType right
            | WT.EInfix (_, (_, op), left, WT.EVariable (_, rightName))
                when rightName = name && isNumericOperator op ->
                numericOperandType left
            | WT.EInfix (_, _, left, right) ->
                inferParameterType name left
                |> Option.orElseWith (fun () -> inferParameterType name right)
            | WT.EEnum (_, _, _, fields, _) ->
                fields |> List.tryPick (inferParameterType name)
            | WT.EApply (_, WT.EFnName (_, functionName), _, args) ->
                match resolveFunction globals (qualifiedFnName functionName) with
                | Some signature when List.length args = List.length signature.Parameters ->
                    List.zip args signature.Parameters
                    |> List.tryPick (fun (argument, parameterType) ->
                        match argument with
                        | WT.EVariable (_, argumentName)
                            when argumentName = name
                                 && not (TypeUnification.containsTVar parameterType) ->
                            Some parameterType
                        | _ -> inferParameterType name argument)
                | _ -> args |> List.tryPick (inferParameterType name)
            | WT.ELet (_, _, value, next, _, _)
            | WT.EStatement (_, value, next) ->
                inferParameterType name value
                |> Option.orElseWith (fun () -> inferParameterType name next)
            | WT.EIf (_, condition, thenBranch, elseBranch, _, _, _) ->
                inferParameterType name condition
                |> Option.orElseWith (fun () -> inferParameterType name thenBranch)
                |> Option.orElseWith (fun () -> elseBranch |> Option.bind (inferParameterType name))
            | _ -> None
        let lambdaExpected =
            match expected with
            | Some (AST.TFunction (argumentTypes, returnType))
                when List.length argumentTypes = List.length patterns ->
                let refinedTypes =
                    List.map2 (fun pattern argumentType ->
                        match pattern with
                        | WT.LPVariable (_, name) when TypeUnification.containsTVar argumentType ->
                            inferParameterType name body |> Option.defaultValue argumentType
                        | _ -> argumentType) patterns argumentTypes
                Some (AST.TFunction (refinedTypes, returnType))
            | Some (AST.TFunction _ as functionType) -> Some functionType
            | Some (AST.TVar _ | AST.TInferenceVar _) -> None
            | Some other -> Some other
            | None -> None
        let lambdaExpected =
            match lambdaExpected with
            | Some _ -> lambdaExpected
            | None ->
                let moduleKey = String.concat "_" globals.ModulePath
                let argumentTypes =
                    patterns
                    |> List.mapi (fun index pattern ->
                        let inferred =
                            match pattern with
                            | WT.LPVariable (_, name) -> inferParameterType name body
                            | _ -> None
                        inferred
                        |> Option.defaultWith (fun () ->
                            let name =
                                $"t$lambda_{moduleKey}_{range.start.row}_{range.start.column}_{index}"
                            AST.TInferenceVar (name, name)))
                let returnName =
                    $"t$lambda_return_{moduleKey}_{range.start.row}_{range.start.column}"
                Some (AST.TFunction (
                    argumentTypes,
                    AST.TInferenceVar (returnName, returnName)))
        match lambdaExpected with
        | Some (AST.TFunction (argumentTypes, returnType))
            when List.length patterns = List.length argumentTypes ->
            List.zip patterns argumentTypes
            |> List.fold (fun result (pattern, argumentType) ->
                result
                |> Result.bind (fun (reversed, capturedLocals, currentSymbols) ->
                    checkLetPattern pattern argumentType currentSymbols
                    |> Result.bind (fun (checkedPattern, bindings, nextSymbols) ->
                        mergePatternBindings capturedLocals bindings
                        |> Result.map (fun merged ->
                            let parameter: CheckedAST.LambdaParameter =
                                { Pattern = checkedPattern
                                  Type = CheckedAST.checkedType argumentType }
                            parameter :: reversed, merged, nextSymbols))))
                (Ok ([], Map.empty, symbols))
            |> Result.bind (fun (reversed, parameters, afterParameters) ->
                match AST.NonEmptyList.tryFromList (List.rev reversed) with
                | None -> Error "Lambda requires at least one parameter"
                | Some checkedParameters ->
                    let bodyLocals = Map.fold (fun acc name binding -> Map.add name binding acc) locals parameters
                    let bodyExpected =
                        match body with
                        | WT.EIf (_, _, _, Some _, _, _, _)
                            when TypeUnification.containsTVar returnType -> None
                        | _ -> Some returnType
                    checkExpression globals bodyLocals afterParameters bodyExpected body
                    |> Result.map (fun (bodyType, checkedBody, finalSymbols) ->
                        let inferredReturn =
                            if TypeUnification.containsTVar returnType then bodyType
                            else returnType
                        AST.TFunction (argumentTypes, inferredReturn),
                        CheckedAST.Lambda (checkedParameters, Some (CheckedAST.checkedType inferredReturn), checkedBody),
                        finalSymbols))
        | Some (AST.TFunction _) -> Error "Lambda parameter count mismatch"
        | _ -> Error "Lambda requires an expected function type"
    | WT.EFnName (_, name) ->
        let spelling = String.concat "." (qualifiedFnName name)
        match Map.tryFind spelling locals with
        | Some (typ, id) -> checkedLiteral expected symbols typ (CheckedAST.Local id)
        | None ->
            match spelling with
            | "Builtin.testNan" | "Builtin.testNan_v0" ->
                literal AST.TFloat64 (CheckedAST.FloatLiteral System.Double.NaN)
            | "Builtin.testInfinity" | "Builtin.testInfinity_v0" ->
                literal AST.TFloat64 (CheckedAST.FloatLiteral System.Double.PositiveInfinity)
            | "Builtin.blobEmpty" ->
                literal AST.TBlob (CheckedAST.BlobLiteral "")
            | _ ->
                match resolveFunction globals (qualifiedFnName name) with
                | Some signature ->
                    checkedLiteral expected symbols
                        (AST.TFunction (signature.Parameters, signature.Return))
                        (CheckedAST.FuncRef signature.Id)
                | None ->
                    match resolveValue globals (qualifiedFnName name) with
                    | Some (typ, id) -> checkedLiteral expected symbols typ (CheckedAST.Local id)
                    | None -> Error $"Unknown function or value '{spelling}'"
    | WT.EApply (range, WT.EFnName (nameRange, name), typeArgs, args)
        when List.isEmpty name.modules && Map.containsKey name.fn.name locals ->
        check symbols expected
            (WT.EApply (range, WT.EVariable (nameRange, name.fn.name), typeArgs, args))
    | WT.EApply (_, WT.EFnName (_, name), typeArgs, args)
        when qualifiedFnName name = ["Builtin"; "negate"] ->
        match typeArgs, args with
        | [], [argument] ->
            check symbols expected argument
            |> Result.bind (fun (typ, checkedArgument, afterArgument) ->
                match typ with
                | AST.TInt | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TInt128 | AST.TFloat64 ->
                    checkedLiteral expected afterArgument typ
                        (CheckedAST.UnaryOp (AST.Neg, checkedArgument))
                | _ -> Error $"Unary negation is unavailable for {typ}")
        | _, _ -> Error "Builtin.negate expects one argument"
    | WT.EApply (_, WT.EFnName (_, name), typeArgs, args)
        when qualifiedFnName name = ["Builtin"; "boolNot"]
             || qualifiedFnName name = ["Builtin"; "bitwiseNot"] ->
        let bitwise = name.fn.name = "bitwiseNot"
        match typeArgs, args with
        | [], [argument] ->
            check symbols None argument
            |> Result.bind (fun (typ, checkedArgument, afterArgument) ->
                match bitwise, typ with
                | _, AST.TNever ->
                    checkedLiteral expected afterArgument AST.TNever checkedArgument
                | false, AST.TBool ->
                    checkedLiteral expected afterArgument AST.TBool
                        (CheckedAST.UnaryOp (AST.Not, checkedArgument))
                | true, _ when
                    List.contains typ
                        [ AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64
                          AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32
                          AST.TUInt64; AST.TUInt128 ] ->
                    checkedLiteral expected afterArgument typ
                        (CheckedAST.UnaryOp (AST.BitNot, checkedArgument))
                | _ -> Error $"Unary operator is unavailable for {typ}")
        | _, _ -> Error "Unary operator expects one argument"
    | WT.EApply (_, WT.EFnName (_, name), typeArgs, args)
        when qualifiedFnName name = ["Builtin"; "unwrap"] ->
        match typeArgs, args with
        | [], [argument] ->
            let argumentExpected =
                match expected, argument with
                | Some outputType, WT.EEnum (_, _, (_, "None"), _, _)
                | Some outputType, WT.EEnum (_, _, (_, "Some"), _, _) ->
                    Ok (Some (AST.TSum ("Darklang.Stdlib.Option.Option", [outputType])))
                | Some outputType, WT.EEnum (_, _, (_, "Error"), _, _)
                | Some outputType, WT.EEnum (_, _, (_, "Ok"), _, _) ->
                    Ok (Some (AST.TSum (
                        "Darklang.Stdlib.Result.Result",
                        [outputType; AST.TInferenceVar ("unwrap_error", "unwrap_error")])))
                // The success payload does not exist for these constructors.
                // TNever keeps the type precise while the runtime reports the failed unwrap.
                | None, WT.EEnum (_, _, (_, "None"), [], _) ->
                    Ok (Some (AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TNever])))
                | None, WT.EEnum (_, _, (_, "Error"), [errorValue], _) ->
                    check symbols None errorValue
                    |> Result.map (fun (errorType, _, _) ->
                        Some (AST.TSum (
                            "Darklang.Stdlib.Result.Result",
                            [AST.TNever; errorType])))
                | _ -> Ok None
            argumentExpected
            |> Result.bind (fun expectedArgument -> check symbols expectedArgument argument)
            |> Result.bind (fun (argumentType, checkedArgument, afterArgument) ->
                let outputType =
                    match argumentType with
                    | AST.TSum ("Darklang.Stdlib.Option.Option", [valueType]) -> Ok valueType
                    | AST.TSum ("Darklang.Stdlib.Result.Result", [valueType; _]) -> Ok valueType
                    | _ -> Error $"Can only unwrap Options and Results, yet got {argumentType}"
                outputType
                |> Result.bind (fun typ ->
                    let functionId, afterFunction =
                        CheckedAST.internFunction "Builtin.unwrap" afterArgument
                    checkedLiteral expected afterFunction typ
                        (CheckedAST.Call (
                            functionId,
                            AST.NonEmptyList.singleton checkedArgument))))
        | [], _ -> Error $"Builtin.unwrap expects 1 argument, got {List.length args}"
        | _ -> Error "Builtin.unwrap does not accept type arguments"
    | WT.EApply (_, WT.EFnName (_, name), typeArgs, args)
        when resolveFunction globals (qualifiedFnName name)
             |> Option.exists (fun signature ->
                 not (List.isEmpty args) && List.length args < List.length signature.Parameters) ->
        match resolveFunction globals (qualifiedFnName name) with
        | None -> Error $"Unknown function '{name.fn.name}'"
        | Some signature ->
            let resolvedTypeArgs =
                typeArgs
                |> ResultList.traverse
                    (resolveWrittenType globals.AllowInternal globals.Types globals.ModulePath globals.TypeParams)
            resolvedTypeArgs
            |> Result.bind (fun givenTypeArgs ->
                if List.length givenTypeArgs <> List.length signature.TypeParams then
                    Error $"Partial application of generic function '{name.fn.name}' requires explicit type arguments"
                else
                    let substitution = Map.ofList (List.zip signature.TypeParams givenTypeArgs)
                    let concreteParameters =
                        signature.Parameters |> List.map (CheckingTypes.applySubst substitution)
                    let concreteReturn = CheckingTypes.applySubst substitution signature.Return
                    let providedTypes = concreteParameters |> List.take args.Length
                    List.zip args providedTypes
                    |> List.fold (fun result (argument, argumentType) ->
                        result
                        |> Result.bind (fun (reversed, currentSymbols) ->
                            check currentSymbols (Some argumentType) argument
                            |> Result.map (fun (_, checkedArgument, afterArgument) ->
                                checkedArgument :: reversed, afterArgument)))
                        (Ok ([], symbols))
                    |> Result.bind (fun (reversedArguments, afterArguments) ->
                        let arguments = List.rev reversedArguments
                        let captured, afterCaptures =
                            arguments
                            |> List.indexed
                            |> List.fold (fun (reversed, currentSymbols) (index, argument) ->
                                let id, nextSymbols =
                                    CheckedAST.allocateBinding $"__partial_capture_{index}" currentSymbols
                                (id, argument) :: reversed, nextSymbols)
                                ([], afterArguments)
                            |> fun (reversed, currentSymbols) -> List.rev reversed, currentSymbols
                        let remainingTypes = concreteParameters |> List.skip args.Length
                        let parameters, afterParameters =
                            remainingTypes
                            |> List.indexed
                            |> List.fold (fun (reversed, currentSymbols) (index, parameterType) ->
                                let id, nextSymbols =
                                    CheckedAST.allocateBinding $"__partial_arg_{index}" currentSymbols
                                let parameter: CheckedAST.LambdaParameter =
                                    { Pattern = CheckedAST.LPVariable id
                                      Type = CheckedAST.checkedType parameterType }
                                (id, parameter) :: reversed, nextSymbols)
                                ([], afterCaptures)
                            |> fun (reversed, currentSymbols) -> List.rev reversed, currentSymbols
                        match AST.NonEmptyList.tryFromList (parameters |> List.map snd) with
                        | None -> Error "Partial application requires remaining parameters"
                        | Some lambdaParameters ->
                            let callArguments =
                                (captured |> List.map (fun (id, _) -> CheckedAST.Local id))
                                @ (parameters |> List.map (fun (id, _) -> CheckedAST.Local id))
                                |> AST.NonEmptyList.fromList
                            let body =
                                if List.isEmpty signature.TypeParams then
                                    CheckedAST.Call (signature.Id, callArguments)
                                else
                                    CheckedAST.TypeApp (
                                        signature.Id,
                                        givenTypeArgs |> List.map CheckedAST.checkedType,
                                        callArguments)
                            let lambda =
                                CheckedAST.Lambda (
                                    lambdaParameters,
                                    Some (CheckedAST.checkedType concreteReturn),
                                    body)
                            let partial =
                                List.foldBack (fun (id, argument) expression ->
                                    CheckedAST.Let (CheckedAST.LPVariable id, argument, expression))
                                    captured lambda
                            checkedLiteral expected afterParameters
                                (AST.TFunction (remainingTypes, concreteReturn)) partial))
    | WT.EApply (range, WT.EFnName (_, name), typeArgs, args)
        when resolveFunction globals (qualifiedFnName name) |> Option.isSome ->
        match resolveFunction globals (qualifiedFnName name) with
        | None -> Error (sprintf "Unknown function '%s'" (String.concat "." (qualifiedFnName name)))
        | Some signature ->
            let signature =
                if List.isEmpty signature.TypeParams then signature
                else
                    let scope = String.concat "." (qualifiedFnName name)
                    let freshParams, renaming =
                        CheckingDiagnostics.freshenTypeParams (Some scope) signature.TypeParams
                    { signature with
                        TypeParams = freshParams
                        Parameters =
                            signature.Parameters
                            |> List.map (CheckingDiagnostics.applyTypeVarRenaming renaming)
                        Return =
                            CheckingDiagnostics.applyTypeVarRenaming renaming signature.Return }
            let normalizedArgs =
                match signature.Parameters, args with
                | [], [WT.EUnit _] -> []
                | [AST.TUnit], [] -> [WT.EUnit range]
                | _ -> args
            let isStdlibEquality =
                match qualifiedFnName name with
                | ["Stdlib"; "equals"] | ["Stdlib"; "notEquals"] -> true
                | _ -> false
            let isDictKeyOperation =
                match qualifiedFnName name, signature.Parameters with
                | "Stdlib" :: "Dict" :: _, AST.TDict (keyType, _) :: parameterType :: _ ->
                    parameterType = keyType
                | _ -> false
            if not (List.isEmpty typeArgs)
               && List.length typeArgs <> List.length signature.TypeParams then
                Error $"Function '{name.fn.name}' expects {List.length signature.TypeParams} type arguments"
            elif List.length normalizedArgs <> List.length signature.Parameters then
                Error $"Function '{name.fn.name}' expects {List.length signature.Parameters} arguments"
            else
                let explicitArgs =
                    typeArgs
                    |> ResultList.traverse
                        (resolveWrittenType globals.AllowInternal globals.Types globals.ModulePath globals.TypeParams)
                let lookaheadTypeArgs =
                    if List.isEmpty signature.TypeParams then None
                    else
                        List.zip normalizedArgs signature.Parameters
                        |> List.choose (fun (argument, parameterType) ->
                            match argument with
                            | WT.ELambda _ -> None
                            | _ ->
                                match check symbols None argument with
                                | Ok (actualType, _, _) -> Some (parameterType, actualType)
                                | Error _ -> None)
                        |> function
                            | [] -> None
                            | pairs ->
                                TypeUnification.inferTypeArgs
                                    signature.TypeParams
                                    (pairs |> List.map fst)
                                    (pairs |> List.map snd)
                                    (Some signature.Return)
                                    expected
                                |> Result.toOption
                let rec checkArgs givenTypeArgs currentSymbols reversed remaining =
                    match remaining with
                    | [] -> Ok (List.rev reversed, currentSymbols)
                    | (arg, parameterType) :: tail ->
                        let structuralDictKey =
                            isDictKeyOperation
                            && List.length reversed = 1
                            && (match reversed with
                                | (AST.TDict (AST.TRecord _, _), _) :: _ -> true
                                | _ -> false)
                        let argumentExpected =
                            if isStdlibEquality || structuralDictKey then None
                            elif List.isEmpty signature.TypeParams then Some parameterType
                            elif not (List.isEmpty givenTypeArgs) then
                                let substitution = Map.ofList (List.zip signature.TypeParams givenTypeArgs)
                                Some (CheckingTypes.applySubst substitution parameterType)
                            else
                                let checkedPrefix = List.rev reversed |> List.map fst
                                let parameterPrefix =
                                    signature.Parameters |> List.take checkedPrefix.Length
                                let inferredPrefix =
                                    TypeUnification.inferTypeArgs
                                        signature.TypeParams parameterPrefix checkedPrefix
                                        (Some signature.Return) expected
                                let inferred =
                                    match inferredPrefix, lookaheadTypeArgs with
                                    | Ok prefix, Some later ->
                                        Some (List.map2 (fun first second ->
                                            if TypeUnification.containsTVar first then second else first)
                                            prefix later)
                                    | Ok prefix, None -> Some prefix
                                    | Error _, Some later -> Some later
                                    | Error _, None -> None
                                inferred
                                |> Option.map (fun typeArguments ->
                                    let substitution =
                                        Map.ofList (List.zip signature.TypeParams typeArguments)
                                    CheckingTypes.applySubst substitution parameterType)
                        check currentSymbols argumentExpected arg
                        |> Result.bind (fun (argType, checkedArg, nextSymbols) ->
                            checkArgs givenTypeArgs nextSymbols ((argType, checkedArg) :: reversed) tail)
                explicitArgs
                |> Result.bind (fun givenTypeArgs ->
                    if not (List.isEmpty givenTypeArgs)
                       && List.length givenTypeArgs <> List.length signature.TypeParams then
                        Error $"Function '{name.fn.name}' expects {List.length signature.TypeParams} type arguments"
                    else
                        checkArgs givenTypeArgs symbols [] (List.zip normalizedArgs signature.Parameters)
                        |> Result.bind (fun (checkedArgs, finalSymbols) ->
                            let rec abortingArgument preceding remaining =
                                match remaining with
                                | [] -> None
                                | (AST.TNever, abort) :: _ ->
                                    let prefix =
                                        preceding
                                        |> List.rev
                                        |> List.filter (function
                                            | CheckedAST.ListLiteral [] -> false
                                            | _ -> true)
                                    Some (List.foldBack (fun prior next ->
                                        CheckedAST.Sequence (prior, next)) prefix abort)
                                | (_, checkedArg) :: tail ->
                                    abortingArgument (checkedArg :: preceding) tail
                            match abortingArgument [] checkedArgs with
                            | Some abort -> checkedLiteral expected finalSymbols AST.TNever abort
                            | None ->
                                let mixedCharString =
                                    match checkedArgs |> List.map fst with
                                    | [AST.TChar; AST.TString]
                                    | [AST.TString; AST.TChar] -> true
                                    | _ -> false
                                let inferenceTypes =
                                    match checkedArgs |> List.map fst with
                                    | first :: second :: rest when isStdlibEquality
                                        && structuralEqualityCompatible globals first second ->
                                        first :: first :: rest
                                    | (AST.TDict (keyType, _) as dictType) :: second :: rest when isDictKeyOperation
                                        && structuralEqualityCompatible globals keyType second ->
                                        dictType :: keyType :: rest
                                    | types -> types
                                let inferred =
                                    if isStdlibEquality && mixedCharString then
                                        Error "Cannot compare Char and String"
                                    elif List.isEmpty signature.TypeParams then Ok []
                                    elif not (List.isEmpty givenTypeArgs) then Ok givenTypeArgs
                                    else
                                        TypeUnification.inferTypeArgs
                                            signature.TypeParams
                                            signature.Parameters
                                            inferenceTypes
                                            (Some signature.Return)
                                            expected
                                inferred
                                |> Result.bind (fun inferredTypeArgs ->
                                    match globals.CurrentFunction with
                                    | Some (currentId, currentName, currentParams)
                                        when currentId = signature.Id
                                             && inferredTypeArgs
                                                <> (currentParams |> List.map AST.TVar) ->
                                        Error $"Polymorphic recursion is not supported inside recursive group member: {currentName}"
                                    | _ -> Ok inferredTypeArgs)
                                |> Result.bind (fun inferredTypeArgs ->
                                    let substitution = Map.ofList (List.zip signature.TypeParams inferredTypeArgs)
                                    let parameterTypes =
                                        signature.Parameters |> List.map (CheckingTypes.applySubst substitution)
                                    let returnType = CheckingTypes.applySubst substitution signature.Return
                                    let mismatched =
                                        List.zip checkedArgs parameterTypes
                                        |> List.indexed
                                        |> List.tryFind (fun (index, ((argType, _), parameterType)) ->
                                            not ((isStdlibEquality && index = 1
                                                  || isDictKeyOperation && index = 1)
                                                 && structuralEqualityCompatible globals parameterType argType)
                                            && (TypeUnification.reconcileTypes None parameterType argType
                                                |> Option.isNone))
                                    match mismatched with
                                    | Some (_, ((actual, _), wanted)) ->
                                        Error $"Function argument type mismatch: expected {wanted}, got {actual}"
                                    | None ->
                                        let convertedArgs =
                                            List.zip checkedArgs parameterTypes
                                            |> List.indexed
                                            |> List.fold (fun state (index, ((actualType, value), targetType)) ->
                                                state
                                                |> Result.bind (fun (reversed, currentSymbols) ->
                                                    if index = 1
                                                       && (isStdlibEquality || isDictKeyOperation)
                                                       && (TypeUnification.reconcileTypes None targetType actualType
                                                           |> Option.isNone)
                                                       && structuralEqualityCompatible globals targetType actualType then
                                                        convertStructuralRecord globals targetType actualType value currentSymbols
                                                        |> Result.map (fun (converted, afterConversion) ->
                                                            converted :: reversed, afterConversion)
                                                    else Ok (value :: reversed, currentSymbols)))
                                                (Ok ([], finalSymbols))
                                        convertedArgs
                                        |> Result.bind (fun (reversedArgs, afterConversion) ->
                                            let callArgs =
                                                List.rev reversedArgs
                                                |> function
                                                    | [] -> AST.NonEmptyList.singleton CheckedAST.UnitLiteral
                                                    | nonempty -> AST.NonEmptyList.fromList nonempty
                                            let call =
                                                if List.isEmpty signature.TypeParams then
                                                    CheckedAST.Call (signature.Id, callArgs)
                                                else
                                                    CheckedAST.TypeApp (
                                                        signature.Id,
                                                        inferredTypeArgs |> List.map CheckedAST.checkedType,
                                                        callArgs)
                                            checkedLiteral expected afterConversion returnType call)
                                    )))
    | WT.EApply (range, (WT.ELambda (_, patterns, _, _, _) as target), [], args)
        when not (List.isEmpty patterns) && List.length args > List.length patterns ->
        let firstArgs = args |> List.take patterns.Length
        let remainingArgs = args |> List.skip patterns.Length
        check symbols expected
            (WT.EApply (
                range,
                WT.EApply (range, target, [], firstArgs),
                [],
                remainingArgs))
    | WT.EApply (_, (WT.ELambda (_, patterns, _, _, _) as target), [], args)
        when List.length patterns = List.length args ->
        args
        |> List.fold (fun result arg ->
            result
            |> Result.bind (fun (reversedTypes, reversedArgs, currentSymbols) ->
                check currentSymbols None arg
                |> Result.map (fun (argType, checkedArg, nextSymbols) ->
                    argType :: reversedTypes, checkedArg :: reversedArgs, nextSymbols)))
            (Ok ([], [], symbols))
        |> Result.bind (fun (reversedTypes, reversedArgs, afterArgs) ->
            let argumentTypes = List.rev reversedTypes
            let returnType =
                expected
                |> Option.defaultValue (AST.TInferenceVar ("t$applied_lambda_return", "t$applied_lambda_return"))
            check afterArgs (Some (AST.TFunction (argumentTypes, returnType))) target
            |> Result.bind (fun (targetType, checkedTarget, finalSymbols) ->
                match targetType, AST.NonEmptyList.tryFromList (List.rev reversedArgs) with
                | AST.TFunction (_, resultType), Some checkedArgs ->
                    checkedLiteral expected finalSymbols resultType
                        (CheckedAST.Apply (checkedTarget, checkedArgs))
                | _ -> Error "An indirect call requires at least one argument"))
    | WT.EApply (_, target, typeArgs, args) ->
        if not (List.isEmpty typeArgs) then
            Error "Type arguments require a named function"
        else
            check symbols None target
            |> Result.bind (fun (targetType, checkedTarget, afterTarget) ->
                match targetType with
                | AST.TFunction (parameters, returnType)
                    when not (List.isEmpty args) && List.length args < List.length parameters ->
                    let providedTypes = parameters |> List.take args.Length
                    List.zip args providedTypes
                    |> List.fold (fun result (argument, parameterType) ->
                        result
                        |> Result.bind (fun (reversed, currentSymbols) ->
                            check currentSymbols (Some parameterType) argument
                            |> Result.map (fun (_, checkedArgument, nextSymbols) ->
                                checkedArgument :: reversed, nextSymbols)))
                        (Ok ([], afterTarget))
                    |> Result.bind (fun (reversedArguments, afterArguments) ->
                        let targetId, afterTargetCapture =
                            CheckedAST.allocateBinding "__partial_target" afterArguments
                        let captures, afterCaptures =
                            List.rev reversedArguments
                            |> List.indexed
                            |> List.fold (fun (reversed, currentSymbols) (index, argument) ->
                                let id, nextSymbols =
                                    CheckedAST.allocateBinding $"__partial_capture_{index}" currentSymbols
                                (id, argument) :: reversed, nextSymbols)
                                ([], afterTargetCapture)
                            |> fun (reversed, currentSymbols) -> List.rev reversed, currentSymbols
                        let remainingTypes = parameters |> List.skip args.Length
                        let remainingParameters, finalSymbols =
                            remainingTypes
                            |> List.indexed
                            |> List.fold (fun (reversed, currentSymbols) (index, parameterType) ->
                                let id, nextSymbols =
                                    CheckedAST.allocateBinding $"__partial_arg_{index}" currentSymbols
                                let parameter: CheckedAST.LambdaParameter =
                                    { Pattern = CheckedAST.LPVariable id
                                      Type = CheckedAST.checkedType parameterType }
                                (id, parameter) :: reversed, nextSymbols)
                                ([], afterCaptures)
                            |> fun (reversed, currentSymbols) -> List.rev reversed, currentSymbols
                        let callArguments =
                            (captures |> List.map (fun (id, _) -> CheckedAST.Local id))
                            @ (remainingParameters |> List.map (fun (id, _) -> CheckedAST.Local id))
                            |> AST.NonEmptyList.fromList
                        let lambda =
                            CheckedAST.Lambda (
                                remainingParameters |> List.map snd |> AST.NonEmptyList.fromList,
                                Some (CheckedAST.checkedType returnType),
                                CheckedAST.Apply (CheckedAST.Local targetId, callArguments))
                        let partial =
                            List.foldBack (fun (id, argument) expression ->
                                CheckedAST.Let (CheckedAST.LPVariable id, argument, expression))
                                captures lambda
                        let partial =
                            CheckedAST.Let (CheckedAST.LPVariable targetId, checkedTarget, partial)
                        checkedLiteral expected finalSymbols
                            (AST.TFunction (remainingTypes, returnType)) partial)
                | AST.TFunction (parameters, returnType) when List.length parameters = List.length args ->
                    List.zip args parameters
                    |> List.fold (fun result (arg, parameterType) ->
                        result
                        |> Result.bind (fun (reversed, currentSymbols) ->
                            check currentSymbols (Some parameterType) arg
                            |> Result.map (fun (_, checkedArg, nextSymbols) ->
                                checkedArg :: reversed, nextSymbols)))
                        (Ok ([], afterTarget))
                    |> Result.bind (fun (reversed, finalSymbols) ->
                        match AST.NonEmptyList.tryFromList (List.rev reversed) with
                        | Some checkedArgs ->
                            checkedLiteral expected finalSymbols returnType
                                (CheckedAST.Apply (checkedTarget, checkedArgs))
                        | None -> Error "An indirect call requires at least one argument")
                | AST.TFunction _ -> Error "Indirect call argument count mismatch"
                | _ -> Error "Expression is not callable")
    | WT.EEnum (_, typeName, (_, caseName), fields, _) ->
        let rec caseFieldsForInference entry =
            match entry.Definition with
            | WT.TDEnum cases ->
                match cases |> List.tryFind (fun (_, item) -> snd item.name = caseName) with
                | Some (_, variant) when List.length variant.fields = List.length fields ->
                    variant.fields
                    |> ResultList.traverse (fun field ->
                        resolveWrittenType globals.AllowInternal globals.Types
                            entry.Path (Set.ofList entry.Params) field.typ)
                    |> Result.map Some
                | _ -> Ok None
            | WT.TDAlias target ->
                resolveWrittenType globals.AllowInternal globals.Types
                    entry.Path (Set.ofList entry.Params) target
                |> Result.bind (function
                    | AST.TSum (targetName, targetArgs) ->
                        match Map.tryFind targetName globals.Types with
                        | Some targetEntry when List.length targetEntry.Params = List.length targetArgs ->
                            let substitution = Map.ofList (List.zip targetEntry.Params targetArgs)
                            caseFieldsForInference targetEntry
                            |> Result.map (Option.map (List.map
                                (CheckingTypes.applyTypeArguments substitution)))
                        | _ -> Error $"Unknown enum type '{targetName}'"
                    | _ -> Error "Expected an enum type")
            | _ -> Ok None
        let resolvedType =
            if typeName.typ.name <> "" then
                let inferredArgs =
                    if not (List.isEmpty typeName.typeArgs) then
                        Ok None
                    else
                        findNamedType globals typeName
                        |> Result.bind (fun (_, entry) ->
                            if List.isEmpty entry.Params then Ok None
                            else
                                caseFieldsForInference entry
                                |> Result.bind (function
                                    | Some fieldTypes ->
                                        checkMany symbols fields [] []
                                        |> Result.bind (fun (checkedFields, _) ->
                                            TypeUnification.inferTypeArgs
                                                entry.Params fieldTypes (checkedFields |> List.map fst)
                                                None None
                                            |> Result.map Some)
                                    | None -> Ok None))
                inferredArgs
                |> Result.bind (resolveNamedType globals expected typeName)
            else
                match expected with
                | Some (AST.TSum (canonical, args)) ->
                    match Map.tryFind canonical globals.Types with
                    | Some entry -> Ok (canonical, entry, args)
                    | None -> Error $"Unknown type '{canonical}'"
                | _ ->
                    let candidates =
                        globals.Types
                        |> Map.toList
                        |> List.choose (fun (canonical, entry) ->
                            match entry.Definition with
                            | WT.TDEnum cases when
                                cases |> List.exists (fun (_, item) -> snd item.name = caseName) ->
                                Some (canonical, entry)
                            | _ -> None)
                    match candidates with
                    | [canonical, entry] ->
                        match entry.Definition with
                        | WT.TDEnum cases ->
                            match cases |> List.tryFind (fun (_, item) -> snd item.name = caseName) with
                            | None -> Error $"Unknown constructor '{caseName}'"
                            | Some (_, variant) when List.length variant.fields <> List.length fields ->
                                Error $"Constructor '{caseName}' expects {List.length variant.fields} fields"
                            | Some (_, variant) ->
                                variant.fields
                                |> ResultList.traverse (fun field ->
                                    resolveWrittenType globals.AllowInternal globals.Types
                                        entry.Path (Set.ofList entry.Params) field.typ)
                                |> Result.bind (fun fieldTypes ->
                                    checkMany symbols fields [] []
                                    |> Result.bind (fun (checkedFields, _) ->
                                        TypeUnification.inferTypeArgs
                                            entry.Params fieldTypes (checkedFields |> List.map fst)
                                            None None
                                        |> Result.map (fun args -> canonical, entry, args)))
                        | _ -> Error $"Type '{canonical}' is not an enum"
                    | [] -> Error $"Unknown constructor '{caseName}'"
                    | _ -> Error $"Constructor '{caseName}' requires an expected enum type"
        resolvedType
        |> Result.bind (fun (canonical, entry, args) ->
            match entry.Definition with
            | WT.TDEnum cases ->
                match cases
                      |> List.indexed
                      |> List.tryFind (fun (_, (_, item)) -> snd item.name = caseName) with
                | None -> Error $"Unknown constructor '{canonical}.{caseName}'"
                | Some (ordinal, (_, item)) when List.length fields <> List.length item.fields ->
                    Error $"Constructor '{canonical}.{caseName}' expects {List.length item.fields} fields"
                | Some (ordinal, (_, item)) ->
                    let substitution = Map.ofList (List.zip entry.Params args)
                    item.fields
                    |> ResultList.traverse (fun field ->
                        resolveWrittenType globals.AllowInternal globals.Types entry.Path (Set.ofList entry.Params) field.typ
                        |> Result.map (CheckingTypes.applySubst substitution))
                    |> Result.bind (fun fieldTypes ->
                        List.zip fields fieldTypes
                        |> List.fold (fun result (fieldExpr, fieldType) ->
                            result
                            |> Result.bind (fun (reversed, currentSymbols) ->
                                check currentSymbols (Some fieldType) fieldExpr
                                |> Result.map (fun (actualType, checkedField, nextSymbols) ->
                                    (actualType, checkedField) :: reversed, nextSymbols)))
                            (Ok ([], symbols))
                        |> Result.bind (fun (reversed, afterFields) ->
                            let checkedFields = List.rev reversed
                            let inferredFields =
                                List.zip fieldTypes (checkedFields |> List.map fst)
                                |> List.fold (fun result (declaredType, actualType) ->
                                    result
                                    |> Result.bind (fun substitution ->
                                        let patternType =
                                            CheckingTypes.applySubst substitution declaredType
                                        TypeUnification.unifyTypes patternType actualType
                                        |> Result.map (fun inferred ->
                                            Map.fold (fun acc name typ -> Map.add name typ acc)
                                                substitution inferred)))
                                    (Ok Map.empty)
                            inferredFields
                            |> Result.bind (fun fieldSubstitution ->
                                let resolvedArgs =
                                    args |> List.map (CheckingTypes.applySubst fieldSubstitution)
                                let tag = caseTag globals.CollidingCases canonical caseName ordinal
                                let constructorId, afterConstructor =
                                    CheckedAST.internConstructor canonical caseName tag afterFields
                                let typeId, afterType = CheckedAST.internType canonical afterConstructor
                                let reference : CheckedAST.ConstructorReference =
                                    { TypeId = typeId
                                      ConstructorId = constructorId
                                      TypeArgs = resolvedArgs |> List.map CheckedAST.checkedType }
                                checkedLiteral expected afterType (AST.TSum (canonical, resolvedArgs))
                                    (CheckedAST.Constructor (reference, checkedFields |> List.map snd)))))
            | _ -> Error $"Type '{canonical}' is not an enum")
    | WT.ERecord (_, name, fields, _, _) ->
        let rec declarationFieldsForInference entry =
            match entry.Definition with
            | WT.TDRecord declaredFields ->
                declaredFields
                |> ResultList.traverse (fun (field, _) ->
                    resolveWrittenType globals.AllowInternal globals.Types
                        entry.Path (Set.ofList entry.Params) field.typ
                    |> Result.map (fun typ -> snd field.name, typ))
            | WT.TDAlias target ->
                resolveWrittenType globals.AllowInternal globals.Types
                    entry.Path (Set.ofList entry.Params) target
                |> Result.bind (function
                    | AST.TRecord (targetName, targetArgs) ->
                        match Map.tryFind targetName globals.Types with
                        | Some targetEntry when List.length targetEntry.Params = List.length targetArgs ->
                            let substitution = Map.ofList (List.zip targetEntry.Params targetArgs)
                            declarationFieldsForInference targetEntry
                            |> Result.map (List.map (fun (fieldName, typ) ->
                                fieldName, CheckingTypes.applyTypeArguments substitution typ))
                        | _ -> Error $"Unknown record type '{targetName}'"
                    | _ -> Error "Expected a record type")
            | _ -> Error "Expected a record type"
        let inferArgsFromFields () =
            findNamedType globals name
            |> Result.bind (fun (canonical, entry) ->
                let concreteExpected =
                    match expected with
                    | Some (AST.TRecord (wanted, args) as typ)
                    | Some (AST.TSum (wanted, args) as typ) ->
                        wanted = canonical
                        && List.length args = List.length entry.Params
                        && not (TypeUnification.containsTVar typ)
                    | _ -> false
                if concreteExpected || List.isEmpty entry.Params then Ok None
                else
                    declarationFieldsForInference entry
                    |> Result.bind (fun declared ->
                        let byName = Map.ofList declared
                        fields
                        |> List.fold (fun result (_, (_, fieldName), value) ->
                            result
                            |> Result.bind (fun (patterns, actuals, currentSymbols) ->
                                match Map.tryFind fieldName byName with
                                | None -> Error $"Unknown field '{fieldName}' on {canonical}"
                                | Some patternType ->
                                    check currentSymbols None value
                                    |> Result.map (fun (actualType, _, nextSymbols) ->
                                        patternType :: patterns,
                                        actualType :: actuals,
                                        nextSymbols)))
                            (Ok ([], [], symbols))
                        |> Result.bind (fun (patterns, actuals, _) ->
                            TypeUnification.inferTypeArgs
                                entry.Params (List.rev patterns) (List.rev actuals)
                                None None
                            |> Result.map Some)))
        let inferredArgs =
            match name.typeArgs with
            | [] -> inferArgsFromFields ()
            | _ -> Ok None
        inferredArgs
        |> Result.bind (resolveNamedType globals expected name)
        |> Result.bind (fun (canonical, entry, args) ->
            recordFields globals.AllowInternal globals.Types entry args
            |> Result.bind (fun declarations ->
                let declared =
                    declarations
                    |> List.indexed
                    |> List.map (fun (index, (fieldName, typ)) -> fieldName, (index, typ))
                    |> Map.ofList
                let typeId, afterType = CheckedAST.internType canonical symbols
                fields
                |> List.fold (fun result (_, (_, fieldName), value) ->
                    result
                    |> Result.bind (fun (reversed, seen, currentSymbols) ->
                        if Set.contains fieldName seen then Error $"Duplicate record field '{fieldName}'"
                        else
                            match Map.tryFind fieldName declared with
                            | None -> Error $"Unknown field '{fieldName}' on {canonical}"
                            | Some (index, fieldType) ->
                                check currentSymbols (Some fieldType) value
                                |> Result.map (fun (_, checkedValue, afterValue) ->
                                    let fieldId, afterField =
                                        CheckedAST.internField canonical fieldName index afterValue
                                    (fieldId, checkedValue) :: reversed,
                                    Set.add fieldName seen,
                                    afterField)))
                    (Ok ([], Set.empty, afterType))
                |> Result.bind (fun (reversed, _, afterFields) ->
                    CheckedAST.completeRecordFields typeId (List.length declarations) (List.rev reversed)
                    |> Result.bind (fun complete ->
                        let reference : CheckedAST.RecordReference =
                            { TypeId = typeId
                              TypeArgs = args |> List.map CheckedAST.checkedType }
                        checkedLiteral expected afterFields (AST.TRecord (canonical, args))
                            (CheckedAST.RecordLiteral (reference, complete))))))
    | WT.ERecordFieldAccess (_, record, (_, fieldName), _) ->
        check symbols None record
        |> Result.bind (fun (recordType, checkedRecord, afterRecord) ->
            match recordType with
            | AST.TRecord (canonical, args) ->
                match Map.tryFind canonical globals.Types with
                | None -> Error $"Unknown record type '{canonical}'"
                | Some entry ->
                    recordFields globals.AllowInternal globals.Types entry args
                    |> Result.bind (fun fields ->
                        match fields |> List.indexed |> List.tryFind (fun (_, (name, _)) -> name = fieldName) with
                        | None -> Error $"Unknown field '{fieldName}' on {canonical}"
                        | Some (index, (_, fieldType)) ->
                            let fieldId, afterField =
                                CheckedAST.internField canonical fieldName index afterRecord
                            checkedLiteral expected afterField fieldType
                                (CheckedAST.RecordAccess (checkedRecord, fieldId)))
            | _ -> Error "Field access requires a record value")
    | WT.ERecordUpdate (_, record, updates, _, _, _) ->
        check symbols None record
        |> Result.bind (fun (recordType, checkedRecord, afterRecord) ->
            match recordType with
            | AST.TRecord (canonical, args) ->
                match Map.tryFind canonical globals.Types with
                | None -> Error $"Unknown record type '{canonical}'"
                | Some entry ->
                    recordFields globals.AllowInternal globals.Types entry args
                    |> Result.bind (fun fields ->
                        let declared =
                            fields
                            |> List.indexed
                            |> List.map (fun (index, (name, typ)) -> name, (index, typ))
                            |> Map.ofList
                        updates
                        |> List.fold (fun result ((_, fieldName), _, value) ->
                            result
                            |> Result.bind (fun (reversed, seen, currentSymbols) ->
                                if Set.contains fieldName seen then Error $"Duplicate record field '{fieldName}'"
                                else
                                    match Map.tryFind fieldName declared with
                                    | None -> Error $"Unknown field '{fieldName}' on {canonical}"
                                    | Some (index, fieldType) ->
                                        check currentSymbols (Some fieldType) value
                                        |> Result.map (fun (_, checkedValue, afterValue) ->
                                            let fieldId, afterField =
                                                CheckedAST.internField canonical fieldName index afterValue
                                            (fieldId, checkedValue) :: reversed,
                                            Set.add fieldName seen,
                                            afterField)))
                            (Ok ([], Set.empty, afterRecord))
                        |> Result.bind (fun (reversed, _, afterUpdates) ->
                            checkedLiteral expected afterUpdates recordType
                                (CheckedAST.RecordUpdate (checkedRecord, List.rev reversed))))
            | _ -> Error "Record update requires a record value")
    | WT.EMatch (_, scrutinee, cases, _, _) ->
        check symbols None scrutinee
        |> Result.bind (fun (scrutineeType, checkedScrutinee, afterScrutinee) ->
            let resultHint =
                match expected with
                | Some _ -> expected
                | None ->
                    cases
                    |> List.tryPick (fun arm ->
                        match checkMatchPattern globals afterScrutinee None scrutineeType arm.pat with
                        | Error _ -> None
                        | Ok (_, bindings, afterPattern) ->
                            let armLocals =
                                Map.fold (fun acc name binding -> Map.add name binding acc)
                                    locals bindings
                            match checkExpression globals armLocals afterPattern None arm.rhs with
                            | Ok (typ, _, _) -> Some typ
                            | Error _ -> None)
            let rec checkCases currentSymbols resultType reversed (remaining: WT.MatchCase list) =
                match remaining with
                | [] ->
                    match resultType with
                    | None -> Error "Match expression must have at least one case"
                    | Some typ -> Ok (typ, List.rev reversed, currentSymbols)
                | arm :: tail ->
                    checkMatchPattern globals currentSymbols None scrutineeType arm.pat
                    |> Result.bind (fun (checkedPattern, bindings, afterPattern) ->
                        let armLocals =
                            Map.fold (fun acc name binding -> Map.add name binding acc) locals bindings
                        let guardResult =
                            match arm.whenCondition with
                            | None -> Ok (None, afterPattern)
                            | Some (_, guard) ->
                                checkExpression globals armLocals afterPattern (Some AST.TBool) guard
                                |> Result.map (fun (_, checkedGuard, afterGuard) ->
                                    Some checkedGuard, afterGuard)
                        guardResult
                        |> Result.bind (fun (checkedGuard, afterGuard) ->
                            let bodyExpected =
                                resultType
                                |> Option.filter ((<>) AST.TNever)
                                |> Option.orElse resultHint
                            checkExpression globals armLocals afterGuard bodyExpected arm.rhs
                            |> Result.bind (fun (bodyType, checkedBody, afterBody) ->
                                let joinedType =
                                    match resultType, bodyType with
                                    | Some AST.TNever, typ -> Ok typ
                                    | Some typ, AST.TNever -> Ok typ
                                    | Some typ, _ ->
                                        match TypeUnification.reconcileTypes None typ bodyType with
                                        | Some resolved -> Ok resolved
                                        | None -> Error "Match arm result types do not agree"
                                    | None, typ -> Ok typ
                                joinedType
                                |> Result.bind (fun resolvedType ->
                                    let checkedArm : CheckedAST.MatchCase =
                                        { Patterns =
                                            match checkedPattern with
                                            | CheckedAST.POr alternatives -> alternatives
                                            | other -> AST.NonEmptyList.singleton other
                                          Guard = checkedGuard
                                          Body = checkedBody }
                                    checkCases afterBody (Some resolvedType) (checkedArm :: reversed) tail))))
            checkCases afterScrutinee None [] cases
            |> Result.bind (fun (resultType, checkedCases, finalSymbols) ->
                if not (matchIsExhaustive globals finalSymbols scrutineeType checkedScrutinee checkedCases) then
                    Error "Non-exhaustive match expression"
                else
                    match AST.NonEmptyList.tryFromList checkedCases with
                    | None -> Error "Match expression must have at least one case"
                    | Some arms ->
                        checkedLiteral expected finalSymbols resultType
                            (CheckedAST.Match (checkedScrutinee, arms))))
    | WT.ETuple (_, first, _, second, rest, _, _) ->
        first :: second :: (rest |> List.map snd)
        |> fun elements ->
            let expectedTypes =
                match expected with
                | Some (AST.TTuple types) when List.length types = List.length elements ->
                    types |> List.map Some
                | _ -> []
            checkMany symbols elements expectedTypes []
        |> Result.bind (fun (elements, nextSymbols) ->
            match elements with
            | (_, firstChecked) :: (_, secondChecked) :: remaining ->
                let types = elements |> List.map fst
                let tuple : CheckedAST.TupleElements<CheckedAST.Expr> =
                    { First = firstChecked
                      Second = secondChecked
                      Rest = remaining |> List.map snd }
                checkedLiteral expected nextSymbols (AST.TTuple types) (CheckedAST.TupleLiteral tuple)
            | _ -> Error "Tuple syntax must contain at least two expressions")
    | WT.EList (_, contents, _, _) ->
        let elementExpected =
            match expected with
            | Some (AST.TList typ) -> Some typ
            | _ -> None
        let initialElementType =
            match elementExpected with
            | Some typ when not (TypeUnification.containsTVar typ) -> Some typ
            | _ ->
                contents
                |> List.tryPick (fun (item, _) ->
                    match checkExpression globals locals symbols None item with
                    | Ok (typ, _, _)
                        when typ <> AST.TNever && not (TypeUnification.containsTVar typ) -> Some typ
                    | _ -> None)
        let rec checkElements currentSymbols inferredType reversed remaining =
            match remaining with
            | [] -> Ok (List.rev reversed, inferredType, currentSymbols)
            | (item, _) :: tail ->
                checkExpression globals locals currentSymbols inferredType item
                |> Result.bind (fun (typ, checkedExpr, nextSymbols) ->
                    let nextType =
                        match inferredType with
                        | Some current -> TypeUnification.reconcileTypes None current typ
                        | None -> Some typ
                    match nextType with
                    | Some reconciled ->
                        checkElements nextSymbols (Some reconciled)
                            ((typ, checkedExpr) :: reversed) tail
                    | None -> Error "List elements must have the same type")
        checkElements symbols initialElementType [] contents
        |> Result.bind (fun (elements, inferredType, nextSymbols) ->
            let elementType =
                inferredType |> Option.orElse elementExpected
            match elementType with
            | None ->
                checkedLiteral expected nextSymbols
                    (AST.TList (AST.TVar TypeUnification.emptyListElementVar))
                    (CheckedAST.ListLiteral [])
            | Some typ ->
                checkedLiteral expected nextSymbols (AST.TList typ)
                    (CheckedAST.ListLiteral (elements |> List.map snd)))
    | WT.EDict (_, entries, _, _, _) ->
        let expectedTypes =
            match expected with
            | Some (AST.TDict (keyType, valueType)) -> Some (keyType, valueType)
            | _ -> None
        let rec checkEntries currentSymbols types reversed remaining =
            match remaining with
            | [] -> Ok (types, List.rev reversed, currentSymbols)
            | (_, key, _, value) :: tail ->
                let keyExpected = types |> Option.map fst
                let valueExpected = types |> Option.map snd
                check currentSymbols keyExpected key
                |> Result.bind (fun (keyType, checkedKey, afterKey) ->
                    let repeatedStringKey =
                        match checkedKey with
                        | CheckedAST.StringLiteral keyText ->
                            if reversed
                               |> List.exists (fun (existingKey, _) ->
                                   match existingKey with
                                   | CheckedAST.StringLiteral existingText -> existingText = keyText
                                   | _ -> false)
                            then Some keyText
                            else None
                        | _ -> None
                    match repeatedStringKey with
                    | Some keyText -> Error $"Duplicate dictionary key \"{keyText}\""
                    | None ->
                        check afterKey valueExpected value
                        |> Result.bind (fun (valueType, checkedValue, afterValue) ->
                            let entryTypes = keyType, valueType
                            match types with
                            | Some (wantedKey, wantedValue) ->
                                match TypeUnification.reconcileTypes None wantedKey keyType,
                                      TypeUnification.reconcileTypes None wantedValue valueType with
                                | Some resolvedKey, Some resolvedValue ->
                                    checkEntries afterValue (Some (resolvedKey, resolvedValue))
                                        ((checkedKey, checkedValue) :: reversed) tail
                                | _ -> Error "Dictionary entries must have the same key and value types"
                            | None ->
                                checkEntries afterValue (Some entryTypes)
                                    ((checkedKey, checkedValue) :: reversed) tail))
        checkEntries symbols expectedTypes [] entries
        |> Result.bind (fun (types, checkedEntries, finalSymbols) ->
            let keyType, valueType =
                types
                |> Option.defaultValue (AST.TVar "dictKey", AST.TVar "dictValue")
            checkedLiteral expected finalSymbols (AST.TDict (keyType, valueType))
                (CheckedAST.DictLiteral (
                    CheckedAST.checkedType keyType,
                    CheckedAST.checkedType valueType,
                    checkedEntries)))
    | WT.EPipe (range, first, segments) ->
        let appendPipe input (_, segment) =
            match segment with
            | WT.EPipeInfix (_, op, right) -> Ok (WT.EInfix (range, op, input, right))
            | WT.EPipeFnCall (_, name, typeArgs, args) ->
                Ok (WT.EApply (range, WT.EFnName (name.range, name), typeArgs, input :: args))
            | WT.EPipeEnum (_, name, caseName, fields, symbolDot) ->
                Ok (WT.EEnum (range, name, caseName, input :: fields, symbolDot))
            | WT.EPipeLambda (_, [pattern], body, keywordFun, arrow) ->
                Ok (WT.ELet (range, pattern, input, body, keywordFun, arrow))
            | WT.EPipeLambda (_, patterns, body, keywordFun, arrow) ->
                let lambda = WT.ELambda (range, patterns, body, keywordFun, arrow)
                Ok (WT.EApply (range, lambda, [], [input]))
            | WT.EPipeVariableOrFnCall (nameRange, name) ->
                let target =
                    if Map.containsKey name locals then WT.EVariable (nameRange, name)
                    else
                        let identifier: WT.Identifier = { range = nameRange; name = name }
                        let qualified: WT.QualifiedFnIdentifier =
                            { range = nameRange; modules = []; fn = identifier }
                        WT.EFnName (nameRange, qualified)
                Ok (WT.EApply (range, target, [], [input]))
        segments
        |> List.fold (fun result segment -> result |> Result.bind (fun input -> appendPipe input segment))
            (Ok first)
        |> Result.bind (check symbols expected)
    | WT.EIf (_, cond, thenExpr, elseExpr, _, _, _) ->
        check symbols (Some AST.TBool) cond
        |> Result.bind (fun (_, checkedCond, afterCond) ->
            let checkedThen =
                match check afterCond expected thenExpr, expected, elseExpr with
                | Error _, None, Some fallback ->
                    check afterCond None fallback
                    |> Result.bind (fun (inferredType, _, _) ->
                        check afterCond (Some inferredType) thenExpr)
                | Ok (inferredType, _, _), None, Some fallback
                    when TypeUnification.containsTVar inferredType ->
                    check afterCond None fallback
                    |> Result.bind (fun (fallbackType, _, _) ->
                        check afterCond (Some fallbackType) thenExpr)
                | result, _, _ -> result
            checkedThen
            |> Result.bind (fun (thenType, checkedThen, afterThen) ->
                let elseResult =
                    match elseExpr with
                    | Some value ->
                        let elseExpected =
                            if thenType = AST.TNever then expected else Some thenType
                        check afterThen elseExpected value
                    | None -> checkedLiteral (Some thenType) afterThen AST.TUnit CheckedAST.UnitLiteral
                elseResult
                |> Result.bind (fun (elseType, checkedElse, finalSymbols) ->
                    match TypeUnification.reconcileTypes None thenType elseType with
                    | Some resultType ->
                        checkedLiteral expected finalSymbols resultType
                            (CheckedAST.If (checkedCond, checkedThen, checkedElse))
                    | None -> Error $"Conditional branches have incompatible types: {thenType} and {elseType}")))
    | WT.ELet (range, pattern, value, body, _, _) ->
        let callsBinding name target =
            match target with
            | WT.EVariable (_, calledName) -> calledName = name
            | WT.EFnName (_, functionName) -> qualifiedFnName functionName = [name]
            | _ -> false
        let rec inferredUsageType name expression =
            match expression with
            | WT.EApply (_, target, [], arguments) when callsBinding name target ->
                arguments
                |> ResultList.traverse (fun argument ->
                    check symbols None argument
                    |> Result.map (fun (typ, _, _) -> typ))
                |> function
                    | Ok argumentTypes ->
                        let returnName =
                            $"t$let_lambda_return_{range.start.row}_{range.start.column}"
                        Some (AST.TFunction (
                            argumentTypes,
                            AST.TInferenceVar (returnName, returnName)))
                    | Error _ -> None
            | WT.EApply (_, WT.EFnName (_, functionName), _, arguments) ->
                match resolveFunction globals (qualifiedFnName functionName) with
                | Some signature when List.length arguments = List.length signature.Parameters ->
                    let argumentParameters = List.zip arguments signature.Parameters
                    let substitutions =
                        argumentParameters
                        |> List.collect (fun (argument, parameterType) ->
                            if callsBinding name argument then []
                            else
                                match check symbols None argument with
                                | Ok (actualType, _, _) ->
                                    TypeUnification.matchTypes parameterType actualType
                                    |> Result.defaultValue []
                                | Error _ -> [])
                        |> Map.ofList
                    argumentParameters
                    |> List.tryPick (fun (argument, parameterType) ->
                        if callsBinding name argument then
                            Some (CheckingTypes.applyTypeArguments substitutions parameterType)
                        else inferredUsageType name argument)
                | _ -> arguments |> List.tryPick (inferredUsageType name)
            | WT.EInfix (_, _, left, right) ->
                inferredUsageType name left
                |> Option.orElseWith (fun () -> inferredUsageType name right)
            | WT.EPipe (_, source, segments) ->
                let pipedCallUsage =
                    match segments with
                    | (_, WT.EPipeFnCall (_, functionName, [], arguments)) :: _ ->
                        match resolveFunction globals (qualifiedFnName functionName),
                              check symbols None source with
                        | Some signature, Ok (sourceType, _, _)
                            when List.length signature.Parameters = 1 + List.length arguments ->
                            match signature.Parameters with
                            | pipedParameter :: otherParameters ->
                                match TypeUnification.matchTypes pipedParameter sourceType with
                                | Ok bindings ->
                                    let substitutions = Map.ofList bindings
                                    List.zip arguments otherParameters
                                    |> List.tryPick (fun (argument, parameterType) ->
                                        if callsBinding name argument then
                                            Some (CheckingTypes.applyTypeArguments substitutions parameterType)
                                        else inferredUsageType name argument)
                                | Error _ -> None
                            | [] -> None
                        | _ -> None
                    | _ -> None
                inferredUsageType name source
                |> Option.orElse pipedCallUsage
            | WT.ELet (_, _, bound, next, _, _)
            | WT.EStatement (_, bound, next) ->
                inferredUsageType name bound
                |> Option.orElseWith (fun () -> inferredUsageType name next)
            | WT.EIf (_, condition, thenBranch, elseBranch, _, _, _) ->
                inferredUsageType name condition
                |> Option.orElseWith (fun () -> inferredUsageType name thenBranch)
                |> Option.orElseWith (fun () -> elseBranch |> Option.bind (inferredUsageType name))
            | _ -> None
        let valueExpected =
            match pattern, value with
            | WT.LPVariable (_, name), WT.ELambda _ -> inferredUsageType name body
            | WT.LPVariable (_, name), WT.EEnum _ -> inferredUsageType name body
            | _ -> None
        let rec referencesSelf name expression =
            match expression with
            | WT.EApply (_, target, _, arguments) ->
                callsBinding name target
                || referencesSelf name target
                || List.exists (referencesSelf name) arguments
            | WT.EInfix (_, _, left, right)
            | WT.EStatement (_, left, right) ->
                referencesSelf name left || referencesSelf name right
            | WT.EIf (_, condition, thenBranch, elseBranch, _, _, _) ->
                referencesSelf name condition
                || referencesSelf name thenBranch
                || (elseBranch |> Option.exists (referencesSelf name))
            | WT.ELet (_, boundPattern, bound, next, _, _) ->
                referencesSelf name bound
                || (match boundPattern with
                    | WT.LPVariable (_, boundName) when boundName = name -> false
                    | _ -> referencesSelf name next)
            | WT.ELambda (_, _, lambdaBody, _, _) -> referencesSelf name lambdaBody
            | _ -> false
        match pattern, value with
        | WT.LPVariable (_, name), WT.ELambda (_, _, lambdaBody, keywordFun, _)
            when not (Map.containsKey name locals)
                 && keywordFun.start = keywordFun.end_
                 && referencesSelf name lambdaBody
                 && (resolveFunction globals [name] |> Option.isSome) ->
            Error $"Nested function name '{name}' is ambiguous"
        | WT.LPVariable (_, name), WT.ELambda (_, parameters, lambdaBody, keywordFun, _)
            when not (Map.containsKey name locals) && referencesSelf name lambdaBody ->
            let provisional =
                valueExpected
                |> Option.defaultWith (fun () ->
                    let parameterTypes =
                        parameters
                        |> List.mapi (fun index _ ->
                            let name = $"t$recursive_{range.start.row}_{range.start.column}_{index}"
                            AST.TInferenceVar (name, name))
                    let resultName = $"t$recursive_return_{range.start.row}_{range.start.column}"
                    AST.TFunction (parameterTypes, AST.TInferenceVar (resultName, resultName)))
            let bindingId, withBinding = CheckedAST.allocateBinding name symbols
            let recursiveLocals = Map.add name (provisional, bindingId) locals
            checkExpression globals recursiveLocals withBinding (Some provisional) value
            |> Result.bind (fun (valueType, checkedValue, afterValue) ->
                let bodyLocals = Map.add name (valueType, bindingId) locals
                checkExpression globals bodyLocals afterValue expected body
                |> Result.map (fun (bodyType, checkedBody, finalSymbols) ->
                    let ordinal = CheckedAST.nextBindingOrdinal symbols
                    let memberId = AST.recursiveMemberId ordinal
                    let kind =
                        if keywordFun.start = keywordFun.end_ then
                            AST.NamedLocalFunctionMember
                        else AST.DirectLambdaValueMember
                    let parsed: AST.ParsedRecursiveMember =
                        { Binding = bindingId
                          Boundary = AST.scopeBoundaryId ordinal
                          Member = memberId
                          SourceName = name
                          Kind = kind }
                    let resolved: AST.ResolvedRecursiveMember =
                        { Parsed = parsed
                          Group = AST.singletonRecursiveGroupId memberId
                          GroupIndex = 0
                          Availability = AST.SelfRecursiveMember }
                    let typed: CheckedAST.RecursiveMember =
                        { Resolved = resolved
                          MonomorphicType = CheckedAST.checkedType valueType }
                    bodyType,
                    CheckedAST.RecursiveLet (
                        typed, checkedValue, checkedBody),
                    finalSymbols))
        | _ ->
            check symbols valueExpected value
            |> Result.bind (fun (valueType, checkedValue, afterValue) ->
                checkLetPattern pattern valueType afterValue
                |> Result.bind (fun (checkedPattern, bindings, afterPattern) ->
                    let bodyLocals = Map.fold (fun acc name binding -> Map.add name binding acc) locals bindings
                    checkExpression globals bodyLocals afterPattern expected body
                    |> Result.map (fun (bodyType, checkedBody, finalSymbols) ->
                        bodyType, CheckedAST.Let (checkedPattern, checkedValue, checkedBody), finalSymbols)))
    | WT.EStatement (_, first, next) ->
        check symbols (Some AST.TUnit) first
        |> Result.bind (fun (_, checkedFirst, afterFirst) ->
            check afterFirst expected next
            |> Result.map (fun (typ, checkedNext, finalSymbols) ->
                typ, CheckedAST.Sequence (checkedFirst, checkedNext), finalSymbols))
    | WT.EInfix (_, (_, infix), left, right) ->
        check symbols None left
        |> Result.bind (fun (leftType, checkedLeft, afterLeft) ->
            let rec needsEnumContext expression =
                match expression with
                | WT.EEnum _ -> true
                | WT.ETuple (_, first, _, second, rest, _, _) ->
                    first :: second :: (rest |> List.map snd)
                    |> List.exists needsEnumContext
                | WT.EList (_, contents, _, _) ->
                    contents |> List.exists (fun (item, _) -> needsEnumContext item)
                | _ -> false
            let rightExpected =
                match infix with
                | WT.InfixFnCall WT.StringConcat -> None
                | WT.InfixFnCall WT.ComparisonEquals
                | WT.InfixFnCall WT.ComparisonNotEquals ->
                    if needsEnumContext right then Some leftType else None
                | _ -> Some leftType
            check afterLeft rightExpected right
            |> Result.bind (fun (rightType, checkedRight, finalSymbols) ->
                let integerTypes =
                    [ AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64
                      AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32
                      AST.TUInt64; AST.TUInt128 ]
                let numeric = List.contains leftType integerTypes || leftType = AST.TFloat64
                let integer = List.contains leftType integerTypes
                let selection =
                    match infix with
                    | WT.BinOp WT.BinOpAnd -> Some (AST.And, (leftType = AST.TBool), AST.TBool)
                    | WT.BinOp WT.BinOpOr -> Some (AST.Or, (leftType = AST.TBool), AST.TBool)
                    | WT.InfixFnCall op ->
                        match op with
                        | WT.ArithmeticPlus -> Some (AST.Add, numeric, leftType)
                        | WT.ArithmeticMinus -> Some (AST.Sub, numeric, leftType)
                        | WT.ArithmeticMultiply -> Some (AST.Mul, numeric, leftType)
                        | WT.ArithmeticDivide -> Some (AST.Div, numeric, leftType)
                        | WT.ArithmeticModulo -> Some (AST.Mod, numeric, leftType)
                        | WT.ArithmeticPower -> Some (AST.Pow, numeric, leftType)
                        | WT.BitwiseAnd -> Some (AST.BitAnd, integer, leftType)
                        | WT.BitwiseOr -> Some (AST.BitOr, integer, leftType)
                        | WT.BitwiseXor -> Some (AST.BitXor, integer, leftType)
                        | WT.ShiftLeft -> Some (AST.Shl, integer, leftType)
                        | WT.ShiftRight -> Some (AST.Shr, integer, leftType)
                        | WT.ComparisonEquals ->
                            let compatible =
                                structuralEqualityCompatible globals leftType rightType
                            let mixedCharString =
                                (leftType = AST.TChar && rightType = AST.TString)
                                || (leftType = AST.TString && rightType = AST.TChar)
                            Some (AST.Eq, compatible && not mixedCharString, AST.TBool)
                        | WT.ComparisonNotEquals ->
                            let compatible =
                                structuralEqualityCompatible globals leftType rightType
                            let mixedCharString =
                                (leftType = AST.TChar && rightType = AST.TString)
                                || (leftType = AST.TString && rightType = AST.TChar)
                            Some (AST.Neq, compatible && not mixedCharString, AST.TBool)
                        | WT.ComparisonLessThan -> Some (AST.Lt, numeric, AST.TBool)
                        | WT.ComparisonLessThanOrEqual -> Some (AST.Lte, numeric, AST.TBool)
                        | WT.ComparisonGreaterThan -> Some (AST.Gt, numeric, AST.TBool)
                        | WT.ComparisonGreaterThanOrEqual -> Some (AST.Gte, numeric, AST.TBool)
                        | WT.StringConcat ->
                            let stringLike typ =
                                typ = AST.TString || typ = AST.TChar || typ = AST.TNever
                            let resultType =
                                if leftType = AST.TNever || rightType = AST.TNever then
                                    AST.TNever
                                else AST.TString
                            Some (AST.StringConcat, stringLike leftType && stringLike rightType, resultType)
                match selection with
                | _ when leftType = AST.TNever ->
                    checkedLiteral expected finalSymbols AST.TNever checkedLeft
                | _ when rightType = AST.TNever ->
                    checkedLiteral expected finalSymbols AST.TNever
                        (CheckedAST.Sequence (checkedLeft, checkedRight))
                | Some (AST.Pow, _, _) when leftType = AST.TInt128 ->
                    Error "Cannot perform numeric operation on Int128 and Int128"
                | Some (AST.Pow, _, _) when leftType = AST.TUInt128 ->
                    Error "Cannot perform numeric operation on UInt128 and UInt128"
                | Some ((AST.Eq | AST.Neq) as op, true, resultType)
                    when (match leftType with
                          | AST.TList _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TDict _
                          | AST.TVar _ | AST.TInferenceVar _ | AST.TFunction _ -> true
                          | _ -> false) ->
                    let normalizedRight =
                        if (TypeUnification.reconcileTypes None leftType rightType |> Option.isNone)
                           && structuralEqualityCompatible globals leftType rightType then
                            convertStructuralRecord globals leftType rightType checkedRight finalSymbols
                        else Ok (checkedRight, finalSymbols)
                    normalizedRight
                    |> Result.bind (fun (rightValue, afterConversion) ->
                        let marker = ComparisonPlanning.internalTypeAppMarkerName ComparisonPlanning.EqHelperDispatch
                        let markerId, withMarker = CheckedAST.internFunction marker afterConversion
                        let equality =
                            CheckedAST.TypeApp (
                                markerId,
                                [CheckedAST.checkedType leftType],
                                AST.NonEmptyList.fromList [checkedLeft; rightValue])
                        let comparison =
                            if op = AST.Neq then CheckedAST.UnaryOp (AST.Not, equality)
                            else equality
                        checkedLiteral expected withMarker resultType comparison)
                | Some (op, true, resultType) ->
                    checkedLiteral expected finalSymbols resultType
                        (CheckedAST.BinOp (op, checkedLeft, checkedRight))
                | Some ((AST.Eq | AST.Neq), false, _)
                    when (leftType = AST.TChar && rightType = AST.TString)
                         || (leftType = AST.TString && rightType = AST.TChar) ->
                    Error "Cannot compare Char and String"
                | _ -> Error "Operator is unavailable for this type"))
    | _ -> Error "Expression requires source name resolution and type checking"

/// This first checking slice accepts one closed entry expression. It provides
/// a direct WrittenTypes-to-CheckedAST path while declaration checking grows.
let checkClosedProgram
    (validated: LibParser.Validation.ValidatedSourceFile)
    : Result<AST.SemanticType * CheckedAST.Program, string> =
    WrittenSource.items validated
    |> Result.bind (function
        | [WrittenSource.Expression ([], expression)] ->
            checkExpression emptyGlobals Map.empty (CheckedAST.emptySymbols ()) None expression
            |> Result.map (fun (typ, checkedExpression, symbols) ->
                typ, CheckedAST.programFromCheckedParts
                    (symbols, [CheckedAST.Expression checkedExpression]))
        | _ -> Error "Closed checking requires a single unscoped entry expression")

let private predeclareTypes (items: WrittenSource.Item list) : Result<TypeInventory, string> =
    items
    |> List.fold (fun result item ->
        result
        |> Result.bind (fun types ->
            match item with
            | WrittenSource.Type (path, declaration) ->
                let name = String.concat "." (path @ [declaration.name.name])
                if Map.containsKey name types then Error $"Duplicate type '{name}'"
                else
                    let kind =
                        match declaration.definition with
                        | WT.TDRecord _ -> RecordKind
                        | WT.TDEnum _ -> SumKind
                        | WT.TDAlias _ -> AliasKind
                    let entry =
                        { Kind = kind
                          Params = declaration.typeParams |> List.map fst
                          Path = path
                          Definition = declaration.definition }
                    Ok (Map.add name entry types)
            | _ -> Ok types))
        (Ok Map.empty)

let private checkTypeDeclaration
    (allowInternal: bool)
    (types: TypeInventory)
    (colliding: Set<string>)
    (symbols: CheckedAST.Symbols)
    (path: string list)
    (declaration: WT.TypeDecl)
    : Result<CheckedAST.TopLevel * CheckedAST.Symbols, string> =
    let name = String.concat "." (path @ [declaration.name.name])
    let typeParams = declaration.typeParams |> List.map fst
    let convert = resolveWrittenType allowInternal types path (Set.ofList typeParams)
    let definition =
        match declaration.definition with
        | WT.TDAlias target ->
            convert target
            |> Result.map (fun targetType -> AST.TypeAlias (name, typeParams, targetType))
        | WT.TDRecord fields ->
            let repeatedField =
                fields
                |> List.countBy (fun (field, _) -> snd field.name)
                |> List.tryFind (fun (_, count) -> count > 1)
            match repeatedField with
            | Some (fieldName, _) ->
                Error $"Duplicate field '{fieldName}' in record type {name}"
            | None ->
                fields
                |> ResultList.traverse (fun (field, _) ->
                    convert field.typ
                    |> Result.map (fun typ -> snd field.name, typ))
                |> Result.map (fun fieldTypes -> AST.RecordDef (name, typeParams, fieldTypes))
        | WT.TDEnum cases ->
            cases
            |> ResultList.traverse (fun (_, caseSyntax) ->
                caseSyntax.fields
                |> ResultList.traverse (fun field -> convert field.typ)
                |> Result.map (fun fieldTypes ->
                    ({ Name = snd caseSyntax.name; Fields = fieldTypes }: AST.Variant)))
            |> Result.map (fun variants -> AST.SumTypeDef (name, typeParams, variants))
    definition
    |> Result.map (fun checkedDefinition ->
        let typeId, afterType = CheckedAST.internType name symbols
        let afterMembers =
            match checkedDefinition with
            | AST.RecordDef (_, _, fields) ->
                fields
                |> List.indexed
                |> List.fold (fun current (index, (fieldName, _)) ->
                    CheckedAST.internField name fieldName index current |> snd) afterType
            | AST.SumTypeDef (_, _, variants) ->
                variants
                |> List.indexed
                |> List.fold (fun current (index, variant) ->
                    let tag = caseTag colliding name variant.Name index
                    CheckedAST.internConstructor name variant.Name tag current |> snd) afterType
            | AST.TypeAlias _ -> afterType
        CheckedAST.TypeDef (typeId, CheckedAST.checkedTypeDef checkedDefinition), afterMembers)

/// Predeclare annotated functions before checking bodies, so calls use stable
/// identities regardless of source order. The nominal registry will replace
/// the restricted resolver as type declarations are brought into this path.
let private predeclareFunctions
    (allowInternal: bool)
    (types: TypeInventory)
    (items: WrittenSource.Item list)
    (symbols: CheckedAST.Symbols)
    : Result<Map<string, FunctionSignature> * CheckedAST.Symbols, string> =
    let builtinNames = ["Builtin.testRuntimeError"; "Builtin.crash"]
    let builtinFunctions, builtinSymbols =
        builtinNames
        |> List.fold (fun (functions, currentSymbols) name ->
            let id, nextSymbols = CheckedAST.internFunction name currentSymbols
            let signature =
                { Id = id
                  TypeParams = []
                  Parameters = [AST.TString]
                  Return = AST.TNever }
            Map.add name signature functions, nextSymbols)
            (Map.empty, symbols)
    let intrinsicFunctions, intrinsicSymbols =
        Stdlib.buildModuleRegistry ()
        |> Map.fold (fun (functions, currentSymbols) name entry ->
            let id, nextSymbols = CheckedAST.internFunction name currentSymbols
            let signature =
                { Id = id
                  TypeParams = entry.TypeParams
                  Parameters = entry.ParamTypes
                  Return = entry.ReturnType }
            Map.add name signature functions, nextSymbols)
            (builtinFunctions, builtinSymbols)
    items
    |> List.fold (fun result item ->
        result
        |> Result.bind (fun (functions, currentSymbols) ->
            match item with
            | WrittenSource.Function (path, fn) ->
                let name = String.concat "." (path @ [fn.name.name])
                if Map.containsKey name functions then Error $"Duplicate function '{name}'"
                else
                    let explicitParams = fn.typeParams |> List.map fst
                    let allParams =
                        fn.parameters
                        |> List.fold (fun found parameter ->
                            match parameter with
                            | WT.FPUnit _ -> found
                            | WT.FPNormal (_, _, typ, _, _, _, _) ->
                                collectWrittenTypeParams found typ)
                            explicitParams
                        |> fun found -> collectWrittenTypeParams found fn.returnType
                    let typeParams = Set.ofList allParams
                    let parameterTypes =
                        fn.parameters
                        |> ResultList.traverse (function
                            | WT.FPUnit _ -> Ok AST.TUnit
                            | WT.FPNormal (_, _, typ, _, _, _, _) ->
                                resolveWrittenType allowInternal types path typeParams typ)
                    parameterTypes
                    |> Result.bind (fun parameters ->
                        resolveWrittenType allowInternal types path typeParams fn.returnType
                        |> Result.map (fun returnType ->
                            let id, nextSymbols = CheckedAST.internFunction name currentSymbols
                            let signature =
                                { Id = id
                                  TypeParams = allParams
                                  Parameters = parameters
                                  Return = returnType }
                            Map.add name signature functions, nextSymbols))
            | _ -> Ok (functions, currentSymbols)))
        (Ok (Map.empty, intrinsicSymbols))
    |> Result.map (fun (sourceFunctions, finalSymbols) ->
        Map.fold (fun functions name signature -> Map.add name signature functions)
            intrinsicFunctions sourceFunctions,
        finalSymbols)

let private checkFunction
    (globals: Globals)
    (symbols: CheckedAST.Symbols)
    (path: string list)
    (fn: WT.FnDecl)
    : Result<CheckedAST.FunctionDef * CheckedAST.Symbols, string> =
    let name = String.concat "." (path @ [fn.name.name])
    match Map.tryFind name globals.Functions with
    | None -> Error $"Function '{name}' was not predeclared"
    | Some signature ->
        let parameterNames =
            fn.parameters
            |> List.mapi (fun index parameter ->
                match parameter with
                | WT.FPUnit _ -> $"_unit{index}"
                | WT.FPNormal (_, identifier, _, _, _, _, _) -> identifier.name)
        if List.length parameterNames <> List.length signature.Parameters then
            Error $"Function '{name}' has mismatched parameter metadata"
        else
            let duplicates =
                parameterNames
                |> List.groupBy id
                |> List.tryPick (fun (parameterName, entries) ->
                    if List.length entries > 1 then Some parameterName else None)
            match duplicates with
            | Some duplicate -> Error $"Duplicate parameter '{duplicate}'"
            | None ->
                List.zip parameterNames signature.Parameters
                |> List.fold (fun (locals, reversed, currentSymbols) (parameterName, typ) ->
                    let id, nextSymbols = CheckedAST.allocateBinding parameterName currentSymbols
                    (Map.add parameterName (typ, id) locals,
                     (id, CheckedAST.checkedType typ) :: reversed,
                     nextSymbols)) (Map.empty, [], symbols)
                |> fun (locals, reversed, afterParams) ->
                    match AST.NonEmptyList.tryFromList (List.rev reversed) with
                    | None -> Error $"Function '{name}' requires a parameter"
                    | Some parameters ->
                        checkExpression
                            { globals with
                                ModulePath = path
                                TypeParams = Set.ofList signature.TypeParams
                                CurrentFunction = Some (signature.Id, name, signature.TypeParams) }
                            locals afterParams
                            (Some signature.Return) fn.body
                        |> Result.map (fun (_, body, afterBody) ->
                            { Id = signature.Id
                              Name = name
                              TypeParams = signature.TypeParams
                              Params = parameters
                              ReturnType = CheckedAST.checkedType signature.Return
                              Body = body
                              Recursion = None }, afterBody)

let private attachRecursiveGroups (program: CheckedAST.Program) : CheckedAST.Program =
    let symbols = CheckedAST.programSymbols program
    let topLevels = CheckedAST.programTopLevels program
    let functions =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef definition -> Some definition
            | _ -> None)
    let names = functions |> List.map _.Name |> Set.ofList
    let graph =
        functions
        |> List.map (fun definition ->
            let dependencies =
                SpecializationIdentity.directDependencies definition.Body
                |> Set.toList
                |> List.choose (fun id -> CheckedAST.functionName id symbols)
                |> Set.ofList
                |> Set.intersect names
            definition.Name, dependencies)
        |> Map.ofList
    let reachableFrom root =
        let rec visit pending visited =
            match pending with
            | [] -> visited
            | name :: rest when Set.contains name visited -> visit rest visited
            | name :: rest ->
                let next = Map.tryFind name graph |> Option.defaultValue Set.empty
                visit (Set.toList next @ rest) (Set.add name visited)
        graph |> Map.tryFind root |> Option.defaultValue Set.empty
        |> Set.toList |> fun first -> visit first Set.empty
    let reachability =
        functions
        |> List.map (fun definition -> definition.Name, reachableFrom definition.Name)
        |> Map.ofList
    let required key inventory =
        match Map.tryFind key inventory with
        | Some value -> value
        | None -> Crash.crash $"Missing recursive function '{key}'"
    let mutuallyReachable left right =
        Set.contains right (required left reachability)
        && Set.contains left (required right reachability)
    let sourceOrdinals =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, item) ->
            match item with
            | CheckedAST.FunctionDef definition -> Some (definition.Name, index + 1)
            | _ -> None)
        |> Map.ofList
    let rec groups
        (ordinal: int)
        (remaining: CheckedAST.FunctionDef list)
        (resolved: Map<string, CheckedAST.RecursiveMember>) =
        match remaining with
        | [] -> resolved
        | first :: rest ->
            let sameGroup, later =
                rest |> List.partition (fun candidate ->
                    mutuallyReachable first.Name candidate.Name)
            let availability =
                if not (List.isEmpty sameGroup) then AST.MutualRecursiveMember
                elif Set.contains first.Name (required first.Name graph) then
                    AST.SelfRecursiveMember
                else AST.CompletedGroupMember
            let members = first :: sameGroup
            let nextResolved =
                members
                |> List.indexed
                |> List.fold (fun current (groupIndex, definition) ->
                    let sourceOrdinal = required definition.Name sourceOrdinals
                    let parsed: AST.ParsedRecursiveMember =
                        { Binding = AST.namedBindingId sourceOrdinal definition.Name
                          Boundary = AST.scopeBoundaryId 0
                          Member = AST.recursiveMemberId sourceOrdinal
                          SourceName = definition.Name
                          Kind = AST.TopLevelFunctionMember }
                    let resolvedMember: AST.ResolvedRecursiveMember =
                        { Parsed = parsed
                          Group = AST.topLevelRecursiveGroupId ordinal
                          GroupIndex = groupIndex
                          Availability = availability }
                    let functionType =
                        AST.TFunction (
                            CheckedAST.functionParameterTypes definition
                            |> AST.NonEmptyList.toList |> List.map snd,
                            CheckedAST.functionReturnType definition)
                    let checkedMember: CheckedAST.RecursiveMember =
                        { Resolved = resolvedMember
                          MonomorphicType = CheckedAST.checkedType functionType }
                    Map.add definition.Name checkedMember current)
                    resolved
            groups (ordinal + 1) later nextResolved
    let resolved = groups 0 functions Map.empty
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef definition ->
            let recursion = Map.tryFind definition.Name resolved
            CheckedAST.FunctionDef { definition with Recursion = recursion }
        | other -> other)
    |> fun checkedTopLevels ->
        CheckedAST.programFromCheckedParts (symbols, checkedTopLevels)

/// Check annotated, nongeneric functions and sequential values directly from
/// WrittenTypes. The production entry point is switched only after declaration
/// catalogs, recursion, matches, and generic checking are included.
let private checkItems
    (baseEnvironment: Environment option)
    (allowInternal: bool)
    (requireEntry: bool)
    (items: WrittenSource.Item list)
    : Result<AST.SemanticType * CheckedAST.Program * Environment, string> =
    predeclareTypes items
        |> Result.bind (fun localTypes ->
        let baseGlobals, baseSymbols =
            match baseEnvironment with
            | Some (Environment (globals, symbols)) -> globals, symbols
            | None -> emptyGlobals, CheckedAST.emptySymbols ()
        let types =
            Map.fold (fun inventory name entry -> Map.add name entry inventory)
                baseGlobals.Types localTypes
        predeclareFunctions allowInternal types items baseSymbols
        |> Result.bind (fun (localFunctions, symbols) ->
            let functions =
                Map.fold (fun inventory name signature -> Map.add name signature inventory)
                    baseGlobals.Functions localFunctions
            let colliding = collidingCaseNames types
            let initialGlobals =
                { baseGlobals with
                    Functions = functions
                    Types = types
                    CollidingCases = colliding
                    AllowInternal = allowInternal
                    ModulePath = []
                    TypeParams = Set.empty
                    CurrentFunction = None }
            items
            |> List.fold (fun result item ->
                result
                |> Result.bind (fun (globals, currentSymbols, reversed, entryType) ->
                    let itemName =
                        match item with
                        | WrittenSource.Function (path, fn) -> String.concat "." (path @ [fn.name.name])
                        | WrittenSource.Value (path, value) -> String.concat "." (path @ [value.name.name])
                        | WrittenSource.Type (path, declaration) -> String.concat "." (path @ [declaration.name.name])
                        | WrittenSource.Expression (path, _) ->
                            String.concat "." (path @ ["<entry>"])
                    (match item with
                    | WrittenSource.Function (path, fn) ->
                        checkFunction globals currentSymbols path fn
                        |> Result.map (fun (definition, nextSymbols) ->
                            globals, nextSymbols, CheckedAST.FunctionDef definition :: reversed, entryType)
                    | WrittenSource.Value (path, value) ->
                        let name = String.concat "." (path @ [value.name.name])
                        checkExpression { globals with ModulePath = path } Map.empty currentSymbols None value.body
                        |> Result.map (fun (typ, body, afterBody) ->
                            let id, nextSymbols = CheckedAST.internValue name afterBody
                            let definition : CheckedAST.ValueDef =
                                { Id = id; Name = name; Type = CheckedAST.checkedType typ; Body = body }
                            let nextGlobals =
                                { globals with Values = Map.add name (typ, id) globals.Values }
                            nextGlobals, nextSymbols, CheckedAST.ValueDef definition :: reversed, entryType)
                    | WrittenSource.Expression (path, expr) ->
                        match entryType with
                        | Some _ -> Error "Multiple program entry expressions"
                        | None ->
                            checkExpression { globals with ModulePath = path } Map.empty currentSymbols None expr
                            |> Result.map (fun (typ, body, nextSymbols) ->
                                globals, nextSymbols, CheckedAST.Expression body :: reversed, Some typ)
                    | WrittenSource.Type (path, declaration) ->
                        checkTypeDeclaration globals.AllowInternal globals.Types globals.CollidingCases currentSymbols path declaration
                        |> Result.map (fun (definition, nextSymbols) ->
                            globals, nextSymbols, definition :: reversed, entryType))
                    |> Result.mapError (fun error -> $"{itemName}: {error}")))
                (Ok (initialGlobals, symbols, [], None))
            |> Result.bind (fun (finalGlobals, finalSymbols, reversed, entryType) ->
                match requireEntry, entryType with
                | true, None -> Error "Program requires an entry expression"
                | _, resultType ->
                    let typ = resultType |> Option.defaultValue AST.TUnit
                    Ok (
                        typ,
                        CheckedAST.programFromCheckedParts (finalSymbols, List.rev reversed)
                        |> attachRecursiveGroups,
                        Environment (finalGlobals, finalSymbols)))))

let checkSimpleProgram
    (requireEntry: bool)
    (validated: LibParser.Validation.ValidatedSourceFile)
    : Result<AST.SemanticType * CheckedAST.Program, string> =
    WrittenSource.items validated
    |> Result.bind (checkItems None false requireEntry)
    |> Result.map (fun (typ, program, _) -> typ, program)

let checkSourceUnitsWithBase
    (baseEnvironment: Environment option)
    (allowInternal: bool)
    (requireEntry: bool)
    (units: LibParser.Validation.ValidatedSourceFile list)
    : Result<AST.SemanticType * CheckedAST.Program * Environment, string> =
    units
    |> ResultList.traverse WrittenSource.items
    |> Result.map List.concat
    |> Result.bind (checkItems baseEnvironment allowInternal requireEntry)

let checkSourceUnits
    (allowInternal: bool)
    (requireEntry: bool)
    (units: LibParser.Validation.ValidatedSourceFile list)
    : Result<AST.SemanticType * CheckedAST.Program, string> =
    checkSourceUnitsWithBase None allowInternal requireEntry units
    |> Result.map (fun (typ, program, _) -> typ, program)

/// Publish the checked declarations to the later compiler passes. This reads
/// the completed checked program; source checking has already finished.
let typeCheckEnvironment (program: CheckedAST.Program) : CheckingTypes.TypeCheckEnv =
    let symbols = CheckedAST.programSymbols program
    let topLevels = CheckedAST.programTopLevels program
    let typeDefs =
        topLevels
        |> List.choose (function
            | CheckedAST.TypeDef (_, definition) ->
                Some (CheckedAST.semanticTypeDef definition)
            | _ -> None)
    let recordTypes, recordParams, aliases, variantLookup =
        typeDefs
        |> List.fold (fun (records, parameters, aliases, variants) definition ->
            match definition with
            | AST.RecordDef (name, typeParams, fields) ->
                Map.add name fields records,
                Map.add name typeParams parameters,
                aliases,
                variants
            | AST.TypeAlias (name, typeParams, target) ->
                records,
                parameters,
                Map.add name (typeParams, target) aliases,
                variants
            | AST.SumTypeDef (name, typeParams, cases) ->
                let updated =
                    cases
                    |> List.fold (fun current variant ->
                        let tag =
                            match CheckedAST.tryFindConstructorId name variant.Name symbols with
                            | Some id -> AST.constructorRuntimeTag id
                            | None ->
                                Crash.crash $"Missing constructor identity for '{name}.{variant.Name}'"
                        let info = name, typeParams, tag, variant.Fields
                        current
                        |> Map.add variant.Name info
                        |> Map.add $"{name}.{variant.Name}" info) variants
                records, parameters, aliases, updated)
            (Map.empty, Map.empty, Map.empty, Map.empty)
    let indexedRecords =
        CheckingTypes.indexTypeRegistry variantLookup recordParams recordTypes
    let indexedSums = CheckingTypes.indexSumTypeRegistry variantLookup
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let functions, values, parameterNames, genericFunctions =
        topLevels
        |> List.fold (fun (functions, values, names, generics) item ->
            match item with
            | CheckedAST.FunctionDef definition ->
                let parameters = CheckedAST.functionParameterTypes definition
                let parameterNames =
                    parameters
                    |> AST.NonEmptyList.toList
                    |> List.map (fun (id, _) ->
                        match CheckedAST.bindingName id symbols with
                        | Some name -> name
                        | None -> Crash.crash $"Missing parameter name in '{definition.Name}'")
                let signature =
                    AST.TFunction (
                        parameters |> AST.NonEmptyList.toList |> List.map snd,
                        CheckedAST.functionReturnType definition)
                let genericFunctions =
                    if List.isEmpty definition.TypeParams then generics
                    else Map.add definition.Name definition.TypeParams generics
                Map.add definition.Name signature functions,
                values,
                Map.add definition.Name parameterNames names,
                genericFunctions
            | CheckedAST.ValueDef definition ->
                let typ = CheckedAST.semanticType definition.Type
                Map.add definition.Name typ functions,
                Map.add definition.Name typ values,
                names,
                generics
            | _ -> functions, values, names, generics)
            (Map.empty, Map.empty, Map.empty, Map.empty)
    let sourceFunctions =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef definition -> Some definition.Name
            | _ -> None)
        |> Set.ofList
    let registeredFunctions =
        Set.union sourceFunctions (moduleRegistry |> Map.keys |> Set.ofSeq)
    let candidate name identity provenance =
        match NameResolution.candidate name identity provenance with
        | Some item -> item
        | None -> Crash.crash $"Invalid checked declaration name '{name}'"
    let namespaceAndTerminal (name: string) =
        let segments = name.Split('.') |> Array.toList
        match List.rev segments with
        | terminal :: reversedModule ->
            let namespaceIdentity =
                match List.rev reversedModule |> AST.NonEmptyList.tryFromList with
                | None -> NameResolution.RootNamespace
                | Some modules -> NameResolution.ModuleNamespace modules
            namespaceIdentity, terminal
        | [] -> Crash.crash "Checked declaration has an empty name"
    let sourceCandidates =
        topLevels
        |> List.indexed
        |> List.collect (fun (index, item) ->
            match item with
            | CheckedAST.FunctionDef definition ->
                let namespaceIdentity, terminal = namespaceAndTerminal definition.Name
                let identity =
                    NameResolution.ModuleFunction (
                        namespaceIdentity,
                        terminal,
                        $"source:{index}:{definition.Name}")
                let versioned = $"{definition.Name}_v0"
                let visible =
                    if definition.Name.EndsWith("_v0")
                       || Set.contains versioned registeredFunctions then
                        [definition.Name]
                    else [definition.Name; versioned]
                visible
                |> List.map (fun spelling ->
                    candidate spelling identity
                        (NameResolution.SourceDeclaration definition.Name))
            | CheckedAST.ValueDef definition ->
                let namespaceIdentity, terminal = namespaceAndTerminal definition.Name
                [candidate definition.Name
                    (NameResolution.ModuleValue (namespaceIdentity, terminal))
                    (NameResolution.SourceDeclaration definition.Name)]
            | CheckedAST.TypeDef (_, definition) ->
                let typeDef = CheckedAST.semanticTypeDef definition
                let typeName, cases =
                    match typeDef with
                    | AST.RecordDef (name, _, _)
                    | AST.TypeAlias (name, _, _) -> name, []
                    | AST.SumTypeDef (name, _, cases) -> name, cases
                let typeCandidate =
                    candidate typeName (NameResolution.UserType typeName)
                        (NameResolution.SourceDeclaration typeName)
                let constructors =
                    cases
                    |> List.collect (fun variant ->
                        let identity =
                            NameResolution.ConstructorSymbol (typeName, variant.Name)
                        let provenance =
                            NameResolution.SourceDeclaration $"{typeName}.{variant.Name}"
                        [candidate variant.Name identity provenance
                         candidate $"{typeName}.{variant.Name}" identity provenance])
                typeCandidate :: constructors
            | CheckedAST.Expression _ -> [])
    let resolutionEnv =
        ResolveDeclarations.declarationResolutionEnvironment [] moduleRegistry true
        |> NameResolution.filterCandidates (fun candidate ->
            match candidate.Provenance with
            | NameResolution.CompilerExtension name ->
                not (Set.contains name sourceFunctions)
            | _ -> true)
        |> NameResolution.addCandidates sourceCandidates
    {
        TypeCatalog = CheckedAST.typeCatalog symbols
        FunctionCatalog = CheckedAST.functionCatalog symbols
        TypeReg = recordTypes
        IndexedTypeReg = indexedRecords
        RecordTypeNames = recordTypes |> Map.keys |> Set.ofSeq
        VariantLookup = variantLookup
        IndexedSumTypeReg = indexedSums
        SumTypeNames = indexedSums |> Map.keys |> Set.ofSeq
        FuncEnv = functions
        Values = values
        FuncParamNames = parameterNames
        GenericFuncReg = {
            Functions = genericFunctions
            RequireExplicitTypeArgsForBareCalls = false
        }
        GenericFuncDefs = Map.empty
        ModuleRegistry = moduleRegistry
        AliasReg = aliases
        ResolutionEnv = resolutionEnv
    }
