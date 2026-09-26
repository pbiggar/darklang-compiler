// CheckedAST.fs - Phase-safe syntax accepted by compiler preparation and ANF lowering.
//
// The parser/checker implementation still uses AST internally while resolving
// and inferring source syntax.  Successful checking crosses this boundary once;
// downstream passes cannot represent missing lambda types, unresolved nominal
// references, unchecked values, or partially resolved recursion metadata.

module CheckedAST

open ResultList

type LetPattern =
    | LPUnit
    | LPWildcard
    | LPVariable of AST.BindingId
    | LPTuple of first:LetPattern * second:LetPattern * rest:LetPattern list

/// Checked source tuple expressions always have at least two elements.
type TupleElements<'a> = {
    First: 'a
    Second: 'a
    Rest: 'a list
}

let tupleElementsToList tuple = tuple.First :: tuple.Second :: tuple.Rest

let tupleElementsFromList = function
    | first :: second :: rest -> Some { First = first; Second = second; Rest = rest }
    | _ -> None

let tupleElementsOfList elements =
    match tupleElementsFromList elements with
    | Some tuple -> tuple
    | None -> Crash.crash "checked tuple has fewer than two elements"

let mapTupleElements f tuple =
    { First = f tuple.First; Second = f tuple.Second; Rest = List.map f tuple.Rest }

/// A checked callable signature cannot retain a call-local inference identity.
/// Nominal and internal signature types remain available for specialization
/// and privileged runtime helpers.
type CheckedSignatureType = private CheckedSignatureType of AST.SemanticType

let signatureSemanticType (CheckedSignatureType typ) = typ

type Pattern =
    | PUnit
    | PWildcard
    | PVariable of AST.BindingId
    | PConstructor of constructor:AST.ConstructorId * fields:Pattern list
    | PInt64 of int64
    | PBigInt of System.Numerics.BigInteger
    | PInt128Literal of System.Int128
    | PInt8Literal of sbyte
    | PInt16Literal of int16
    | PInt32Literal of int32
    | PUInt8Literal of byte
    | PUInt16Literal of uint16
    | PUInt32Literal of uint32
    | PUInt64Literal of uint64
    | PUInt128Literal of System.UInt128
    | PBool of bool
    | PString of string
    | PChar of string
    | PFloat of float
    | PTuple of Pattern list
    | PList of Pattern list
    | PListCons of head:Pattern list * tail:Pattern
    | POr of Pattern AST.NonEmptyList

type LambdaParameter = {
    Pattern: LetPattern
    Type: CheckedSignatureType
}

type RecordReference = {
    TypeId: AST.TypeId
    TypeArgs: AST.SemanticType list
}

type ConstructorReference = {
    TypeId: AST.TypeId
    ConstructorId: AST.ConstructorId
}

/// A checked record literal contains each declaration slot exactly once.
/// The list retains source evaluation order; layout order is selected only
/// after every initializer has been evaluated.
type RecordFields<'a> = private RecordFields of (AST.FieldId * 'a) list

let recordFieldsInSourceOrder (RecordFields fields) = fields

let mapRecordFields f (RecordFields fields) =
    fields |> List.map (fun (field, value) -> field, f value) |> RecordFields

let traverseRecordFields f (RecordFields fields) =
    fields
    |> ResultList.traverse (fun (field, value) -> f value |> Result.map (fun value' -> field, value'))
    |> Result.map RecordFields

let mapFoldRecordFields f state (RecordFields fields) =
    let mapped, finalState =
        fields
        |> List.mapFold (fun current (field, value) ->
            let value', next = f current value
            (field, value'), next) state
    RecordFields mapped, finalState

let traverseStateRecordFields f state (RecordFields fields) =
    fields
    |> List.fold (fun result (field, value) ->
        result
        |> Result.bind (fun (reversed, current) ->
            f value current
            |> Result.map (fun (value', next) -> ((field, value') :: reversed, next))))
        (Ok ([], state))
    |> Result.map (fun (reversed, finalState) -> RecordFields (List.rev reversed), finalState)

let completeRecordFields owner fieldCount fields : Result<RecordFields<'a>, string> =
    let valid =
        if fieldCount <= 64 then
            let count, seen, valid =
                fields
                |> List.fold (fun (count, seen, valid) (field, _) ->
                    let index = AST.fieldRuntimeIndex field
                    let bit = if index >= 0 && index < fieldCount then 1UL <<< index else 0UL
                    (count + 1,
                     seen ||| bit,
                     valid && AST.fieldIdOwner field = owner && bit <> 0UL && (seen &&& bit) = 0UL))
                    (0, 0UL, true)
            valid && count = fieldCount
        else
            let count, indices, valid =
                fields
                |> List.fold (fun (count, indices, valid) (field, _) ->
                    let index = AST.fieldRuntimeIndex field
                    (count + 1,
                     Set.add index indices,
                     valid && AST.fieldIdOwner field = owner && index >= 0 && index < fieldCount))
                    (0, Set.empty, true)
            valid && count = fieldCount && Set.count indices = fieldCount
    if not valid then
        Error "record literal does not contain exactly the declared field slots"
    else
        Ok (RecordFields fields)

type StringPart =
    | StringText of string
    | StringExpr of Expr

and Expr =
    | UnitLiteral
    | Int64Literal of int64
    | Int128Literal of System.Int128
    | Int8Literal of sbyte
    | Int16Literal of int16
    | Int32Literal of int32
    | UInt8Literal of byte
    | UInt16Literal of uint16
    | UInt32Literal of uint32
    | UInt64Literal of uint64
    | UInt128Literal of System.UInt128
    | BigIntLiteral of System.Numerics.BigInteger
    | BoolLiteral of bool
    | StringLiteral of string
    | BlobLiteral of string
    | CharLiteral of string
    | FloatLiteral of float
    | InterpolatedString of StringPart list
    | BinOp of AST.BinOp * Expr * Expr
    | UnaryOp of AST.UnaryOp * Expr
    | Let of pattern:LetPattern * value:Expr * body:Expr
    | RecursiveLet of recursion:AST.TypedRecursiveMember * value:Expr * body:Expr
    | Local of AST.BindingId
    | If of cond:Expr * thenBranch:Expr * elseBranch:Expr
    | Sequence of first:Expr * next:Expr
    | Call of functionId:AST.FunctionId * args:AST.NonEmptyList<Expr>
    | TypeApp of functionId:AST.FunctionId * typeArgs:AST.SemanticType list * args:AST.NonEmptyList<Expr>
    | TupleLiteral of TupleElements<Expr>
    | TupleAccess of tuple:Expr * index:int
    | DictLiteral of keyType:AST.SemanticType * valueType:AST.SemanticType * entries:(Expr * Expr) list
    | RecordLiteral of reference:RecordReference * fields:RecordFields<Expr>
    | RecordUpdate of record:Expr * updates:(AST.FieldId * Expr) list
    | RecordAccess of record:Expr * field:AST.FieldId
    | Constructor of reference:ConstructorReference * fields:Expr list
    | Match of scrutinee:Expr * cases:AST.NonEmptyList<MatchCase>
    | ListLiteral of Expr list
    | Lambda of parameters:AST.NonEmptyList<LambdaParameter> * returnAnnotation:CheckedSignatureType option * body:Expr
    | Apply of func:Expr * args:AST.NonEmptyList<Expr>
    | IndirectApply of func:Expr * args:AST.NonEmptyList<Expr>
    | FuncRef of AST.FunctionId
    | Closure of AST.FunctionId * captures:Expr list
    | RuntimeError of message:string
    | BoundaryRender of renderer:AST.FunctionId * value:Expr

and MatchCase = {
    Patterns: AST.NonEmptyList<Pattern>
    Guard: Expr option
    Body: Expr
}

type FunctionDef = {
    Id: AST.FunctionId
    Name: string
    TypeParams: string list
    Params: AST.NonEmptyList<AST.BindingId * CheckedSignatureType>
    ReturnType: CheckedSignatureType
    Body: Expr
    Recursion: AST.TypedRecursiveMember option
}

let functionParameterTypes (definition: FunctionDef) =
    definition.Params
    |> AST.NonEmptyList.map (fun (id, typ) -> id, signatureSemanticType typ)

let functionReturnType (definition: FunctionDef) =
    signatureSemanticType definition.ReturnType

type ValueDef = {
    Id: AST.BindingId
    Name: string
    Type: AST.SemanticType
    Body: Expr
}

type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of AST.TypeId * AST.TypeDef
    | ValueDef of ValueDef
    | Expression of Expr

type SemanticMetadata = {
    TypeNames: Map<AST.TypeId, string>
}

/// Stable IDs inherited by the next checked unit; only names touched by that
/// unit are copied into its own Symbols table during conversion.
type TypeCatalog = private {
    Names: Map<AST.TypeId, string>
    Ids: Map<string, AST.TypeId>
    NextOrdinal: int
}

let emptyTypeCatalog = { Names = Map.empty; Ids = Map.empty; NextOrdinal = 0 }

/// Immutable declaration catalog shared by independently checked units.
/// Checked bodies own their lexical BindingIds; this catalog contains only
/// cross-unit declarations and an allocation cursor used while constructing a
/// new body.
type GlobalCatalog = private {
    BindingNames: Map<AST.BindingId, string>
    ValueIds: Map<string, AST.BindingId>
    NextBindingOrdinal: int
    FunctionNames: Map<AST.FunctionId, string>
    FunctionIds: Map<string, AST.FunctionId>
    TypeNames: Map<AST.TypeId, string>
    TypeIds: Map<string, AST.TypeId>
    BaseTypes: TypeCatalog
    NextTypeOrdinal: int
    ConstructorNames: Map<AST.ConstructorId, string * string>
    ConstructorIds: Map<string * string, AST.ConstructorId>
    ConstructorLookups: Map<string, string * string list * int * AST.SemanticType list> list
    FieldNames: Map<AST.FieldId, string * string>
    FieldIds: Map<string * string, AST.FieldId>
}

type Symbols = GlobalCatalog

type Program = Program of Symbols * TopLevel list

let private emptySymbolsWithTypes baseTypes =
    let startId = AST.functionIdForName "_start"
    let programEntryId = AST.functionIdForName "__dark_compiler_program_entry"
    { BindingNames = Map.empty
      ValueIds = Map.empty
      NextBindingOrdinal = -1
      FunctionNames =
        Map.ofList [startId, "_start"; programEntryId, "__dark_compiler_program_entry"]
      FunctionIds =
        Map.ofList ["_start", startId; "__dark_compiler_program_entry", programEntryId]
      TypeNames = Map.empty
      TypeIds = Map.empty
      BaseTypes = baseTypes
      NextTypeOrdinal = baseTypes.NextOrdinal
      ConstructorNames = Map.empty
      ConstructorIds = Map.empty
      ConstructorLookups = []
      FieldNames = Map.empty
      FieldIds = Map.empty }

let emptySymbols () = emptySymbolsWithTypes emptyTypeCatalog

let private registerBinding id name symbols =
    { symbols with BindingNames = Map.add id name symbols.BindingNames }

let allocateBinding name symbols =
    let id = AST.namedBindingId symbols.NextBindingOrdinal name
    let symbols' =
        { symbols with
            BindingNames = Map.add id name symbols.BindingNames
            NextBindingOrdinal = symbols.NextBindingOrdinal - 1 }
    (id, symbols')

let internValue name symbols =
    match Map.tryFind name symbols.ValueIds with
    | Some id -> (id, symbols)
    | None ->
        let id = AST.topLevelValueId name
        (id, { symbols with ValueIds = Map.add name id symbols.ValueIds })

let bindingName id (_symbols: Symbols) : string option = AST.bindingDisplayName id
let tryFindValueId name symbols = Map.tryFind name symbols.ValueIds

let internFunction name symbols =
    match Map.tryFind name symbols.FunctionIds with
    | Some id -> (id, symbols)
    | None ->
        let id = AST.functionIdForName name
        (id,
         { symbols with
             FunctionIds = Map.add name id symbols.FunctionIds
             FunctionNames = Map.add id name symbols.FunctionNames })

let internType name symbols =
    match Map.tryFind name symbols.TypeIds with
    | Some id -> (id, symbols)
    | None ->
        let id, nextOrdinal =
            match Map.tryFind name symbols.BaseTypes.Ids with
            | Some id -> id, symbols.NextTypeOrdinal
            | None -> AST.typeId symbols.NextTypeOrdinal, symbols.NextTypeOrdinal + 1
        (id,
         { symbols with
             TypeIds = Map.add name id symbols.TypeIds
             TypeNames = Map.add id name symbols.TypeNames
             NextTypeOrdinal = nextOrdinal })

let internConstructor typeName name tag symbols =
    let (_, symbols) = internType typeName symbols
    match Map.tryFind (typeName, name) symbols.ConstructorIds with
    | Some id -> (id, symbols)
    | None ->
        let owner =
            Map.tryFind typeName symbols.TypeIds
            |> Option.defaultWith (fun () -> Crash.crash $"Constructor owner '{typeName}' is absent")
        let id = AST.constructorId owner name tag
        let symbols =
            { symbols with
                ConstructorIds = Map.add (typeName, name) id symbols.ConstructorIds
                ConstructorNames = Map.add id (typeName, name) symbols.ConstructorNames }
        (id, symbols)

let internField typeName name index symbols =
    let (_, symbols) = internType typeName symbols
    match Map.tryFind (typeName, name) symbols.FieldIds with
    | Some id -> (id, symbols)
    | None ->
        let owner =
            Map.tryFind typeName symbols.TypeIds
            |> Option.defaultWith (fun () -> Crash.crash $"Field owner '{typeName}' is absent")
        let id = AST.fieldId owner index
        let symbols =
            { symbols with
                FieldIds = Map.add (typeName, name) id symbols.FieldIds
                FieldNames = Map.add id (typeName, name) symbols.FieldNames }
        (id, symbols)

let functionName id symbols =
    Map.tryFind id symbols.FunctionNames
    |> Option.orElse (AST.tryFunctionCanonicalName id)

let functionNames symbols = symbols.FunctionNames
let tryFindFunctionId name symbols = Map.tryFind name symbols.FunctionIds
let typeName id symbols =
    Map.tryFind id symbols.TypeNames
    |> Option.orElseWith (fun () -> Map.tryFind id symbols.BaseTypes.Names)
let typeNames symbols = symbols.TypeNames
let tryFindTypeId name symbols =
    Map.tryFind name symbols.TypeIds
    |> Option.orElseWith (fun () -> Map.tryFind name symbols.BaseTypes.Ids)
let typeCatalog symbols = {
    Names = Map.fold (fun names id name -> Map.add id name names) symbols.BaseTypes.Names symbols.TypeNames
    Ids = Map.fold (fun ids name id -> Map.add name id ids) symbols.BaseTypes.Ids symbols.TypeIds
    NextOrdinal = symbols.NextTypeOrdinal
}
let constructorInfo id symbols = Map.tryFind id symbols.ConstructorNames
let constructorTag id (_symbols: Symbols) = Some (AST.constructorRuntimeTag id)

let tryFindConstructorId typeName name symbols =
    Map.tryFind (typeName, name) symbols.ConstructorIds
    |> Option.orElseWith (fun () ->
        symbols.ConstructorLookups
        |> List.tryPick (fun lookup ->
            Map.tryFind $"{typeName}.{name}" lookup
            |> Option.bind (fun (owner, _, tag, _) ->
                if owner = typeName then
                    tryFindTypeId owner symbols
                    |> Option.map (fun ownerId -> AST.constructorId ownerId name tag)
                else
                    None)))
let fieldInfo id symbols = Map.tryFind id symbols.FieldNames
let fieldIndex id (_symbols: Symbols) = Some (AST.fieldRuntimeIndex id)

let semanticMetadata symbols : SemanticMetadata = {
    TypeNames = symbols.TypeNames
}

let tryFindFieldId typeName name symbols =
    Map.tryFind (typeName, name) symbols.FieldIds

let programSymbols (Program (symbols, _)) : Symbols = symbols

let programTopLevels (Program (_, topLevels)) : TopLevel list = topLevels

let withProgramTopLevels topLevels (Program (symbols, _)) : Program =
    Program (symbols, topLevels)

/// A checked unit is self-describing: semantic IDs carry canonical identity
/// and lexical IDs carry body-local presentation metadata. Reusable artifacts
/// therefore retain only their fresh-binding cursor, not a projection of the
/// global declaration catalog and not a walk-derived symbol slice.
let catalogForCheckedUnit (symbols: Symbols) : Symbols =
    { emptySymbols () with NextBindingOrdinal = symbols.NextBindingOrdinal }

let bindingCursor (symbols: Symbols) : int = symbols.NextBindingOrdinal

let includeBindingCursor (cursor: int) (symbols: Symbols) : Symbols =
    { symbols with NextBindingOrdinal = min cursor symbols.NextBindingOrdinal }

/// Compose independently checked units structurally. Cross-unit declarations
/// have canonical identities and lexical BindingIds are body-local, so neither
/// checked expressions nor recursive evidence require rewriting.
let composeTopLevels
    (sourceCatalog: Symbols)
    (targetCatalog: Symbols)
    (topLevels: TopLevel list)
    : Symbols * TopLevel list =
    let merge source target =
        Map.fold (fun combined key value -> Map.add key value combined) target source
    let mergeTypeName names id name =
        match Map.tryFind id names with
        | Some existing when existing <> name ->
            Crash.crash "Composed type catalogs assign one TypeId to different names"
        | _ -> Map.add id name names
    let mergeTypeId ids name id =
        match Map.tryFind name ids with
        | Some existing when existing <> id ->
            Crash.crash "Composed type catalogs assign different TypeIds to one name"
        | _ -> Map.add name id ids
    let catalog = {
        BindingNames = merge sourceCatalog.BindingNames targetCatalog.BindingNames
        ValueIds = merge sourceCatalog.ValueIds targetCatalog.ValueIds
        NextBindingOrdinal = min sourceCatalog.NextBindingOrdinal targetCatalog.NextBindingOrdinal
        FunctionNames = merge sourceCatalog.FunctionNames targetCatalog.FunctionNames
        FunctionIds = merge sourceCatalog.FunctionIds targetCatalog.FunctionIds
        TypeNames = Map.fold mergeTypeName targetCatalog.TypeNames sourceCatalog.TypeNames
        TypeIds = Map.fold mergeTypeId targetCatalog.TypeIds sourceCatalog.TypeIds
        BaseTypes = targetCatalog.BaseTypes
        NextTypeOrdinal = max sourceCatalog.NextTypeOrdinal targetCatalog.NextTypeOrdinal
        ConstructorNames = merge sourceCatalog.ConstructorNames targetCatalog.ConstructorNames
        ConstructorIds = merge sourceCatalog.ConstructorIds targetCatalog.ConstructorIds
        ConstructorLookups = sourceCatalog.ConstructorLookups @ targetCatalog.ConstructorLookups
        FieldNames = merge sourceCatalog.FieldNames targetCatalog.FieldNames
        FieldIds = merge sourceCatalog.FieldIds targetCatalog.FieldIds
    }
    (catalog, topLevels)

let valueDefName (valueDef: ValueDef) : string = valueDef.Name

let valueDefId (valueDef: ValueDef) : AST.BindingId = valueDef.Id

let valueDefBody (valueDef: ValueDef) : Expr = valueDef.Body

let programValues (Program (_, topLevels)) : Map<string, AST.SemanticType * Expr> =
    topLevels
    |> List.choose (function
        | ValueDef valueDef -> Some (valueDef.Name, (valueDef.Type, valueDef.Body))
        | _ -> None)
    |> Map.ofList

let rec letPatternBindings (pattern: LetPattern) : AST.BindingId list =
    match pattern with
    | LPVariable name -> [name]
    | LPTuple (first, second, rest) ->
        first :: second :: rest |> List.collect letPatternBindings
    | LPUnit | LPWildcard -> []

let rec patternBindings pattern : AST.BindingId list =
    match pattern with
    | PVariable id -> [id]
    | PConstructor (_, fields) -> fields |> List.collect patternBindings
    | PTuple patterns | PList patterns -> List.collect patternBindings patterns
    | PListCons (heads, tail) -> List.collect patternBindings heads @ patternBindings tail
    | POr alternatives -> alternatives |> AST.NonEmptyList.head |> patternBindings
    | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
    | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
    | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _
    | PString _ | PChar _ | PFloat _ -> []

let recursiveBindingName (memberInfo: AST.TypedRecursiveMember) : string =
    memberInfo.Resolved.Parsed.SourceName

let recursiveBindingId (memberInfo: AST.TypedRecursiveMember) : AST.BindingId =
    memberInfo.Resolved.Parsed.Binding

let recursiveBindingAvailability
    (memberInfo: AST.TypedRecursiveMember)
    : AST.RecursiveAvailability =
    memberInfo.Resolved.Availability

let private conversionError location detail =
    Error $"Checked AST construction failed at {location}: {detail}"

let private map2 f first second =
    first
    |> Result.bind (fun firstValue ->
        second |> Result.map (fun secondValue -> f firstValue secondValue))

/// Inference identities are meaningful only while checking a call. Erase them
/// as types cross the checked-program boundary; downstream specialization
/// needs stable, alpha-equivalent names for still-open generic arguments.
let rec normalizeInferenceType (typ: AST.SemanticType) : AST.SemanticType =
    match typ with
    | AST.TInferenceVar (displayName, _) -> AST.TVar displayName
    | AST.TFunction (parameters, result) ->
        AST.TFunction (List.map normalizeInferenceType parameters, normalizeInferenceType result)
    | AST.TTuple elements -> AST.TTuple (List.map normalizeInferenceType elements)
    | AST.TRecord (name, arguments) -> AST.TRecord (name, List.map normalizeInferenceType arguments)
    | AST.TSum (name, arguments) -> AST.TSum (name, List.map normalizeInferenceType arguments)
    | AST.TList element -> AST.TList (normalizeInferenceType element)
    | AST.TStream element -> AST.TStream (normalizeInferenceType element)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (normalizeInferenceType keyType, normalizeInferenceType valueType)
    | AST.TVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
    | AST.TUnit | AST.TNever | AST.TInternalRawPtr -> typ

let checkedSignatureType typ = CheckedSignatureType (normalizeInferenceType typ)

let checkedSignatureParams parameters =
    AST.NonEmptyList.map (fun (id, typ) -> id, checkedSignatureType typ) parameters

let private convertRecordReference
    (reference: AST.RecordReference)
    (symbols: Symbols)
    : RecordReference * Symbols =
    let typeId, symbols = internType reference.ResolvedTypeName symbols
    ({ TypeId = typeId; TypeArgs = List.map normalizeInferenceType reference.TypeArgs }, symbols)

let private convertConstructorReference
    (location: string)
    (reference: AST.ConstructorReference)
    (variantName: string)
    (symbols: Symbols)
    : Result<ConstructorReference, string> =
    match reference with
    | AST.ResolvedConstructor _ ->
        match AST.constructorReferenceTypeName reference with
        | Some typeName ->
            match tryFindConstructorId typeName variantName symbols with
            | Some id ->
                let typeId = AST.constructorIdOwner id
                Ok { TypeId = typeId; ConstructorId = id }
            | None -> conversionError location "resolved constructor has no semantic identity"
        | None -> conversionError location "resolved constructor has no declaring type"
    | AST.UnresolvedConstructor _ ->
        conversionError location "constructor reference was not resolved"

let private extendEnvironment bindings environment =
    bindings
    |> List.fold (fun current (name, id) -> Map.add name id current) environment

let rec private allocateLetPattern symbols pattern =
    match pattern with
    | AST.LPUnit -> (LPUnit, [], symbols)
    | AST.LPWildcard -> (LPWildcard, [], symbols)
    | AST.LPVariable name ->
        let (id, symbols') = allocateBinding name symbols
        (LPVariable id, [(name, id)], symbols')
    | AST.LPTuple (first, second, rest) ->
        let (first', firstBindings, afterFirst) = allocateLetPattern symbols first
        let (second', secondBindings, afterSecond) = allocateLetPattern afterFirst second
        let (rest', restBindings, following) =
            rest
            |> List.fold (fun (converted, bindings, currentSymbols) item ->
                let (item', itemBindings, nextSymbols) = allocateLetPattern currentSymbols item
                (item' :: converted, bindings @ itemBindings, nextSymbols)) ([], [], afterSecond)
        (LPTuple (first', second', List.rev rest'), firstBindings @ secondBindings @ restBindings, following)

let rec private convertPattern bindingIds symbols pattern =
    let convert = convertPattern bindingIds symbols
    match pattern with
    | AST.PUnit -> PUnit
    | AST.PWildcard -> PWildcard
    | AST.PVar name ->
        match Map.tryFind name bindingIds with
        | Some id -> PVariable id
        | None -> PWildcard
    | AST.PConstructor (name, _) ->
        Crash.crash $"Unresolved constructor pattern '{name}' crossed the checked boundary"
    | AST.PResolvedConstructor (typeName, name, _, fields) ->
        match tryFindConstructorId typeName name symbols with
        | Some id -> PConstructor (id, List.map convert fields)
        | None -> Crash.crash "Resolved constructor pattern has no semantic identity"
    | AST.PInt64 value -> PInt64 value
    | AST.PBigInt value -> PBigInt value
    | AST.PInt128Literal value -> PInt128Literal value
    | AST.PInt8Literal value -> PInt8Literal value
    | AST.PInt16Literal value -> PInt16Literal value
    | AST.PInt32Literal value -> PInt32Literal value
    | AST.PUInt8Literal value -> PUInt8Literal value
    | AST.PUInt16Literal value -> PUInt16Literal value
    | AST.PUInt32Literal value -> PUInt32Literal value
    | AST.PUInt64Literal value -> PUInt64Literal value
    | AST.PUInt128Literal value -> PUInt128Literal value
    | AST.PBool value -> PBool value
    | AST.PString value -> PString value
    | AST.PChar value -> PChar value
    | AST.PFloat value -> PFloat value
    | AST.PTuple patterns -> PTuple (List.map convert patterns)
    | AST.PList patterns -> PList (List.map convert patterns)
    | AST.PListCons (heads, tail) -> PListCons (List.map convert heads, convert tail)
    | AST.POr alternatives ->
        alternatives
        |> AST.NonEmptyList.toList
        |> List.map convert
        |> AST.NonEmptyList.fromList
        |> POr

let private allocateMatchBindings symbols pattern =
    let rec allBindings pattern =
        match pattern with
        | AST.PVar name -> [name]
        | AST.PConstructor (_, fields) -> fields |> List.collect allBindings
        | AST.PResolvedConstructor (_, _, _, fields) -> fields |> List.collect allBindings
        | AST.PTuple patterns | AST.PList patterns -> List.collect allBindings patterns
        | AST.PListCons (heads, tail) -> List.collect allBindings heads @ allBindings tail
        | AST.POr alternatives -> alternatives |> AST.NonEmptyList.head |> allBindings
        | _ -> []
    match AST.validateBinders (AST.MatchBinderPattern pattern) with
    | Error detail -> conversionError "match pattern" detail
    | Ok names ->
        let (bindings, symbols') =
            allBindings pattern
            |> List.mapFold (fun currentSymbols name ->
                let (id, nextSymbols) = allocateBinding name currentSymbols
                ((name, id), nextSymbols)) symbols
        Ok (bindings |> Map.ofList, symbols')

let rec private convertExpr recordFieldCounts location environment symbols expr : Result<Expr * Symbols, string> =
    let convert = convertExpr recordFieldCounts location environment
    let convertList values currentSymbols =
        values
        |> List.fold (fun result value ->
            result
            |> Result.bind (fun (converted, state) ->
                convert state value
                |> Result.map (fun (value', next) -> (value' :: converted, next)))) (Ok ([], currentSymbols))
        |> Result.map (fun (converted, state) -> (List.rev converted, state))
    let convertNonEmpty values currentSymbols =
        convertList (AST.NonEmptyList.toList values) currentSymbols
        |> Result.map (fun (converted, state) -> (AST.NonEmptyList.fromList converted, state))
    let convertPair first second currentSymbols =
        convert currentSymbols first
        |> Result.bind (fun (first', afterFirst) ->
            convert afterFirst second
            |> Result.map (fun (second', following) -> (first', second', following)))
    let convertFields fields currentSymbols =
        fields
        |> List.fold (fun result (reference: AST.RecordFieldReference, value) ->
            result
            |> Result.bind (fun (converted, state) ->
                match reference.ResolvedTypeName, reference.ResolvedFieldIndex with
                | Some typeName, Some fieldIndex ->
                    let (fieldId, state) =
                        internField typeName reference.SourceFieldName fieldIndex state
                    convert state value
                    |> Result.map (fun (value', next) -> ((fieldId, value') :: converted, next))
                | _ ->
                    conversionError location "record field has no resolved owner and declaration slot"))
            (Ok ([], currentSymbols))
        |> Result.map (fun (converted, state) -> (List.rev converted, state))
    match expr with
    | AST.UnitLiteral -> Ok (UnitLiteral, symbols)
    | AST.Int64Literal value -> Ok (Int64Literal value, symbols)
    | AST.Int128Literal value -> Ok (Int128Literal value, symbols)
    | AST.Int8Literal value -> Ok (Int8Literal value, symbols)
    | AST.Int16Literal value -> Ok (Int16Literal value, symbols)
    | AST.Int32Literal value -> Ok (Int32Literal value, symbols)
    | AST.UInt8Literal value -> Ok (UInt8Literal value, symbols)
    | AST.UInt16Literal value -> Ok (UInt16Literal value, symbols)
    | AST.UInt32Literal value -> Ok (UInt32Literal value, symbols)
    | AST.UInt64Literal value -> Ok (UInt64Literal value, symbols)
    | AST.UInt128Literal value -> Ok (UInt128Literal value, symbols)
    | AST.BigIntLiteral value -> Ok (BigIntLiteral value, symbols)
    | AST.BoolLiteral value -> Ok (BoolLiteral value, symbols)
    | AST.StringLiteral value -> Ok (StringLiteral value, symbols)
    | AST.CharLiteral value -> Ok (CharLiteral value, symbols)
    | AST.FloatLiteral value -> Ok (FloatLiteral value, symbols)
    | AST.InterpolatedString parts ->
        parts
        |> List.fold (fun result part ->
            result
            |> Result.bind (fun (converted, state) ->
                match part with
                | AST.StringText text -> Ok (StringText text :: converted, state)
                | AST.StringExpr inner ->
                    convert state inner
                    |> Result.map (fun (inner', next) -> (StringExpr inner' :: converted, next)))) (Ok ([], symbols))
        |> Result.map (fun (converted, state) -> (InterpolatedString (List.rev converted), state))
    | AST.BinOp (op, left, right) ->
        convertPair left right symbols
        |> Result.map (fun (left', right', state) -> (BinOp (op, left', right'), state))
    | AST.UnaryOp (op, inner) ->
        convert symbols inner |> Result.map (fun (value, state) -> (UnaryOp (op, value), state))
    | AST.Let (pattern, value, body) ->
        convert symbols value
        |> Result.bind (fun (value', afterValue) ->
            let (pattern', bindings, afterPattern) = allocateLetPattern afterValue pattern
            let bodyEnvironment = extendEnvironment bindings environment
            convertExpr recordFieldCounts location bodyEnvironment afterPattern body
            |> Result.map (fun (body', following) -> (Let (pattern', value', body'), following)))
    | AST.RecursiveLet (recursion, value, body) ->
        match recursion with
        | AST.TypedRecursiveBinding typed ->
            let name = typed.Resolved.Parsed.SourceName
            let id = typed.Resolved.Parsed.Binding
            let withSymbol = registerBinding id name symbols
            let bodyEnvironment = Map.add name id environment
            let valueEnvironment =
                match typed.Resolved.Availability with
                | AST.OrdinaryBinding -> environment
                | AST.SelfRecursiveMember -> bodyEnvironment
                | AST.MutualRecursiveMember | AST.CompletedGroupMember | AST.ImportedGroupMember ->
                    bodyEnvironment
            convertExpr recordFieldCounts location valueEnvironment withSymbol value
            |> Result.bind (fun (value', afterValue) ->
                convertExpr recordFieldCounts location bodyEnvironment afterValue body
                |> Result.map (fun (body', following) ->
                    let typed = { typed with MonomorphicType = normalizeInferenceType typed.MonomorphicType }
                    (RecursiveLet (typed, value', body'), following)))
        | _ -> conversionError location "recursive let has no typed recursion evidence"
    | AST.Var name ->
        if name = "Builtin.testNan" then Ok (FloatLiteral System.Double.NaN, symbols)
        elif name = "Builtin.testInfinity" then Ok (FloatLiteral System.Double.PositiveInfinity, symbols)
        elif name = "Builtin.blobEmpty" then Ok (BlobLiteral "", symbols)
        else
            match Map.tryFind name environment with
            | Some id -> Ok (Local id, symbols)
            | None ->
                match tryFindValueId name symbols with
                | Some id -> Ok (Local id, symbols)
                | None ->
                    match tryFindFunctionId name symbols with
                    | Some id -> Ok (FuncRef id, symbols)
                    | None ->
                        // Successful checking has already established that a
                        // remaining non-value variable denotes a function.
                        // Allocate its dense identity at this boundary just as
                        // direct Call and FuncRef nodes do.
                        let (id, symbols) = internFunction name symbols
                        Ok (FuncRef id, symbols)
    | AST.If (condition, thenBranch, elseBranch) ->
        convert symbols condition
        |> Result.bind (fun (condition', afterCondition) ->
            convertPair thenBranch elseBranch afterCondition
            |> Result.map (fun (thenBranch', elseBranch', following) ->
                (If (condition', thenBranch', elseBranch'), following)))
    | AST.Sequence (first, next) ->
        convertPair first next symbols
        |> Result.map (fun (first', next', state) -> (Sequence (first', next'), state))
    | AST.Apply (AST.Var name, typeArgs, args) ->
        convertNonEmpty args symbols
        |> Result.map (fun (converted, state) ->
            match typeArgs, Map.tryFind name environment with
            | [], Some id -> (Apply (Local id, converted), state)
            | [], None ->
                let (functionId, state) = internFunction name state
                (Call (functionId, converted), state)
            | _, _ ->
                let (functionId, state) = internFunction name state
                (TypeApp (functionId, List.map normalizeInferenceType typeArgs, converted), state))
    | AST.TupleLiteral elements ->
        convertList elements symbols
        |> Result.bind (fun (values, state) ->
            match tupleElementsFromList values with
            | Some tuple -> Ok (TupleLiteral tuple, state)
            | None -> conversionError location "tuple literal has fewer than two elements")
    | AST.TupleAccess (tuple, index) ->
        convert symbols tuple |> Result.map (fun (value, state) -> (TupleAccess (value, index), state))
    | AST.DictLiteral (keyType, valueType, entries) ->
        entries
        |> List.fold (fun result (key, value) ->
            result
            |> Result.bind (fun (converted, state) ->
                convertPair key value state
                |> Result.map (fun (key', value', next) -> ((key', value') :: converted, next)))) (Ok ([], symbols))
        |> Result.map (fun (converted, state) ->
            (DictLiteral (normalizeInferenceType keyType, normalizeInferenceType valueType, List.rev converted), state))
    | AST.RecordLiteral (reference, fields) ->
        convertFields fields symbols
        |> Result.bind (fun (converted, state) ->
            let recordName = reference.ResolvedTypeName
            let checkedReference, state = convertRecordReference reference state
            match recordFieldCounts recordName with
            | Some fieldCount ->
                completeRecordFields checkedReference.TypeId fieldCount converted
                |> Result.map (fun complete -> (RecordLiteral (checkedReference, complete), state))
            | None -> conversionError location "record declaration layout is absent")
    | AST.RecordUpdate (record, updates) ->
        convert symbols record
        |> Result.bind (fun (record', afterRecord) ->
            convertFields updates afterRecord
            |> Result.map (fun (updates', following) -> (RecordUpdate (record', updates'), following)))
    | AST.RecordAccess (record, fieldReference) ->
        convert symbols record
        |> Result.bind (fun (value, state) ->
            match fieldReference.ResolvedTypeName, fieldReference.ResolvedFieldIndex with
            | Some typeName, Some fieldIndex ->
                let (fieldId, state) = internField typeName fieldReference.SourceFieldName fieldIndex state
                Ok (RecordAccess (value, fieldId), state)
            | _ -> conversionError location "record field reference was not resolved")
    | AST.Constructor (reference, variantName, fields) ->
        convertConstructorReference location reference variantName symbols
        |> Result.bind (fun reference' ->
            convertList fields symbols
            |> Result.map (fun (fields', state) -> (Constructor (reference', fields'), state)))
    | AST.Match (scrutinee, cases) ->
        convert symbols scrutinee
        |> Result.bind (fun (scrutinee', afterScrutinee) ->
            cases
            |> List.fold (fun result case ->
                result
                |> Result.bind (fun (convertedCases, state) ->
                    let firstPattern = AST.NonEmptyList.head case.Patterns
                    allocateMatchBindings state firstPattern
                    |> Result.bind (fun (bindingIds, afterBindings) ->
                        let patterns =
                            case.Patterns
                            |> AST.NonEmptyList.toList
                            |> List.map (convertPattern bindingIds afterBindings)
                            |> AST.NonEmptyList.fromList
                        let caseEnvironment =
                            extendEnvironment (Map.toList bindingIds) environment
                        let guardResult =
                            match case.Guard with
                            | None -> Ok (None, afterBindings)
                            | Some guard ->
                                convertExpr recordFieldCounts location caseEnvironment afterBindings guard
                                |> Result.map (fun (guard', next) -> (Some guard', next))
                        guardResult
                        |> Result.bind (fun (guard', afterGuard) ->
                            convertExpr recordFieldCounts location caseEnvironment afterGuard case.Body
                            |> Result.map (fun (body', following) ->
                                ({ Patterns = patterns; Guard = guard'; Body = body' } :: convertedCases,
                                 following)))))) (Ok ([], afterScrutinee))
            |> Result.bind (fun (convertedCases, state) ->
                match AST.NonEmptyList.tryFromList (List.rev convertedCases) with
                | Some cases -> Ok (Match (scrutinee', cases), state)
                | None -> conversionError location "match has no cases"))
    | AST.ListLiteral elements ->
        convertList elements symbols |> Result.map (fun (values, state) -> (ListLiteral values, state))
    | AST.Lambda (parameters, returnAnnotation, body) ->
        parameters
        |> AST.NonEmptyList.toList
        |> List.fold (fun result parameter ->
            result
            |> Result.bind (fun (converted, bindings, state) ->
                match parameter.InferredType with
                | None -> conversionError location "lambda parameter has no inferred type"
                | Some typ ->
                    let (pattern', patternBindings, next) = allocateLetPattern state parameter.Pattern
                    Ok ({ Pattern = pattern'; Type = checkedSignatureType typ } :: converted,
                        bindings @ patternBindings,
                        next))) (Ok ([], [], symbols))
        |> Result.bind (fun (convertedParameters, bindings, afterParameters) ->
            let bodyEnvironment = extendEnvironment bindings environment
            convertExpr recordFieldCounts location bodyEnvironment afterParameters body
            |> Result.map (fun (body', following) ->
                (Lambda (AST.NonEmptyList.fromList (List.rev convertedParameters),
                         Option.map checkedSignatureType returnAnnotation, body'),
                 following)))
    | AST.Apply (func, [], args) ->
        convert symbols func
        |> Result.bind (fun (func', afterFunc) ->
            convertNonEmpty args afterFunc
            |> Result.map (fun (args', following) -> (Apply (func', args'), following)))
    | AST.Apply (_, _ :: _, _) ->
        conversionError location "explicit type arguments require a named function"
    | AST.IndirectApply (func, args) ->
        convert symbols func
        |> Result.bind (fun (func', afterFunc) ->
            convertNonEmpty args afterFunc
            |> Result.map (fun (args', following) -> (IndirectApply (func', args'), following)))
    | AST.Closure (name, captures) ->
        convertList captures symbols
        |> Result.map (fun (values, state) ->
            let (functionId, state) = internFunction name state
            (Closure (functionId, values), state))
    | AST.RuntimeError message -> Ok (RuntimeError message, symbols)
    | AST.BoundaryRender (renderer, value) ->
        convert symbols value
        |> Result.map (fun (converted, state) ->
            let (functionId, state) = internFunction renderer state
            (BoundaryRender (functionId, converted), state))

let private convertFunctionWithEnvironment
    recordFieldCounts
    (outerEnvironment: Map<string, AST.BindingId>)
    symbols
    (funcDef: AST.FunctionDef)
    : Result<FunctionDef * Symbols, string> =
    let recursion =
        match funcDef.Recursion with
        | None -> Ok None
        | Some (AST.TypedRecursiveBinding typed) ->
            Ok (Some { typed with MonomorphicType = normalizeInferenceType typed.MonomorphicType })
        | Some _ -> conversionError $"function '{funcDef.Name}'" "function has no typed recursion evidence"
    recursion
    |> Result.bind (fun recursion' ->
        let (functionId, symbols) = internFunction funcDef.Name symbols
        let symbols =
            match recursion' with
            | Some typed -> registerBinding typed.Resolved.Parsed.Binding typed.Resolved.Parsed.SourceName symbols
            | None -> symbols
        funcDef.Params
        |> AST.NonEmptyList.toList
        |> List.mapFold (fun currentSymbols (name, typ) ->
            let (id, next) = allocateBinding name currentSymbols
            ((id, checkedSignatureType typ), next)) symbols
        |> fun (parameters, afterParameters) ->
            let environment =
                List.zip (funcDef.Params |> AST.NonEmptyList.toList |> List.map fst) (parameters |> List.map fst)
                |> List.fold (fun environment (name, id) -> Map.add name id environment) outerEnvironment
            convertExpr recordFieldCounts $"function '{funcDef.Name}'" environment afterParameters funcDef.Body
            |> Result.map (fun (body, following) ->
                ({ Id = functionId
                   Name = funcDef.Name
                   TypeParams = funcDef.TypeParams
                   Params = AST.NonEmptyList.fromList parameters
                   ReturnType = checkedSignatureType funcDef.ReturnType
                   Body = body
                   Recursion = recursion' },
                 following)))

let ofTypedFunction
    (variantLookup: Map<string, string * string list * int * AST.SemanticType list>)
    (recordFieldCounts: string -> int option)
    symbols
    funcDef
    : Result<FunctionDef * Symbols, string> =
    let symbols = { symbols with ConstructorLookups = [variantLookup] }
    convertFunctionWithEnvironment recordFieldCounts Map.empty symbols funcDef

let ofTypedProgram
    (variantLookup: Map<string, string * string list * int * AST.SemanticType list>)
    (externalValueNames: Set<string>)
    (baseTypes: TypeCatalog)
    (recordFieldCounts: string -> int option)
    (AST.Program topLevels)
    : Result<Program, string> =
    let valueEnvironment, initialSymbols =
        topLevels
        |> List.choose (function
            | AST.ValueDef (AST.CheckedValueDef (name, _, _)) -> Some name
            | _ -> None)
        |> Set.ofList
        |> Set.union externalValueNames
        |> Set.toList
        |> List.fold (fun (environment, symbols) name ->
            let (id, symbols) = internValue name symbols
            (Map.add name id environment, symbols)) (Map.empty, emptySymbolsWithTypes baseTypes)
    let initialSymbols =
        topLevels
        |> List.fold (fun symbols topLevel ->
            match topLevel with
            | AST.FunctionDef functionDef ->
                internFunction functionDef.Name symbols |> snd
            | AST.TypeDef typeDef ->
                let name =
                    match typeDef with
                    | AST.RecordDef (name, _, _)
                    | AST.SumTypeDef (name, _, _)
                    | AST.TypeAlias (name, _, _) -> name
                internType name symbols |> snd
            | _ -> symbols) initialSymbols
    let initialSymbols = { initialSymbols with ConstructorLookups = [variantLookup] }
    let initialSymbols =
        topLevels
        |> List.fold (fun symbols topLevel ->
            match topLevel with
            | AST.TypeDef (AST.RecordDef (typeName, _, fields)) ->
                fields
                |> List.indexed
                |> List.fold (fun symbols (index, (fieldName, _)) ->
                    internField typeName fieldName index symbols |> snd) symbols
            | _ -> symbols) initialSymbols
    let convertTopLevel symbols topLevel =
        match topLevel with
        | AST.FunctionDef funcDef ->
            convertFunctionWithEnvironment recordFieldCounts valueEnvironment symbols funcDef
            |> Result.map (fun (converted, state) -> (FunctionDef converted, state))
        | AST.TypeDef typeDef ->
            let name =
                match typeDef with
                | AST.RecordDef (name, _, _)
                | AST.SumTypeDef (name, _, _)
                | AST.TypeAlias (name, _, _) -> name
            let (id, symbols) = internType name symbols
            Ok (TypeDef (id, typeDef), symbols)
        | AST.ValueDef (AST.CheckedValueDef (name, typ, body)) ->
            convertExpr recordFieldCounts $"value '{name}'" valueEnvironment symbols body
            |> Result.map (fun (checkedBody, state) ->
                let id =
                    match Map.tryFind name valueEnvironment with
                    | Some id -> id
                    | None -> Crash.crash "Checked value identity allocation was lost"
                (ValueDef { Id = id; Name = name; Type = normalizeInferenceType typ; Body = checkedBody }, state))
        | AST.ValueDef (AST.UncheckedValueDef (name, _)) ->
            conversionError $"value '{name}'" "value definition was not checked"
        | AST.Expression (_, expr) ->
            convertExpr recordFieldCounts "entry expression" valueEnvironment symbols expr
            |> Result.map (fun (converted, state) -> (Expression converted, state))
    topLevels
    |> List.fold (fun result topLevel ->
        result
        |> Result.bind (fun (converted, symbols) ->
            convertTopLevel symbols topLevel
            |> Result.map (fun (item, next) -> (item :: converted, next)))) (Ok ([], initialSymbols))
    |> Result.map (fun (converted, symbols) -> Program (symbols, List.rev converted))
