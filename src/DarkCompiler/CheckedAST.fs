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
    Type: AST.SemanticType
}

type RecordReference = {
    TypeId: AST.TypeId
    TypeArgs: AST.SemanticType list
}

type ConstructorReference = {
    TypeId: AST.TypeId
    ConstructorId: AST.ConstructorId
}

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
    | TupleLiteral of Expr list
    | TupleAccess of tuple:Expr * index:int
    | DictLiteral of keyType:AST.SemanticType * valueType:AST.SemanticType * entries:(Expr * Expr) list
    | RecordLiteral of reference:RecordReference * fields:(AST.FieldId * Expr) list
    | RecordUpdate of record:Expr * updates:(AST.FieldId * Expr) list
    | RecordAccess of record:Expr * field:AST.FieldId
    | Constructor of reference:ConstructorReference * fields:Expr list
    | Match of scrutinee:Expr * cases:MatchCase list
    | ListLiteral of Expr list
    | Lambda of parameters:AST.NonEmptyList<LambdaParameter> * returnAnnotation:AST.SemanticType option * body:Expr
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
    Params: AST.NonEmptyList<AST.BindingId * AST.SemanticType>
    ReturnType: AST.SemanticType
    Body: Expr
    Recursion: AST.TypedRecursiveMember option
}

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
    ConstructorNames: Map<AST.ConstructorId, string * string>
    ConstructorIds: Map<string * string, AST.ConstructorId>
    FieldNames: Map<AST.FieldId, string * string>
    FieldIds: Map<string * string, AST.FieldId>
}

type Symbols = GlobalCatalog

type Program = Program of Symbols * TopLevel list

let emptySymbols () =
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
      ConstructorNames = Map.empty
      ConstructorIds = Map.empty
      FieldNames = Map.empty
      FieldIds = Map.empty }

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

let private intern
    makeId
    key
    displayName
    ids
    names
    nextOrdinal
    rebuild =
    match Map.tryFind key ids with
    | Some id -> (id, rebuild ids names nextOrdinal)
    | None ->
        let id = makeId nextOrdinal
        (id, rebuild (Map.add key id ids) (Map.add id displayName names) (nextOrdinal + 1))

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
        let id = AST.typeIdForName name
        (id,
         { symbols with
             TypeIds = Map.add name id symbols.TypeIds
             TypeNames = Map.add id name symbols.TypeNames })

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
        let id = AST.fieldId owner name index
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
    |> Option.orElse (Some (AST.typeIdValue id))
let typeNames symbols = symbols.TypeNames
let tryFindTypeId name symbols = Map.tryFind name symbols.TypeIds
let constructorInfo id symbols = Map.tryFind id symbols.ConstructorNames
let constructorTag id (_symbols: Symbols) = Some (AST.constructorRuntimeTag id)

let tryFindConstructorId typeName name symbols =
    Map.tryFind (typeName, name) symbols.ConstructorIds
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

/// Retain only the symbol metadata referenced by the supplied checked
/// declarations.  Prepared artifacts must not capture the complete symbol
/// namespace from which they were produced: doing so makes importing one
/// small function proportional to the size of the stdlib.
let private legacySymbolsForTopLevels (symbols: Symbols) (topLevels: TopLevel list) : Symbols =
    let addBinding id (bindings, functions, types, constructors, fields) =
        (Set.add id bindings, functions, types, constructors, fields)
    let addFunction id (bindings, functions, types, constructors, fields) =
        (bindings, Set.add id functions, types, constructors, fields)
    let addType id (bindings, functions, types, constructors, fields) =
        (bindings, functions, Set.add id types, constructors, fields)
    let addConstructor id (bindings, functions, types, constructors, fields) =
        (bindings, functions, types, Set.add id constructors, fields)
    let addField id (bindings, functions, types, constructors, fields) =
        (bindings, functions, types, constructors, Set.add id fields)
    let rec collectLetPattern state pattern =
        match pattern with
        | LPUnit | LPWildcard -> state
        | LPVariable id -> addBinding id state
        | LPTuple (first, second, rest) ->
            first :: second :: rest |> List.fold collectLetPattern state
    let rec collectPattern state pattern =
        match pattern with
        | PVariable id -> addBinding id state
        | PConstructor (id, fields) ->
            fields |> List.fold collectPattern (addConstructor id state)
        | PTuple patterns | PList patterns ->
            patterns |> List.fold collectPattern state
        | PListCons (heads, tail) ->
            collectPattern (heads |> List.fold collectPattern state) tail
        | POr alternatives ->
            alternatives |> AST.NonEmptyList.toList |> List.fold collectPattern state
        | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
        | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
        | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _ | PString _
        | PChar _ | PFloat _ -> state
    let addRecursion state (recursion: AST.TypedRecursiveMember) =
        addBinding recursion.Resolved.Parsed.Binding state
    let rec collectExpr state expr =
        let collectArgs state args =
            args |> AST.NonEmptyList.toList |> List.fold collectExpr state
        match expr with
        | Local id -> addBinding id state
        | Let (pattern, value, body) ->
            collectExpr (collectExpr (collectLetPattern state pattern) value) body
        | RecursiveLet (recursion, value, body) ->
            collectExpr (collectExpr (addRecursion state recursion) value) body
        | Lambda (parameters, _, body) ->
            let state =
                parameters
                |> AST.NonEmptyList.toList
                |> List.fold (fun state parameter -> collectLetPattern state parameter.Pattern) state
            collectExpr state body
        | Match (scrutinee, cases) ->
            let state = collectExpr state scrutinee
            cases
            |> List.fold (fun state case ->
                let state =
                    case.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.fold collectPattern state
                let state = case.Guard |> Option.map (collectExpr state) |> Option.defaultValue state
                collectExpr state case.Body) state
        | BoundaryRender (renderer, value) -> collectExpr (addFunction renderer state) value
        | BinOp (_, left, right) -> collectExpr (collectExpr state left) right
        | UnaryOp (_, value) -> collectExpr state value
        | If (condition, thenBranch, elseBranch) ->
            collectExpr (collectExpr (collectExpr state condition) thenBranch) elseBranch
        | Sequence (first, next) -> collectExpr (collectExpr state first) next
        | Call (id, args) | TypeApp (id, _, args) -> collectArgs (addFunction id state) args
        | TupleLiteral values | ListLiteral values -> values |> List.fold collectExpr state
        | TupleAccess (tuple, _) -> collectExpr state tuple
        | DictLiteral (_, _, entries) ->
            entries
            |> List.fold (fun state (key, value) -> collectExpr (collectExpr state key) value) state
        | RecordLiteral (reference, fieldValues) ->
            fieldValues
            |> List.fold (fun state (field, value) -> collectExpr (addField field state) value)
                (addType reference.TypeId state)
        | RecordUpdate (record, fieldValues) ->
            fieldValues
            |> List.fold (fun state (field, value) -> collectExpr (addField field state) value)
                (collectExpr state record)
        | RecordAccess (record, field) -> collectExpr (addField field state) record
        | Constructor (reference, values) ->
            values
            |> List.fold collectExpr
                (state |> addType reference.TypeId |> addConstructor reference.ConstructorId)
        | Apply (func, args) | IndirectApply (func, args) ->
            collectArgs (collectExpr state func) args
        | FuncRef id -> addFunction id state
        | Closure (id, captures) -> captures |> List.fold collectExpr (addFunction id state)
        | InterpolatedString parts ->
            parts
            |> List.fold (fun state part ->
                match part with
                | StringText _ -> state
                | StringExpr value -> collectExpr state value) state
        | UnitLiteral | Int64Literal _ | Int128Literal _ | Int8Literal _ | Int16Literal _
        | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
        | UInt128Literal _ | BigIntLiteral _ | BoolLiteral _ | StringLiteral _ | BlobLiteral _
        | CharLiteral _ | FloatLiteral _ | RuntimeError _ -> state
    let empty = (Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)
    let bindings, functions, types, constructors, fields =
        topLevels
        |> List.fold (fun state topLevel ->
            match topLevel with
            | FunctionDef functionDef ->
                let state = addFunction functionDef.Id state
                let state =
                    functionDef.Params
                    |> AST.NonEmptyList.toList
                    |> List.fold (fun state (id, _) -> addBinding id state) state
                let state = functionDef.Recursion |> Option.map (addRecursion state) |> Option.defaultValue state
                collectExpr state functionDef.Body
            | ValueDef valueDef -> collectExpr (addBinding valueDef.Id state) valueDef.Body
            | TypeDef (id, _) -> addType id state
            | Expression expr -> collectExpr state expr) empty
    let retainKeys keys map = map |> Map.filter (fun key _ -> Set.contains key keys)
    let functionNames =
        functions
        |> Seq.choose (fun id ->
            symbols.FunctionNames
            |> Map.tryFind id
            |> Option.orElseWith (fun () -> AST.tryFunctionCanonicalName id)
            |> Option.map (fun name -> id, name))
        |> Map.ofSeq
    let typeNames = retainKeys types symbols.TypeNames
    let constructorNames = retainKeys constructors symbols.ConstructorNames
    let fieldNames = retainKeys fields symbols.FieldNames
    { symbols with
        BindingNames = retainKeys bindings symbols.BindingNames
        ValueIds = symbols.ValueIds |> Map.filter (fun _ id -> Set.contains id bindings)
        FunctionNames = functionNames
        FunctionIds = functionNames |> Map.toSeq |> Seq.map (fun (id, name) -> name, id) |> Map.ofSeq
        TypeNames = typeNames
        TypeIds = typeNames |> Map.toSeq |> Seq.map (fun (id, name) -> name, id) |> Map.ofSeq
        ConstructorNames = constructorNames
        ConstructorIds =
            constructorNames
            |> Map.toSeq
            |> Seq.map (fun (id, key) -> key, id)
            |> Map.ofSeq
        FieldNames = fieldNames
        FieldIds =
            fieldNames
            |> Map.toSeq
            |> Seq.map (fun (id, key) -> key, id)
            |> Map.ofSeq }

/// A checked unit is self-describing: semantic IDs carry canonical identity
/// and lexical IDs carry body-local presentation metadata. Reusable artifacts
/// therefore retain only their fresh-binding cursor, not a projection of the
/// global declaration catalog and not a walk-derived symbol slice.
let catalogForCheckedUnit (symbols: Symbols) : Symbols =
    { emptySymbols () with NextBindingOrdinal = symbols.NextBindingOrdinal }

/// Import checked declarations from another independently allocated symbol
/// namespace. Every source binding receives a fresh target identity, while
/// all references and recursive metadata are rewritten consistently.
let private legacyImportTopLevels
    (sourceSymbols: Symbols)
    (targetSymbols: Symbols)
    (topLevels: TopLevel list)
    : Symbols * TopLevel list =
    let (valueRemap, symbols) =
        sourceSymbols.ValueIds
        |> Map.toList
        |> List.mapFold (fun symbols (name, sourceId) ->
            let (targetId, symbols) = internValue name symbols
            ((sourceId, targetId), symbols)) targetSymbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let sourceValueIds = sourceSymbols.ValueIds |> Map.values |> Set.ofSeq
    let (bindingRemap, symbols) =
        sourceSymbols.BindingNames
        |> Map.toList
        |> List.filter (fun (sourceId, _) -> not (Set.contains sourceId sourceValueIds))
        |> List.mapFold (fun symbols (sourceId, name) ->
            let (targetId, symbols) = allocateBinding name symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let remap = Map.fold (fun result sourceId targetId -> Map.add sourceId targetId result) bindingRemap valueRemap
    let (functionRemap, symbols) =
        sourceSymbols.FunctionNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, name) ->
            let (targetId, symbols) = internFunction name symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let (typeRemap, symbols) =
        sourceSymbols.TypeNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, name) ->
            let (targetId, symbols) = internType name symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let (fieldRemap, symbols) =
        sourceSymbols.FieldNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, (typeName, fieldName)) ->
            let index =
                fieldIndex sourceId sourceSymbols
                |> Option.defaultWith (fun () -> Crash.crash "Imported checked field layout is absent")
            let (targetId, symbols) = internField typeName fieldName index symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let (constructorRemap, symbols) =
        sourceSymbols.ConstructorNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, (typeName, constructorName)) ->
            let tag =
                constructorTag sourceId sourceSymbols
                |> Option.defaultWith (fun () -> Crash.crash "Imported checked constructor layout is absent")
            let (targetId, symbols) = internConstructor typeName constructorName tag symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let mapId id =
        match Map.tryFind id remap with
        | Some mapped -> mapped
        | None -> Crash.crash "Imported checked binding is absent from its source symbol table"
    let mapTypeId id =
        match Map.tryFind id typeRemap with
        | Some mapped -> mapped
        | None -> Crash.crash "Imported checked type is absent from its source symbol table"
    let mapFunctionId id =
        match Map.tryFind id functionRemap with
        | Some mapped -> mapped
        | None ->
            Crash.crash
                $"Imported checked function {AST.functionIdValue id} is absent from its source symbol table"
    let mapFieldId id =
        match Map.tryFind id fieldRemap with
        | Some mapped -> mapped
        | None -> Crash.crash "Imported checked field is absent from its source symbol table"
    let mapConstructorId id =
        match Map.tryFind id constructorRemap with
        | Some mapped -> mapped
        | None -> Crash.crash "Imported checked constructor is absent from its source symbol table"
    let mapRecursion (typed: AST.TypedRecursiveMember) =
        { typed with
            Resolved =
                { typed.Resolved with
                    Parsed =
                        { typed.Resolved.Parsed with
                            Binding = mapId typed.Resolved.Parsed.Binding } } }
    let rec mapLetPattern pattern =
        match pattern with
        | LPUnit -> LPUnit
        | LPWildcard -> LPWildcard
        | LPVariable id -> LPVariable (mapId id)
        | LPTuple (first, second, rest) ->
            LPTuple (mapLetPattern first, mapLetPattern second, List.map mapLetPattern rest)
    let rec mapPattern pattern =
        match pattern with
        | PVariable id -> PVariable (mapId id)
        | PTuple patterns -> PTuple (List.map mapPattern patterns)
        | PList patterns -> PList (List.map mapPattern patterns)
        | PListCons (heads, tail) -> PListCons (List.map mapPattern heads, mapPattern tail)
        | PConstructor (id, fields) -> PConstructor (mapConstructorId id, List.map mapPattern fields)
        | POr alternatives -> POr (AST.NonEmptyList.map mapPattern alternatives)
        | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
        | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
        | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _ | PString _
        | PChar _ | PFloat _ -> pattern
    let rec mapExpr expr =
        let mapArgs = AST.NonEmptyList.map mapExpr
        match expr with
        | Local id -> Local (mapId id)
        | Let (pattern, value, body) -> Let (mapLetPattern pattern, mapExpr value, mapExpr body)
        | RecursiveLet (recursion, value, body) ->
            RecursiveLet (mapRecursion recursion, mapExpr value, mapExpr body)
        | Lambda (parameters, annotation, body) ->
            Lambda (
                parameters
                |> AST.NonEmptyList.map (fun parameter ->
                    { parameter with Pattern = mapLetPattern parameter.Pattern }),
                annotation,
                mapExpr body
            )
        | Match (scrutinee, cases) ->
            Match (
                mapExpr scrutinee,
                cases
                |> List.map (fun case ->
                    { Patterns = AST.NonEmptyList.map mapPattern case.Patterns
                      Guard = Option.map mapExpr case.Guard
                      Body = mapExpr case.Body })
            )
        | BoundaryRender (renderer, value) -> BoundaryRender (mapFunctionId renderer, mapExpr value)
        | BinOp (op, left, right) -> BinOp (op, mapExpr left, mapExpr right)
        | UnaryOp (op, value) -> UnaryOp (op, mapExpr value)
        | If (condition, thenBranch, elseBranch) ->
            If (mapExpr condition, mapExpr thenBranch, mapExpr elseBranch)
        | Sequence (first, next) -> Sequence (mapExpr first, mapExpr next)
        | Call (id, args) -> Call (mapFunctionId id, mapArgs args)
        | TypeApp (id, types, args) -> TypeApp (mapFunctionId id, types, mapArgs args)
        | TupleLiteral values -> TupleLiteral (List.map mapExpr values)
        | TupleAccess (tuple, index) -> TupleAccess (mapExpr tuple, index)
        | DictLiteral (keyType, valueType, entries) ->
            DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> mapExpr key, mapExpr value))
        | RecordLiteral (reference, fields) ->
            RecordLiteral (
                { reference with TypeId = mapTypeId reference.TypeId },
                fields |> List.map (fun (field, value) -> mapFieldId field, mapExpr value)
            )
        | RecordUpdate (record, fields) ->
            RecordUpdate (mapExpr record, fields |> List.map (fun (field, value) -> mapFieldId field, mapExpr value))
        | RecordAccess (record, field) -> RecordAccess (mapExpr record, mapFieldId field)
        | Constructor (reference, fields) ->
            Constructor (
                { TypeId = mapTypeId reference.TypeId
                  ConstructorId = mapConstructorId reference.ConstructorId },
                List.map mapExpr fields
            )
        | ListLiteral values -> ListLiteral (List.map mapExpr values)
        | Apply (func, args) -> Apply (mapExpr func, mapArgs args)
        | IndirectApply (func, args) -> IndirectApply (mapExpr func, mapArgs args)
        | Closure (id, captures) -> Closure (mapFunctionId id, List.map mapExpr captures)
        | InterpolatedString parts ->
            InterpolatedString (
                parts
                |> List.map (function
                    | StringText _ as text -> text
                    | StringExpr value -> StringExpr (mapExpr value))
            )
        | FuncRef id -> FuncRef (mapFunctionId id)
        | UnitLiteral | Int64Literal _ | Int128Literal _ | Int8Literal _ | Int16Literal _
        | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
        | UInt128Literal _ | BigIntLiteral _ | BoolLiteral _ | StringLiteral _ | BlobLiteral _ | CharLiteral _
        | FloatLiteral _ | RuntimeError _ -> expr
    let mapFunction functionDef =
        { functionDef with
            Id = mapFunctionId functionDef.Id
            Params =
                functionDef.Params
                |> AST.NonEmptyList.map (fun (id, typ) -> mapId id, typ)
            Body = mapExpr functionDef.Body
            Recursion = Option.map mapRecursion functionDef.Recursion }
    let mapped =
        topLevels
        |> List.map (function
            | FunctionDef functionDef -> FunctionDef (mapFunction functionDef)
            | ValueDef valueDef ->
                ValueDef { valueDef with Id = mapId valueDef.Id; Body = mapExpr valueDef.Body }
            | Expression expr -> Expression (mapExpr expr)
            | TypeDef (id, typeDef) -> TypeDef (mapTypeId id, typeDef))
    (symbols, mapped)

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
    let catalog = {
        BindingNames = merge sourceCatalog.BindingNames targetCatalog.BindingNames
        ValueIds = merge sourceCatalog.ValueIds targetCatalog.ValueIds
        NextBindingOrdinal = min sourceCatalog.NextBindingOrdinal targetCatalog.NextBindingOrdinal
        FunctionNames = merge sourceCatalog.FunctionNames targetCatalog.FunctionNames
        FunctionIds = merge sourceCatalog.FunctionIds targetCatalog.FunctionIds
        TypeNames = merge sourceCatalog.TypeNames targetCatalog.TypeNames
        TypeIds = merge sourceCatalog.TypeIds targetCatalog.TypeIds
        ConstructorNames = merge sourceCatalog.ConstructorNames targetCatalog.ConstructorNames
        ConstructorIds = merge sourceCatalog.ConstructorIds targetCatalog.ConstructorIds
        FieldNames = merge sourceCatalog.FieldNames targetCatalog.FieldNames
        FieldIds = merge sourceCatalog.FieldIds targetCatalog.FieldIds
    }
    (catalog, topLevels)

let resolveUnboundValueLocals
    (symbols: Symbols)
    (values: Map<string, AST.BindingId>)
    (expr: Expr)
    : Expr =
    let rec letBindings pattern =
        match pattern with
        | LPVariable id -> [id]
        | LPTuple (first, second, rest) ->
            first :: second :: rest |> List.collect letBindings
        | LPUnit | LPWildcard -> []
    let rec matchBindings pattern =
        match pattern with
        | PVariable id -> [id]
        | PConstructor (_, fields) -> fields |> List.collect matchBindings
        | PTuple patterns | PList patterns -> List.collect matchBindings patterns
        | PListCons (heads, tail) -> List.collect matchBindings heads @ matchBindings tail
        | POr alternatives -> alternatives |> AST.NonEmptyList.head |> matchBindings
        | _ -> []
    let rec rewrite bound expression =
        let recurse = rewrite bound
        let mapArgs = AST.NonEmptyList.map recurse
        match expression with
        | Local id when not (Set.contains id bound) ->
            bindingName id symbols
            |> Option.bind (fun name -> Map.tryFind name values)
            |> Option.map Local
            |> Option.defaultValue expression
        | Let (pattern, value, body) ->
            Let (
                pattern,
                recurse value,
                rewrite (Set.union bound (letBindings pattern |> Set.ofList)) body
            )
        | RecursiveLet (recursion, value, body) ->
            let id = recursion.Resolved.Parsed.Binding
            let bodyBound = Set.add id bound
            let valueBound =
                match recursion.Resolved.Availability with
                | AST.OrdinaryBinding -> bound
                | _ -> bodyBound
            RecursiveLet (recursion, rewrite valueBound value, rewrite bodyBound body)
        | Lambda (parameters, annotation, body) ->
            let parameterIds =
                parameters
                |> AST.NonEmptyList.toList
                |> List.collect (fun parameter -> letBindings parameter.Pattern)
                |> Set.ofList
            Lambda (parameters, annotation, rewrite (Set.union bound parameterIds) body)
        | Match (scrutinee, cases) ->
            Match (
                recurse scrutinee,
                cases
                |> List.map (fun case ->
                    let caseIds =
                        case.Patterns
                        |> AST.NonEmptyList.head
                        |> matchBindings
                        |> Set.ofList
                    let caseBound = Set.union bound caseIds
                    { case with
                        Guard = Option.map (rewrite caseBound) case.Guard
                        Body = rewrite caseBound case.Body })
            )
        | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
        | BinOp (op, left, right) -> BinOp (op, recurse left, recurse right)
        | UnaryOp (op, value) -> UnaryOp (op, recurse value)
        | If (condition, thenBranch, elseBranch) -> If (recurse condition, recurse thenBranch, recurse elseBranch)
        | Sequence (first, next) -> Sequence (recurse first, recurse next)
        | Call (name, args) -> Call (name, mapArgs args)
        | TypeApp (name, types, args) -> TypeApp (name, types, mapArgs args)
        | TupleLiteral elements -> TupleLiteral (List.map recurse elements)
        | TupleAccess (tuple, index) -> TupleAccess (recurse tuple, index)
        | DictLiteral (keyType, valueType, entries) ->
            DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> recurse key, recurse value))
        | RecordLiteral (reference, fields) ->
            RecordLiteral (reference, fields |> List.map (fun (name, value) -> name, recurse value))
        | RecordUpdate (record, fields) ->
            RecordUpdate (recurse record, fields |> List.map (fun (name, value) -> name, recurse value))
        | RecordAccess (record, field) -> RecordAccess (recurse record, field)
        | Constructor (reference, fields) -> Constructor (reference, List.map recurse fields)
        | ListLiteral elements -> ListLiteral (List.map recurse elements)
        | Apply (func, args) -> Apply (recurse func, mapArgs args)
        | IndirectApply (func, args) -> IndirectApply (recurse func, mapArgs args)
        | Closure (name, captures) -> Closure (name, List.map recurse captures)
        | InterpolatedString parts ->
            InterpolatedString (
                parts
                |> List.map (function
                    | StringText _ as text -> text
                    | StringExpr value -> StringExpr (recurse value))
            )
        | _ -> expression
    rewrite Set.empty expr

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

let rec mapLetPatternBindings (f: AST.BindingId -> AST.BindingId) (pattern: LetPattern) : LetPattern =
    match pattern with
    | LPVariable name -> LPVariable (f name)
    | LPTuple (first, second, rest) ->
        LPTuple (
            mapLetPatternBindings f first,
            mapLetPatternBindings f second,
            rest |> List.map (mapLetPatternBindings f)
        )
    | LPUnit -> LPUnit
    | LPWildcard -> LPWildcard

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

let private convertRecordReference
    (reference: AST.RecordReference)
    (symbols: Symbols)
    : RecordReference * Symbols =
    let typeId, symbols = internType reference.ResolvedTypeName symbols
    ({ TypeId = typeId; TypeArgs = reference.TypeArgs }, symbols)

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
                let typeId =
                    tryFindTypeId typeName symbols
                    |> Option.defaultWith (fun () ->
                        Crash.crash $"Constructor owner '{typeName}' is absent from type symbols")
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

let rec private convertExpr location environment symbols expr : Result<Expr * Symbols, string> =
    let convert = convertExpr location environment
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
            convertExpr location bodyEnvironment afterPattern body
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
            convertExpr location valueEnvironment withSymbol value
            |> Result.bind (fun (value', afterValue) ->
                convertExpr location bodyEnvironment afterValue body
                |> Result.map (fun (body', following) -> (RecursiveLet (typed, value', body'), following)))
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
                (TypeApp (functionId, typeArgs, converted), state))
    | AST.TupleLiteral elements ->
        convertList elements symbols |> Result.map (fun (values, state) -> (TupleLiteral values, state))
    | AST.TupleAccess (tuple, index) ->
        convert symbols tuple |> Result.map (fun (value, state) -> (TupleAccess (value, index), state))
    | AST.DictLiteral (keyType, valueType, entries) ->
        entries
        |> List.fold (fun result (key, value) ->
            result
            |> Result.bind (fun (converted, state) ->
                convertPair key value state
                |> Result.map (fun (key', value', next) -> ((key', value') :: converted, next)))) (Ok ([], symbols))
        |> Result.map (fun (converted, state) -> (DictLiteral (keyType, valueType, List.rev converted), state))
    | AST.RecordLiteral (reference, fields) ->
        convertFields fields symbols
        |> Result.map (fun (converted, state) ->
            let reference, state = convertRecordReference reference state
            (RecordLiteral (reference, converted), state))
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
                                convertExpr location caseEnvironment afterBindings guard
                                |> Result.map (fun (guard', next) -> (Some guard', next))
                        guardResult
                        |> Result.bind (fun (guard', afterGuard) ->
                            convertExpr location caseEnvironment afterGuard case.Body
                            |> Result.map (fun (body', following) ->
                                ({ Patterns = patterns; Guard = guard'; Body = body' } :: convertedCases,
                                 following)))))) (Ok ([], afterScrutinee))
            |> Result.map (fun (convertedCases, state) ->
                (Match (scrutinee', List.rev convertedCases), state)))
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
                    Ok ({ Pattern = pattern'; Type = typ } :: converted,
                        bindings @ patternBindings,
                        next))) (Ok ([], [], symbols))
        |> Result.bind (fun (convertedParameters, bindings, afterParameters) ->
            let bodyEnvironment = extendEnvironment bindings environment
            convertExpr location bodyEnvironment afterParameters body
            |> Result.map (fun (body', following) ->
                (Lambda (AST.NonEmptyList.fromList (List.rev convertedParameters), returnAnnotation, body'),
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
    (outerEnvironment: Map<string, AST.BindingId>)
    symbols
    (funcDef: AST.FunctionDef)
    : Result<FunctionDef * Symbols, string> =
    let recursion =
        match funcDef.Recursion with
        | None -> Ok None
        | Some (AST.TypedRecursiveBinding typed) -> Ok (Some typed)
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
            ((id, typ), next)) symbols
        |> fun (parameters, afterParameters) ->
            let environment =
                List.zip (funcDef.Params |> AST.NonEmptyList.toList |> List.map fst) (parameters |> List.map fst)
                |> List.fold (fun environment (name, id) -> Map.add name id environment) outerEnvironment
            convertExpr $"function '{funcDef.Name}'" environment afterParameters funcDef.Body
            |> Result.map (fun (body, following) ->
                ({ Id = functionId
                   Name = funcDef.Name
                   TypeParams = funcDef.TypeParams
                   Params = AST.NonEmptyList.fromList parameters
                   ReturnType = funcDef.ReturnType
                   Body = body
                   Recursion = recursion' },
                 following)))

let ofTypedFunction
    (variantLookup: Map<string, string * string list * int * AST.SemanticType list>)
    symbols
    funcDef
    : Result<FunctionDef * Symbols, string> =
    let symbols =
        variantLookup
        |> Map.fold (fun symbols lookupName (typeName, _, tag, _) ->
            let variantName = lookupName.Split('.') |> Array.last
            internConstructor typeName variantName tag symbols |> snd) symbols
    convertFunctionWithEnvironment Map.empty symbols funcDef

let ofTypedProgram
    (variantLookup: Map<string, string * string list * int * AST.SemanticType list>)
    (externalValueNames: Set<string>)
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
            (Map.add name id environment, symbols)) (Map.empty, emptySymbols ())
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
    let initialSymbols =
        variantLookup
        |> Map.fold (fun symbols lookupName (typeName, _, tag, _) ->
            let variantName = lookupName.Split('.') |> Array.last
            internConstructor typeName variantName tag symbols |> snd) initialSymbols
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
            convertFunctionWithEnvironment valueEnvironment symbols funcDef
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
            convertExpr $"value '{name}'" valueEnvironment symbols body
            |> Result.map (fun (checkedBody, state) ->
                let id =
                    match Map.tryFind name valueEnvironment with
                    | Some id -> id
                    | None -> Crash.crash "Checked value identity allocation was lost"
                (ValueDef { Id = id; Name = name; Type = typ; Body = checkedBody }, state))
        | AST.ValueDef (AST.UncheckedValueDef (name, _)) ->
            conversionError $"value '{name}'" "value definition was not checked"
        | AST.Expression (_, expr) ->
            convertExpr "entry expression" valueEnvironment symbols expr
            |> Result.map (fun (converted, state) -> (Expression converted, state))
    topLevels
    |> List.fold (fun result topLevel ->
        result
        |> Result.bind (fun (converted, symbols) ->
            convertTopLevel symbols topLevel
            |> Result.map (fun (item, next) -> (item :: converted, next)))) (Ok ([], initialSymbols))
    |> Result.map (fun (converted, symbols) -> Program (symbols, List.rev converted))
