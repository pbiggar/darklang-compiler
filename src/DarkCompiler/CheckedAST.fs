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
    Type: AST.Type
}

type RecordReference = {
    TypeName: string
    TypeArgs: AST.Type list
}

type ConstructorReference = {
    TypeName: string
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
    | CharLiteral of string
    | FloatLiteral of float
    | InterpolatedString of StringPart list
    | BinOp of AST.BinOp * Expr * Expr
    | UnaryOp of AST.UnaryOp * Expr
    | Let of pattern:LetPattern * value:Expr * body:Expr
    | RecursiveLet of recursion:AST.TypedRecursiveMember * value:Expr * body:Expr
    | Local of AST.BindingId
    | NamedValue of string
    | If of cond:Expr * thenBranch:Expr * elseBranch:Expr
    | Sequence of first:Expr * next:Expr
    | Call of funcName:string * args:AST.NonEmptyList<Expr>
    | TypeApp of funcName:string * typeArgs:AST.Type list * args:AST.NonEmptyList<Expr>
    | TupleLiteral of Expr list
    | TupleAccess of tuple:Expr * index:int
    | DictLiteral of keyType:AST.Type * valueType:AST.Type * entries:(Expr * Expr) list
    | RecordLiteral of reference:RecordReference * fields:(AST.FieldId * Expr) list
    | RecordUpdate of record:Expr * updates:(AST.FieldId * Expr) list
    | RecordAccess of record:Expr * field:AST.FieldId
    | Constructor of reference:ConstructorReference * fields:Expr list
    | Match of scrutinee:Expr * cases:MatchCase list
    | ListLiteral of Expr list
    | Lambda of parameters:AST.NonEmptyList<LambdaParameter> * returnAnnotation:AST.Type option * body:Expr
    | Apply of func:Expr * args:AST.NonEmptyList<Expr>
    | IndirectApply of func:Expr * args:AST.NonEmptyList<Expr>
    | FuncRef of funcName:string
    | Closure of funcName:string * captures:Expr list
    | RuntimeError of message:string
    | BoundaryRender of renderer:string * value:Expr

and MatchCase = {
    Patterns: AST.NonEmptyList<Pattern>
    Guard: Expr option
    Body: Expr
}

type FunctionDef = {
    Name: string
    TypeParams: string list
    Params: AST.NonEmptyList<AST.BindingId * AST.Type>
    ReturnType: AST.Type
    Body: Expr
    Recursion: AST.TypedRecursiveMember option
}

type ValueDef = {
    Id: AST.BindingId
    Name: string
    Type: AST.Type
    Body: Expr
}

type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of AST.TypeId * AST.TypeDef
    | ValueDef of ValueDef
    | Expression of Expr

type Symbols = private {
    NamespaceToken: obj
    BindingNames: Map<AST.BindingId, string>
    NextBindingOrdinal: int
    FunctionNames: Map<AST.FunctionId, string>
    FunctionIds: Map<string, AST.FunctionId>
    NextFunctionOrdinal: int
    TypeNames: Map<AST.TypeId, string>
    TypeIds: Map<string, AST.TypeId>
    NextTypeOrdinal: int
    ConstructorNames: Map<AST.ConstructorId, string * string>
    ConstructorIds: Map<string * string, AST.ConstructorId>
    NextConstructorOrdinal: int
    FieldNames: Map<AST.FieldId, string * string>
    FieldIds: Map<string * string, AST.FieldId>
    NextFieldOrdinal: int
}

type Program = Program of Symbols * TopLevel list

let emptySymbols () =
    { NamespaceToken = System.Object()
      BindingNames = Map.empty
      NextBindingOrdinal = -1
      FunctionNames = Map.empty
      FunctionIds = Map.empty
      NextFunctionOrdinal = 0
      TypeNames = Map.empty
      TypeIds = Map.empty
      NextTypeOrdinal = 0
      ConstructorNames = Map.empty
      ConstructorIds = Map.empty
      NextConstructorOrdinal = 0
      FieldNames = Map.empty
      FieldIds = Map.empty
      NextFieldOrdinal = 0 }

let private registerBinding id name symbols =
    { symbols with BindingNames = Map.add id name symbols.BindingNames }

let allocateBinding name symbols =
    let id = AST.bindingId symbols.NextBindingOrdinal
    let symbols' =
        { symbols with
            BindingNames = Map.add id name symbols.BindingNames
            NextBindingOrdinal = symbols.NextBindingOrdinal - 1 }
    (id, symbols')

let bindingName id symbols : string option = Map.tryFind id symbols.BindingNames

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
    intern AST.functionId name name symbols.FunctionIds symbols.FunctionNames symbols.NextFunctionOrdinal
        (fun ids names next -> { symbols with FunctionIds = ids; FunctionNames = names; NextFunctionOrdinal = next })

let internType name symbols =
    intern AST.typeId name name symbols.TypeIds symbols.TypeNames symbols.NextTypeOrdinal
        (fun ids names next -> { symbols with TypeIds = ids; TypeNames = names; NextTypeOrdinal = next })

let internConstructor typeName name tag symbols =
    match Map.tryFind (typeName, name) symbols.ConstructorIds with
    | Some id -> (id, symbols)
    | None ->
        let id = AST.constructorId symbols.NextConstructorOrdinal tag
        let symbols =
            { symbols with
                ConstructorIds = Map.add (typeName, name) id symbols.ConstructorIds
                ConstructorNames = Map.add id (typeName, name) symbols.ConstructorNames
                NextConstructorOrdinal = symbols.NextConstructorOrdinal + 1 }
        (id, symbols)

let internField typeName name index symbols =
    match Map.tryFind (typeName, name) symbols.FieldIds with
    | Some id -> (id, symbols)
    | None ->
        let id = AST.fieldId symbols.NextFieldOrdinal index
        let symbols =
            { symbols with
                FieldIds = Map.add (typeName, name) id symbols.FieldIds
                FieldNames = Map.add id (typeName, name) symbols.FieldNames
                NextFieldOrdinal = symbols.NextFieldOrdinal + 1 }
        (id, symbols)

let functionName id symbols = Map.tryFind id symbols.FunctionNames
let typeName id symbols = Map.tryFind id symbols.TypeNames
let constructorInfo id symbols = Map.tryFind id symbols.ConstructorNames

let tryFindConstructorId typeName name symbols =
    Map.tryFind (typeName, name) symbols.ConstructorIds
let fieldInfo id symbols = Map.tryFind id symbols.FieldNames

let tryFindFieldId typeName name symbols =
    Map.tryFind (typeName, name) symbols.FieldIds

let sameSymbolNamespace first second =
    obj.ReferenceEquals(first.NamespaceToken, second.NamespaceToken)

let programSymbols (Program (symbols, _)) : Symbols = symbols

let programTopLevels (Program (_, topLevels)) : TopLevel list = topLevels

let withProgramTopLevels topLevels (Program (symbols, _)) : Program =
    Program (symbols, topLevels)

/// Import checked declarations from another independently allocated symbol
/// namespace. Every source binding receives a fresh target identity, while
/// all references and recursive metadata are rewritten consistently.
let importTopLevels
    (sourceSymbols: Symbols)
    (targetSymbols: Symbols)
    (topLevels: TopLevel list)
    : Symbols * TopLevel list =
    let (remap, symbols) =
        sourceSymbols.BindingNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, name) ->
            let (targetId, symbols) = allocateBinding name symbols
            ((sourceId, targetId), symbols)) targetSymbols
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
            let (targetId, symbols) = internField typeName fieldName (AST.fieldIndex sourceId) symbols
            ((sourceId, targetId), symbols)) symbols
        |> fun (entries, symbols) -> (Map.ofList entries, symbols)
    let (constructorRemap, symbols) =
        sourceSymbols.ConstructorNames
        |> Map.toList
        |> List.mapFold (fun symbols (sourceId, (typeName, constructorName)) ->
            let (targetId, symbols) =
                internConstructor typeName constructorName (AST.constructorTag sourceId) symbols
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
        | BoundaryRender (renderer, value) -> BoundaryRender (renderer, mapExpr value)
        | BinOp (op, left, right) -> BinOp (op, mapExpr left, mapExpr right)
        | UnaryOp (op, value) -> UnaryOp (op, mapExpr value)
        | If (condition, thenBranch, elseBranch) ->
            If (mapExpr condition, mapExpr thenBranch, mapExpr elseBranch)
        | Sequence (first, next) -> Sequence (mapExpr first, mapExpr next)
        | Call (name, args) -> Call (name, mapArgs args)
        | TypeApp (name, types, args) -> TypeApp (name, types, mapArgs args)
        | TupleLiteral values -> TupleLiteral (List.map mapExpr values)
        | TupleAccess (tuple, index) -> TupleAccess (mapExpr tuple, index)
        | DictLiteral (keyType, valueType, entries) ->
            DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> mapExpr key, mapExpr value))
        | RecordLiteral (reference, fields) ->
            RecordLiteral (reference, fields |> List.map (fun (field, value) -> mapFieldId field, mapExpr value))
        | RecordUpdate (record, fields) ->
            RecordUpdate (mapExpr record, fields |> List.map (fun (field, value) -> mapFieldId field, mapExpr value))
        | RecordAccess (record, field) -> RecordAccess (mapExpr record, mapFieldId field)
        | Constructor (reference, fields) ->
            Constructor (
                { reference with ConstructorId = mapConstructorId reference.ConstructorId },
                List.map mapExpr fields
            )
        | ListLiteral values -> ListLiteral (List.map mapExpr values)
        | Apply (func, args) -> Apply (mapExpr func, mapArgs args)
        | IndirectApply (func, args) -> IndirectApply (mapExpr func, mapArgs args)
        | Closure (name, captures) -> Closure (name, List.map mapExpr captures)
        | InterpolatedString parts ->
            InterpolatedString (
                parts
                |> List.map (function
                    | StringText _ as text -> text
                    | StringExpr value -> StringExpr (mapExpr value))
            )
        | UnitLiteral | Int64Literal _ | Int128Literal _ | Int8Literal _ | Int16Literal _
        | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
        | UInt128Literal _ | BigIntLiteral _ | BoolLiteral _ | StringLiteral _ | CharLiteral _
        | FloatLiteral _ | NamedValue _ | FuncRef _ | RuntimeError _ -> expr
    let mapFunction functionDef =
        { functionDef with
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

/// Resolve selected externally named values when importing them into a
/// lexical materialization scope. Local identities are already final and are
/// never reconsidered by spelling.
let rec resolveNamedValues (values: Map<string, AST.BindingId>) (expr: Expr) : Expr =
    let recurse = resolveNamedValues values
    let mapArgs = AST.NonEmptyList.map recurse
    match expr with
    | NamedValue name ->
        match Map.tryFind name values with
        | Some id -> Local id
        | None -> expr
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
    | BinOp (op, left, right) -> BinOp (op, recurse left, recurse right)
    | UnaryOp (op, value) -> UnaryOp (op, recurse value)
    | Let (pattern, value, body) -> Let (pattern, recurse value, recurse body)
    | RecursiveLet (recursion, value, body) -> RecursiveLet (recursion, recurse value, recurse body)
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
    | Match (scrutinee, cases) ->
        Match (
            recurse scrutinee,
            cases
            |> List.map (fun case ->
                { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })
        )
    | ListLiteral elements -> ListLiteral (List.map recurse elements)
    | Lambda (parameters, annotation, body) -> Lambda (parameters, annotation, recurse body)
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
    | UnitLiteral | Int64Literal _ | Int128Literal _ | Int8Literal _ | Int16Literal _
    | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
    | UInt128Literal _ | BigIntLiteral _ | BoolLiteral _ | StringLiteral _ | CharLiteral _
    | FloatLiteral _ | Local _ | FuncRef _ | RuntimeError _ -> expr

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

let programValues (Program (_, topLevels)) : Map<string, AST.Type * Expr> =
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

let constructorReferenceTypeName (reference: ConstructorReference) : string =
    reference.TypeName

let private conversionError location detail =
    Error $"Checked AST construction failed at {location}: {detail}"

let private map2 f first second =
    first
    |> Result.bind (fun firstValue ->
        second |> Result.map (fun secondValue -> f firstValue secondValue))

let private convertRecordReference (reference: AST.RecordReference) : RecordReference =
    { TypeName = reference.ResolvedTypeName; TypeArgs = reference.TypeArgs }

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
            | Some id -> Ok { TypeName = typeName; ConstructorId = id }
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
        match Map.tryFind name environment with
        | Some id -> Ok (Local id, symbols)
        | None -> Ok (NamedValue name, symbols)
    | AST.If (condition, thenBranch, elseBranch) ->
        convert symbols condition
        |> Result.bind (fun (condition', afterCondition) ->
            convertPair thenBranch elseBranch afterCondition
            |> Result.map (fun (thenBranch', elseBranch', following) ->
                (If (condition', thenBranch', elseBranch'), following)))
    | AST.Sequence (first, next) ->
        convertPair first next symbols
        |> Result.map (fun (first', next', state) -> (Sequence (first', next'), state))
    | AST.Call (name, args) ->
        convertNonEmpty args symbols
        |> Result.map (fun (converted, state) ->
            match Map.tryFind name environment with
            | Some id -> (Apply (Local id, converted), state)
            | None -> (Call (name, converted), state))
    | AST.TypeApp (name, typeArgs, args) ->
        convertNonEmpty args symbols
        |> Result.map (fun (converted, state) -> (TypeApp (name, typeArgs, converted), state))
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
        |> Result.map (fun (converted, state) -> (RecordLiteral (convertRecordReference reference, converted), state))
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
    | AST.Apply (func, args) ->
        convert symbols func
        |> Result.bind (fun (func', afterFunc) ->
            convertNonEmpty args afterFunc
            |> Result.map (fun (args', following) -> (Apply (func', args'), following)))
    | AST.IndirectApply (func, args) ->
        convert symbols func
        |> Result.bind (fun (func', afterFunc) ->
            convertNonEmpty args afterFunc
            |> Result.map (fun (args', following) -> (IndirectApply (func', args'), following)))
    | AST.FuncRef name -> Ok (FuncRef name, symbols)
    | AST.Closure (name, captures) ->
        convertList captures symbols |> Result.map (fun (values, state) -> (Closure (name, values), state))
    | AST.RuntimeError message -> Ok (RuntimeError message, symbols)
    | AST.BoundaryRender (renderer, value) ->
        convert symbols value |> Result.map (fun (converted, state) -> (BoundaryRender (renderer, converted), state))

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
                ({ Name = funcDef.Name
                   TypeParams = funcDef.TypeParams
                   Params = AST.NonEmptyList.fromList parameters
                   ReturnType = funcDef.ReturnType
                   Body = body
                   Recursion = recursion' },
                 following)))

let ofTypedFunction
    (variantLookup: Map<string, string * string list * int * AST.Type list>)
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
    (variantLookup: Map<string, string * string list * int * AST.Type list>)
    (AST.Program topLevels)
    : Result<Program, string> =
    let valueEnvironment, initialSymbols =
        topLevels
        |> List.choose (function
            | AST.ValueDef (AST.CheckedValueDef (name, _, _)) -> Some name
            | _ -> None)
        |> List.fold (fun (environment, symbols) name ->
            let (id, symbols) = allocateBinding name symbols
            (Map.add name id environment, symbols)) (Map.empty, emptySymbols ())
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
