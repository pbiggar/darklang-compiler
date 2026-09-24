// AST.fs - Abstract Syntax Tree
//
// Defines the abstract syntax tree data structures that represent the parsed
// program structure. The AST is the output of parsing and the internal input
// to name resolution and semantic checking. Successful checking constructs the
// phase-safe CheckedAST consumed by compiler preparation and ANF lowering.
// Keep this file as the structural source of truth for syntax-facing compiler
// nodes; language support and compatibility boundaries belong in
// docs/compatibility/overview.md.

module AST

/// A list guaranteed to have at least one element (makes invalid states unrepresentable)
type NonEmptyList<'a> = { Head: 'a; Tail: 'a list }

/// Compiler-wide warning settings passed from the driver into compiler passes.
/// Duplicate binders are language errors, not configurable warnings.
type WarningSettings =
    private
    | WarningSettings

let defaultWarningSettings : WarningSettings = WarningSettings

/// Type spellings accepted by the parser. Semantic-only states such as `Never`
/// cannot be represented before name resolution and checking.
type ParsedType =
    | PTInt8
    | PTInt16
    | PTInt32
    | PTInt64
    | PTInt128
    | PTInt
    | PTUInt8
    | PTUInt16
    | PTUInt32
    | PTUInt64
    | PTUInt128
    | PTBool
    | PTFloat64
    | PTString
    | PTBlob
    | PTChar
    | PTDateTime
    | PTUnit
    | PTFunction of ParsedType list * ParsedType
    | PTTuple of ParsedType list
    | PTRecord of string * ParsedType list
    | PTSum of string * ParsedType list
    | PTList of ParsedType
    | PTStream of ParsedType
    | PTVar of string
    /// Accepted only by privileged compiler sources and rejected at the public
    /// parser boundary. The parsed/semantic boundary turns it into the
    /// privileged internal-signature pointer type.
    | PTInternalRawPtr
    | PTDict of keyType:ParsedType * valueType:ParsedType

/// Types used by semantic checking and the currently shared lowering pipeline.
/// Parsed source reaches this representation only through name resolution.
type SemanticType =
    // Signed integers
    | TInt8
    | TInt16
    | TInt32
    | TInt64
    | TInt128
    | TInt       // Arbitrary-precision signed integer
    // Unsigned integers
    | TUInt8
    | TUInt16
    | TUInt32
    | TUInt64
    | TUInt128
    // Other primitives
    | TBool
    | TFloat64
    | TString
    | TBlob     // Byte array: [refcount:8][length:8][data:N][padding]
    | TChar      // Extended Grapheme Cluster (single visual character)
    | TDateTime  // Opaque UTC instant stored as signed 100ns Unix ticks
    | TUnit
    | TNever                          // Semantic bottom: expressions which do not return
    | TFunction of SemanticType list * SemanticType  // parameter types * return type
    | TTuple of SemanticType list             // tuple type: (Int, Bool, String)
    | TRecord of string * SemanticType list   // record type by name with type args: Point<T>, Pair<A, B>, etc.
    | TSum of string * SemanticType list      // sum type by name with type args: Result<Int64, String>
    | TList of SemanticType                    // List<T> - polymorphic list type
    | TStream of SemanticType                  // Stream<T> - opaque, lazy, single-consumer handle
    | TVar of string                  // type variable: T, A, B, etc. (for generics)
    | TInferenceVar of displayName:string * identity:string
    | TInternalRawPtr                         // Raw pointer to unmanaged memory (internal, for HAMT)
    // Native HAMT machinery retains both components. Public source syntax is
    // String-keyed and renders only the value component as Dict<Value>.
    | TDict of keyType:SemanticType * valueType:SemanticType

/// Nominal identity carried by record construction from parsing onward.
/// SourceTypeName preserves an alias spelling for diagnostics, while
/// ResolvedTypeName is filled with the canonical declaration identity during
/// name/type resolution. TypeArgs always follows declaration parameter order,
/// including parameters that do not occur in any field.
type RecordReferenceNode<'t> = {
    SourceTypeName: string
    ResolvedTypeName: string
    TypeArgs: 't list
}

type RecordReference = RecordReferenceNode<SemanticType>
type ParsedRecordReference = RecordReferenceNode<ParsedType>

let unresolvedRecordReference (sourceTypeName: string) (typeArgs: 't list) : RecordReferenceNode<'t> =
    { SourceTypeName = sourceTypeName; ResolvedTypeName = sourceTypeName; TypeArgs = typeArgs }

/// A field spelling before or after the checker proves its declaring record.
/// The resolved owner is semantic evidence required to assign a declaration-
/// scoped FieldId at the checked-program boundary.
type RecordFieldReference = {
    SourceFieldName: string
    ResolvedTypeName: string option
    ResolvedFieldIndex: int option
}

let unresolvedRecordFieldReference fieldName : RecordFieldReference =
    { SourceFieldName = fieldName; ResolvedTypeName = None; ResolvedFieldIndex = None }

let resolvedRecordFieldReference typeName fieldName fieldIndex : RecordFieldReference =
    { SourceFieldName = fieldName
      ResolvedTypeName = Some typeName
      ResolvedFieldIndex = Some fieldIndex }

/// A source constructor reference before or after nominal resolution.
/// `None` is the genuinely unqualified form; no empty-name sentinel is used.
type ConstructorReference =
    | UnresolvedConstructor of declaringType:string option
    | ResolvedConstructor of declaringModule:string list * declaringType:string

let constructorReferenceTypeName (reference: ConstructorReference) : string option =
    match reference with
    | UnresolvedConstructor declaringType -> declaringType
    | ResolvedConstructor (declaringModule, declaringType) ->
        Some (String.concat "." (declaringModule @ [declaringType]))

let resolvedConstructorReference (canonicalTypeName: string) : ConstructorReference =
    match canonicalTypeName.Split('.') |> Array.toList |> List.rev with
    | declaringType :: reversedModule ->
        ResolvedConstructor (List.rev reversedModule, declaringType)
    | [] ->
        Crash.crash "Cannot resolve a constructor against an empty declaring type name"

/// Binary operators
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Mod  // %
    | Pow  // ^
    // Bitwise operations
    | Shl     // << (left shift)
    | Shr     // >> (right shift)
    | BitAnd  // & (bitwise and)
    | BitOr   // ||| (bitwise or)
    | BitXor  // ^ (bitwise xor)
    // String operations
    | StringConcat  // ++
    // Comparisons (return bool)
    | Eq   // ==
    | Neq  // !=
    | Lt   // <
    | Gt   // >
    | Lte  // <=
    | Gte  // >=
    // Boolean operations
    | And  // &&
    | Or   // ||

/// Unary operators
type UnaryOp =
    | Neg     // Unary negation: -expr
    | Not     // Boolean not: !expr
    | BitNot  // Bitwise not: ~~~expr

/// NonEmptyList helper functions
module NonEmptyList =
    let singleton x = { Head = x; Tail = [] }
    let cons x nel = { Head = x; Tail = nel.Head :: nel.Tail }
    let toList nel = nel.Head :: nel.Tail
    let map f nel = { Head = f nel.Head; Tail = List.map f nel.Tail }
    let length nel = 1 + List.length nel.Tail
    let appendList nel items = { Head = nel.Head; Tail = nel.Tail @ items }
    let snoc nel item = { Head = nel.Head; Tail = nel.Tail @ [item] }
    let head nel = nel.Head
    let tryFromList = function
        | [] -> None
        | h :: t -> Some { Head = h; Tail = t }
    let fromList = function
        | [] -> Crash.crash "NonEmptyList.fromList: empty list"
        | h :: t -> { Head = h; Tail = t }

/// Canonical native identity for an enum case whose display name is shared by
/// multiple nominal declarations. The native backends encode case tags as
/// immediates, so declarations validate collisions in this bounded space.
let constructorRuntimeIdentity (declaringType: string) (caseName: string) : int =
    match declaringType, caseName with
    // Runtime I/O and string intrinsics construct these two foundational
    // stdlib types directly. Their ABI tags predate user-defined ADTs.
    | "Darklang.Stdlib.Option.Option", "Some"
    | "Darklang.Stdlib.Result.Result", "Ok" -> 0
    | "Darklang.Stdlib.Option.Option", "None"
    | "Darklang.Stdlib.Result.Result", "Error" -> 1
    | _ ->
        $"{declaringType}.{caseName}"
        |> Seq.fold (fun hash character -> (hash ^^^ uint32 character) * 16777619u) 2166136261u
        |> fun hash -> 2 + int (hash % 4094u)

/// Pattern matching patterns
type Pattern =
    | PUnit                                                // () - matches unit value
    | PWildcard                                            // _
    | PVar of string                                       // x (binds value to variable)
    | PConstructor of variantName:string * fields:Pattern list  // Red, Some(x), Pair(a, b)
    | PResolvedConstructor of declaringType:string * variantName:string * tag:int * fields:Pattern list
    | PInt64 of int64                                      // 42 (Int64 literal)
    | PBigInt of System.Numerics.BigInteger                // 42 (Int literal)
    | PInt128Literal of System.Int128                      // 42Q
    | PInt8Literal of sbyte                                // 1y
    | PInt16Literal of int16                               // 1s
    | PInt32Literal of int32                               // 1l
    | PUInt8Literal of byte                                // 1uy
    | PUInt16Literal of uint16                             // 1us
    | PUInt32Literal of uint32                             // 1ul
    | PUInt64Literal of uint64                             // 1UL
    | PUInt128Literal of System.UInt128                    // 42Z
    | PBool of bool                                        // true, false
    | PString of string                                    // "hello"
    | PChar of string                                      // 'x'
    | PFloat of float                                      // 3.14
    | PTuple of Pattern list                               // (a, b, c)
    | PList of Pattern list                                // [a, b, c] - exact length match
    | PListCons of head:Pattern list * tail:Pattern        // a :: b :: t - head elements + rest
    | POr of Pattern NonEmptyList                          // p1 | p2 - left-to-right alternatives

/// The deliberately restricted pattern language shared by non-recursive lets
/// and lambda parameters. Match-only patterns cannot be represented here.
type LetPattern =
    | LPUnit
    | LPWildcard
    | LPVariable of string
    | LPTuple of first:LetPattern * second:LetPattern * rest:LetPattern list

/// A lambda binder is parsed without an annotation. Type checking fills in its
/// inferred type without changing the source-level binding pattern.
type LambdaParameterNode<'t> = {
    Pattern: LetPattern
    SourceAnnotation: 't option
    InferredType: 't option
}

type LambdaParameter = LambdaParameterNode<SemanticType>
type ParsedLambdaParameter = LambdaParameterNode<ParsedType>

let lambdaParameter (pattern: LetPattern) : LambdaParameterNode<'t> =
    { Pattern = pattern; SourceAnnotation = None; InferredType = None }

let typedLambdaVariable (name: string) (typ: 't) : LambdaParameterNode<'t> =
    { Pattern = LPVariable name; SourceAnnotation = Some typ; InferredType = Some typ }

let inferredLambdaVariable (name: string) (typ: 't) : LambdaParameterNode<'t> =
    { Pattern = LPVariable name; SourceAnnotation = None; InferredType = Some typ }

let rec letPatternBindings (pattern: LetPattern) : string list =
    match pattern with
    | LPVariable name -> [name]
    | LPTuple (first, second, rest) ->
        first :: second :: rest |> List.collect letPatternBindings
    | LPUnit | LPWildcard -> []

let rec mapLetPatternBindings (f: string -> string) (pattern: LetPattern) : LetPattern =
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

type BinderStructure =
    | LetBinderPatterns of LetPattern list
    | MatchBinderPattern of Pattern

/// Stable semantic identities assigned at the parsed-program boundary. The
/// representation is private so source spellings cannot be used as identities.
[<Struct; StructuralEquality; StructuralComparison>]
type BindingId =
    private
    | LocalBindingId of ordinal:int * sourceName:string option
    | TopLevelValueId of canonicalName:string

[<Struct; StructuralEquality; StructuralComparison>]
type FunctionId = private FunctionId of canonicalName:string

[<Struct; StructuralEquality; StructuralComparison>]
type TypeId = private TypeId of canonicalName:string

[<Struct; StructuralEquality; StructuralComparison>]
type ConstructorId =
    private ConstructorId of owner:TypeId * canonicalName:string * runtimeTag:int

[<Struct; StructuralEquality; StructuralComparison>]
type FieldId =
    private FieldId of owner:TypeId * canonicalName:string * runtimeIndex:int

[<Struct; StructuralEquality; StructuralComparison>]
type ScopeBoundaryId = private ScopeBoundaryId of int

[<Struct; StructuralEquality; StructuralComparison>]
type RecursiveGroupId = private RecursiveGroupId of int

[<Struct; StructuralEquality; StructuralComparison>]
type RecursiveMemberId = private RecursiveMemberId of int

let bindingId ordinal = LocalBindingId (ordinal, None)
let namedBindingId ordinal sourceName = LocalBindingId (ordinal, Some sourceName)
let topLevelValueId canonicalName = TopLevelValueId canonicalName
let bindingDisplayName = function
    | LocalBindingId (_, sourceName) -> sourceName
    | TopLevelValueId canonicalName -> Some canonicalName
let functionIdForName canonicalName = FunctionId canonicalName
let functionIdValue (FunctionId canonicalName) = canonicalName
let tryFunctionCanonicalName (FunctionId canonicalName) = Some canonicalName
let allocateFunctionIds (_existing: seq<FunctionId>) (names: seq<string>) : Map<string, FunctionId> =
    names
    |> Seq.distinct
    |> Seq.sort
    |> Seq.map (fun name -> name, functionIdForName name)
    |> Map.ofSeq
let typeIdForName canonicalName = TypeId canonicalName
let typeIdValue (TypeId canonicalName) = canonicalName
let constructorId owner canonicalName runtimeTag =
    ConstructorId (owner, canonicalName, runtimeTag)
let constructorIdOwner (ConstructorId (owner, _, _)) = owner
let constructorIdValue (ConstructorId (_, canonicalName, _)) = canonicalName
let constructorRuntimeTag (ConstructorId (_, _, runtimeTag)) = runtimeTag
let fieldId owner canonicalName runtimeIndex =
    FieldId (owner, canonicalName, runtimeIndex)
let fieldIdOwner (FieldId (owner, _, _)) = owner
let fieldIdValue (FieldId (_, canonicalName, _)) = canonicalName
let fieldRuntimeIndex (FieldId (_, _, runtimeIndex)) = runtimeIndex
let scopeBoundaryId ordinal = ScopeBoundaryId ordinal
// Group IDs share one compact namespace: declaration groups are even and
// singleton local-recursion groups are odd.
let topLevelRecursiveGroupId ordinal = RecursiveGroupId (ordinal * 2)
let recursiveMemberId ordinal = RecursiveMemberId ordinal
let singletonRecursiveGroupId (RecursiveMemberId ordinal) =
    RecursiveGroupId (ordinal * 2 + 1)

type RecursiveMemberKind =
    | TopLevelFunctionMember
    | NamedLocalFunctionMember
    | DirectLambdaValueMember

type RecursiveAvailability =
    | OrdinaryBinding
    | SelfRecursiveMember
    | MutualRecursiveMember
    | CompletedGroupMember
    | ImportedGroupMember

type RecursiveDependencyKind =
    | DelayedCallableDependency
    | EagerValueDependency
    | TypeAliasDependency

/// Parser-only evidence that a declaration is eligible for recursive
/// resolution. `NameSyntax.normalizeSource` replaces this with a parsed member.
type RecursiveCandidate = {
    SourceName: string
    Kind: RecursiveMemberKind
}

type ParsedRecursiveMember = {
    Binding: BindingId
    Boundary: ScopeBoundaryId
    Member: RecursiveMemberId
    SourceName: string
    Kind: RecursiveMemberKind
}

type ResolvedRecursiveMember = {
    Parsed: ParsedRecursiveMember
    Group: RecursiveGroupId
    GroupIndex: int
    Availability: RecursiveAvailability
}

type TypedRecursiveMember = {
    Resolved: ResolvedRecursiveMember
    MonomorphicType: SemanticType
}

type LoweredRecursiveMember = {
    Typed: TypedRecursiveMember
    EnvironmentIndex: int
}

/// Every materialized group is nonempty by construction.
type ParsedRecursiveGroup = {
    Boundary: ScopeBoundaryId
    Members: NonEmptyList<ParsedRecursiveMember>
}

type ResolvedRecursiveGroup = {
    Group: RecursiveGroupId
    Members: NonEmptyList<ResolvedRecursiveMember>
}

type TypedRecursiveGroup = {
    Group: RecursiveGroupId
    Members: NonEmptyList<TypedRecursiveMember>
}

type LoweredRecursiveGroup = {
    Group: RecursiveGroupId
    Members: NonEmptyList<LoweredRecursiveMember>
}

type RecursiveBindingInfo =
    | RecursiveBindingCandidate of RecursiveCandidate
    | ParsedRecursiveBinding of ParsedRecursiveMember
    | ResolvedRecursiveBinding of ResolvedRecursiveMember
    | TypedRecursiveBinding of TypedRecursiveMember

let recursiveBindingName info =
    match info with
    | RecursiveBindingCandidate candidate -> candidate.SourceName
    | ParsedRecursiveBinding parsed -> parsed.SourceName
    | ResolvedRecursiveBinding resolved -> resolved.Parsed.SourceName
    | TypedRecursiveBinding typed -> typed.Resolved.Parsed.SourceName

let recursiveBindingKind info =
    match info with
    | RecursiveBindingCandidate candidate -> candidate.Kind
    | ParsedRecursiveBinding parsed -> parsed.Kind
    | ResolvedRecursiveBinding resolved -> resolved.Parsed.Kind
    | TypedRecursiveBinding typed -> typed.Resolved.Parsed.Kind

let recursiveBindingId info =
    match info with
    | ParsedRecursiveBinding parsed -> Some parsed.Binding
    | ResolvedRecursiveBinding resolved -> Some resolved.Parsed.Binding
    | TypedRecursiveBinding typed -> Some typed.Resolved.Parsed.Binding
    | RecursiveBindingCandidate _ -> None

let recursiveBindingAvailability info =
    match info with
    | ResolvedRecursiveBinding resolved -> Some resolved.Availability
    | TypedRecursiveBinding typed -> Some typed.Resolved.Availability
    | RecursiveBindingCandidate _ | ParsedRecursiveBinding _ -> None

/// Validate one complete binder structure before any of its names enter scope.
/// The returned list preserves source order and never contains ignored names.
let validateBinders (structure: BinderStructure) : Result<string list, string> =
    let rec matchPatternBindings pattern =
        match pattern with
        | PVar name -> [name]
        | PConstructor (_, fields) ->
            fields |> List.collect matchPatternBindings
        | PResolvedConstructor (_, _, _, fields) ->
            fields |> List.collect matchPatternBindings
        | PTuple patterns | PList patterns -> patterns |> List.collect matchPatternBindings
        | PListCons (heads, tail) ->
            (heads |> List.collect matchPatternBindings) @ matchPatternBindings tail
        | POr alternatives ->
            alternatives |> NonEmptyList.head |> matchPatternBindings
        | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
        | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
        | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _
        | PString _ | PChar _ | PFloat _ -> []

    let names =
        match structure with
        | LetBinderPatterns patterns -> patterns |> List.collect letPatternBindings
        | MatchBinderPattern pattern -> matchPatternBindings pattern

    let usableNames =
        names |> List.filter (fun name -> name <> "" && not (name.StartsWith "_"))

    let duplicate =
        usableNames
        |> List.fold (fun (seen, found) name ->
            match found with
            | Some _ -> (seen, found)
            | None when Set.contains name seen -> (seen, Some name)
            | None -> (Set.add name seen, None)) (Set.empty, None)
        |> snd

    match duplicate with
    | Some name -> Error $"Duplicate binding '{name}' in the same pattern"
    | None -> Ok usableNames

/// Part of an interpolated string: either a literal or an expression
type StringPartNode<'t> =
    | StringText of string    // Literal text: "Hello "
    | StringExpr of ExprNode<'t>      // Interpolated expression: {name}

/// Expression nodes
and ExprNode<'t> =
    | UnitLiteral                           // Unit value: ()
    | Int64Literal of int64                 // 64-bit signed (default): 42, 42L
    | Int128Literal of System.Int128        // 42Q
    | Int8Literal of sbyte                  // 8-bit signed: 42y
    | Int16Literal of int16                 // 16-bit signed: 42s
    | Int32Literal of int32                 // 32-bit signed: 42l
    | UInt8Literal of byte                  // 8-bit unsigned: 42uy
    | UInt16Literal of uint16               // 16-bit unsigned: 42us
    | UInt32Literal of uint32               // 32-bit unsigned: 42ul
    | UInt64Literal of uint64               // 64-bit unsigned: 42UL
    | UInt128Literal of System.UInt128      // 42Z
    | BigIntLiteral of System.Numerics.BigInteger // Arbitrary-precision unsuffixed Int
    | BoolLiteral of bool
    | StringLiteral of string
    | CharLiteral of string   // Single Extended Grapheme Cluster stored as UTF-8 string
    | FloatLiteral of float
    | InterpolatedString of StringPartNode<'t> list // $"Hello {name}!"
    | BinOp of BinOp * ExprNode<'t> * ExprNode<'t>
    | UnaryOp of UnaryOp * ExprNode<'t>
    | Let of pattern:LetPattern * value:ExprNode<'t> * body:ExprNode<'t>  // Atomic non-recursive binding
    | RecursiveLet of recursion:RecursiveBindingInfo * value:ExprNode<'t> * body:ExprNode<'t>
    | Var of string  // Variable reference
    | If of cond:ExprNode<'t> * thenBranch:ExprNode<'t> * elseBranch:ExprNode<'t>  // If expression: if cond then thenBranch else elseBranch
    | Sequence of first:ExprNode<'t> * next:ExprNode<'t>  // Statement sequence: first must produce Unit; next supplies the value
    /// A source-level call.  Resolution classifies the callee as a direct
    /// function, intrinsic, or dynamic value at the checked-AST boundary.
    | Apply of callee:ExprNode<'t> * typeArgs:'t list * args:NonEmptyList<ExprNode<'t>>
    | TupleLiteral of ExprNode<'t> list              // Tuple literal: (1, 2, 3)
    | TupleAccess of tuple:ExprNode<'t> * index:int  // Tuple access: t.0, t.1, etc.
    | DictLiteral of keyType:'t * valueType:'t * entries:(ExprNode<'t> * ExprNode<'t>) list
    | RecordLiteral of reference:RecordReferenceNode<'t> * fields:(RecordFieldReference * ExprNode<'t>) list
    | RecordUpdate of record:ExprNode<'t> * updates:(RecordFieldReference * ExprNode<'t>) list // { record with x = 1, y = 2 }
    | RecordAccess of record:ExprNode<'t> * field:RecordFieldReference        // p.x, p.y
    | Constructor of reference:ConstructorReference * variantName:string * fields:ExprNode<'t> list
    | Match of scrutinee:ExprNode<'t> * cases:MatchCaseNode<'t> list  // match e with | p1 when g -> e1 | p2 -> e2
    | ListLiteral of ExprNode<'t> list                               // [1, 2, 3]
    | Lambda of parameters:NonEmptyList<LambdaParameterNode<'t>> * returnAnnotation:'t option * body:ExprNode<'t>
    | IndirectApply of func:ExprNode<'t> * args:NonEmptyList<ExprNode<'t>>            // Compiler-generated call through a raw function pointer
    | Closure of funcName:string * captures:ExprNode<'t> list        // Closure: function + captured values
    | RuntimeError of message:string                         // Compiler-generated interpreter runtime error
    | BoundaryRender of renderer:string * value:ExprNode<'t>        // Compiler-generated eval-result rendering

/// Match case with optional guard clause and pattern grouping
/// Syntax: | pat1 | pat2 when guard -> body
and MatchCaseNode<'t> = {
    Patterns: NonEmptyList<Pattern>  // One or more patterns (pattern grouping via |)
    Guard: ExprNode<'t> option               // Optional guard clause (when condition)
    Body: ExprNode<'t>                       // Body expression
}

type Expr = ExprNode<SemanticType>
type ParsedExpr = ExprNode<ParsedType>
type StringPart = StringPartNode<SemanticType>
type ParsedStringPart = StringPartNode<ParsedType>
type MatchCase = MatchCaseNode<SemanticType>
type ParsedMatchCase = MatchCaseNode<ParsedType>

let applyNamed (name: string) (args: NonEmptyList<ExprNode<'t>>) : ExprNode<'t> =
    Apply (Var name, [], args)

let applyNamedWithTypes
    (name: string)
    (typeArgs: 't list)
    (args: NonEmptyList<ExprNode<'t>>)
    : ExprNode<'t> =
    Apply (Var name, typeArgs, args)

/// Function definition
type FunctionDefNode<'t> = {
    Name: string
    TypeParams: string list           // Type parameters for generics: ["T", "U", etc.], empty for non-generic
    Params: NonEmptyList<(string * 't)>  // Parameter names with REQUIRED type annotations
    ReturnType: 't                  // REQUIRED return type annotation
    Body: ExprNode<'t>
    Recursion: RecursiveBindingInfo option
}

/// Variant in a sum type with zero or more ordered constructor fields.
type VariantNode<'t> = {
    Name: string
    Fields: 't list
}

/// Type definition (record types, sum types, etc.)
type TypeDefNode<'t> =
    | RecordDef of name:string * typeParams:string list * fields:(string * 't) list  // type Point<T> = { x: T, y: T }
    | SumTypeDef of name:string * typeParams:string list * variants:VariantNode<'t> list       // type Result<T, E> = Ok of T | Error of E
    | TypeAlias of name:string * typeParams:string list * targetType:'t              // type Id = String

/// A source value before and after its body has been type checked.
type ValueDefNode<'t> =
    | UncheckedValueDef of name:string * body:ExprNode<'t>
    | CheckedValueDef of name:string * typ:'t * body:ExprNode<'t>

type FunctionDef = FunctionDefNode<SemanticType>
type ParsedFunctionDef = FunctionDefNode<ParsedType>
type Variant = VariantNode<SemanticType>
type ParsedVariant = VariantNode<ParsedType>
type TypeDef = TypeDefNode<SemanticType>
type ParsedTypeDef = TypeDefNode<ParsedType>
type ValueDef = ValueDefNode<SemanticType>
type ParsedValueDef = ValueDefNode<ParsedType>

let valueDefName (valueDef: ValueDefNode<'t>) : string =
    match valueDef with
    | UncheckedValueDef (name, _)
    | CheckedValueDef (name, _, _) -> name

let valueDefBody (valueDef: ValueDefNode<'t>) : ExprNode<'t> =
    match valueDef with
    | UncheckedValueDef (_, body)
    | CheckedValueDef (_, _, body) -> body

/// Case names that require a nominal native tag because they occur in more
/// than one declaring type in the same compilation unit.
let collidingConstructorCaseNames (typeDefs: TypeDefNode<'t> list) : Set<string> =
    typeDefs
    |> List.collect (function
        | SumTypeDef (typeName, _, variants) ->
            variants |> List.map (fun variant -> (variant.Name, typeName))
        | _ -> [])
    |> List.groupBy fst
    |> List.choose (fun (caseName, entries) ->
        let ownerCount = entries |> List.map snd |> List.distinct |> List.length
        if ownerCount > 1 then Some caseName else None)
    |> Set.ofList

/// Top-level program elements
type TopLevelNode<'t> =
    | FunctionDef of FunctionDefNode<'t>
    | TypeDef of TypeDefNode<'t>
    | ValueDef of ValueDefNode<'t>
    | Expression of modulePath:string list * ExprNode<'t>

/// Program is a list of top-level definitions (functions and/or expressions)
type ProgramNode<'t> = Program of TopLevelNode<'t> list

type TopLevel = TopLevelNode<SemanticType>
type ParsedTopLevel = TopLevelNode<ParsedType>
type Program = ProgramNode<SemanticType>
type ParsedProgram = ProgramNode<ParsedType>

let rec semanticTypeOfParsed (typ: ParsedType) : SemanticType =
    let recurse = semanticTypeOfParsed
    match typ with
    | PTInt8 -> TInt8
    | PTInt16 -> TInt16
    | PTInt32 -> TInt32
    | PTInt64 -> TInt64
    | PTInt128 -> TInt128
    | PTInt -> TInt
    | PTUInt8 -> TUInt8
    | PTUInt16 -> TUInt16
    | PTUInt32 -> TUInt32
    | PTUInt64 -> TUInt64
    | PTUInt128 -> TUInt128
    | PTBool -> TBool
    | PTFloat64 -> TFloat64
    | PTString -> TString
    | PTBlob -> TBlob
    | PTChar -> TChar
    | PTDateTime -> TDateTime
    | PTUnit -> TUnit
    | PTFunction (parameters, returnType) ->
        TFunction (List.map recurse parameters, recurse returnType)
    | PTTuple elements -> TTuple (List.map recurse elements)
    | PTRecord (name, typeArgs) -> TRecord (name, List.map recurse typeArgs)
    | PTSum (name, typeArgs) -> TSum (name, List.map recurse typeArgs)
    | PTList element -> TList (recurse element)
    | PTStream element -> TStream (recurse element)
    | PTVar name -> TVar name
    | PTInternalRawPtr -> TInternalRawPtr
    | PTDict (keyType, valueType) -> TDict (recurse keyType, recurse valueType)

let semanticProgramOfParsed (Program topLevels: ParsedProgram) : Program =
    let rec mapExpr (expr: ParsedExpr) : Expr =
        let mapArgs = NonEmptyList.map mapExpr
        match expr with
        | InterpolatedString parts ->
            parts
            |> List.map (function StringText text -> StringText text | StringExpr value -> StringExpr (mapExpr value))
            |> InterpolatedString
        | BinOp (op, left, right) -> BinOp (op, mapExpr left, mapExpr right)
        | UnaryOp (op, value) -> UnaryOp (op, mapExpr value)
        | Let (pattern, value, body) -> Let (pattern, mapExpr value, mapExpr body)
        | RecursiveLet (recursion, value, body) -> RecursiveLet (recursion, mapExpr value, mapExpr body)
        | If (condition, thenBranch, elseBranch) -> If (mapExpr condition, mapExpr thenBranch, mapExpr elseBranch)
        | Sequence (first, next) -> Sequence (mapExpr first, mapExpr next)
        | Apply (callee, typeArgs, args) ->
            Apply (mapExpr callee, List.map semanticTypeOfParsed typeArgs, mapArgs args)
        | TupleLiteral values -> TupleLiteral (List.map mapExpr values)
        | TupleAccess (tuple, index) -> TupleAccess (mapExpr tuple, index)
        | DictLiteral (keyType, valueType, entries) ->
            DictLiteral (
                semanticTypeOfParsed keyType,
                semanticTypeOfParsed valueType,
                entries |> List.map (fun (key, value) -> (mapExpr key, mapExpr value)))
        | RecordLiteral (reference, fields) ->
            let reference' : RecordReference =
                { SourceTypeName = reference.SourceTypeName
                  ResolvedTypeName = reference.ResolvedTypeName
                  TypeArgs = List.map semanticTypeOfParsed reference.TypeArgs }
            RecordLiteral (reference', fields |> List.map (fun (field, value) -> (field, mapExpr value)))
        | RecordUpdate (record, updates) ->
            RecordUpdate (mapExpr record, updates |> List.map (fun (field, value) -> (field, mapExpr value)))
        | RecordAccess (record, field) -> RecordAccess (mapExpr record, field)
        | Constructor (reference, name, fields) -> Constructor (reference, name, List.map mapExpr fields)
        | Match (scrutinee, cases) ->
            let cases' =
                cases
                |> List.map (fun case ->
                    { Patterns = case.Patterns
                      Guard = Option.map mapExpr case.Guard
                      Body = mapExpr case.Body })
            Match (mapExpr scrutinee, cases')
        | ListLiteral values -> ListLiteral (List.map mapExpr values)
        | Lambda (parameters, returnAnnotation, body) ->
            let parameters' =
                parameters
                |> NonEmptyList.map (fun parameter ->
                    { Pattern = parameter.Pattern
                      SourceAnnotation = Option.map semanticTypeOfParsed parameter.SourceAnnotation
                      InferredType = Option.map semanticTypeOfParsed parameter.InferredType })
            Lambda (parameters', Option.map semanticTypeOfParsed returnAnnotation, mapExpr body)
        | IndirectApply (func, args) -> IndirectApply (mapExpr func, mapArgs args)
        | Closure (name, captures) -> Closure (name, List.map mapExpr captures)
        | BoundaryRender (renderer, value) -> BoundaryRender (renderer, mapExpr value)
        | UnitLiteral -> UnitLiteral
        | Int64Literal value -> Int64Literal value
        | Int128Literal value -> Int128Literal value
        | Int8Literal value -> Int8Literal value
        | Int16Literal value -> Int16Literal value
        | Int32Literal value -> Int32Literal value
        | UInt8Literal value -> UInt8Literal value
        | UInt16Literal value -> UInt16Literal value
        | UInt32Literal value -> UInt32Literal value
        | UInt64Literal value -> UInt64Literal value
        | UInt128Literal value -> UInt128Literal value
        | BigIntLiteral value -> BigIntLiteral value
        | BoolLiteral value -> BoolLiteral value
        | StringLiteral value -> StringLiteral value
        | CharLiteral value -> CharLiteral value
        | FloatLiteral value -> FloatLiteral value
        | Var name -> Var name
        | RuntimeError message -> RuntimeError message

    let mapFunction (definition: ParsedFunctionDef) : FunctionDef =
        { Name = definition.Name
          TypeParams = definition.TypeParams
          Params = definition.Params |> NonEmptyList.map (fun (name, typ) -> (name, semanticTypeOfParsed typ))
          ReturnType = semanticTypeOfParsed definition.ReturnType
          Body = mapExpr definition.Body
          Recursion = definition.Recursion }
    let mapTypeDef (definition: ParsedTypeDef) : TypeDef =
        match definition with
        | RecordDef (name, typeParams, fields) ->
            RecordDef (name, typeParams, fields |> List.map (fun (field, typ) -> (field, semanticTypeOfParsed typ)))
        | SumTypeDef (name, typeParams, variants) ->
            SumTypeDef (
                name,
                typeParams,
                variants
                |> List.map (fun variant ->
                    { Name = variant.Name
                      Fields = List.map semanticTypeOfParsed variant.Fields }))
        | TypeAlias (name, typeParams, targetType) ->
            TypeAlias (name, typeParams, semanticTypeOfParsed targetType)
    let mapValueDef (definition: ParsedValueDef) : ValueDef =
        match definition with
        | UncheckedValueDef (name, body) -> UncheckedValueDef (name, mapExpr body)
        | CheckedValueDef (name, typ, body) ->
            CheckedValueDef (name, semanticTypeOfParsed typ, mapExpr body)
    topLevels
    |> List.map (function
        | FunctionDef definition -> FunctionDef (mapFunction definition)
        | TypeDef definition -> TypeDef (mapTypeDef definition)
        | ValueDef definition -> ValueDef (mapValueDef definition)
        | Expression (modulePath, expr) -> Expression (modulePath, mapExpr expr))
    |> Program

/// Module function definition - a function within a module
type ModuleFunc = {
    Name: string                     // Function name (e.g., "add")
    TypeParams: string list          // Type parameters (e.g., ["v"] for generic intrinsics)
    ParamTypes: SemanticType list            // Parameter types (may contain TVar references)
    ReturnType: SemanticType                 // Return type (may contain TVar references)
}

/// Module definition - represents a namespace of functions
type ModuleDef = {
    Name: string                     // Full module path (e.g., "Darklang.Stdlib.Int64")
    Functions: ModuleFunc list       // Functions in this module
}

/// Module registry - maps full function paths to their definitions
type ModuleRegistry = Map<string, ModuleFunc>
