// AST.fs - Abstract Syntax Tree
//
// Defines the abstract syntax tree data structures that represent the parsed
// program structure. The AST is the output of the Parser and input to the ANF
// transformation.
// Keep this file as the structural source of truth for syntax-facing compiler
// nodes; language support and compatibility boundaries belong in
// docs/darklang-differences.md.

module AST

/// A list guaranteed to have at least one element (makes invalid states unrepresentable)
type NonEmptyList<'a> = { Head: 'a; Tail: 'a list }

/// Compiler-wide warning settings passed from the driver into compiler passes.
/// Duplicate binders are language errors, not configurable warnings.
type WarningSettings =
    private
    | WarningSettings

let defaultWarningSettings : WarningSettings = WarningSettings

/// Type system shared by parsing, type checking, ANF lowering, and later passes.
type Type =
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
    | TBlob     // Byte array: [length:8][data:N][refcount:8]
    | TChar      // Extended Grapheme Cluster (single visual character)
    | TDateTime  // Opaque UTC instant stored as signed 100ns Unix ticks
    | TUnit
    | TRuntimeError                 // Bottom-like type for guaranteed runtime-failing expressions
    | TFunction of Type list * Type  // parameter types * return type
    | TTuple of Type list             // tuple type: (Int, Bool, String)
    /// Ordered fields of an enum case. Unlike TTuple, these are separate
    /// constructor arguments (`Case of A * B`), not one tuple argument
    /// (`Case of (A * B)`). This syntax-only shape is lowered as a tuple block.
    | TEnumFields of Type list
    | TRecord of string * Type list   // record type by name with type args: Point<T>, Pair<A, B>, etc.
    | TSum of string * Type list      // sum type by name with type args: Result<Int64, String>
    | TList of Type                    // List<T> - polymorphic list type
    | TStream of Type                  // Stream<T> - opaque, lazy, single-consumer handle
    | TVar of string                  // type variable: T, A, B, etc. (for generics)
    | TRawPtr                         // Raw pointer to unmanaged memory (internal, for HAMT)
    // Native HAMT machinery retains both components. Public source syntax is
    // String-keyed and renders only the value component as Dict<Value>.
    | TDict of keyType:Type * valueType:Type

/// Nominal identity carried by record construction from parsing onward.
/// SourceTypeName preserves an alias spelling for diagnostics, while
/// ResolvedTypeName is filled with the canonical declaration identity during
/// name/type resolution. TypeArgs always follows declaration parameter order,
/// including parameters that do not occur in any field.
type RecordReference = {
    SourceTypeName: string
    ResolvedTypeName: string
    TypeArgs: Type list
}

let unresolvedRecordReference (sourceTypeName: string) (typeArgs: Type list) : RecordReference =
    { SourceTypeName = sourceTypeName; ResolvedTypeName = sourceTypeName; TypeArgs = typeArgs }

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
    | "Stdlib.Option.Option", "Some"
    | "Stdlib.Result.Result", "Ok" -> 0
    | "Stdlib.Option.Option", "None"
    | "Stdlib.Result.Result", "Error" -> 1
    | _ ->
        $"{declaringType}.{caseName}"
        |> Seq.fold (fun hash character -> (hash ^^^ uint32 character) * 16777619u) 2166136261u
        |> fun hash -> 2 + int (hash % 4094u)

/// Pattern matching patterns
type Pattern =
    | PUnit                                                // () - matches unit value
    | PWildcard                                            // _
    | PVar of string                                       // x (binds value to variable)
    | PConstructor of variantName:string * payload:Pattern option  // Red, Some(x)
    | PInt64 of int64                                      // 42 (Int64 literal)
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
type LambdaParameter = {
    Pattern: LetPattern
    SourceAnnotation: Type option
    InferredType: Type option
}

let lambdaParameter (pattern: LetPattern) : LambdaParameter =
    { Pattern = pattern; SourceAnnotation = None; InferredType = None }

let typedLambdaVariable (name: string) (typ: Type) : LambdaParameter =
    { Pattern = LPVariable name; SourceAnnotation = Some typ; InferredType = Some typ }

let inferredLambdaVariable (name: string) (typ: Type) : LambdaParameter =
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
[<StructuralEquality; StructuralComparison>]
type BindingId = private BindingId of int list

[<StructuralEquality; StructuralComparison>]
type ScopeBoundaryId = private ScopeBoundaryId of int list

[<StructuralEquality; StructuralComparison>]
type RecursiveGroupId = private RecursiveGroupId of int list

[<StructuralEquality; StructuralComparison>]
type RecursiveMemberId = private RecursiveMemberId of int list

let bindingId path = BindingId path
let scopeBoundaryId path = ScopeBoundaryId path
let recursiveGroupId path = RecursiveGroupId path
let recursiveMemberId path = RecursiveMemberId path
let singletonRecursiveGroupId (RecursiveMemberId path) = RecursiveGroupId (1 :: path)

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
    MonomorphicType: Type
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
        | PConstructor (_, payload) ->
            payload |> Option.map matchPatternBindings |> Option.defaultValue []
        | PTuple patterns | PList patterns -> patterns |> List.collect matchPatternBindings
        | PListCons (heads, tail) ->
            (heads |> List.collect matchPatternBindings) @ matchPatternBindings tail
        | POr alternatives ->
            alternatives |> NonEmptyList.head |> matchPatternBindings
        | PUnit | PWildcard | PInt64 _ | PInt128Literal _ | PInt8Literal _
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
type StringPart =
    | StringText of string    // Literal text: "Hello "
    | StringExpr of Expr      // Interpolated expression: {name}

/// Expression nodes
and Expr =
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
    | BigIntLiteral of System.Numerics.BigInteger // Arbitrary-precision Int: 42I
    | BoolLiteral of bool
    | StringLiteral of string
    | CharLiteral of string   // Single Extended Grapheme Cluster stored as UTF-8 string
    | FloatLiteral of float
    | InterpolatedString of StringPart list // $"Hello {name}!"
    | BinOp of BinOp * Expr * Expr
    | UnaryOp of UnaryOp * Expr
    | Let of pattern:LetPattern * value:Expr * body:Expr  // Atomic non-recursive binding
    | RecursiveLet of recursion:RecursiveBindingInfo * value:Expr * body:Expr
    | Var of string  // Variable reference
    | If of cond:Expr * thenBranch:Expr * elseBranch:Expr  // If expression: if cond then thenBranch else elseBranch
    | Sequence of first:Expr * next:Expr  // Statement sequence: first must produce Unit; next supplies the value
    | Call of funcName:string * args:NonEmptyList<Expr>  // Function call: funcName(arg1, arg2, ...)
    | TypeApp of funcName:string * typeArgs:Type list * args:NonEmptyList<Expr>  // Generic call: funcName<T, U>(args)
    | TupleLiteral of Expr list              // Tuple literal: (1, 2, 3)
    | TupleAccess of tuple:Expr * index:int  // Tuple access: t.0, t.1, etc.
    | DictLiteral of valueType:Type * entries:(string * Expr) list  // Dict { key = value; ... }
    | RecordLiteral of reference:RecordReference * fields:(string * Expr) list
    | RecordUpdate of record:Expr * updates:(string * Expr) list      // { record with x = 1, y = 2 }
    | RecordAccess of record:Expr * fieldName:string                  // p.x, p.y
    | Constructor of reference:ConstructorReference * variantName:string * payload:Expr option
    | Match of scrutinee:Expr * cases:MatchCase list  // match e with | p1 when g -> e1 | p2 -> e2
    | ListLiteral of Expr list                               // [1, 2, 3]
    | Lambda of parameters:NonEmptyList<LambdaParameter> * returnAnnotation:Type option * body:Expr
    | Apply of func:Expr * args:NonEmptyList<Expr>                    // Apply function expr: f(x) where f is expression
    | IndirectApply of func:Expr * args:NonEmptyList<Expr>            // Compiler-generated call through a raw function pointer
    | FuncRef of funcName:string                             // Reference to a function (for passing as value)
    | Closure of funcName:string * captures:Expr list        // Closure: function + captured values
    | RuntimeError of message:string                         // Compiler-generated interpreter runtime error
    | BoundaryRender of renderer:string * value:Expr        // Compiler-generated eval-result rendering

/// Match case with optional guard clause and pattern grouping
/// Syntax: | pat1 | pat2 when guard -> body
and MatchCase = {
    Patterns: NonEmptyList<Pattern>  // One or more patterns (pattern grouping via |)
    Guard: Expr option               // Optional guard clause (when condition)
    Body: Expr                       // Body expression
}

/// Function definition
type FunctionDef = {
    Name: string
    TypeParams: string list           // Type parameters for generics: ["T", "U", etc.], empty for non-generic
    Params: NonEmptyList<(string * Type)>  // Parameter names with REQUIRED type annotations
    ReturnType: Type                  // REQUIRED return type annotation
    Body: Expr
    Recursion: RecursiveBindingInfo option
}

/// Variant in a sum type, optionally carrying one payload type.
type Variant = {
    Name: string
    Payload: Type option  // None for simple enums, Some for payload-carrying variants
}

/// Type definition (record types, sum types, etc.)
type TypeDef =
    | RecordDef of name:string * typeParams:string list * fields:(string * Type) list  // type Point<T> = { x: T, y: T }
    | SumTypeDef of name:string * typeParams:string list * variants:Variant list       // type Result<T, E> = Ok of T | Error of E
    | TypeAlias of name:string * typeParams:string list * targetType:Type              // type Id = String

/// Case names that require a nominal native tag because they occur in more
/// than one declaring type in the same compilation unit.
let collidingConstructorCaseNames (typeDefs: TypeDef list) : Set<string> =
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
type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of TypeDef
    | Expression of Expr

/// Program is a list of top-level definitions (functions and/or expressions)
type Program = Program of TopLevel list

/// Module function definition - a function within a module
type ModuleFunc = {
    Name: string                     // Function name (e.g., "add")
    TypeParams: string list          // Type parameters (e.g., ["v"] for generic intrinsics)
    ParamTypes: Type list            // Parameter types (may contain TVar references)
    ReturnType: Type                 // Return type (may contain TVar references)
}

/// Module definition - represents a namespace of functions
type ModuleDef = {
    Name: string                     // Full module path (e.g., "Stdlib.Int64")
    Functions: ModuleFunc list       // Functions in this module
}

/// Module registry - maps full function paths to their definitions
type ModuleRegistry = Map<string, ModuleFunc>

/// A typed compiler-visible value supplied by the standard library.
type ModuleValue = {
    Name: string
    Type: Type
}

type ModuleValueRegistry = Map<string, ModuleValue>
