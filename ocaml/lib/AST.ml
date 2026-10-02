(*
   AST.fs - Abstract Syntax Tree
   Defines the abstract syntax tree data structures that represent the parsed
   program structure. The AST is the output of parsing and the internal input
   to name resolution and semantic checking. Successful checking constructs the
   phase-safe CheckedAST consumed by compiler preparation and ANF lowering.
   Keep this file as the structural source of truth for syntax-facing compiler
   nodes; language support and compatibility boundaries belong in
   docs/compatibility/overview.md.
   A list guaranteed to have at least one element (makes invalid states unrepresentable)
   Nominal identity carried by record construction from parsing onward.
   SourceTypeName preserves an alias spelling for diagnostics, while
   ResolvedTypeName is filled with the canonical declaration identity during
   name/type resolution. TypeArgs always follows declaration parameter order,
   including parameters that do not occur in any field.
   A lambda binder is parsed without an annotation. Type checking fills in its
   inferred type without changing the source-level binding pattern.
   Part of an interpolated string: either a literal or an expression
   Literal text: "Hello "
   Interpolated expression: {name}
   Expression nodes
   Unit value: ()
   64-bit signed (default): 42, 42L
   42Q
   8-bit signed: 42y
   16-bit signed: 42s
   32-bit signed: 42l
   8-bit unsigned: 42uy
   16-bit unsigned: 42us
   32-bit unsigned: 42ul
   64-bit unsigned: 42UL
   42Z
   Arbitrary-precision unsuffixed Int
   Single Extended Grapheme Cluster stored as UTF-8 string
   $"Hello {name}!"
   Atomic non-recursive binding
   Variable reference
   If expression: if cond then thenBranch else elseBranch
   Statement sequence: first must produce Unit; next supplies the value
   A source-level call.  Resolution classifies the callee as a direct
   function, intrinsic, or dynamic value at the checked-AST boundary.
   Tuple literal: (1, 2, 3)
   Tuple access: t.0, t.1, etc.
   { record with x = 1, y = 2 }
   p.x, p.y
   match e with | p1 when g -> e1 | p2 -> e2
   [1, 2, 3]
   Compiler-generated call through a raw function pointer
   Closure: function + captured values
   Compiler-generated interpreter runtime error
   Compiler-generated eval-result rendering
   Match case with optional guard clause and pattern grouping
   Syntax: | pat1 | pat2 when guard -> body
   One or more patterns (pattern grouping via |)
   Optional guard clause (when condition)
   Body expression
   Function definition
   Type parameters for generics: ["T", "U", etc.], empty for non-generic
   Parameter names with REQUIRED type annotations
   REQUIRED return type annotation
   Variant in a sum type with zero or more ordered constructor fields.
   Type definition (record types, sum types, etc.)
   type Point<T> = { x: T, y: T }
   type Result<T, E> = Ok of T | Error of E
   type Id = String
   A source value before and after its body has been type checked.
   Top-level program elements
   Program is a list of top-level definitions (functions and/or expressions)
*)
(* AST.ml - Complete semantic syntax and deterministic compiler identity helpers. *)
[@@@warning "-30"]
type 'a nonEmptyList = 'a NonEmptyList.t

(*
   Compiler-wide warning settings passed from the driver into compiler passes.
   Duplicate binders are language errors, not configurable warnings.
*)
type warningSettings = 
  | WarningSettings

(*
   Types used by semantic checking and the lowering pipeline.
   Signed integers
   Arbitrary-precision signed integer
   Unsigned integers
   Other primitives
   Byte array: [refcount:8][length:8][data:N][padding]
   Extended Grapheme Cluster (single visual character)
   Opaque UTC instant stored as signed 100ns Unix ticks
   Semantic bottom: expressions which do not return
   parameter types * return type
   tuple type: (Int, Bool, String)
   record type by name with type args: Point<T>, Pair<A, B>, etc.
   sum type by name with type args: Result<Int64, String>
   List<T> - polymorphic list type
   Stream<T> - opaque, lazy, single-consumer handle
   type variable: T, A, B, etc. (for generics)
   Raw pointer to unmanaged memory (internal, for HAMT)
   Native HAMT machinery retains both components. Public source syntax is
   String-keyed and renders only the value component as Dict<Value>.
*)
type semanticType = 
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TInt128
  | TInt
  | TUInt8
  | TUInt16
  | TUInt32
  | TUInt64
  | TUInt128
  | TBool
  | TFloat64
  | TString
  | TBlob
  | TChar
  | TDateTime
  | TUnit
  | TNever
  | TFunction of semanticType list * semanticType
  | TTuple of semanticType list
  | TRecord of string * semanticType list
  | TSum of string * semanticType list
  | TList of semanticType
  | TStream of semanticType
  | TVar of string
  | TInferenceVar of string * string
  | TInternalRawPtr
  | TDict of semanticType * semanticType

type 't recordReferenceNode = {sourceTypeName : string; resolvedTypeName : string; typeArgs : 't list}

type recordReference = semanticType recordReferenceNode

(*
   A field spelling before or after the checker proves its declaring record.
   The resolved owner is semantic evidence required to assign a declaration-
   scoped FieldId at the checked-program boundary.
*)
type recordFieldReference = {sourceFieldName : string; resolvedTypeName : string option; resolvedFieldIndex : int option}

(*
   A source constructor reference before or after nominal resolution.
   `None` is the genuinely unqualified form; no empty-name sentinel is used.
*)
type constructorReference = 
  | UnresolvedConstructor of string option
  | ResolvedConstructor of string list * string * semanticType list

(*
   Binary operators
   Arithmetic
   %
   Bitwise operations
   << (left shift)
   >> (right shift)
   & (bitwise and)
   ||| (bitwise or)
   ^ (bitwise xor)
   String operations
   ++
   Comparisons (return bool)
   !=
   <=
   Boolean operations
   ||
*)
type binOp = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Shl
  | Shr
  | BitAnd
  | BitOr
  | BitXor
  | StringConcat
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or

(*
   Unary operators
   Unary negation: -expr
   Boolean not: !expr
   Bitwise not: ~~~expr
   NonEmptyList helper functions
*)
type unaryOp = 
  | Neg
  | Not
  | BitNot

(*
   Pattern matching patterns
   () - matches unit value
   x (binds value to variable)
   Red, Some(x), Pair(a, b)
   42 (Int64 literal)
   42 (Int literal)
   42Q
   1y
   1s
   1uy
   1us
   1ul
   1UL
   42Z
   true, false
   "hello"
   'x'
   3.14
   (a, b, c)
   [a, b, c] - exact length match
   a :: b :: t - head elements + rest
   p1 | p2 - left-to-right alternatives
*)
type pattern = 
  | PUnit
  | PWildcard
  | PVar of string
  | PConstructor of string * pattern list
  | PResolvedConstructor of string * string * int * pattern list
  | PInt64 of int64
  | PBigInt of Z.t
  | PInt128Literal of Z.t
  | PInt8Literal of int
  | PInt16Literal of int
  | PInt32Literal of int32
  | PUInt8Literal of int
  | PUInt16Literal of int
  | PUInt32Literal of int64
  | PUInt64Literal of int64
  | PUInt128Literal of Z.t
  | PBool of bool
  | PString of string
  | PChar of string
  | PFloat of float
  | PTuple of pattern list
  | PList of pattern list
  | PListCons of pattern list * pattern
  | POr of pattern nonEmptyList

(*
   The deliberately restricted pattern language shared by non-recursive lets
   and lambda parameters. Match-only patterns cannot be represented here.
*)
type letPattern = 
  | LPUnit
  | LPWildcard
  | LPVariable of string
  | LPTuple of letPattern * letPattern * letPattern list

type 't lambdaParameterNode = {pattern : letPattern; sourceAnnotation : 't option; inferredType : 't option}

type lambdaParameter = semanticType lambdaParameterNode

(*
   Stable semantic identities assigned at the parsed-program boundary. The
   representation is private so source spellings cannot be used as identities.
*)
type binderStructure = 
  | LetBinderPatterns of letPattern list
  | MatchBinderPattern of pattern

type bindingId = 
  | LocalBindingId of int * string option
  | TopLevelValueId of string

type functionId = 
  | FunctionId of int64

type typeId = 
  | TypeId of int

type constructorId = 
  | ConstructorId of typeId * string * int

type fieldId = 
  | FieldId of typeId * int

type scopeBoundaryId = 
  | ScopeBoundaryId of int

type recursiveGroupId = 
  | RecursiveGroupId of int

type recursiveMemberId = 
  | RecursiveMemberId of int

type recursiveMemberKind = 
  | TopLevelFunctionMember
  | NamedLocalFunctionMember
  | DirectLambdaValueMember

type recursiveAvailability = 
  | OrdinaryBinding
  | SelfRecursiveMember
  | MutualRecursiveMember
  | CompletedGroupMember
  | ImportedGroupMember

type recursiveDependencyKind = 
  | DelayedCallableDependency
  | EagerValueDependency
  | TypeAliasDependency

(*
   Parser-only evidence that a declaration is eligible for recursive
   resolution.
*)
type recursiveCandidate = {sourceName : string; kind : recursiveMemberKind}

type parsedRecursiveMember = {binding : bindingId; boundary : scopeBoundaryId; member : recursiveMemberId; sourceName : string; kind : recursiveMemberKind}

type resolvedRecursiveMember = {parsed : parsedRecursiveMember; group : recursiveGroupId; groupIndex : int; availability : recursiveAvailability}

type typedRecursiveMember = {resolved : resolvedRecursiveMember; monomorphicType : semanticType}

type loweredRecursiveMember = {typed : typedRecursiveMember; environmentIndex : int}

(*
   Every materialized group is nonempty by construction.
*)
type parsedRecursiveGroup = {boundary : scopeBoundaryId; members : parsedRecursiveMember nonEmptyList}

type resolvedRecursiveGroup = {group : recursiveGroupId; members : resolvedRecursiveMember nonEmptyList}

type typedRecursiveGroup = {group : recursiveGroupId; members : typedRecursiveMember nonEmptyList}

type loweredRecursiveGroup = {group : recursiveGroupId; members : loweredRecursiveMember nonEmptyList}

type recursiveBindingInfo = 
  | RecursiveBindingCandidate of recursiveCandidate
  | ParsedRecursiveBinding of parsedRecursiveMember
  | ResolvedRecursiveBinding of resolvedRecursiveMember
  | TypedRecursiveBinding of typedRecursiveMember

type 't stringPartNode = 
  | StringText of string
  | StringExpr of 't exprNode

and 't exprNode = 
  | UnitLiteral
  | Int64Literal of int64
  | Int128Literal of Z.t
  | Int8Literal of int
  | Int16Literal of int
  | Int32Literal of int32
  | UInt8Literal of int
  | UInt16Literal of int
  | UInt32Literal of int64
  | UInt64Literal of int64
  | UInt128Literal of Z.t
  | BigIntLiteral of Z.t
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of string
  | FloatLiteral of float
  | InterpolatedString of 't stringPartNode list
  | BinOp of binOp * 't exprNode * 't exprNode
  | UnaryOp of unaryOp * 't exprNode
  | Let of letPattern * 't exprNode * 't exprNode
  | RecursiveLet of recursiveBindingInfo * 't exprNode * 't exprNode
  | Var of string
  | If of 't exprNode * 't exprNode * 't exprNode
  | Sequence of 't exprNode * 't exprNode
  | Apply of 't exprNode * 't list * 't exprNode nonEmptyList
  | TupleLiteral of 't exprNode list
  | TupleAccess of 't exprNode * int
  | DictLiteral of 't * 't * ('t exprNode * 't exprNode) list
  | RecordLiteral of 't recordReferenceNode * (recordFieldReference * 't exprNode) list
  | RecordUpdate of 't exprNode * (recordFieldReference * 't exprNode) list
  | RecordAccess of 't exprNode * recordFieldReference
  | Constructor of constructorReference * string * 't exprNode list
  | Match of 't exprNode * 't matchCaseNode list
  | ListLiteral of 't exprNode list
  | Lambda of 't lambdaParameterNode nonEmptyList * 't option * 't exprNode
  | IndirectApply of 't exprNode * 't exprNode nonEmptyList
  | Closure of string * 't exprNode list
  | RuntimeError of string
  | BoundaryRender of string * 't exprNode

and 't matchCaseNode = {patterns : pattern nonEmptyList; guard : 't exprNode option; body : 't exprNode}

type expr = semanticType exprNode

type stringPart = semanticType stringPartNode

type matchCase = semanticType matchCaseNode

type 't functionDefNode = {name : string; typeParams : string list; params : (string * 't) nonEmptyList; returnType : 't; body : 't exprNode; recursion : recursiveBindingInfo option}

type 't variantNode = {name : string; fields : 't list}

type 't typeDefNode = 
  | RecordDef of string * string list * (string * 't) list
  | SumTypeDef of string * string list * 't variantNode list
  | TypeAlias of string * string list * 't

type 't valueDefNode = 
  | UncheckedValueDef of string * 't exprNode
  | CheckedValueDef of string * 't * 't exprNode

type functionDef = semanticType functionDefNode

type variant = semanticType variantNode

type typeDef = semanticType typeDefNode

type valueDef = semanticType valueDefNode

type 't topLevelNode = 
  | FunctionDef of 't functionDefNode
  | TypeDef of 't typeDefNode
  | ValueDef of 't valueDefNode
  | Expression of string list * 't exprNode

type 't programNode = 
  | Program of 't topLevelNode list

type topLevel = semanticType topLevelNode

type program = semanticType programNode

(*
   Module function definition - a function within a module
   Function name (e.g., "add")
   Type parameters (e.g., ["v"] for generic intrinsics)
   Parameter types (may contain TVar references)
   Return type (may contain TVar references)
*)
type moduleFunc = {name : string; typeParams : string list; paramTypes : semanticType list; returnType : semanticType}

(*
   Module definition - represents a namespace of functions
   Full module path (e.g., "Darklang.Stdlib.Int64")
   Functions in this module
*)
type moduleDef = {name : string; functions : moduleFunc list}

(*
   Module registry - maps full function paths to their definitions
*)
type moduleRegistry = moduleFunc StringOrder.Map.t
module NonEmptyList = NonEmptyList
let defaultWarningSettings = WarningSettings
let unresolvedRecordReference sourceTypeName typeArgs = {sourceTypeName; resolvedTypeName = sourceTypeName; typeArgs}
let unresolvedRecordFieldReference sourceFieldName = {sourceFieldName; resolvedTypeName = None; resolvedFieldIndex = None}
let resolvedRecordFieldReference typeName sourceFieldName index = {sourceFieldName; resolvedTypeName = Some typeName; resolvedFieldIndex = Some index}
let constructorReferenceTypeName = function
  | UnresolvedConstructor name -> name
  | ResolvedConstructor (path, name, _) -> Some (String.concat "." (path @ [name]))
let resolvedConstructorReference canonical = match List.rev (String.split_on_char '.' canonical) with
  | name :: path -> ResolvedConstructor (List.rev path, name, [])
  | [] -> Crash.crash "Cannot resolve a constructor against an empty declaring type name"
let resolvedConstructorReferenceWithTypeArgs canonical typeArgs = match resolvedConstructorReference canonical with
  | ResolvedConstructor (path, name, _) -> ResolvedConstructor (path, name, typeArgs)
  | UnresolvedConstructor _ -> Crash.crash "Resolved constructor helper returned an unresolved reference"
(*
   Canonical native identity for an enum case whose display name is shared by
   multiple nominal declarations. The native backends encode case tags as
   immediates, so declarations validate collisions in this bounded space.
   Runtime I/O and string intrinsics construct these two foundational
   stdlib types directly. Their ABI tags predate user-defined ADTs.
*)
let constructorRuntimeIdentity declaringType caseName =
  match declaringType, caseName with
  | "Darklang.Stdlib.Option.Option", "Some" | "Darklang.Stdlib.Result.Result", "Ok" -> 0
  | "Darklang.Stdlib.Option.Option", "None" | "Darklang.Stdlib.Result.Result", "Error" -> 1
  | _ ->
      let hash = Array.fold_left (fun hash unit -> Int32.mul (Int32.logxor hash (Int32.of_int unit)) 16777619l) 0x811c9dc5l (HostText.utf16Units (declaringType ^ "." ^ caseName)) in
      2 + Int64.to_int (Int64.rem (Int64.logand (Int64.of_int32 hash) 0xffffffffL) 4094L)
let lambdaParameter pattern = {pattern; sourceAnnotation = None; inferredType = None}
let typedLambdaVariable name typ = {pattern = LPVariable name; sourceAnnotation = Some typ; inferredType = Some typ}
let inferredLambdaVariable name typ = {pattern = LPVariable name; sourceAnnotation = None; inferredType = Some typ}
let rec letPatternBindings = function LPVariable name -> [name]
  | LPTuple (first, second, rest) -> List.concat_map letPatternBindings (first :: second :: rest)
  | LPUnit | LPWildcard -> []
let rec mapLetPatternBindings fn = function
  | LPVariable name -> LPVariable (fn name)
  | LPTuple (first, second, rest) -> LPTuple (mapLetPatternBindings fn first, mapLetPatternBindings fn second, List.map (mapLetPatternBindings fn) rest)
  | LPUnit -> LPUnit | LPWildcard -> LPWildcard
let bindingId ordinal = LocalBindingId (ordinal, None)
let namedBindingId ordinal name = LocalBindingId (ordinal, Some name)
let topLevelValueId canonical = TopLevelValueId canonical
let bindingDisplayName = function LocalBindingId (_, name) -> name | TopLevelValueId canonical -> Some canonical
(* Bias the opaque representation so structural comparison retains uint64 order. *)
let functionId ordinal = FunctionId (Int64.logxor ordinal Int64.min_int)
let functionIdValue (FunctionId ordinal) = Int64.logxor ordinal Int64.min_int
let nextFunctionIdOrdinal ordinal = if ordinal = -1L then Crash.crash "Function identity allocation exhausted" else Int64.add ordinal 1L
(*
   Allocate deterministic identities from an already-maintained catalog cursor.
*)
let allocateFunctionIdsFromOrdinal first names =
  let sorted = Seq.fold_left (fun names name -> StringOrder.Set.add name names) StringOrder.Set.empty names in
  snd (StringOrder.Set.fold (fun name (next, ids) -> nextFunctionIdOrdinal next, StringOrder.Map.add name (functionId next) ids) sorted (first, StringOrder.Map.empty))
let allocateFunctionIds existing names =
  let first = Seq.fold_left (fun next identity ->
    let after = nextFunctionIdOrdinal (functionIdValue identity) in if Int64.unsigned_compare next after >= 0 then next else after) 0L existing in
  allocateFunctionIdsFromOrdinal first names
let typeId ordinal = TypeId ordinal
let constructorId owner name tag = ConstructorId (owner, name, tag)
let constructorIdOwner (ConstructorId (owner, _, _)) = owner
let constructorIdValue (ConstructorId (_, name, _)) = name
let constructorRuntimeTag (ConstructorId (_, _, tag)) = tag
let fieldId owner index = FieldId (owner, index)
let fieldIdOwner (FieldId (owner, _)) = owner
let fieldRuntimeIndex (FieldId (_, index)) = index
let scopeBoundaryId ordinal = ScopeBoundaryId ordinal
(*
   Group IDs share one compact namespace: declaration groups are even and
   singleton local-recursion groups are odd.
*)
let topLevelRecursiveGroupId ordinal = RecursiveGroupId (Int32.to_int (Int32.mul (Int32.of_int ordinal) 2l))
let recursiveMemberId ordinal = RecursiveMemberId ordinal
let singletonRecursiveGroupId (RecursiveMemberId ordinal) = RecursiveGroupId (Int32.to_int (Int32.add (Int32.mul (Int32.of_int ordinal) 2l) 1l))
let recursiveBindingName = function RecursiveBindingCandidate candidate -> candidate.sourceName | ParsedRecursiveBinding parsed -> parsed.sourceName
  | ResolvedRecursiveBinding resolved -> resolved.parsed.sourceName | TypedRecursiveBinding typed -> typed.resolved.parsed.sourceName
let recursiveBindingKind = function RecursiveBindingCandidate candidate -> candidate.kind | ParsedRecursiveBinding parsed -> parsed.kind
  | ResolvedRecursiveBinding resolved -> resolved.parsed.kind | TypedRecursiveBinding typed -> typed.resolved.parsed.kind
let recursiveBindingId = function ParsedRecursiveBinding parsed -> Some parsed.binding | ResolvedRecursiveBinding resolved -> Some resolved.parsed.binding
  | TypedRecursiveBinding typed -> Some typed.resolved.parsed.binding | RecursiveBindingCandidate _ -> None
let recursiveBindingAvailability = function ResolvedRecursiveBinding resolved -> Some resolved.availability | TypedRecursiveBinding typed -> Some typed.resolved.availability
  | RecursiveBindingCandidate _ | ParsedRecursiveBinding _ -> None
(*
   Validate one complete binder structure before any of its names enter scope.
   The returned list preserves source order and never contains ignored names.
*)
let validateBinders structure =
  let rec matchBindings = function
    | PVar name -> [name]
    | PConstructor (_, fields) | PResolvedConstructor (_, _, _, fields) | PTuple fields | PList fields -> List.concat_map matchBindings fields
    | PListCons (heads, tail) -> List.concat_map matchBindings heads @ matchBindings tail
    | POr alternatives -> matchBindings (NonEmptyList.head alternatives)
    | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _ | PInt16Literal _ | PInt32Literal _
    | PUInt8Literal _ | PUInt16Literal _ | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _ | PString _ | PChar _ | PFloat _ -> [] in
  let names = match structure with LetBinderPatterns patterns -> List.concat_map letPatternBindings patterns | MatchBinderPattern pattern -> matchBindings pattern in
  let usable = List.filter (fun name -> name <> "" && not (String.starts_with ~prefix:"_" name)) names in
  let duplicate = snd (List.fold_left (fun (seen, found) name -> match found with
    | Some _ -> seen, found | None when StringOrder.Set.mem name seen -> seen, Some name | None -> StringOrder.Set.add name seen, None) (StringOrder.Set.empty, None) usable) in
  match duplicate with Some name -> Error ("Duplicate binding '" ^ name ^ "' in the same pattern") | None -> Ok usable
let applyNamed name args = Apply (Var name, [], args)
let applyNamedWithTypes name typeArgs args = Apply (Var name, typeArgs, args)
let valueDefName = function UncheckedValueDef (name, _) | CheckedValueDef (name, _, _) -> name
let valueDefBody = function UncheckedValueDef (_, body) | CheckedValueDef (_, _, body) -> body
(*
   Case names that require a nominal native tag because they occur in more
   than one declaring type in the same compilation unit.
*)
let collidingConstructorCaseNames definitions =
  let owners = List.fold_left (fun owners -> function
    | SumTypeDef (typeName, _, variants) -> List.fold_left (fun owners (variant : 't variantNode) ->
        let previous = match StringOrder.Map.find_opt variant.name owners with Some value -> value | None -> StringOrder.Set.empty in
        StringOrder.Map.add variant.name (StringOrder.Set.add typeName previous) owners) owners variants
    | RecordDef _ | TypeAlias _ -> owners) StringOrder.Map.empty definitions in
  StringOrder.Map.fold (fun case owners collisions -> if StringOrder.Set.cardinal owners > 1 then StringOrder.Set.add case collisions else collisions) owners StringOrder.Set.empty
