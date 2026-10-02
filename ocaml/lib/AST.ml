(* AST.ml - Complete semantic syntax and deterministic compiler identity helpers. *)
[@@@warning "-30"]
type 'a nonEmptyList = 'a NonEmptyList.t

type warningSettings = 
  | WarningSettings

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

type recordFieldReference = {sourceFieldName : string; resolvedTypeName : string option; resolvedFieldIndex : int option}

type constructorReference = 
  | UnresolvedConstructor of string option
  | ResolvedConstructor of string list * string * semanticType list

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

type unaryOp = 
  | Neg
  | Not
  | BitNot

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

type letPattern = 
  | LPUnit
  | LPWildcard
  | LPVariable of string
  | LPTuple of letPattern * letPattern * letPattern list

type 't lambdaParameterNode = {pattern : letPattern; sourceAnnotation : 't option; inferredType : 't option}

type lambdaParameter = semanticType lambdaParameterNode

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

type recursiveCandidate = {sourceName : string; kind : recursiveMemberKind}

type parsedRecursiveMember = {binding : bindingId; boundary : scopeBoundaryId; member : recursiveMemberId; sourceName : string; kind : recursiveMemberKind}

type resolvedRecursiveMember = {parsed : parsedRecursiveMember; group : recursiveGroupId; groupIndex : int; availability : recursiveAvailability}

type typedRecursiveMember = {resolved : resolvedRecursiveMember; monomorphicType : semanticType}

type loweredRecursiveMember = {typed : typedRecursiveMember; environmentIndex : int}

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

type moduleFunc = {name : string; typeParams : string list; paramTypes : semanticType list; returnType : semanticType}

type moduleDef = {name : string; functions : moduleFunc list}

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
let collidingConstructorCaseNames definitions =
  let owners = List.fold_left (fun owners -> function
    | SumTypeDef (typeName, _, variants) -> List.fold_left (fun owners (variant : 't variantNode) ->
        let previous = match StringOrder.Map.find_opt variant.name owners with Some value -> value | None -> StringOrder.Set.empty in
        StringOrder.Map.add variant.name (StringOrder.Set.add typeName previous) owners) owners variants
    | RecordDef _ | TypeAlias _ -> owners) StringOrder.Map.empty definitions in
  StringOrder.Map.fold (fun case owners collisions -> if StringOrder.Set.cardinal owners > 1 then StringOrder.Set.add case collisions else collisions) owners StringOrder.Set.empty
