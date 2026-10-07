(* AST.mli - Complete semantic syntax, stable identities, and recursive binding evidence. *)
[@@@warning "-30"]
type 'a nonEmptyList = 'a NonEmptyList.t

type warningSettings

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

type bindingId

type functionId

type typeId

type constructorId

type fieldId

type scopeBoundaryId

type recursiveGroupId

type recursiveMemberId

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
module NonEmptyList : module type of NonEmptyList
val defaultWarningSettings : warningSettings
val unresolvedRecordReference : string -> 't list -> 't recordReferenceNode
val unresolvedRecordFieldReference : string -> recordFieldReference
val resolvedRecordFieldReference : string -> string -> int -> recordFieldReference
val constructorReferenceTypeName : constructorReference -> string option
val resolvedConstructorReference : string -> constructorReference
val resolvedConstructorReferenceWithTypeArgs : string -> semanticType list -> constructorReference
val constructorRuntimeIdentity : string -> string -> int
val lambdaParameter : letPattern -> 't lambdaParameterNode
val typedLambdaVariable : string -> 't -> 't lambdaParameterNode
val inferredLambdaVariable : string -> 't -> 't lambdaParameterNode
val letPatternBindings : letPattern -> string list
val mapLetPatternBindings : (string -> string) -> letPattern -> letPattern
val bindingId : int -> bindingId
val namedBindingId : int -> string -> bindingId
val topLevelValueId : string -> bindingId
val bindingDisplayName : bindingId -> string option
val functionId : int64 -> functionId
val functionIdValue : functionId -> int64
val nextFunctionIdOrdinal : int64 -> int64
val allocateFunctionIdsFromOrdinal : int64 -> string Seq.t -> functionId StringOrder.Map.t
val allocateFunctionIds : functionId Seq.t -> string Seq.t -> functionId StringOrder.Map.t
val typeId : int -> typeId
val constructorId : typeId -> string -> int -> constructorId
val constructorIdOwner : constructorId -> typeId
val constructorIdValue : constructorId -> string
val constructorRuntimeTag : constructorId -> int
val fieldId : typeId -> int -> fieldId
val fieldIdOwner : fieldId -> typeId
val fieldRuntimeIndex : fieldId -> int
val scopeBoundaryId : int -> scopeBoundaryId
val topLevelRecursiveGroupId : int -> recursiveGroupId
val recursiveMemberId : int -> recursiveMemberId
val singletonRecursiveGroupId : recursiveMemberId -> recursiveGroupId
val recursiveBindingName : recursiveBindingInfo -> string
val recursiveBindingKind : recursiveBindingInfo -> recursiveMemberKind
val recursiveBindingId : recursiveBindingInfo -> bindingId option
val recursiveBindingAvailability : recursiveBindingInfo -> recursiveAvailability option
val validateBinders : binderStructure -> (string list, string) result
val applyNamed : string -> 't exprNode nonEmptyList -> 't exprNode
val applyNamedWithTypes : string -> 't list -> 't exprNode nonEmptyList -> 't exprNode
val valueDefName : 't valueDefNode -> string
val valueDefBody : 't valueDefNode -> 't exprNode
val collidingConstructorCaseNames : 't typeDefNode list -> StringOrder.Set.t
(* Structural ordering uses declaration-order cases and native string keys. *)
val compareBindingId : bindingId -> bindingId -> int
val compareTypeId : typeId -> typeId -> int
val compareConstructorId : constructorId -> constructorId -> int
val compareFieldId : fieldId -> fieldId -> int
val compareSemanticType : semanticType -> semanticType -> int

(* Display descriptions preserve stable diagnostic spelling without exposing constructors. *)
module DiagnosticFormatting : sig
 val binding : bindingId -> StructuralValue.value
 val func : functionId -> StructuralValue.value
 val typ : typeId -> StructuralValue.value
 val constructor : constructorId -> StructuralValue.value
 val field : fieldId -> StructuralValue.value
 val scope : scopeBoundaryId -> StructuralValue.value
 val group : recursiveGroupId -> StructuralValue.value
 val memberId : recursiveMemberId -> StructuralValue.value
end
