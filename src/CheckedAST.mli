(* CheckedAST.mli - Phase-safe syntax, declaration catalogs, and certified conversion. *)
[@@@warning "-30"]

module BindingIdMap : Map.S with type key = AST.bindingId
module TypeIdMap : Map.S with type key = AST.typeId
module ConstructorIdMap : Map.S with type key = AST.constructorId
module FieldIdMap : Map.S with type key = AST.fieldId
module NamePairMap : Map.S with type key = string * string

type letPattern =
  | LPUnit
  | LPWildcard
  | LPVariable of AST.bindingId
  | LPTuple of letPattern * letPattern * letPattern list

type 'a tupleElements = { first : 'a; second : 'a; rest : 'a list }
type checkedType
type checkedTypeDef

type recursiveMember = {
  resolved : AST.resolvedRecursiveMember;
  monomorphicType : checkedType;
}

type pattern =
  | PUnit
  | PWildcard
  | PVariable of AST.bindingId
  | PConstructor of AST.constructorId * pattern list
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
  | POr of pattern NonEmptyList.t

type lambdaParameter = { pattern : letPattern; typ : checkedType }
type recordReference = { typeId : AST.typeId; typeArgs : checkedType list }

type constructorReference = {
  typeId : AST.typeId;
  constructorId : AST.constructorId;
  typeArgs : checkedType list;
}

type 'a recordFields

type stringPart = StringText of string | StringExpr of expr

and expr =
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
  | BlobLiteral of string
  | CharLiteral of string
  | FloatLiteral of float
  | InterpolatedString of stringPart list
  | BinOp of AST.binOp * expr * expr
  | UnaryOp of AST.unaryOp * expr
  | Let of letPattern * expr * expr
  | RecursiveLet of recursiveMember * expr * expr
  | Local of AST.bindingId
  | If of expr * expr * expr
  | Sequence of expr * expr
  | Call of AST.functionId * expr NonEmptyList.t
  | TypeApp of AST.functionId * checkedType list * expr NonEmptyList.t
  | TupleLiteral of expr tupleElements
  | TupleAccess of expr * int
  | DictLiteral of checkedType * checkedType * (expr * expr) list
  | RecordLiteral of recordReference * expr recordFields
  | RecordUpdate of expr * (AST.fieldId * expr) list
  | RecordAccess of expr * AST.fieldId
  | Constructor of constructorReference * expr list
  | Match of expr * matchCase NonEmptyList.t
  | ListLiteral of expr list
  | Lambda of lambdaParameter NonEmptyList.t * checkedType option * expr
  | Apply of expr * expr NonEmptyList.t
  | IndirectApply of expr * expr NonEmptyList.t
  | FuncRef of AST.functionId
  | Closure of AST.functionId * expr list
  | RuntimeError of string
  | BoundaryRender of AST.functionId * expr

and matchCase = {
  patterns : pattern NonEmptyList.t;
  guard : expr option;
  body : expr;
}

type functionDef = {
  id : AST.functionId;
  name : string;
  typeParams : string list;
  params : (AST.bindingId * checkedType) NonEmptyList.t;
  returnType : checkedType;
  body : expr;
  recursion : recursiveMember option;
}

type valueDef = {
  id : AST.bindingId;
  name : string;
  typ : checkedType;
  body : expr;
}

type topLevel =
  | FunctionDef of functionDef
  | TypeDef of AST.typeId * checkedTypeDef
  | ValueDef of valueDef
  | Expression of expr

type semanticMetadata = { typeNames : string TypeIdMap.t }
type typeCatalog
type functionCatalog
type globalCatalog
type symbols = globalCatalog
type program

val tupleElementsToList : 'a tupleElements -> 'a list
val tupleElementsFromList : 'a list -> 'a tupleElements option
val tupleElementsOfList : 'a list -> 'a tupleElements
val mapTupleElements : ('a -> 'b) -> 'a tupleElements -> 'b tupleElements
val semanticType : checkedType -> AST.semanticType
val semanticTypeArgs : checkedType list -> AST.semanticType list
val semanticTypeDef : checkedTypeDef -> AST.typeDef
val recursiveMemberType : recursiveMember -> AST.semanticType
val semanticRecursiveMember : recursiveMember -> AST.typedRecursiveMember
val recordFieldsInSourceOrder : 'a recordFields -> (AST.fieldId * 'a) list
val mapRecordFields : ('a -> 'b) -> 'a recordFields -> 'b recordFields

val traverseRecordFields :
  ('a -> ('b, 'error) result) ->
  'a recordFields ->
  ('b recordFields, 'error) result

val mapFoldRecordFields :
  ('state -> 'a -> 'b * 'state) ->
  'state ->
  'a recordFields ->
  'b recordFields * 'state

val traverseStateRecordFields :
  ('a -> 'state -> ('b * 'state, 'error) result) ->
  'state ->
  'a recordFields ->
  ('b recordFields * 'state, 'error) result

val completeRecordFields :
  AST.typeId ->
  int ->
  (AST.fieldId * 'a) list ->
  ('a recordFields, string) result

val functionParameterTypes :
  functionDef -> (AST.bindingId * AST.semanticType) NonEmptyList.t

val functionReturnType : functionDef -> AST.semanticType
val emptyTypeCatalog : typeCatalog
val emptyFunctionCatalog : functionCatalog
val includeFunctionNames : string Seq.t -> functionCatalog -> functionCatalog
val viewProgram : program -> symbols * topLevel list
val programFromCheckedParts : symbols * topLevel list -> program
val emptySymbols : unit -> symbols
val allocateBinding : string -> symbols -> AST.bindingId * symbols
val nextBindingOrdinal : symbols -> int
val internValue : string -> symbols -> AST.bindingId * symbols
val bindingName : AST.bindingId -> symbols -> string option
val tryFindValueId : string -> symbols -> AST.bindingId option
val internFunction : string -> symbols -> AST.functionId * symbols
val registerGeneratedFunction : string -> AST.functionId -> symbols -> symbols

val allocatedFunctionNamesSince :
  int64 -> symbols -> (AST.functionId * string) list

val includeAllocatedFunctionNames : symbols -> symbols -> symbols
val internType : string -> symbols -> AST.typeId * symbols

val internConstructor :
  string -> string -> int -> symbols -> AST.constructorId * symbols

val internField : string -> string -> int -> symbols -> AST.fieldId * symbols
val functionName : AST.functionId -> symbols -> string option
val functionNames : symbols -> string FunctionIdMap.t
val functionIds : symbols -> AST.functionId StringOrder.Map.t
val nextFunctionOrdinal : symbols -> int64
val functionCatalog : symbols -> functionCatalog
val tryFindFunctionId : string -> symbols -> AST.functionId option
val typeName : AST.typeId -> symbols -> string option
val typeNames : symbols -> string TypeIdMap.t
val tryFindTypeId : string -> symbols -> AST.typeId option
val typeCatalog : symbols -> typeCatalog
val constructorInfo : AST.constructorId -> symbols -> (string * string) option
val constructorTag : AST.constructorId -> symbols -> int option

val tryFindConstructorId :
  string -> string -> symbols -> AST.constructorId option

val fieldInfo : AST.fieldId -> symbols -> (string * string) option
val fieldIndex : AST.fieldId -> symbols -> int option
val semanticMetadata : symbols -> semanticMetadata
val tryFindFieldId : string -> string -> symbols -> AST.fieldId option
val programSymbols : program -> symbols
val programTopLevels : program -> topLevel list
val withProgramTopLevels : topLevel list -> program -> program
val catalogForCheckedUnit : symbols -> symbols
val bindingCursor : symbols -> int
val includeBindingCursor : int -> symbols -> symbols

val composeTopLevels :
  symbols -> symbols -> topLevel list -> symbols * topLevel list

val composeDeclaredTopLevels :
  symbols -> symbols -> topLevel list -> symbols * topLevel list

val valueDefName : valueDef -> string
val valueDefId : valueDef -> AST.bindingId
val valueDefBody : valueDef -> expr
val programValues : program -> (AST.semanticType * expr) StringOrder.Map.t
val letPatternBindings : letPattern -> AST.bindingId list
val patternBindings : pattern -> AST.bindingId list
val recursiveBindingName : recursiveMember -> string
val recursiveBindingId : recursiveMember -> AST.bindingId
val recursiveBindingAvailability : recursiveMember -> AST.recursiveAvailability
val normalizeInferenceType : AST.semanticType -> AST.semanticType
val checkedType : AST.semanticType -> checkedType
val checkedTypeDef : AST.typeDef -> checkedTypeDef
val checkedRecursiveMember : AST.typedRecursiveMember -> recursiveMember
val checkedTypeArgs : AST.semanticType list -> checkedType list

val checkedParams :
  (AST.bindingId * AST.semanticType) NonEmptyList.t ->
  (AST.bindingId * checkedType) NonEmptyList.t

val ofTypedFunction :
  (string * string list * int * AST.semanticType list) StringOrder.Map.t ->
  (string -> int option) ->
  symbols ->
  AST.functionDef ->
  (functionDef * symbols, string) result

val ofTypedProgram :
  (string * string list * int * AST.semanticType list) StringOrder.Map.t ->
  StringOrder.Set.t ->
  typeCatalog ->
  functionCatalog ->
  (string -> int option) ->
  AST.program ->
  (program, string) result

val map2 :
  ('a -> 'b -> 'c) ->
  ('a, 'error) result ->
  ('b, 'error) result ->
  ('c, 'error) result
