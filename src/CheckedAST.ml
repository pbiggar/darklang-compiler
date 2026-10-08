(*
   CheckedAST.ml - Phase-safe syntax accepted by compiler preparation and ANF lowering.
   The parser/checker implementation still uses AST internally while resolving
   and inferring source syntax.  Successful checking crosses this boundary once;
   downstream passes cannot represent missing lambda types, unresolved nominal
   references, unchecked values, or partially resolved recursion metadata.
   Checked source tuple expressions always have at least two elements.
   A checked record literal contains each declaration slot exactly once.
   The list retains source evaluation order; layout order is selected only
   after every initializer has been evaluated.
*)
(* CheckedAST.ml - Phase-safe checked syntax and immutable semantic declaration catalogs. *)
[@@@warning "-30"]

module BindingIdMap = Map.Make (struct
  type t = AST.bindingId

  let compare = AST.compareBindingId
end)

module TypeIdMap = Map.Make (struct
  type t = AST.typeId

  let compare = AST.compareTypeId
end)

module ConstructorIdMap = Map.Make (struct
  type t = AST.constructorId

  let compare = AST.compareConstructorId
end)

module FieldIdMap = Map.Make (struct
  type t = AST.fieldId

  let compare = AST.compareFieldId
end)

module NamePairMap = Map.Make (struct
  type t = string * string

  let compare (left, leftName) (right, rightName) =
    let first = StringOrder.compare left right in
    if first <> 0 then first else StringOrder.compare leftName rightName
end)

type letPattern =
  | LPUnit
  | LPWildcard
  | LPVariable of AST.bindingId
  | LPTuple of letPattern * letPattern * letPattern list

type 'a tupleElements = { first : 'a; second : 'a; rest : 'a list }

(*
   Checked type fields cannot retain call-local inference identities. Nominal
   and privileged internal types remain available where those types carry
   specialization, layout, or runtime signature information.
*)
type checkedType = CheckedType of AST.semanticType

(*
   Checked declarations carry certified types while retaining their source
   declaration order and nominal names for downstream layout registries.
*)
type checkedTypeDef = CheckedTypeDef of checkedType AST.typeDefNode

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

type 'a recordFields = RecordFields of (AST.fieldId * 'a) list

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
  | GenericFuncRef of AST.functionId * checkedType list * checkedType
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

(*
   Stable IDs inherited by the next checked unit; only names touched by that
   unit are copied into its own Symbols table during conversion.
*)
type typeCatalog = {
  names : string TypeIdMap.t;
  ids : AST.typeId StringOrder.Map.t;
  nextOrdinal : int;
}

type functionCatalog = {
  names : string FunctionIdMap.t;
  ids : AST.functionId StringOrder.Map.t;
  nextOrdinal : int64;
  ordinals : AST.functionId list;
}

(*
   Immutable declaration catalog shared by independently checked units.
   Checked bodies own their lexical BindingIds; this catalog contains only
   cross-unit declarations and an allocation cursor used while constructing a
   new body.
*)
type globalCatalog = {
  bindingNames : string BindingIdMap.t;
  valueIds : AST.bindingId StringOrder.Map.t;
  nextBindingOrdinal : int;
  functionNames : string FunctionIdMap.t;
  functionIds : AST.functionId StringOrder.Map.t;
  functionOrdinals : AST.functionId list;
  nextFunctionOrdinal : int64;
  typeNames : string TypeIdMap.t;
  typeIds : AST.typeId StringOrder.Map.t;
  baseTypes : typeCatalog;
  nextTypeOrdinal : int;
  constructorNames : (string * string) ConstructorIdMap.t;
  constructorIds : AST.constructorId NamePairMap.t;
  constructorLookups :
    (string * string list * int * AST.semanticType list) StringOrder.Map.t list;
  fieldNames : (string * string) FieldIdMap.t;
  fieldIds : AST.fieldId NamePairMap.t;
}

type symbols = globalCatalog

(*
   Read-only view for passes; constructing a checked program is confined to
   successful checker conversion and trusted compiler-internal transformations.
*)
type program = CheckedProgram of symbols * topLevel list

let tupleElementsToList tuple = tuple.first :: tuple.second :: tuple.rest

let tupleElementsFromList = function
  | first :: second :: rest -> Some { first; second; rest }
  | [] | [ _ ] -> None

let tupleElementsOfList elements =
  match tupleElementsFromList elements with
  | Some tuple -> tuple
  | None -> Crash.crash "checked tuple has fewer than two elements"

let mapTupleElements f tuple =
  {
    first = f tuple.first;
    second = f tuple.second;
    rest = List.map f tuple.rest;
  }

let semanticType (CheckedType typ) = typ
let semanticTypeArgs args = List.map semanticType args

let semanticTypeDef (CheckedTypeDef definition) : AST.typeDef =
  match definition with
  | AST.RecordDef (name, params, fields) ->
      AST.RecordDef
        ( name,
          params,
          List.map (fun (field, typ) -> (field, semanticType typ)) fields )
  | AST.SumTypeDef (name, params, variants) ->
      AST.SumTypeDef
        ( name,
          params,
          List.map
            (fun (variant : checkedType AST.variantNode) ->
              {
                AST.name = variant.AST.name;
                fields = List.map semanticType variant.AST.fields;
              })
            variants )
  | AST.TypeAlias (name, params, target) ->
      AST.TypeAlias (name, params, semanticType target)

let recursiveMemberType (memberInfo : recursiveMember) =
  semanticType memberInfo.monomorphicType

let semanticRecursiveMember (memberInfo : recursiveMember) :
    AST.typedRecursiveMember =
  {
    AST.resolved = memberInfo.resolved;
    monomorphicType = recursiveMemberType memberInfo;
  }

let recordFieldsInSourceOrder (RecordFields fields) = fields

let mapRecordFields f (RecordFields fields) =
  RecordFields (List.map (fun (field, value) -> (field, f value)) fields)

let traverseRecordFields f (RecordFields fields) =
  Result.map
    (fun fields -> RecordFields fields)
    (ResultList.traverse
       (fun (field, value) ->
         Result.map (fun value -> (field, value)) (f value))
       fields)

let mapFold f state items =
  let reversed, final =
    List.fold_left
      (fun (mapped, state) item ->
        let item, state = f state item in
        (item :: mapped, state))
      ([], state) items
  in
  (List.rev reversed, final)

let mapFoldRecordFields f state (RecordFields fields) =
  let mapped, final =
    mapFold
      (fun current (field, value) ->
        let value, next = f current value in
        ((field, value), next))
      state fields
  in
  (RecordFields mapped, final)

let traverseStateRecordFields f state (RecordFields fields) =
  List.fold_left
    (fun result (field, value) ->
      Result.bind result (fun (reversed, current) ->
          Result.map
            (fun (value, next) -> ((field, value) :: reversed, next))
            (f value current)))
    (Ok ([], state))
    fields
  |> Result.map (fun (reversed, final) ->
      (RecordFields (List.rev reversed), final))

module Indices = Set.Make (Int)

let completeRecordFields owner fieldCount fields =
  let valid =
    if fieldCount <= 64 then
      let count, _, valid =
        List.fold_left
          (fun (count, seen, valid) (field, _) ->
            let index = AST.fieldRuntimeIndex field in
            let bit =
              if index >= 0 && index < fieldCount then Int64.shift_left 1L index
              else 0L
            in
            ( count + 1,
              Int64.logor seen bit,
              valid
              && AST.fieldIdOwner field = owner
              && bit <> 0L
              && Int64.logand seen bit = 0L ))
          (0, 0L, true) fields
      in
      valid && count = fieldCount
    else
      let count, indices, valid =
        List.fold_left
          (fun (count, indices, valid) (field, _) ->
            let index = AST.fieldRuntimeIndex field in
            ( count + 1,
              Indices.add index indices,
              valid
              && AST.fieldIdOwner field = owner
              && index >= 0 && index < fieldCount ))
          (0, Indices.empty, true) fields
      in
      valid && count = fieldCount && Indices.cardinal indices = fieldCount
  in
  if valid then Ok (RecordFields fields)
  else Error "record literal does not contain exactly the declared field slots"

let functionParameterTypes (definition : functionDef) =
  NonEmptyList.map (fun (id, typ) -> (id, semanticType typ)) definition.params

let functionReturnType (definition : functionDef) =
  semanticType definition.returnType

let emptyTypeCatalog : typeCatalog =
  { names = TypeIdMap.empty; ids = StringOrder.Map.empty; nextOrdinal = 0 }

let compareFunctionId left right =
  Int64.unsigned_compare (AST.functionIdValue left) (AST.functionIdValue right)

(*
   Descending identity index: a checked unit's allocation suffix shares the
   unchanged catalog tail. Range queries stop at the inherited cursor.
*)
let mergeFunctionOrdinals left right =
  let rec merge prefix left right =
    match (left, right) with
    | [], rest | rest, [] ->
        List.fold_left (fun tail id -> id :: tail) rest prefix
    | leftId :: leftRest, rightId :: rightRest ->
        if leftId = rightId then merge (leftId :: prefix) leftRest rightRest
        else if compareFunctionId leftId rightId > 0 then
          merge (leftId :: prefix) leftRest right
        else merge (rightId :: prefix) left rightRest
  in
  merge [] left right

let emptyFunctionCatalog : functionCatalog =
  {
    names =
      FunctionIdMap.ofList
        [
          (AST.functionId 0L, "_start");
          (AST.functionId 1L, "__dark_compiler_program_entry");
        ];
    ids =
      StringOrder.Map.of_list
        [
          ("_start", AST.functionId 0L);
          ("__dark_compiler_program_entry", AST.functionId 1L);
        ];
    ordinals = [ AST.functionId 1L; AST.functionId 0L ];
    nextOrdinal = 2L;
  }

let includeFunctionNames names (catalog : functionCatalog) =
  let names = StringOrder.Set.of_seq names |> StringOrder.Set.elements in
  List.fold_left
    (fun (catalog : functionCatalog) name ->
      if StringOrder.Map.mem name catalog.ids then catalog
      else
        let id = AST.functionId catalog.nextOrdinal in
        {
          names = FunctionIdMap.add id name catalog.names;
          ordinals = id :: catalog.ordinals;
          ids = StringOrder.Map.add name id catalog.ids;
          nextOrdinal = AST.nextFunctionIdOrdinal catalog.nextOrdinal;
        })
    catalog names

let viewProgram (CheckedProgram (symbols, topLevels)) = (symbols, topLevels)

let programFromCheckedParts (symbols, topLevels) =
  CheckedProgram (symbols, topLevels)

let emptySymbolsWithCatalogs (baseTypes : typeCatalog)
    (baseFunctions : functionCatalog) : symbols =
  let initial =
    {
      bindingNames = BindingIdMap.empty;
      valueIds = StringOrder.Map.empty;
      nextBindingOrdinal = -1;
      functionOrdinals = baseFunctions.ordinals;
      functionNames = baseFunctions.names;
      functionIds = baseFunctions.ids;
      nextFunctionOrdinal = baseFunctions.nextOrdinal;
      typeNames = TypeIdMap.empty;
      typeIds = StringOrder.Map.empty;
      baseTypes;
      nextTypeOrdinal = baseTypes.nextOrdinal;
      constructorNames = ConstructorIdMap.empty;
      constructorIds = NamePairMap.empty;
      constructorLookups = [];
      fieldNames = FieldIdMap.empty;
      fieldIds = NamePairMap.empty;
    }
  in
  List.fold_left
    (fun (symbols : symbols) name ->
      if StringOrder.Map.mem name symbols.functionIds then symbols
      else
        let id = AST.functionId symbols.nextFunctionOrdinal in
        {
          symbols with
          functionOrdinals = id :: symbols.functionOrdinals;
          functionNames = FunctionIdMap.add id name symbols.functionNames;
          functionIds = StringOrder.Map.add name id symbols.functionIds;
          nextFunctionOrdinal =
            AST.nextFunctionIdOrdinal symbols.nextFunctionOrdinal;
        })
    initial
    [ "_start"; "__dark_compiler_program_entry" ]

let emptySymbols () =
  emptySymbolsWithCatalogs emptyTypeCatalog emptyFunctionCatalog

let registerBinding id name (symbols : symbols) =
  { symbols with bindingNames = BindingIdMap.add id name symbols.bindingNames }

let int32Add value amount =
  Int32.to_int (Int32.add (Int32.of_int value) (Int32.of_int amount))

let allocateBinding name (symbols : symbols) =
  let id = AST.namedBindingId symbols.nextBindingOrdinal name in
  ( id,
    {
      symbols with
      bindingNames = BindingIdMap.add id name symbols.bindingNames;
      nextBindingOrdinal = int32Add symbols.nextBindingOrdinal (-1);
    } )

let nextBindingOrdinal (symbols : symbols) = symbols.nextBindingOrdinal

let internValue name (symbols : symbols) =
  match StringOrder.Map.find_opt name symbols.valueIds with
  | Some id -> (id, symbols)
  | None ->
      let id = AST.topLevelValueId name in
      ( id,
        { symbols with valueIds = StringOrder.Map.add name id symbols.valueIds }
      )

let bindingName id (_symbols : symbols) = AST.bindingDisplayName id

let tryFindValueId name (symbols : symbols) =
  StringOrder.Map.find_opt name symbols.valueIds

let internFunction name (symbols : symbols) =
  match StringOrder.Map.find_opt name symbols.functionIds with
  | Some id -> (id, symbols)
  | None ->
      let id = AST.functionId symbols.nextFunctionOrdinal in
      ( id,
        {
          symbols with
          functionOrdinals = id :: symbols.functionOrdinals;
          functionIds = StringOrder.Map.add name id symbols.functionIds;
          functionNames = FunctionIdMap.add id name symbols.functionNames;
          nextFunctionOrdinal =
            AST.nextFunctionIdOrdinal symbols.nextFunctionOrdinal;
        } )

let unsigned value =
  if value < 0L then
    Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64))
  else Int64.to_string value

let unsignedMax left right =
  if Int64.unsigned_compare left right >= 0 then left else right

let registerGeneratedFunction name id (symbols : symbols) =
  match
    ( StringOrder.Map.find_opt name symbols.functionIds,
      FunctionIdMap.tryFind id symbols.functionNames )
  with
  | Some existing, _ when existing <> id ->
      Crash.crash "Generated function name has a different allocated identity"
  | _, Some existing when existing <> name ->
      Crash.crash "Generated function identity belongs to another name"
  | _ ->
      {
        symbols with
        functionOrdinals =
          (if FunctionIdMap.containsKey id symbols.functionNames then
             symbols.functionOrdinals
           else mergeFunctionOrdinals [ id ] symbols.functionOrdinals);
        functionIds = StringOrder.Map.add name id symbols.functionIds;
        functionNames = FunctionIdMap.add id name symbols.functionNames;
        nextFunctionOrdinal =
          unsignedMax symbols.nextFunctionOrdinal
            (AST.nextFunctionIdOrdinal (AST.functionIdValue id));
      }

(*
   Names allocated at or after an inherited catalog cursor, including helpers
   referenced only by checked bodies. Only the new identity prefix is visited.
*)
let allocatedFunctionNamesSince ordinal (symbols : symbols) =
  let rec takeWhile = function
    | [] -> []
    | id :: rest ->
        if Int64.unsigned_compare (AST.functionIdValue id) ordinal >= 0 then
          id :: takeWhile rest
        else []
  in
  takeWhile symbols.functionOrdinals
  |> List.map (fun id ->
      match FunctionIdMap.tryFind id symbols.functionNames with
      | Some name -> (id, name)
      | None -> Crash.crash "Function allocation index lost its catalog entry")

let includeAllocatedFunctionNames (allocated : symbols) (target : symbols) =
  let included =
    List.fold_left
      (fun symbols (id, name) -> registerGeneratedFunction name id symbols)
      target
      (allocatedFunctionNamesSince target.nextFunctionOrdinal allocated)
  in
  {
    included with
    nextFunctionOrdinal =
      unsignedMax target.nextFunctionOrdinal allocated.nextFunctionOrdinal;
  }

let internType name (symbols : symbols) =
  match StringOrder.Map.find_opt name symbols.typeIds with
  | Some id -> (id, symbols)
  | None ->
      let id, nextOrdinal =
        match StringOrder.Map.find_opt name symbols.baseTypes.ids with
        | Some id -> (id, symbols.nextTypeOrdinal)
        | None ->
            ( AST.typeId symbols.nextTypeOrdinal,
              int32Add symbols.nextTypeOrdinal 1 )
      in
      ( id,
        {
          symbols with
          typeIds = StringOrder.Map.add name id symbols.typeIds;
          typeNames = TypeIdMap.add id name symbols.typeNames;
          nextTypeOrdinal = nextOrdinal;
        } )

let internConstructor typeName name tag (symbols : symbols) =
  let _, symbols = internType typeName symbols in
  match NamePairMap.find_opt (typeName, name) symbols.constructorIds with
  | Some id -> (id, symbols)
  | None ->
      let owner =
        match StringOrder.Map.find_opt typeName symbols.typeIds with
        | Some id -> id
        | None -> Crash.crash ("Constructor owner '" ^ typeName ^ "' is absent")
      in
      let id = AST.constructorId owner name tag in
      ( id,
        {
          symbols with
          constructorIds =
            NamePairMap.add (typeName, name) id symbols.constructorIds;
          constructorNames =
            ConstructorIdMap.add id (typeName, name) symbols.constructorNames;
        } )

let internField typeName name index (symbols : symbols) =
  let _, symbols = internType typeName symbols in
  match NamePairMap.find_opt (typeName, name) symbols.fieldIds with
  | Some id -> (id, symbols)
  | None ->
      let owner =
        match StringOrder.Map.find_opt typeName symbols.typeIds with
        | Some id -> id
        | None -> Crash.crash ("Field owner '" ^ typeName ^ "' is absent")
      in
      let id = AST.fieldId owner index in
      ( id,
        {
          symbols with
          fieldIds = NamePairMap.add (typeName, name) id symbols.fieldIds;
          fieldNames = FieldIdMap.add id (typeName, name) symbols.fieldNames;
        } )

let functionName id (symbols : symbols) =
  FunctionIdMap.tryFind id symbols.functionNames

let functionNames (symbols : symbols) = symbols.functionNames
let functionIds (symbols : symbols) = symbols.functionIds
let nextFunctionOrdinal (symbols : symbols) = symbols.nextFunctionOrdinal

let functionCatalog (symbols : symbols) : functionCatalog =
  {
    names = symbols.functionNames;
    ids = symbols.functionIds;
    ordinals = symbols.functionOrdinals;
    nextOrdinal = symbols.nextFunctionOrdinal;
  }

let tryFindFunctionId name (symbols : symbols) =
  StringOrder.Map.find_opt name symbols.functionIds

let typeName id (symbols : symbols) =
  match TypeIdMap.find_opt id symbols.typeNames with
  | Some name -> Some name
  | None -> TypeIdMap.find_opt id symbols.baseTypes.names

let typeNames (symbols : symbols) = symbols.typeNames

let tryFindTypeId name (symbols : symbols) =
  match StringOrder.Map.find_opt name symbols.typeIds with
  | Some id -> Some id
  | None -> StringOrder.Map.find_opt name symbols.baseTypes.ids

let typeCatalog (symbols : symbols) : typeCatalog =
  {
    names =
      TypeIdMap.fold TypeIdMap.add symbols.typeNames symbols.baseTypes.names;
    ids =
      StringOrder.Map.fold StringOrder.Map.add symbols.typeIds
        symbols.baseTypes.ids;
    nextOrdinal = symbols.nextTypeOrdinal;
  }

let constructorInfo id (symbols : symbols) =
  ConstructorIdMap.find_opt id symbols.constructorNames

let constructorTag id (_symbols : symbols) = Some (AST.constructorRuntimeTag id)

let tryFindConstructorId typeName name (symbols : symbols) =
  match NamePairMap.find_opt (typeName, name) symbols.constructorIds with
  | Some id -> Some id
  | None ->
      List.find_map
        (fun lookup ->
          Option.bind
            (StringOrder.Map.find_opt (typeName ^ "." ^ name) lookup)
            (fun (owner, _, tag, _) ->
              if owner = typeName then
                Option.map
                  (fun ownerId -> AST.constructorId ownerId name tag)
                  (tryFindTypeId owner symbols)
              else None))
        symbols.constructorLookups

let fieldInfo id (symbols : symbols) = FieldIdMap.find_opt id symbols.fieldNames
let fieldIndex id (_symbols : symbols) = Some (AST.fieldRuntimeIndex id)

let semanticMetadata (symbols : symbols) : semanticMetadata =
  { typeNames = symbols.typeNames }

let tryFindFieldId typeName name (symbols : symbols) =
  NamePairMap.find_opt (typeName, name) symbols.fieldIds

let programSymbols (CheckedProgram (symbols, _)) = symbols
let programTopLevels (CheckedProgram (_, topLevels)) = topLevels

let withProgramTopLevels topLevels (CheckedProgram (symbols, _)) =
  CheckedProgram (symbols, topLevels)

(*
   Keep the function catalog for checked bodies that refer to functions beyond
   their top-level declarations. Lexical binding names stay local to each body.
*)
let catalogForCheckedUnit (symbols : symbols) =
  {
    (emptySymbols ()) with
    nextBindingOrdinal = symbols.nextBindingOrdinal;
    functionOrdinals = symbols.functionOrdinals;
    functionNames = symbols.functionNames;
    functionIds = symbols.functionIds;
    nextFunctionOrdinal = symbols.nextFunctionOrdinal;
  }

let bindingCursor (symbols : symbols) = symbols.nextBindingOrdinal

let includeBindingCursor cursor (symbols : symbols) =
  { symbols with nextBindingOrdinal = min cursor symbols.nextBindingOrdinal }

(*
   Compose checked units that share an allocation catalog. Lexical BindingIds
   remain body-local, so checked expressions do not need rewriting.
*)
let composeTopLevels (sourceCatalog : symbols) (targetCatalog : symbols)
    topLevels =
  let mergeTypeName id name names =
    match TypeIdMap.find_opt id names with
    | Some existing when existing = name -> names
    | Some existing when existing <> name ->
        Crash.crash
          "Composed type catalogs assign one TypeId to different names"
    | None | Some _ -> TypeIdMap.add id name names
  in
  let mergeTypeId name id ids =
    match StringOrder.Map.find_opt name ids with
    | Some existing when existing = id -> ids
    | Some existing when existing <> id ->
        Crash.crash
          "Composed type catalogs assign different TypeIds to one name"
    | None | Some _ -> StringOrder.Map.add name id ids
  in
  let mergeFunctionName names id name =
    match FunctionIdMap.tryFind id names with
    | Some existing when existing = name -> names
    | Some existing when existing <> name ->
        Crash.crash
          ("Composed function catalogs assign FunctionId "
          ^ unsigned (AST.functionIdValue id)
          ^ " to both '" ^ existing ^ "' and '" ^ name ^ "'")
    | None | Some _ -> FunctionIdMap.add id name names
  in
  let mergeFunctionId name id ids =
    match StringOrder.Map.find_opt name ids with
    | Some existing when existing = id -> ids
    | Some existing when existing <> id ->
        Crash.crash
          ("Composed function catalogs assign '" ^ name ^ "' both FunctionIds "
          ^ unsigned (AST.functionIdValue existing)
          ^ " and "
          ^ unsigned (AST.functionIdValue id))
    | None | Some _ -> StringOrder.Map.add name id ids
  in
  let catalog : symbols =
    {
      bindingNames =
        BindingIdMap.fold
          (fun key value combined ->
            match BindingIdMap.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> BindingIdMap.add key value combined)
          sourceCatalog.bindingNames targetCatalog.bindingNames;
      valueIds =
        StringOrder.Map.fold
          (fun key value combined ->
            match StringOrder.Map.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> StringOrder.Map.add key value combined)
          sourceCatalog.valueIds targetCatalog.valueIds;
      nextBindingOrdinal =
        min sourceCatalog.nextBindingOrdinal targetCatalog.nextBindingOrdinal;
      functionOrdinals =
        mergeFunctionOrdinals
          (List.filter
             (fun id ->
               not (FunctionIdMap.containsKey id targetCatalog.functionNames))
             sourceCatalog.functionOrdinals)
          targetCatalog.functionOrdinals;
      functionNames =
        FunctionIdMap.fold mergeFunctionName targetCatalog.functionNames
          sourceCatalog.functionNames;
      functionIds =
        StringOrder.Map.fold mergeFunctionId sourceCatalog.functionIds
          targetCatalog.functionIds;
      nextFunctionOrdinal =
        unsignedMax sourceCatalog.nextFunctionOrdinal
          targetCatalog.nextFunctionOrdinal;
      typeNames =
        TypeIdMap.fold mergeTypeName sourceCatalog.typeNames
          targetCatalog.typeNames;
      typeIds =
        StringOrder.Map.fold mergeTypeId sourceCatalog.typeIds
          targetCatalog.typeIds;
      baseTypes = targetCatalog.baseTypes;
      nextTypeOrdinal =
        max sourceCatalog.nextTypeOrdinal targetCatalog.nextTypeOrdinal;
      constructorNames =
        ConstructorIdMap.fold
          (fun key value combined ->
            match ConstructorIdMap.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> ConstructorIdMap.add key value combined)
          sourceCatalog.constructorNames targetCatalog.constructorNames;
      constructorIds =
        NamePairMap.fold
          (fun key value combined ->
            match NamePairMap.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> NamePairMap.add key value combined)
          sourceCatalog.constructorIds targetCatalog.constructorIds;
      constructorLookups =
        sourceCatalog.constructorLookups @ targetCatalog.constructorLookups;
      fieldNames =
        FieldIdMap.fold
          (fun key value combined ->
            match FieldIdMap.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> FieldIdMap.add key value combined)
          sourceCatalog.fieldNames targetCatalog.fieldNames;
      fieldIds =
        NamePairMap.fold
          (fun key value combined ->
            match NamePairMap.find_opt key combined with
            | Some existing when existing = value -> combined
            | None | Some _ -> NamePairMap.add key value combined)
          sourceCatalog.fieldIds targetCatalog.fieldIds;
    }
  in
  (catalog, topLevels)

(*
   Import only declarations introduced by a checked unit. References in its
   bodies carry canonical IDs, while the target already owns external names.
   The checked unit inherited the target's earlier function catalog. Its
   new names include helpers referenced only from bodies, so the delta
   cannot be reconstructed from top-level definitions alone.
*)
let composeDeclaredTopLevels (sourceCatalog : symbols) (targetCatalog : symbols)
    topLevels =
  let newFunctionNames, newFunctionIds, newFunctionOrdinals =
    if
      Int64.unsigned_compare sourceCatalog.nextFunctionOrdinal
        targetCatalog.nextFunctionOrdinal
      < 0
    then
      ( sourceCatalog.functionNames,
        sourceCatalog.functionIds,
        sourceCatalog.functionOrdinals )
    else
      let allocated =
        allocatedFunctionNamesSince targetCatalog.nextFunctionOrdinal
          sourceCatalog
      in
      ( FunctionIdMap.ofList allocated,
        StringOrder.Map.of_list
          (List.map (fun (id, name) -> (name, id)) allocated),
        List.map fst allocated )
  in
  let initial =
    {
      (catalogForCheckedUnit sourceCatalog) with
      functionNames = newFunctionNames;
      functionIds = newFunctionIds;
      functionOrdinals = newFunctionOrdinals;
      nextTypeOrdinal = sourceCatalog.nextTypeOrdinal;
      constructorLookups = sourceCatalog.constructorLookups;
    }
  in
  let declarations =
    List.fold_left
      (fun (symbols : symbols) (topLevel : topLevel) ->
        match topLevel with
        | FunctionDef definition ->
            {
              symbols with
              functionOrdinals =
                (if
                   FunctionIdMap.containsKey definition.id symbols.functionNames
                 then symbols.functionOrdinals
                 else
                   mergeFunctionOrdinals [ definition.id ]
                     symbols.functionOrdinals);
              functionNames =
                FunctionIdMap.add definition.id definition.name
                  symbols.functionNames;
              functionIds =
                StringOrder.Map.add definition.name definition.id
                  symbols.functionIds;
            }
        | ValueDef definition ->
            {
              symbols with
              bindingNames =
                BindingIdMap.add definition.id definition.name
                  symbols.bindingNames;
              valueIds =
                StringOrder.Map.add definition.name definition.id
                  symbols.valueIds;
            }
        | TypeDef (id, checkedDefinition) -> (
            let definition = semanticTypeDef checkedDefinition in
            let name =
              match definition with
              | AST.RecordDef (name, _, _)
              | AST.SumTypeDef (name, _, _)
              | AST.TypeAlias (name, _, _) ->
                  name
            in
            let symbols =
              {
                symbols with
                typeNames = TypeIdMap.add id name symbols.typeNames;
                typeIds = StringOrder.Map.add name id symbols.typeIds;
              }
            in
            match definition with
            | AST.RecordDef (_, _, fields) ->
                List.fold_left
                  (fun (symbols : symbols) (fieldName, _) ->
                    match
                      NamePairMap.find_opt (name, fieldName)
                        sourceCatalog.fieldIds
                    with
                    | None -> symbols
                    | Some fieldId ->
                        {
                          symbols with
                          fieldNames =
                            FieldIdMap.add fieldId (name, fieldName)
                              symbols.fieldNames;
                          fieldIds =
                            NamePairMap.add (name, fieldName) fieldId
                              symbols.fieldIds;
                        })
                  symbols fields
            | AST.SumTypeDef (_, _, variants) ->
                List.fold_left
                  (fun (symbols : symbols) (variant : AST.variant) ->
                    match
                      NamePairMap.find_opt (name, variant.AST.name)
                        sourceCatalog.constructorIds
                    with
                    | None -> symbols
                    | Some constructorId ->
                        {
                          symbols with
                          constructorNames =
                            ConstructorIdMap.add constructorId
                              (name, variant.AST.name) symbols.constructorNames;
                          constructorIds =
                            NamePairMap.add (name, variant.AST.name)
                              constructorId symbols.constructorIds;
                        })
                  symbols variants
            | AST.TypeAlias _ -> symbols)
        | Expression _ -> symbols)
      initial topLevels
  in
  composeTopLevels declarations targetCatalog topLevels

let valueDefName (value : valueDef) = value.name
let valueDefId (value : valueDef) = value.id
let valueDefBody (value : valueDef) = value.body

let programValues (CheckedProgram (_, topLevels)) =
  List.filter_map
    (fun (topLevel : topLevel) ->
      match topLevel with
      | ValueDef value -> Some (value.name, (semanticType value.typ, value.body))
      | FunctionDef _ | TypeDef _ | Expression _ -> None)
    topLevels
  |> StringOrder.Map.of_list

let rec letPatternBindings (pattern : letPattern) =
  match pattern with
  | LPVariable id -> [ id ]
  | LPTuple (first, second, rest) ->
      List.concat_map letPatternBindings (first :: second :: rest)
  | LPUnit | LPWildcard -> []

let rec patternBindings = function
  | PVariable id -> [ id ]
  | PConstructor (_, fields) | PTuple fields | PList fields ->
      List.concat_map patternBindings fields
  | PListCons (heads, tail) ->
      List.concat_map patternBindings heads @ patternBindings tail
  | POr alternatives -> patternBindings (NonEmptyList.head alternatives)
  | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
  | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
  | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _
  | PString _ | PChar _ | PFloat _ ->
      []

let recursiveBindingName (memberInfo : recursiveMember) =
  memberInfo.resolved.AST.parsed.AST.sourceName

let recursiveBindingId (memberInfo : recursiveMember) =
  memberInfo.resolved.AST.parsed.AST.binding

let recursiveBindingAvailability (memberInfo : recursiveMember) =
  memberInfo.resolved.AST.availability

let conversionError location detail =
  Error ("Checked AST construction failed at " ^ location ^ ": " ^ detail)

let map2 f first second =
  Result.bind first (fun first ->
      Result.map (fun second -> f first second) second)

(*
   Inference identities are meaningful only while checking a call. Erase them
   as types cross the checked-program boundary; downstream specialization
   needs stable, alpha-equivalent names for still-open generic arguments.
*)
let rec normalizeInferenceType = function
  | AST.TInferenceVar (display, _) -> AST.TVar display
  | AST.TFunction (params, ret) ->
      AST.TFunction
        (List.map normalizeInferenceType params, normalizeInferenceType ret)
  | AST.TTuple elems -> AST.TTuple (List.map normalizeInferenceType elems)
  | AST.TRecord (name, args) ->
      AST.TRecord (name, List.map normalizeInferenceType args)
  | AST.TSum (name, args) ->
      AST.TSum (name, List.map normalizeInferenceType args)
  | AST.TList elem -> AST.TList (normalizeInferenceType elem)
  | AST.TStream elem -> AST.TStream (normalizeInferenceType elem)
  | AST.TDict (key, value) ->
      AST.TDict (normalizeInferenceType key, normalizeInferenceType value)
  | ( AST.TVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TInt128 | AST.TInt | AST.TUInt8 | AST.TUInt16 | AST.TUInt32
    | AST.TUInt64 | AST.TUInt128 | AST.TBool | AST.TFloat64 | AST.TString
    | AST.TBlob | AST.TChar | AST.TDateTime | AST.TUnit | AST.TNever
    | AST.TInternalRawPtr ) as typ ->
      typ

let checkedType typ = CheckedType (normalizeInferenceType typ)

let checkedTypeDef (definition : AST.typeDef) =
  CheckedTypeDef
    (match definition with
    | AST.RecordDef (name, params, fields) ->
        AST.RecordDef
          ( name,
            params,
            List.map (fun (field, typ) -> (field, checkedType typ)) fields )
    | AST.SumTypeDef (name, params, variants) ->
        AST.SumTypeDef
          ( name,
            params,
            List.map
              (fun (variant : AST.variant) ->
                {
                  AST.name = variant.AST.name;
                  fields = List.map checkedType variant.AST.fields;
                })
              variants )
    | AST.TypeAlias (name, params, target) ->
        AST.TypeAlias (name, params, checkedType target))

let checkedRecursiveMember (memberInfo : AST.typedRecursiveMember) :
    recursiveMember =
  {
    resolved = memberInfo.AST.resolved;
    monomorphicType = checkedType memberInfo.AST.monomorphicType;
  }

let checkedTypeArgs args = List.map checkedType args

let checkedParams parameters =
  NonEmptyList.map (fun (id, typ) -> (id, checkedType typ)) parameters

let convertRecordReference (reference : AST.recordReference) symbols :
    recordReference * symbols =
  let typeId, symbols = internType reference.AST.resolvedTypeName symbols in
  ({ typeId; typeArgs = checkedTypeArgs reference.AST.typeArgs }, symbols)

let convertConstructorReference location reference variantName symbols =
  match reference with
  | AST.ResolvedConstructor (_, _, typeArgs) -> (
      match AST.constructorReferenceTypeName reference with
      | Some typeName -> (
          match tryFindConstructorId typeName variantName symbols with
          | Some id ->
              Ok
                {
                  typeId = AST.constructorIdOwner id;
                  constructorId = id;
                  typeArgs = checkedTypeArgs typeArgs;
                }
          | None ->
              conversionError location
                "resolved constructor has no semantic identity")
      | None ->
          conversionError location "resolved constructor has no declaring type")
  | AST.UnresolvedConstructor _ ->
      conversionError location "constructor reference was not resolved"

let extendEnvironment bindings environment =
  List.fold_left
    (fun environment (name, id) -> StringOrder.Map.add name id environment)
    environment bindings

let rec allocateLetPattern symbols pattern :
    letPattern * (string * AST.bindingId) list * symbols =
  match pattern with
  | AST.LPUnit -> (LPUnit, [], symbols)
  | AST.LPWildcard -> (LPWildcard, [], symbols)
  | AST.LPVariable name ->
      let id, symbols = allocateBinding name symbols in
      (LPVariable id, [ (name, id) ], symbols)
  | AST.LPTuple (first, second, rest) ->
      let first, firstBindings, afterFirst = allocateLetPattern symbols first in
      let second, secondBindings, afterSecond =
        allocateLetPattern afterFirst second
      in
      let reversed, restBindings, following =
        List.fold_left
          (fun (converted, bindings, symbols) item ->
            let item, itemBindings, next = allocateLetPattern symbols item in
            (item :: converted, bindings @ itemBindings, next))
          ([], [], afterSecond) rest
      in
      ( LPTuple (first, second, List.rev reversed),
        firstBindings @ secondBindings @ restBindings,
        following )

let rec convertPattern bindingIds symbols pattern : pattern =
  let convert = convertPattern bindingIds symbols in
  match pattern with
  | AST.PUnit -> PUnit
  | AST.PWildcard -> PWildcard
  | AST.PVar name -> (
      match StringOrder.Map.find_opt name bindingIds with
      | Some id -> PVariable id
      | None -> PWildcard)
  | AST.PConstructor (name, _) ->
      Crash.crash
        ("Unresolved constructor pattern '" ^ name
       ^ "' crossed the checked boundary")
  | AST.PResolvedConstructor (typeName, name, _, fields) -> (
      match tryFindConstructorId typeName name symbols with
      | Some id -> PConstructor (id, List.map convert fields)
      | None ->
          Crash.crash "Resolved constructor pattern has no semantic identity")
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
  | AST.PListCons (heads, tail) ->
      PListCons (List.map convert heads, convert tail)
  | AST.POr alternatives -> POr (NonEmptyList.map convert alternatives)

let allocateMatchBindings symbols pattern =
  let rec allBindings = function
    | AST.PVar name -> [ name ]
    | AST.PConstructor (_, fields)
    | AST.PResolvedConstructor (_, _, _, fields)
    | AST.PTuple fields
    | AST.PList fields ->
        List.concat_map allBindings fields
    | AST.PListCons (heads, tail) ->
        List.concat_map allBindings heads @ allBindings tail
    | AST.POr alternatives -> allBindings (NonEmptyList.head alternatives)
    | AST.PUnit | AST.PWildcard | AST.PInt64 _ | AST.PBigInt _
    | AST.PInt128Literal _ | AST.PInt8Literal _ | AST.PInt16Literal _
    | AST.PInt32Literal _ | AST.PUInt8Literal _ | AST.PUInt16Literal _
    | AST.PUInt32Literal _ | AST.PUInt64Literal _ | AST.PUInt128Literal _
    | AST.PBool _ | AST.PString _ | AST.PChar _ | AST.PFloat _ ->
        []
  in
  match AST.validateBinders (AST.MatchBinderPattern pattern) with
  | Error detail -> conversionError "match pattern" detail
  | Ok _names ->
      let bindings, symbols =
        mapFold
          (fun symbols name ->
            let id, next = allocateBinding name symbols in
            ((name, id), next))
          symbols (allBindings pattern)
      in
      Ok (StringOrder.Map.of_list bindings, symbols)

let ( let* ) = Result.bind

(*
   Successful checking has already established that a
   remaining non-value variable denotes a function.
   Allocate its dense identity at this boundary just as
   direct Call and FuncRef nodes do.
*)
let rec convertExpr recordFieldCounts location environment symbols expr :
    (expr * symbols, string) result =
  let convert = convertExpr recordFieldCounts location environment in
  let convertList values currentSymbols =
    List.fold_left
      (fun result value ->
        let* converted, state = result in
        let* value, next = convert state value in
        Ok (value :: converted, next))
      (Ok ([], currentSymbols))
      values
    |> Result.map (fun (converted, state) -> (List.rev converted, state))
  in
  let convertNonEmpty values currentSymbols =
    Result.map
      (fun (converted, state) -> (NonEmptyList.fromList converted, state))
      (convertList (NonEmptyList.toList values) currentSymbols)
  in
  let convertPair first second currentSymbols =
    let* first, afterFirst = convert currentSymbols first in
    let* second, following = convert afterFirst second in
    Ok (first, second, following)
  in
  let convertFields fields currentSymbols =
    List.fold_left
      (fun result ((reference : AST.recordFieldReference), value) ->
        let* converted, state = result in
        match
          (reference.AST.resolvedTypeName, reference.AST.resolvedFieldIndex)
        with
        | Some typeName, Some fieldIndex ->
            let fieldId, state =
              internField typeName reference.AST.sourceFieldName fieldIndex
                state
            in
            let* value, next = convert state value in
            Ok ((fieldId, value) :: converted, next)
        | None, _ | _, None ->
            conversionError location
              "record field has no resolved owner and declaration slot")
      (Ok ([], currentSymbols))
      fields
    |> Result.map (fun (converted, state) -> (List.rev converted, state))
  in
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
      List.fold_left
        (fun result part ->
          let* converted, state = result in
          match part with
          | AST.StringText text -> Ok (StringText text :: converted, state)
          | AST.StringExpr inner ->
              let* inner, next = convert state inner in
              Ok (StringExpr inner :: converted, next))
        (Ok ([], symbols))
        parts
      |> Result.map (fun (converted, state) ->
          (InterpolatedString (List.rev converted), state))
  | AST.BinOp (op, left, right) ->
      Result.map
        (fun (left, right, state) -> (BinOp (op, left, right), state))
        (convertPair left right symbols)
  | AST.UnaryOp (op, inner) ->
      Result.map
        (fun (value, state) -> (UnaryOp (op, value), state))
        (convert symbols inner)
  | AST.Let (pattern, value, body) ->
      let* value, afterValue = convert symbols value in
      let pattern, bindings, afterPattern =
        allocateLetPattern afterValue pattern
      in
      let* body, following =
        convertExpr recordFieldCounts location
          (extendEnvironment bindings environment)
          afterPattern body
      in
      Ok (Let (pattern, value, body), following)
  | AST.RecursiveLet (recursion, value, body) -> (
      match recursion with
      | AST.TypedRecursiveBinding typed ->
          let name = typed.AST.resolved.AST.parsed.AST.sourceName
          and id = typed.AST.resolved.AST.parsed.AST.binding in
          let withSymbol = registerBinding id name symbols in
          let bodyEnvironment = StringOrder.Map.add name id environment in
          let valueEnvironment =
            match typed.AST.resolved.AST.availability with
            | AST.OrdinaryBinding -> environment
            | AST.SelfRecursiveMember | AST.MutualRecursiveMember
            | AST.CompletedGroupMember | AST.ImportedGroupMember ->
                bodyEnvironment
          in
          let* value, afterValue =
            convertExpr recordFieldCounts location valueEnvironment withSymbol
              value
          in
          let* body, following =
            convertExpr recordFieldCounts location bodyEnvironment afterValue
              body
          in
          Ok
            (RecursiveLet (checkedRecursiveMember typed, value, body), following)
      | AST.RecursiveBindingCandidate _ | AST.ParsedRecursiveBinding _
      | AST.ResolvedRecursiveBinding _ ->
          conversionError location
            "recursive let has no typed recursion evidence")
  | AST.Var name -> (
      if name = "Builtin.testNan" then
        Ok (FloatLiteral (Int64.float_of_bits 0xfff8000000000000L), symbols)
      else if name = "Builtin.testInfinity" then
        Ok (FloatLiteral infinity, symbols)
      else if name = "Builtin.blobEmpty" then Ok (BlobLiteral "", symbols)
      else
        match StringOrder.Map.find_opt name environment with
        | Some id -> Ok (Local id, symbols)
        | None -> (
            match tryFindValueId name symbols with
            | Some id -> Ok (Local id, symbols)
            | None -> (
                match tryFindFunctionId name symbols with
                | Some id -> Ok (FuncRef id, symbols)
                | None ->
                    let id, symbols = internFunction name symbols in
                    Ok (FuncRef id, symbols))))
  | AST.If (condition, yes, no) ->
      let* condition, afterCondition = convert symbols condition in
      let* yes, no, following = convertPair yes no afterCondition in
      Ok (If (condition, yes, no), following)
  | AST.Sequence (first, next) ->
      Result.map
        (fun (first, next, state) -> (Sequence (first, next), state))
        (convertPair first next symbols)
  | AST.Apply (AST.Var name, typeArgs, args) -> (
      let* converted, state = convertNonEmpty args symbols in
      match (typeArgs, StringOrder.Map.find_opt name environment) with
      | [], Some id -> Ok (Apply (Local id, converted), state)
      | [], None ->
          let functionId, state = internFunction name state in
          Ok (Call (functionId, converted), state)
      | _ :: _, _ ->
          let functionId, state = internFunction name state in
          Ok (TypeApp (functionId, checkedTypeArgs typeArgs, converted), state))
  | AST.TupleLiteral elements -> (
      let* values, state = convertList elements symbols in
      match tupleElementsFromList values with
      | Some tuple -> Ok (TupleLiteral tuple, state)
      | None ->
          conversionError location "tuple literal has fewer than two elements")
  | AST.TupleAccess (tuple, index) ->
      Result.map
        (fun (value, state) -> (TupleAccess (value, index), state))
        (convert symbols tuple)
  | AST.DictLiteral (keyType, valueType, entries) ->
      List.fold_left
        (fun result (key, value) ->
          let* converted, state = result in
          let* key, value, next = convertPair key value state in
          Ok ((key, value) :: converted, next))
        (Ok ([], symbols))
        entries
      |> Result.map (fun (converted, state) ->
          ( DictLiteral
              (checkedType keyType, checkedType valueType, List.rev converted),
            state ))
  | AST.RecordLiteral (reference, fields) -> (
      let* converted, state = convertFields fields symbols in
      let recordName = reference.AST.resolvedTypeName in
      let checkedReference, state = convertRecordReference reference state in
      match recordFieldCounts recordName with
      | Some fieldCount ->
          let* complete =
            completeRecordFields checkedReference.typeId fieldCount converted
          in
          Ok (RecordLiteral (checkedReference, complete), state)
      | None -> conversionError location "record declaration layout is absent")
  | AST.RecordUpdate (record, updates) ->
      let* record, afterRecord = convert symbols record in
      let* updates, following = convertFields updates afterRecord in
      Ok (RecordUpdate (record, updates), following)
  | AST.RecordAccess (record, reference) -> (
      let* value, state = convert symbols record in
      match
        (reference.AST.resolvedTypeName, reference.AST.resolvedFieldIndex)
      with
      | Some typeName, Some index ->
          let id, state =
            internField typeName reference.AST.sourceFieldName index state
          in
          Ok (RecordAccess (value, id), state)
      | None, _ | _, None ->
          conversionError location "record field reference was not resolved")
  | AST.Constructor (reference, variant, fields) ->
      let* reference =
        convertConstructorReference location reference variant symbols
      in
      let* fields, state = convertList fields symbols in
      Ok (Constructor (reference, fields), state)
  | AST.Match (scrutinee, cases) -> (
      let* scrutinee, afterScrutinee = convert symbols scrutinee in
      let* convertedCases, state =
        List.fold_left
          (fun result (case : AST.matchCase) ->
            let* convertedCases, state = result in
            let first = NonEmptyList.head case.AST.patterns in
            let* bindingIds, afterBindings =
              allocateMatchBindings state first
            in
            let patterns =
              NonEmptyList.map
                (convertPattern bindingIds afterBindings)
                case.AST.patterns
            in
            let caseEnvironment =
              extendEnvironment
                (StringOrder.Map.bindings bindingIds)
                environment
            in
            let* guard, afterGuard =
              match case.AST.guard with
              | None -> Ok (None, afterBindings)
              | Some guard ->
                  Result.map
                    (fun (guard, next) -> (Some guard, next))
                    (convertExpr recordFieldCounts location caseEnvironment
                       afterBindings guard)
            in
            let* body, following =
              convertExpr recordFieldCounts location caseEnvironment afterGuard
                case.AST.body
            in
            Ok ({ patterns; guard; body } :: convertedCases, following))
          (Ok ([], afterScrutinee))
          cases
      in
      match NonEmptyList.tryFromList (List.rev convertedCases) with
      | Some cases -> Ok (Match (scrutinee, cases), state)
      | None -> conversionError location "match has no cases")
  | AST.ListLiteral elements ->
      Result.map
        (fun (values, state) -> (ListLiteral values, state))
        (convertList elements symbols)
  | AST.Lambda (parameters, returnAnnotation, body) ->
      let* reversed, bindings, afterParameters =
        List.fold_left
          (fun result (parameter : AST.lambdaParameter) ->
            let* converted, bindings, state = result in
            match parameter.AST.inferredType with
            | None ->
                conversionError location "lambda parameter has no inferred type"
            | Some typ ->
                let pattern, patternBindings, next =
                  allocateLetPattern state parameter.AST.pattern
                in
                Ok
                  ( ({ pattern; typ = checkedType typ } : lambdaParameter)
                    :: converted,
                    bindings @ patternBindings,
                    next ))
          (Ok ([], [], symbols))
          (NonEmptyList.toList parameters)
      in
      let* body, following =
        convertExpr recordFieldCounts location
          (extendEnvironment bindings environment)
          afterParameters body
      in
      Ok
        ( Lambda
            ( NonEmptyList.fromList (List.rev reversed),
              Option.map checkedType returnAnnotation,
              body ),
          following )
  | AST.Apply (func, [], args) ->
      let* func, afterFunc = convert symbols func in
      let* args, following = convertNonEmpty args afterFunc in
      Ok (Apply (func, args), following)
  | AST.Apply (_, _ :: _, _) ->
      conversionError location
        "explicit type arguments require a named function"
  | AST.IndirectApply (func, args) ->
      let* func, afterFunc = convert symbols func in
      let* args, following = convertNonEmpty args afterFunc in
      Ok (IndirectApply (func, args), following)
  | AST.Closure (name, captures) ->
      let* values, state = convertList captures symbols in
      let functionId, state = internFunction name state in
      Ok (Closure (functionId, values), state)
  | AST.RuntimeError message -> Ok (RuntimeError message, symbols)
  | AST.BoundaryRender (renderer, value) ->
      let* value, state = convert symbols value in
      let functionId, state = internFunction renderer state in
      Ok (BoundaryRender (functionId, value), state)

let convertFunctionWithEnvironment recordFieldCounts outerEnvironment symbols
    (definition : AST.functionDef) : (functionDef * symbols, string) result =
  let* recursion =
    match definition.AST.recursion with
    | None -> Ok None
    | Some (AST.TypedRecursiveBinding typed) ->
        Ok (Some (checkedRecursiveMember typed))
    | Some
        ( AST.RecursiveBindingCandidate _ | AST.ParsedRecursiveBinding _
        | AST.ResolvedRecursiveBinding _ ) ->
        conversionError
          ("function '" ^ definition.AST.name ^ "'")
          "function has no typed recursion evidence"
  in
  let functionId, symbols = internFunction definition.AST.name symbols in
  let symbols =
    match recursion with
    | Some typed ->
        registerBinding typed.resolved.AST.parsed.AST.binding
          typed.resolved.AST.parsed.AST.sourceName symbols
    | None -> symbols
  in
  let parameters, afterParameters =
    mapFold
      (fun symbols (name, typ) ->
        let id, next = allocateBinding name symbols in
        ((id, checkedType typ), next))
      symbols
      (NonEmptyList.toList definition.AST.params)
  in
  let environment =
    List.fold_left2
      (fun environment (name, _) (id, _) ->
        StringOrder.Map.add name id environment)
      outerEnvironment
      (NonEmptyList.toList definition.AST.params)
      parameters
  in
  let* body, following =
    convertExpr recordFieldCounts
      ("function '" ^ definition.AST.name ^ "'")
      environment afterParameters definition.AST.body
  in
  Ok
    ( {
        id = functionId;
        name = definition.AST.name;
        typeParams = definition.AST.typeParams;
        params = NonEmptyList.fromList parameters;
        returnType = checkedType definition.AST.returnType;
        body;
        recursion;
      },
      following )

let ofTypedFunction variantLookup recordFieldCounts (symbols : symbols)
    definition =
  convertFunctionWithEnvironment recordFieldCounts StringOrder.Map.empty
    { symbols with constructorLookups = [ variantLookup ] }
    definition

let ofTypedProgram variantLookup externalValueNames baseTypes baseFunctions
    recordFieldCounts (AST.Program topLevels) =
  let valueNames =
    List.filter_map
      (function
        | AST.ValueDef (AST.CheckedValueDef (name, _, _)) -> Some name
        | AST.FunctionDef _ | AST.TypeDef _
        | AST.ValueDef (AST.UncheckedValueDef _)
        | AST.Expression _ ->
            None)
      topLevels
    |> StringOrder.Set.of_list
    |> StringOrder.Set.union externalValueNames
    |> StringOrder.Set.elements
  in
  let valueEnvironment, initialSymbols =
    List.fold_left
      (fun (environment, symbols) name ->
        let id, symbols = internValue name symbols in
        (StringOrder.Map.add name id environment, symbols))
      (StringOrder.Map.empty, emptySymbolsWithCatalogs baseTypes baseFunctions)
      valueNames
  in
  let initialSymbols =
    List.fold_left
      (fun symbols -> function
        | AST.FunctionDef definition ->
            snd (internFunction definition.AST.name symbols)
        | AST.TypeDef definition ->
            let name =
              match definition with
              | AST.RecordDef (name, _, _)
              | AST.SumTypeDef (name, _, _)
              | AST.TypeAlias (name, _, _) ->
                  name
            in
            snd (internType name symbols)
        | AST.ValueDef _ | AST.Expression _ -> symbols)
      initialSymbols topLevels
  in
  let initialSymbols =
    { initialSymbols with constructorLookups = [ variantLookup ] }
  in
  let initialSymbols =
    List.fold_left
      (fun symbols -> function
        | AST.TypeDef (AST.RecordDef (name, _, fields)) ->
            List.fold_left
              (fun symbols (index, (fieldName, _)) ->
                snd (internField name fieldName index symbols))
              symbols
              (List.mapi (fun index field -> (index, field)) fields)
        | AST.FunctionDef _
        | AST.TypeDef (AST.SumTypeDef _ | AST.TypeAlias _)
        | AST.ValueDef _ | AST.Expression _ ->
            symbols)
      initialSymbols topLevels
  in
  let convertTopLevel symbols = function
    | AST.FunctionDef definition ->
        Result.map
          (fun (definition, symbols) -> (FunctionDef definition, symbols))
          (convertFunctionWithEnvironment recordFieldCounts valueEnvironment
             symbols definition)
    | AST.TypeDef definition ->
        let name =
          match definition with
          | AST.RecordDef (name, _, _)
          | AST.SumTypeDef (name, _, _)
          | AST.TypeAlias (name, _, _) ->
              name
        in
        let id, symbols = internType name symbols in
        Ok (TypeDef (id, checkedTypeDef definition), symbols)
    | AST.ValueDef (AST.CheckedValueDef (name, typ, body)) ->
        let* checkedBody, state =
          convertExpr recordFieldCounts
            ("value '" ^ name ^ "'")
            valueEnvironment symbols body
        in
        let id =
          match StringOrder.Map.find_opt name valueEnvironment with
          | Some id -> id
          | None -> Crash.crash "Checked value identity allocation was lost"
        in
        Ok
          ( ValueDef { id; name; typ = checkedType typ; body = checkedBody },
            state )
    | AST.ValueDef (AST.UncheckedValueDef (name, _)) ->
        conversionError
          ("value '" ^ name ^ "'")
          "value definition was not checked"
    | AST.Expression (_, expr) ->
        Result.map
          (fun (expr, state) -> (Expression expr, state))
          (convertExpr recordFieldCounts "entry expression" valueEnvironment
             symbols expr)
  in
  List.fold_left
    (fun result topLevel ->
      let* converted, symbols = result in
      let* item, next = convertTopLevel symbols topLevel in
      Ok (item :: converted, next))
    (Ok ([], initialSymbols))
    topLevels
  |> Result.map (fun (converted, symbols) ->
      CheckedProgram (symbols, List.rev converted))
