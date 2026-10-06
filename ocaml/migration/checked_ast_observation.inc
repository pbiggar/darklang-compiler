
(* Migration-only complete observations appended to the real checked boundary. *)
let observationScalar kind value = `Assoc ["kind", `String kind; "value", `String value]
let observationUnion typ case fields = `Assoc ["type", `String typ; "case", `String case; "fields", `List fields]
let observationTuple values = `Assoc ["tuple", `List values]
let observationRecord name fields = `Assoc ["record", `String name; "fields", `List (List.map (fun (name, value) -> `List [`String name; value]) fields)]
let observationString value = `String value
let observationInt value = observationScalar "int32" (string_of_int value)
let observationOption encode = function None -> observationUnion "FSharpOption" "None" [] | Some value -> observationUnion "FSharpOption" "Some" [encode value]
let observationResult encode = function Ok value -> observationUnion "FSharpResult" "Ok" [encode value] | Error error -> observationUnion "FSharpResult" "Error" [observationString error]
let observationBinding id = match AST.MigrationObservation.bindingOrdinal id with
  | Some ordinal -> observationUnion "BindingId" "LocalBindingId" [observationInt ordinal; observationOption observationString (AST.bindingDisplayName id)]
  | None -> observationUnion "BindingId" "TopLevelValueId" [observationString ((match AST.bindingDisplayName id with Some name -> name | None -> Crash.crash "Global binding has no display name"))]
let observationFunction id = observationUnion "FunctionId" "FunctionId" [observationScalar "uint64" (unsigned (AST.functionIdValue id))]
let observationTypeId id = observationUnion "TypeId" "TypeId" [observationInt (AST.MigrationObservation.typeOrdinal id)]
let observationConstructorId id = observationUnion "ConstructorId" "ConstructorId" [observationTypeId (AST.constructorIdOwner id); observationString (AST.constructorIdValue id); observationInt (AST.constructorRuntimeTag id)]
let observationFieldId id = observationUnion "FieldId" "FieldId" [observationTypeId (AST.fieldIdOwner id); observationInt (AST.fieldRuntimeIndex id)]
let observationScopeId id = observationUnion "ScopeBoundaryId" "ScopeBoundaryId" [observationInt (AST.MigrationObservation.scopeOrdinal id)]
let observationGroupId id = observationUnion "RecursiveGroupId" "RecursiveGroupId" [observationInt (AST.MigrationObservation.groupOrdinal id)]
let observationMemberId id = observationUnion "RecursiveMemberId" "RecursiveMemberId" [observationInt (AST.MigrationObservation.memberOrdinal id)]

let observationMap key value entries = `Assoc ["map", `List (List.map (fun (k, v) -> observationTuple [key k; value v]) entries)]
let observationNonEmpty encode (value : 'a NonEmptyList.t) = observationRecord "NonEmptyList" ["Head", encode value.NonEmptyList.head; "Tail", `List (List.map encode value.NonEmptyList.tail)]

let rec observationSemanticType (value : AST.semanticType) = match value with
  | AST.TInt8 -> observationUnion "SemanticType" "TInt8" []
  | AST.TInt16 -> observationUnion "SemanticType" "TInt16" []
  | AST.TInt32 -> observationUnion "SemanticType" "TInt32" []
  | AST.TInt64 -> observationUnion "SemanticType" "TInt64" []
  | AST.TInt128 -> observationUnion "SemanticType" "TInt128" []
  | AST.TInt -> observationUnion "SemanticType" "TInt" []
  | AST.TUInt8 -> observationUnion "SemanticType" "TUInt8" []
  | AST.TUInt16 -> observationUnion "SemanticType" "TUInt16" []
  | AST.TUInt32 -> observationUnion "SemanticType" "TUInt32" []
  | AST.TUInt64 -> observationUnion "SemanticType" "TUInt64" []
  | AST.TUInt128 -> observationUnion "SemanticType" "TUInt128" []
  | AST.TBool -> observationUnion "SemanticType" "TBool" []
  | AST.TFloat64 -> observationUnion "SemanticType" "TFloat64" []
  | AST.TString -> observationUnion "SemanticType" "TString" []
  | AST.TBlob -> observationUnion "SemanticType" "TBlob" []
  | AST.TChar -> observationUnion "SemanticType" "TChar" []
  | AST.TDateTime -> observationUnion "SemanticType" "TDateTime" []
  | AST.TUnit -> observationUnion "SemanticType" "TUnit" []
  | AST.TNever -> observationUnion "SemanticType" "TNever" []
  | AST.TFunction (field0, field1) -> observationUnion "SemanticType" "TFunction" [`List (List.map (fun item -> observationSemanticType item) field0); observationSemanticType field1]
  | AST.TTuple field0 -> observationUnion "SemanticType" "TTuple" [`List (List.map (fun item -> observationSemanticType item) field0)]
  | AST.TRecord (field0, field1) -> observationUnion "SemanticType" "TRecord" [observationString field0; `List (List.map (fun item -> observationSemanticType item) field1)]
  | AST.TSum (field0, field1) -> observationUnion "SemanticType" "TSum" [observationString field0; `List (List.map (fun item -> observationSemanticType item) field1)]
  | AST.TList field0 -> observationUnion "SemanticType" "TList" [observationSemanticType field0]
  | AST.TStream field0 -> observationUnion "SemanticType" "TStream" [observationSemanticType field0]
  | AST.TVar field0 -> observationUnion "SemanticType" "TVar" [observationString field0]
  | AST.TInferenceVar (field0, field1) -> observationUnion "SemanticType" "TInferenceVar" [observationString field0; observationString field1]
  | AST.TInternalRawPtr -> observationUnion "SemanticType" "TInternalRawPtr" []
  | AST.TDict (field0, field1) -> observationUnion "SemanticType" "TDict" [observationSemanticType field0; observationSemanticType field1]
and observationBinOp (value : AST.binOp) = match value with
  | AST.Add -> observationUnion "BinOp" "Add" []
  | AST.Sub -> observationUnion "BinOp" "Sub" []
  | AST.Mul -> observationUnion "BinOp" "Mul" []
  | AST.Div -> observationUnion "BinOp" "Div" []
  | AST.Mod -> observationUnion "BinOp" "Mod" []
  | AST.Pow -> observationUnion "BinOp" "Pow" []
  | AST.Shl -> observationUnion "BinOp" "Shl" []
  | AST.Shr -> observationUnion "BinOp" "Shr" []
  | AST.BitAnd -> observationUnion "BinOp" "BitAnd" []
  | AST.BitOr -> observationUnion "BinOp" "BitOr" []
  | AST.BitXor -> observationUnion "BinOp" "BitXor" []
  | AST.StringConcat -> observationUnion "BinOp" "StringConcat" []
  | AST.Eq -> observationUnion "BinOp" "Eq" []
  | AST.Neq -> observationUnion "BinOp" "Neq" []
  | AST.Lt -> observationUnion "BinOp" "Lt" []
  | AST.Gt -> observationUnion "BinOp" "Gt" []
  | AST.Lte -> observationUnion "BinOp" "Lte" []
  | AST.Gte -> observationUnion "BinOp" "Gte" []
  | AST.And -> observationUnion "BinOp" "And" []
  | AST.Or -> observationUnion "BinOp" "Or" []
and observationUnaryOp (value : AST.unaryOp) = match value with
  | AST.Neg -> observationUnion "UnaryOp" "Neg" []
  | AST.Not -> observationUnion "UnaryOp" "Not" []
  | AST.BitNot -> observationUnion "UnaryOp" "BitNot" []
and observationRecursiveMemberKind (value : AST.recursiveMemberKind) = match value with
  | AST.TopLevelFunctionMember -> observationUnion "RecursiveMemberKind" "TopLevelFunctionMember" []
  | AST.NamedLocalFunctionMember -> observationUnion "RecursiveMemberKind" "NamedLocalFunctionMember" []
  | AST.DirectLambdaValueMember -> observationUnion "RecursiveMemberKind" "DirectLambdaValueMember" []
and observationRecursiveAvailability (value : AST.recursiveAvailability) = match value with
  | AST.OrdinaryBinding -> observationUnion "RecursiveAvailability" "OrdinaryBinding" []
  | AST.SelfRecursiveMember -> observationUnion "RecursiveAvailability" "SelfRecursiveMember" []
  | AST.MutualRecursiveMember -> observationUnion "RecursiveAvailability" "MutualRecursiveMember" []
  | AST.CompletedGroupMember -> observationUnion "RecursiveAvailability" "CompletedGroupMember" []
  | AST.ImportedGroupMember -> observationUnion "RecursiveAvailability" "ImportedGroupMember" []
and observationParsedRecursiveMember (value : AST.parsedRecursiveMember) = observationRecord "ParsedRecursiveMember" [
  "Binding", observationBinding value.AST.binding;
  "Boundary", observationScopeId value.AST.boundary;
  "Member", observationMemberId value.AST.member;
  "SourceName", observationString value.AST.sourceName;
  "Kind", observationRecursiveMemberKind value.AST.kind;
]
and observationResolvedRecursiveMember (value : AST.resolvedRecursiveMember) = observationRecord "ResolvedRecursiveMember" [
  "Parsed", observationParsedRecursiveMember value.AST.parsed;
  "Group", observationGroupId value.AST.group;
  "GroupIndex", observationInt value.AST.groupIndex;
  "Availability", observationRecursiveAvailability value.AST.availability;
]
and observationVariantNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.variantNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "VariantNode" [
  "Name", observationString value.AST.name;
  "Fields", `List (List.map (fun item -> encodeA item) value.AST.fields);
]
and observationTypeDefNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.typeDefNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.RecordDef (field0, field1, field2) -> observationUnion "TypeDefNode" "RecordDef" [observationString field0; `List (List.map (fun item -> observationString item) field1); `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationString part0; encodeA part1])) field2)]
  | AST.SumTypeDef (field0, field1, field2) -> observationUnion "TypeDefNode" "SumTypeDef" [observationString field0; `List (List.map (fun item -> observationString item) field1); `List (List.map (fun item -> observationVariantNode (fun item -> encodeA item) item) field2)]
  | AST.TypeAlias (field0, field1, field2) -> observationUnion "TypeDefNode" "TypeAlias" [observationString field0; `List (List.map (fun item -> observationString item) field1); encodeA field2]
and observationLetPattern (value : letPattern) = match value with
  | LPUnit -> observationUnion "LetPattern" "LPUnit" []
  | LPWildcard -> observationUnion "LetPattern" "LPWildcard" []
  | LPVariable field0 -> observationUnion "LetPattern" "LPVariable" [observationBinding field0]
  | LPTuple (field0, field1, field2) -> observationUnion "LetPattern" "LPTuple" [observationLetPattern field0; observationLetPattern field1; `List (List.map (fun item -> observationLetPattern item) field2)]
and observationTupleElements : 'a. ('a -> Yojson.Basic.t) -> 'a tupleElements -> Yojson.Basic.t = fun encodeA value -> observationRecord "TupleElements" [
  "First", encodeA value.first;
  "Second", encodeA value.second;
  "Rest", `List (List.map (fun item -> encodeA item) value.rest);
]
and observationCheckedType (value : checkedType) = match value with
  | CheckedType field0 -> observationUnion "CheckedType" "CheckedType" [observationSemanticType field0]
and observationCheckedTypeDef (value : checkedTypeDef) = match value with
  | CheckedTypeDef field0 -> observationUnion "CheckedTypeDef" "CheckedTypeDef" [observationTypeDefNode (fun item -> observationCheckedType item) field0]
and observationRecursiveMember (value : recursiveMember) = observationRecord "RecursiveMember" [
  "Resolved", observationResolvedRecursiveMember value.resolved;
  "MonomorphicType", observationCheckedType value.monomorphicType;
]
and observationPattern (value : pattern) = match value with
  | PUnit -> observationUnion "Pattern" "PUnit" []
  | PWildcard -> observationUnion "Pattern" "PWildcard" []
  | PVariable field0 -> observationUnion "Pattern" "PVariable" [observationBinding field0]
  | PConstructor (field0, field1) -> observationUnion "Pattern" "PConstructor" [observationConstructorId field0; `List (List.map (fun item -> observationPattern item) field1)]
  | PInt64 field0 -> observationUnion "Pattern" "PInt64" [observationScalar "int64" (Int64.to_string field0)]
  | PBigInt field0 -> observationUnion "Pattern" "PBigInt" [(fun x -> observationScalar "bigint" (Z.to_string x)) field0]
  | PInt128Literal field0 -> observationUnion "Pattern" "PInt128Literal" [observationScalar "int128" (Z.to_string field0)]
  | PInt8Literal field0 -> observationUnion "Pattern" "PInt8Literal" [observationScalar "int8" (string_of_int field0)]
  | PInt16Literal field0 -> observationUnion "Pattern" "PInt16Literal" [observationScalar "int16" (string_of_int field0)]
  | PInt32Literal field0 -> observationUnion "Pattern" "PInt32Literal" [observationScalar "int32" (Int32.to_string field0)]
  | PUInt8Literal field0 -> observationUnion "Pattern" "PUInt8Literal" [observationScalar "uint8" (string_of_int field0)]
  | PUInt16Literal field0 -> observationUnion "Pattern" "PUInt16Literal" [observationScalar "uint16" (string_of_int field0)]
  | PUInt32Literal field0 -> observationUnion "Pattern" "PUInt32Literal" [observationScalar "uint32" (Int64.to_string field0)]
  | PUInt64Literal field0 -> observationUnion "Pattern" "PUInt64Literal" [observationScalar "uint64" (unsigned field0)]
  | PUInt128Literal field0 -> observationUnion "Pattern" "PUInt128Literal" [observationScalar "uint128" (Z.to_string field0)]
  | PBool field0 -> observationUnion "Pattern" "PBool" [(fun x -> `Bool x) field0]
  | PString field0 -> observationUnion "Pattern" "PString" [observationString field0]
  | PChar field0 -> observationUnion "Pattern" "PChar" [observationString field0]
  | PFloat field0 -> observationUnion "Pattern" "PFloat" [(fun x -> observationScalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float x))) field0]
  | PTuple field0 -> observationUnion "Pattern" "PTuple" [`List (List.map (fun item -> observationPattern item) field0)]
  | PList field0 -> observationUnion "Pattern" "PList" [`List (List.map (fun item -> observationPattern item) field0)]
  | PListCons (field0, field1) -> observationUnion "Pattern" "PListCons" [`List (List.map (fun item -> observationPattern item) field0); observationPattern field1]
  | POr field0 -> observationUnion "Pattern" "POr" [observationNonEmpty (fun item -> observationPattern item) field0]
and observationLambdaParameter (value : lambdaParameter) = observationRecord "LambdaParameter" [
  "Pattern", observationLetPattern value.pattern;
  "Type", observationCheckedType value.typ;
]
and observationRecordReference (value : recordReference) = observationRecord "RecordReference" [
  "TypeId", observationTypeId value.typeId;
  "TypeArgs", `List (List.map (fun item -> observationCheckedType item) value.typeArgs);
]
and observationConstructorReference (value : constructorReference) = observationRecord "ConstructorReference" [
  "TypeId", observationTypeId value.typeId;
  "ConstructorId", observationConstructorId value.constructorId;
  "TypeArgs", `List (List.map (fun item -> observationCheckedType item) value.typeArgs);
]
and observationRecordFields : 'a. ('a -> Yojson.Basic.t) -> 'a recordFields -> Yojson.Basic.t = fun encodeA value -> match value with
  | RecordFields field0 -> observationUnion "RecordFields" "RecordFields" [`List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationFieldId part0; encodeA part1])) field0)]
and observationStringPart (value : stringPart) = match value with
  | StringText field0 -> observationUnion "StringPart" "StringText" [observationString field0]
  | StringExpr field0 -> observationUnion "StringPart" "StringExpr" [observationExpr field0]
and observationExpr (value : expr) = match value with
  | UnitLiteral -> observationUnion "Expr" "UnitLiteral" []
  | Int64Literal field0 -> observationUnion "Expr" "Int64Literal" [observationScalar "int64" (Int64.to_string field0)]
  | Int128Literal field0 -> observationUnion "Expr" "Int128Literal" [observationScalar "int128" (Z.to_string field0)]
  | Int8Literal field0 -> observationUnion "Expr" "Int8Literal" [observationScalar "int8" (string_of_int field0)]
  | Int16Literal field0 -> observationUnion "Expr" "Int16Literal" [observationScalar "int16" (string_of_int field0)]
  | Int32Literal field0 -> observationUnion "Expr" "Int32Literal" [observationScalar "int32" (Int32.to_string field0)]
  | UInt8Literal field0 -> observationUnion "Expr" "UInt8Literal" [observationScalar "uint8" (string_of_int field0)]
  | UInt16Literal field0 -> observationUnion "Expr" "UInt16Literal" [observationScalar "uint16" (string_of_int field0)]
  | UInt32Literal field0 -> observationUnion "Expr" "UInt32Literal" [observationScalar "uint32" (Int64.to_string field0)]
  | UInt64Literal field0 -> observationUnion "Expr" "UInt64Literal" [observationScalar "uint64" (unsigned field0)]
  | UInt128Literal field0 -> observationUnion "Expr" "UInt128Literal" [observationScalar "uint128" (Z.to_string field0)]
  | BigIntLiteral field0 -> observationUnion "Expr" "BigIntLiteral" [(fun x -> observationScalar "bigint" (Z.to_string x)) field0]
  | BoolLiteral field0 -> observationUnion "Expr" "BoolLiteral" [(fun x -> `Bool x) field0]
  | StringLiteral field0 -> observationUnion "Expr" "StringLiteral" [observationString field0]
  | BlobLiteral field0 -> observationUnion "Expr" "BlobLiteral" [observationString field0]
  | CharLiteral field0 -> observationUnion "Expr" "CharLiteral" [observationString field0]
  | FloatLiteral field0 -> observationUnion "Expr" "FloatLiteral" [(fun x -> observationScalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float x))) field0]
  | InterpolatedString field0 -> observationUnion "Expr" "InterpolatedString" [`List (List.map (fun item -> observationStringPart item) field0)]
  | BinOp (field0, field1, field2) -> observationUnion "Expr" "BinOp" [observationBinOp field0; observationExpr field1; observationExpr field2]
  | UnaryOp (field0, field1) -> observationUnion "Expr" "UnaryOp" [observationUnaryOp field0; observationExpr field1]
  | Let (field0, field1, field2) -> observationUnion "Expr" "Let" [observationLetPattern field0; observationExpr field1; observationExpr field2]
  | RecursiveLet (field0, field1, field2) -> observationUnion "Expr" "RecursiveLet" [observationRecursiveMember field0; observationExpr field1; observationExpr field2]
  | Local field0 -> observationUnion "Expr" "Local" [observationBinding field0]
  | If (field0, field1, field2) -> observationUnion "Expr" "If" [observationExpr field0; observationExpr field1; observationExpr field2]
  | Sequence (field0, field1) -> observationUnion "Expr" "Sequence" [observationExpr field0; observationExpr field1]
  | Call (field0, field1) -> observationUnion "Expr" "Call" [observationFunction field0; observationNonEmpty (fun item -> observationExpr item) field1]
  | TypeApp (field0, field1, field2) -> observationUnion "Expr" "TypeApp" [observationFunction field0; `List (List.map (fun item -> observationCheckedType item) field1); observationNonEmpty (fun item -> observationExpr item) field2]
  | TupleLiteral field0 -> observationUnion "Expr" "TupleLiteral" [observationTupleElements (fun item -> observationExpr item) field0]
  | TupleAccess (field0, field1) -> observationUnion "Expr" "TupleAccess" [observationExpr field0; observationInt field1]
  | DictLiteral (field0, field1, field2) -> observationUnion "Expr" "DictLiteral" [observationCheckedType field0; observationCheckedType field1; `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationExpr part0; observationExpr part1])) field2)]
  | RecordLiteral (field0, field1) -> observationUnion "Expr" "RecordLiteral" [observationRecordReference field0; observationRecordFields (fun item -> observationExpr item) field1]
  | RecordUpdate (field0, field1) -> observationUnion "Expr" "RecordUpdate" [observationExpr field0; `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationFieldId part0; observationExpr part1])) field1)]
  | RecordAccess (field0, field1) -> observationUnion "Expr" "RecordAccess" [observationExpr field0; observationFieldId field1]
  | Constructor (field0, field1) -> observationUnion "Expr" "Constructor" [observationConstructorReference field0; `List (List.map (fun item -> observationExpr item) field1)]
  | Match (field0, field1) -> observationUnion "Expr" "Match" [observationExpr field0; observationNonEmpty (fun item -> observationMatchCase item) field1]
  | ListLiteral field0 -> observationUnion "Expr" "ListLiteral" [`List (List.map (fun item -> observationExpr item) field0)]
  | Lambda (field0, field1, field2) -> observationUnion "Expr" "Lambda" [observationNonEmpty (fun item -> observationLambdaParameter item) field0; observationOption (fun item -> observationCheckedType item) field1; observationExpr field2]
  | Apply (field0, field1) -> observationUnion "Expr" "Apply" [observationExpr field0; observationNonEmpty (fun item -> observationExpr item) field1]
  | IndirectApply (field0, field1) -> observationUnion "Expr" "IndirectApply" [observationExpr field0; observationNonEmpty (fun item -> observationExpr item) field1]
  | FuncRef field0 -> observationUnion "Expr" "FuncRef" [observationFunction field0]
  | Closure (field0, field1) -> observationUnion "Expr" "Closure" [observationFunction field0; `List (List.map (fun item -> observationExpr item) field1)]
  | RuntimeError field0 -> observationUnion "Expr" "RuntimeError" [observationString field0]
  | BoundaryRender (field0, field1) -> observationUnion "Expr" "BoundaryRender" [observationFunction field0; observationExpr field1]
and observationMatchCase (value : matchCase) = observationRecord "MatchCase" [
  "Patterns", observationNonEmpty (fun item -> observationPattern item) value.patterns;
  "Guard", observationOption (fun item -> observationExpr item) value.guard;
  "Body", observationExpr value.body;
]
and observationFunctionDef (value : functionDef) = observationRecord "FunctionDef" [
  "Id", observationFunction value.id;
  "Name", observationString value.name;
  "TypeParams", `List (List.map (fun item -> observationString item) value.typeParams);
  "Params", observationNonEmpty (fun item -> (let part0, part1 = item in observationTuple [observationBinding part0; observationCheckedType part1])) value.params;
  "ReturnType", observationCheckedType value.returnType;
  "Body", observationExpr value.body;
  "Recursion", observationOption (fun item -> observationRecursiveMember item) value.recursion;
]
and observationValueDef (value : valueDef) = observationRecord "ValueDef" [
  "Id", observationBinding value.id;
  "Name", observationString value.name;
  "Type", observationCheckedType value.typ;
  "Body", observationExpr value.body;
]
and observationTopLevel (value : topLevel) = match value with
  | FunctionDef field0 -> observationUnion "TopLevel" "FunctionDef" [observationFunctionDef field0]
  | TypeDef (field0, field1) -> observationUnion "TopLevel" "TypeDef" [observationTypeId field0; observationCheckedTypeDef field1]
  | ValueDef field0 -> observationUnion "TopLevel" "ValueDef" [observationValueDef field0]
  | Expression field0 -> observationUnion "TopLevel" "Expression" [observationExpr field0]
and observationSemanticMetadata (value : semanticMetadata) = observationRecord "SemanticMetadata" [
  "TypeNames", observationMap (fun key -> observationTypeId key) (fun item -> observationString item) (TypeIdMap.bindings value.typeNames);
]
and observationTypeCatalog (value : typeCatalog) = observationRecord "TypeCatalog" [
  "Names", observationMap (fun key -> observationTypeId key) (fun item -> observationString item) (TypeIdMap.bindings value.names);
  "Ids", observationMap (fun key -> observationString key) (fun item -> observationTypeId item) (StringOrder.Map.bindings value.ids);
  "NextOrdinal", observationInt value.nextOrdinal;
]
and observationFunctionCatalog (value : functionCatalog) = observationRecord "FunctionCatalog" [
  "Names", observationUnion "FunctionIdMap" "FunctionIdMap" [observationMap (fun key -> observationScalar "uint64" (unsigned (AST.functionIdValue key))) (fun item -> observationString item) (FunctionIdMap.toList value.names)];
  "Ids", observationMap (fun key -> observationString key) (fun item -> observationFunction item) (StringOrder.Map.bindings value.ids);
  "NextOrdinal", observationScalar "uint64" (unsigned value.nextOrdinal);
  "Ordinals", `List (List.map (fun item -> observationFunction item) value.ordinals);
]
and observationGlobalCatalog (value : globalCatalog) = observationRecord "GlobalCatalog" [
  "BindingNames", observationMap (fun key -> observationBinding key) (fun item -> observationString item) (BindingIdMap.bindings value.bindingNames);
  "ValueIds", observationMap (fun key -> observationString key) (fun item -> observationBinding item) (StringOrder.Map.bindings value.valueIds);
  "NextBindingOrdinal", observationInt value.nextBindingOrdinal;
  "FunctionNames", observationUnion "FunctionIdMap" "FunctionIdMap" [observationMap (fun key -> observationScalar "uint64" (unsigned (AST.functionIdValue key))) (fun item -> observationString item) (FunctionIdMap.toList value.functionNames)];
  "FunctionIds", observationMap (fun key -> observationString key) (fun item -> observationFunction item) (StringOrder.Map.bindings value.functionIds);
  "FunctionOrdinals", `List (List.map (fun item -> observationFunction item) value.functionOrdinals);
  "NextFunctionOrdinal", observationScalar "uint64" (unsigned value.nextFunctionOrdinal);
  "TypeNames", observationMap (fun key -> observationTypeId key) (fun item -> observationString item) (TypeIdMap.bindings value.typeNames);
  "TypeIds", observationMap (fun key -> observationString key) (fun item -> observationTypeId item) (StringOrder.Map.bindings value.typeIds);
  "BaseTypes", observationTypeCatalog value.baseTypes;
  "NextTypeOrdinal", observationInt value.nextTypeOrdinal;
  "ConstructorNames", observationMap (fun key -> observationConstructorId key) (fun item -> (let part0, part1 = item in observationTuple [observationString part0; observationString part1])) (ConstructorIdMap.bindings value.constructorNames);
  "ConstructorIds", observationMap (fun key -> (let part0, part1 = key in observationTuple [observationString part0; observationString part1])) (fun item -> observationConstructorId item) (NamePairMap.bindings value.constructorIds);
  "ConstructorLookups", `List (List.map (fun item -> observationMap (fun key -> observationString key) (fun item -> (let part0, part1, part2, part3 = item in observationTuple [observationString part0; `List (List.map (fun item -> observationString item) part1); observationInt part2; `List (List.map (fun item -> observationSemanticType item) part3)])) (StringOrder.Map.bindings item)) value.constructorLookups);
  "FieldNames", observationMap (fun key -> observationFieldId key) (fun item -> (let part0, part1 = item in observationTuple [observationString part0; observationString part1])) (FieldIdMap.bindings value.fieldNames);
  "FieldIds", observationMap (fun key -> (let part0, part1 = key in observationTuple [observationString part0; observationString part1])) (fun item -> observationFieldId item) (NamePairMap.bindings value.fieldIds);
]
and observationProgram (value : program) = match value with
  | CheckedProgram (field0, field1) -> observationUnion "Program" "CheckedProgram" [observationGlobalCatalog field0; `List (List.map (fun item -> observationTopLevel item) field1)]

let observe source =
  let one value = NonEmptyList.singleton value in
  let var = AST.Var "x" and unit = AST.UnitLiteral in
  let field index = AST.resolvedRecordFieldReference "R" ("field" ^ string_of_int index) index in
  let record = {AST.sourceTypeName = "R"; resolvedTypeName = "R"; typeArgs = [AST.TInferenceVar (source, "fixed")]} in
  let parameter = AST.inferredLambdaVariable "x" AST.TInt64 in
  let patterns = [AST.PUnit; AST.PWildcard; AST.PVar "x";
    AST.PResolvedConstructor ("S", "Choice", 17, [AST.PVar "x"]); AST.PInt64 Int64.min_int;
    AST.PBigInt (Z.shift_left Z.one 256); AST.PInt128Literal (Z.neg (Z.shift_left Z.one 127));
    AST.PInt8Literal (-128); AST.PInt16Literal (-32768); AST.PInt32Literal Int32.min_int;
    AST.PUInt8Literal 255; AST.PUInt16Literal 65535; AST.PUInt32Literal 4294967295L;
    AST.PUInt64Literal (-1L); AST.PUInt128Literal (Z.pred (Z.shift_left Z.one 128));
    AST.PBool true; AST.PString source; AST.PChar source; AST.PFloat (-0.);
    AST.PTuple [AST.PVar "x"; AST.PWildcard]; AST.PList [AST.PVar "x"];
    AST.PListCons ([AST.PWildcard], AST.PVar "x"); AST.POr (one (AST.PVar "x"))] in
  let expressions : AST.expr list = [unit; AST.Int64Literal Int64.min_int; AST.Int128Literal (Z.neg (Z.shift_left Z.one 127));
    AST.Int8Literal (-128); AST.Int16Literal (-32768); AST.Int32Literal Int32.min_int; AST.UInt8Literal 255;
    AST.UInt16Literal 65535; AST.UInt32Literal 4294967295L; AST.UInt64Literal (-1L); AST.UInt128Literal (Z.pred (Z.shift_left Z.one 128));
    AST.BigIntLiteral (Z.shift_left Z.one 256); AST.BoolLiteral true; AST.StringLiteral source; AST.CharLiteral source; AST.FloatLiteral (-0.);
    AST.InterpolatedString [AST.StringText source; AST.StringExpr var]; AST.BinOp (AST.Add, var, unit); AST.UnaryOp (AST.Not, var);
    AST.Let (AST.LPTuple (AST.LPVariable "x", AST.LPWildcard, [AST.LPUnit]), unit, var);
    AST.Var source; AST.Var "Builtin.testNan"; AST.Var "Builtin.testInfinity"; AST.Var "Builtin.blobEmpty";
    AST.If (var, unit, var); AST.Sequence (unit, var); AST.Apply (var, [], one unit); AST.Apply (var, [AST.TInt64], one unit);
    AST.TupleLiteral [unit; var; unit]; AST.TupleAccess (var, 2); AST.DictLiteral (AST.TString, AST.TInt64, [AST.StringLiteral source, var]);
    AST.RecordLiteral (record, [field 1, var; field 0, unit]); AST.RecordUpdate (var, [field 1, unit]); AST.RecordAccess (var, field 1);
    AST.Constructor (AST.ResolvedConstructor (["a"], "S", [AST.TInt64]), "Choice", [var]);
    AST.ListLiteral [var; unit]; AST.Lambda (one parameter, Some AST.TInt64, var);
    AST.Apply (AST.Lambda (one parameter, None, var), [], one unit); AST.IndirectApply (var, one unit);
    AST.Closure (source, [var]); AST.RuntimeError source; AST.BoundaryRender (source, var);
    AST.TupleLiteral []; AST.TupleLiteral [unit]; AST.Match (unit, []);
    AST.RecordLiteral (record, []); AST.RecordAccess (var, AST.unresolvedRecordFieldReference "field");
    AST.Constructor (AST.UnresolvedConstructor None, "Absent", []); AST.Lambda (one (AST.lambdaParameter (AST.LPVariable "x")), None, var);
    AST.Apply (unit, [AST.TUnit], one unit)] @
    List.map (fun pattern -> AST.Match (var, [{AST.patterns = one pattern; guard = Some var; body = var}])) patterns in
  let lookup = StringOrder.Map.singleton "S.Choice" ("S", [], 17, []) in
  let catalog = includeFunctionNames (List.to_seq [source; "z"; "aa"; "z"]) emptyFunctionCatalog in
  let convert topLevels = ofTypedProgram lookup (StringOrder.Set.singleton "external") emptyTypeCatalog catalog (fun name -> if name = "R" then Some 2 else None) (AST.Program (AST.TypeDef (AST.RecordDef ("R", [], ["field0", AST.TUnit; "field1", AST.TInt64])) :: AST.TypeDef (AST.SumTypeDef ("S", [], [{AST.name = "Choice"; fields = []}])) :: topLevels)) in
  let converted = List.map (fun expr -> convert [AST.Expression ([], expr)]) expressions in
  let definition : AST.functionDef = {AST.name = source; typeParams = ["a"]; params = one ("x", AST.TInferenceVar (source, "fixed")); returnType = AST.TInt64; body = AST.Let (AST.LPVariable "local", unit, var); recursion = None} in
  let declarations = convert [AST.TypeDef (AST.RecordDef ("R", ["a"], ["field0", AST.TVar "a"; "field1", AST.TInt64]));
    AST.TypeDef (AST.SumTypeDef ("S", [], [{AST.name = "Choice"; fields = []}])); AST.TypeDef (AST.TypeAlias ("A", [], AST.TInferenceVar (source, "fixed")));
    AST.FunctionDef definition; AST.ValueDef (AST.CheckedValueDef ("v", AST.TInt64, AST.Var source)); AST.Expression ([], AST.Var "v")] in
  let owner = AST.typeId 5 in
  let recordCases = List.concat_map (fun count ->
    let entries = List.init count (fun index -> AST.fieldId owner index, index) in
    [completeRecordFields owner count entries; completeRecordFields owner count (List.rev entries);
     completeRecordFields owner count ((AST.fieldId owner 0, 99) :: entries);
     completeRecordFields owner count [AST.fieldId (AST.typeId 6) 0, 99]]) [0; 1; 2; 64; 65] in
  let orderingTypes = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TFunction ([AST.TVar source], AST.TInt64); AST.TTuple []; AST.TRecord (source, []); AST.TSum (source, []); AST.TList AST.TInt64; AST.TStream AST.TInt64; AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TInternalRawPtr; AST.TDict (AST.TString, AST.TInt64); AST.TRecord ("aa", []); AST.TRecord ("z", []); AST.TTuple [AST.TInt64]] in
  let ordering = List.concat_map (fun left -> List.map (fun right -> Int.compare (AST.compareSemanticType left right) 0) orderingTypes) orderingTypes in
  observationTuple [`List (List.map observationInt ordering); observationSemanticMetadata (semanticMetadata (emptySymbols ())); observationFunctionCatalog catalog;
    `List (List.map (observationResult observationProgram) converted);
    observationResult observationProgram declarations;
    observationResult observationProgram (convert [AST.ValueDef (AST.UncheckedValueDef (source, unit))]);
    `List (List.map (observationResult (observationRecordFields observationInt)) recordCases)]

let observationFixtures source =
  let one value = NonEmptyList.singleton value in
  let var = AST.Var "x" and unit = AST.UnitLiteral in
  let field index = AST.resolvedRecordFieldReference "R" ("field" ^ string_of_int index) index in
  let record = {AST.sourceTypeName = "R"; resolvedTypeName = "R"; typeArgs = [AST.TInferenceVar (source, "fixed")]} in
  let parameter = AST.inferredLambdaVariable "x" AST.TInt64 in
  let patterns = [AST.PUnit; AST.PWildcard; AST.PVar "x";
    AST.PResolvedConstructor ("S", "Choice", 17, [AST.PVar "x"]); AST.PInt64 Int64.min_int;
    AST.PBigInt (Z.shift_left Z.one 256); AST.PInt128Literal (Z.neg (Z.shift_left Z.one 127));
    AST.PInt8Literal (-128); AST.PInt16Literal (-32768); AST.PInt32Literal Int32.min_int;
    AST.PUInt8Literal 255; AST.PUInt16Literal 65535; AST.PUInt32Literal 4294967295L;
    AST.PUInt64Literal (-1L); AST.PUInt128Literal (Z.pred (Z.shift_left Z.one 128));
    AST.PBool true; AST.PString source; AST.PChar source; AST.PFloat (-0.);
    AST.PTuple [AST.PVar "x"; AST.PWildcard]; AST.PList [AST.PVar "x"];
    AST.PListCons ([AST.PWildcard], AST.PVar "x"); AST.POr (one (AST.PVar "x"))] in
  let expressions : AST.expr list = [unit; AST.Int64Literal Int64.min_int; AST.Int128Literal (Z.neg (Z.shift_left Z.one 127));
    AST.Int8Literal (-128); AST.Int16Literal (-32768); AST.Int32Literal Int32.min_int; AST.UInt8Literal 255;
    AST.UInt16Literal 65535; AST.UInt32Literal 4294967295L; AST.UInt64Literal (-1L); AST.UInt128Literal (Z.pred (Z.shift_left Z.one 128));
    AST.BigIntLiteral (Z.shift_left Z.one 256); AST.BoolLiteral true; AST.StringLiteral source; AST.CharLiteral source; AST.FloatLiteral (-0.);
    AST.InterpolatedString [AST.StringText source; AST.StringExpr var]; AST.BinOp (AST.Add, var, unit); AST.UnaryOp (AST.Not, var);
    AST.Let (AST.LPTuple (AST.LPVariable "x", AST.LPWildcard, [AST.LPUnit]), unit, var);
    AST.Var source; AST.Var "Builtin.testNan"; AST.Var "Builtin.testInfinity"; AST.Var "Builtin.blobEmpty";
    AST.If (var, unit, var); AST.Sequence (unit, var); AST.Apply (var, [], one unit); AST.Apply (var, [AST.TInt64], one unit);
    AST.TupleLiteral [unit; var; unit]; AST.TupleAccess (var, 2); AST.DictLiteral (AST.TString, AST.TInt64, [AST.StringLiteral source, var]);
    AST.RecordLiteral (record, [field 1, var; field 0, unit]); AST.RecordUpdate (var, [field 1, unit]); AST.RecordAccess (var, field 1);
    AST.Constructor (AST.ResolvedConstructor (["a"], "S", [AST.TInt64]), "Choice", [var]);
    AST.ListLiteral [var; unit]; AST.Lambda (one parameter, Some AST.TInt64, var);
    AST.Apply (AST.Lambda (one parameter, None, var), [], one unit); AST.IndirectApply (var, one unit);
    AST.Closure (source, [var]); AST.RuntimeError source; AST.BoundaryRender (source, var);
    AST.TupleLiteral []; AST.TupleLiteral [unit]; AST.Match (unit, []);
    AST.RecordLiteral (record, []); AST.RecordAccess (var, AST.unresolvedRecordFieldReference "field");
    AST.Constructor (AST.UnresolvedConstructor None, "Absent", []); AST.Lambda (one (AST.lambdaParameter (AST.LPVariable "x")), None, var);
    AST.Apply (unit, [AST.TUnit], one unit)] @
    List.map (fun pattern -> AST.Match (var, [{AST.patterns = one pattern; guard = Some var; body = var}])) patterns in
  let lookup = StringOrder.Map.singleton "S.Choice" ("S", [], 17, []) in
  let catalog = includeFunctionNames (List.to_seq [source; "z"; "aa"; "z"]) emptyFunctionCatalog in
  let convert topLevels = ofTypedProgram lookup (StringOrder.Set.singleton "external") emptyTypeCatalog catalog (fun name -> if name = "R" then Some 2 else None) (AST.Program (AST.TypeDef (AST.RecordDef ("R", [], ["field0", AST.TUnit; "field1", AST.TInt64])) :: AST.TypeDef (AST.SumTypeDef ("S", [], [{AST.name = "Choice"; fields = []}])) :: topLevels)) in
  let converted = List.map (fun expr -> convert [AST.Expression ([], expr)]) expressions in
  converted
