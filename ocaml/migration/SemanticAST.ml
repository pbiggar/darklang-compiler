open Dark_compiler

(* Migration-only complete observations appended to the real checked boundary. *)
let unsigned value = if value < 0L then Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)) else Int64.to_string value
let observationScalar kind value = `Assoc ["kind", `String kind; "value", `String value]
let observationUnion typ case fields = `Assoc ["type", `String typ; "case", `String case; "fields", `List fields]
let observationTuple values = `Assoc ["tuple", `List values]
let observationRecord name fields = `Assoc ["record", `String name; "fields", `List (List.map (fun (name, value) -> `List [`String name; value]) fields)]
let observationString value = `String value
let observationInt value = observationScalar "int32" (string_of_int value)
let observationOption encode = function None -> observationUnion "FSharpOption" "None" [] | Some value -> observationUnion "FSharpOption" "Some" [encode value]
let observationBinding id = match AST.MigrationObservation.bindingOrdinal id with
  | Some ordinal -> observationUnion "BindingId" "LocalBindingId" [observationInt ordinal; observationOption observationString (AST.bindingDisplayName id)]
  | None -> observationUnion "BindingId" "TopLevelValueId" [observationString ((match AST.bindingDisplayName id with Some name -> name | None -> Crash.crash "Global binding has no display name"))]
let observationScopeId id = observationUnion "ScopeBoundaryId" "ScopeBoundaryId" [observationInt (AST.MigrationObservation.scopeOrdinal id)]
let observationGroupId id = observationUnion "RecursiveGroupId" "RecursiveGroupId" [observationInt (AST.MigrationObservation.groupOrdinal id)]
let observationMemberId id = observationUnion "RecursiveMemberId" "RecursiveMemberId" [observationInt (AST.MigrationObservation.memberOrdinal id)]

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
and observationRecordReferenceNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.recordReferenceNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "RecordReferenceNode" [
  "SourceTypeName", observationString value.AST.sourceTypeName;
  "ResolvedTypeName", observationString value.AST.resolvedTypeName;
  "TypeArgs", `List (List.map (fun item -> encodeA item) value.AST.typeArgs);
]
and observationRecordFieldReference (value : AST.recordFieldReference) = observationRecord "RecordFieldReference" [
  "SourceFieldName", observationString value.AST.sourceFieldName;
  "ResolvedTypeName", observationOption (fun item -> observationString item) value.AST.resolvedTypeName;
  "ResolvedFieldIndex", observationOption (fun item -> observationInt item) value.AST.resolvedFieldIndex;
]
and observationConstructorReference (value : AST.constructorReference) = match value with
  | AST.UnresolvedConstructor field0 -> observationUnion "ConstructorReference" "UnresolvedConstructor" [observationOption (fun item -> observationString item) field0]
  | AST.ResolvedConstructor (field0, field1, field2) -> observationUnion "ConstructorReference" "ResolvedConstructor" [`List (List.map (fun item -> observationString item) field0); observationString field1; `List (List.map (fun item -> observationSemanticType item) field2)]
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
and observationPattern (value : AST.pattern) = match value with
  | AST.PUnit -> observationUnion "Pattern" "PUnit" []
  | AST.PWildcard -> observationUnion "Pattern" "PWildcard" []
  | AST.PVar field0 -> observationUnion "Pattern" "PVar" [observationString field0]
  | AST.PConstructor (field0, field1) -> observationUnion "Pattern" "PConstructor" [observationString field0; `List (List.map (fun item -> observationPattern item) field1)]
  | AST.PResolvedConstructor (field0, field1, field2, field3) -> observationUnion "Pattern" "PResolvedConstructor" [observationString field0; observationString field1; observationInt field2; `List (List.map (fun item -> observationPattern item) field3)]
  | AST.PInt64 field0 -> observationUnion "Pattern" "PInt64" [observationScalar "int64" (Int64.to_string field0)]
  | AST.PBigInt field0 -> observationUnion "Pattern" "PBigInt" [(fun x -> observationScalar "bigint" (Z.to_string x)) field0]
  | AST.PInt128Literal field0 -> observationUnion "Pattern" "PInt128Literal" [observationScalar "int128" (Z.to_string field0)]
  | AST.PInt8Literal field0 -> observationUnion "Pattern" "PInt8Literal" [observationScalar "int8" (string_of_int field0)]
  | AST.PInt16Literal field0 -> observationUnion "Pattern" "PInt16Literal" [observationScalar "int16" (string_of_int field0)]
  | AST.PInt32Literal field0 -> observationUnion "Pattern" "PInt32Literal" [observationScalar "int32" (Int32.to_string field0)]
  | AST.PUInt8Literal field0 -> observationUnion "Pattern" "PUInt8Literal" [observationScalar "uint8" (string_of_int field0)]
  | AST.PUInt16Literal field0 -> observationUnion "Pattern" "PUInt16Literal" [observationScalar "uint16" (string_of_int field0)]
  | AST.PUInt32Literal field0 -> observationUnion "Pattern" "PUInt32Literal" [observationScalar "uint32" (Int64.to_string field0)]
  | AST.PUInt64Literal field0 -> observationUnion "Pattern" "PUInt64Literal" [observationScalar "uint64" (unsigned field0)]
  | AST.PUInt128Literal field0 -> observationUnion "Pattern" "PUInt128Literal" [observationScalar "uint128" (Z.to_string field0)]
  | AST.PBool field0 -> observationUnion "Pattern" "PBool" [(fun x -> `Bool x) field0]
  | AST.PString field0 -> observationUnion "Pattern" "PString" [observationString field0]
  | AST.PChar field0 -> observationUnion "Pattern" "PChar" [observationString field0]
  | AST.PFloat field0 -> observationUnion "Pattern" "PFloat" [(fun x -> observationScalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float x))) field0]
  | AST.PTuple field0 -> observationUnion "Pattern" "PTuple" [`List (List.map (fun item -> observationPattern item) field0)]
  | AST.PList field0 -> observationUnion "Pattern" "PList" [`List (List.map (fun item -> observationPattern item) field0)]
  | AST.PListCons (field0, field1) -> observationUnion "Pattern" "PListCons" [`List (List.map (fun item -> observationPattern item) field0); observationPattern field1]
  | AST.POr field0 -> observationUnion "Pattern" "POr" [observationNonEmpty (fun item -> observationPattern item) field0]
and observationLetPattern (value : AST.letPattern) = match value with
  | AST.LPUnit -> observationUnion "LetPattern" "LPUnit" []
  | AST.LPWildcard -> observationUnion "LetPattern" "LPWildcard" []
  | AST.LPVariable field0 -> observationUnion "LetPattern" "LPVariable" [observationString field0]
  | AST.LPTuple (field0, field1, field2) -> observationUnion "LetPattern" "LPTuple" [observationLetPattern field0; observationLetPattern field1; `List (List.map (fun item -> observationLetPattern item) field2)]
and observationLambdaParameterNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.lambdaParameterNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "LambdaParameterNode" [
  "Pattern", observationLetPattern value.AST.pattern;
  "SourceAnnotation", observationOption (fun item -> encodeA item) value.AST.sourceAnnotation;
  "InferredType", observationOption (fun item -> encodeA item) value.AST.inferredType;
]
and observationBinderStructure (value : AST.binderStructure) = match value with
  | AST.LetBinderPatterns field0 -> observationUnion "BinderStructure" "LetBinderPatterns" [`List (List.map (fun item -> observationLetPattern item) field0)]
  | AST.MatchBinderPattern field0 -> observationUnion "BinderStructure" "MatchBinderPattern" [observationPattern field0]
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
and observationRecursiveDependencyKind (value : AST.recursiveDependencyKind) = match value with
  | AST.DelayedCallableDependency -> observationUnion "RecursiveDependencyKind" "DelayedCallableDependency" []
  | AST.EagerValueDependency -> observationUnion "RecursiveDependencyKind" "EagerValueDependency" []
  | AST.TypeAliasDependency -> observationUnion "RecursiveDependencyKind" "TypeAliasDependency" []
and observationRecursiveCandidate (value : AST.recursiveCandidate) = observationRecord "RecursiveCandidate" [
  "SourceName", observationString value.AST.sourceName;
  "Kind", observationRecursiveMemberKind value.AST.kind;
]
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
and observationTypedRecursiveMember (value : AST.typedRecursiveMember) = observationRecord "TypedRecursiveMember" [
  "Resolved", observationResolvedRecursiveMember value.AST.resolved;
  "MonomorphicType", observationSemanticType value.AST.monomorphicType;
]
and observationLoweredRecursiveMember (value : AST.loweredRecursiveMember) = observationRecord "LoweredRecursiveMember" [
  "Typed", observationTypedRecursiveMember value.AST.typed;
  "EnvironmentIndex", observationInt value.AST.environmentIndex;
]
and observationParsedRecursiveGroup (value : AST.parsedRecursiveGroup) = observationRecord "ParsedRecursiveGroup" [
  "Boundary", observationScopeId value.AST.boundary;
  "Members", observationNonEmpty (fun item -> observationParsedRecursiveMember item) value.AST.members;
]
and observationResolvedRecursiveGroup (value : AST.resolvedRecursiveGroup) = observationRecord "ResolvedRecursiveGroup" [
  "Group", observationGroupId value.AST.group;
  "Members", observationNonEmpty (fun item -> observationResolvedRecursiveMember item) value.AST.members;
]
and observationTypedRecursiveGroup (value : AST.typedRecursiveGroup) = observationRecord "TypedRecursiveGroup" [
  "Group", observationGroupId value.AST.group;
  "Members", observationNonEmpty (fun item -> observationTypedRecursiveMember item) value.AST.members;
]
and observationLoweredRecursiveGroup (value : AST.loweredRecursiveGroup) = observationRecord "LoweredRecursiveGroup" [
  "Group", observationGroupId value.AST.group;
  "Members", observationNonEmpty (fun item -> observationLoweredRecursiveMember item) value.AST.members;
]
and observationRecursiveBindingInfo (value : AST.recursiveBindingInfo) = match value with
  | AST.RecursiveBindingCandidate field0 -> observationUnion "RecursiveBindingInfo" "RecursiveBindingCandidate" [observationRecursiveCandidate field0]
  | AST.ParsedRecursiveBinding field0 -> observationUnion "RecursiveBindingInfo" "ParsedRecursiveBinding" [observationParsedRecursiveMember field0]
  | AST.ResolvedRecursiveBinding field0 -> observationUnion "RecursiveBindingInfo" "ResolvedRecursiveBinding" [observationResolvedRecursiveMember field0]
  | AST.TypedRecursiveBinding field0 -> observationUnion "RecursiveBindingInfo" "TypedRecursiveBinding" [observationTypedRecursiveMember field0]
and observationStringPartNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.stringPartNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.StringText field0 -> observationUnion "StringPartNode" "StringText" [observationString field0]
  | AST.StringExpr field0 -> observationUnion "StringPartNode" "StringExpr" [observationExprNode (fun item -> encodeA item) field0]
and observationExprNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.exprNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.UnitLiteral -> observationUnion "ExprNode" "UnitLiteral" []
  | AST.Int64Literal field0 -> observationUnion "ExprNode" "Int64Literal" [observationScalar "int64" (Int64.to_string field0)]
  | AST.Int128Literal field0 -> observationUnion "ExprNode" "Int128Literal" [observationScalar "int128" (Z.to_string field0)]
  | AST.Int8Literal field0 -> observationUnion "ExprNode" "Int8Literal" [observationScalar "int8" (string_of_int field0)]
  | AST.Int16Literal field0 -> observationUnion "ExprNode" "Int16Literal" [observationScalar "int16" (string_of_int field0)]
  | AST.Int32Literal field0 -> observationUnion "ExprNode" "Int32Literal" [observationScalar "int32" (Int32.to_string field0)]
  | AST.UInt8Literal field0 -> observationUnion "ExprNode" "UInt8Literal" [observationScalar "uint8" (string_of_int field0)]
  | AST.UInt16Literal field0 -> observationUnion "ExprNode" "UInt16Literal" [observationScalar "uint16" (string_of_int field0)]
  | AST.UInt32Literal field0 -> observationUnion "ExprNode" "UInt32Literal" [observationScalar "uint32" (Int64.to_string field0)]
  | AST.UInt64Literal field0 -> observationUnion "ExprNode" "UInt64Literal" [observationScalar "uint64" (unsigned field0)]
  | AST.UInt128Literal field0 -> observationUnion "ExprNode" "UInt128Literal" [observationScalar "uint128" (Z.to_string field0)]
  | AST.BigIntLiteral field0 -> observationUnion "ExprNode" "BigIntLiteral" [(fun x -> observationScalar "bigint" (Z.to_string x)) field0]
  | AST.BoolLiteral field0 -> observationUnion "ExprNode" "BoolLiteral" [(fun x -> `Bool x) field0]
  | AST.StringLiteral field0 -> observationUnion "ExprNode" "StringLiteral" [observationString field0]
  | AST.CharLiteral field0 -> observationUnion "ExprNode" "CharLiteral" [observationString field0]
  | AST.FloatLiteral field0 -> observationUnion "ExprNode" "FloatLiteral" [(fun x -> observationScalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float x))) field0]
  | AST.InterpolatedString field0 -> observationUnion "ExprNode" "InterpolatedString" [`List (List.map (fun item -> observationStringPartNode (fun item -> encodeA item) item) field0)]
  | AST.BinOp (field0, field1, field2) -> observationUnion "ExprNode" "BinOp" [observationBinOp field0; observationExprNode (fun item -> encodeA item) field1; observationExprNode (fun item -> encodeA item) field2]
  | AST.UnaryOp (field0, field1) -> observationUnion "ExprNode" "UnaryOp" [observationUnaryOp field0; observationExprNode (fun item -> encodeA item) field1]
  | AST.Let (field0, field1, field2) -> observationUnion "ExprNode" "Let" [observationLetPattern field0; observationExprNode (fun item -> encodeA item) field1; observationExprNode (fun item -> encodeA item) field2]
  | AST.RecursiveLet (field0, field1, field2) -> observationUnion "ExprNode" "RecursiveLet" [observationRecursiveBindingInfo field0; observationExprNode (fun item -> encodeA item) field1; observationExprNode (fun item -> encodeA item) field2]
  | AST.Var field0 -> observationUnion "ExprNode" "Var" [observationString field0]
  | AST.If (field0, field1, field2) -> observationUnion "ExprNode" "If" [observationExprNode (fun item -> encodeA item) field0; observationExprNode (fun item -> encodeA item) field1; observationExprNode (fun item -> encodeA item) field2]
  | AST.Sequence (field0, field1) -> observationUnion "ExprNode" "Sequence" [observationExprNode (fun item -> encodeA item) field0; observationExprNode (fun item -> encodeA item) field1]
  | AST.Apply (field0, field1, field2) -> observationUnion "ExprNode" "Apply" [observationExprNode (fun item -> encodeA item) field0; `List (List.map (fun item -> encodeA item) field1); observationNonEmpty (fun item -> observationExprNode (fun item -> encodeA item) item) field2]
  | AST.TupleLiteral field0 -> observationUnion "ExprNode" "TupleLiteral" [`List (List.map (fun item -> observationExprNode (fun item -> encodeA item) item) field0)]
  | AST.TupleAccess (field0, field1) -> observationUnion "ExprNode" "TupleAccess" [observationExprNode (fun item -> encodeA item) field0; observationInt field1]
  | AST.DictLiteral (field0, field1, field2) -> observationUnion "ExprNode" "DictLiteral" [encodeA field0; encodeA field1; `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationExprNode (fun item -> encodeA item) part0; observationExprNode (fun item -> encodeA item) part1])) field2)]
  | AST.RecordLiteral (field0, field1) -> observationUnion "ExprNode" "RecordLiteral" [observationRecordReferenceNode (fun item -> encodeA item) field0; `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationRecordFieldReference part0; observationExprNode (fun item -> encodeA item) part1])) field1)]
  | AST.RecordUpdate (field0, field1) -> observationUnion "ExprNode" "RecordUpdate" [observationExprNode (fun item -> encodeA item) field0; `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationRecordFieldReference part0; observationExprNode (fun item -> encodeA item) part1])) field1)]
  | AST.RecordAccess (field0, field1) -> observationUnion "ExprNode" "RecordAccess" [observationExprNode (fun item -> encodeA item) field0; observationRecordFieldReference field1]
  | AST.Constructor (field0, field1, field2) -> observationUnion "ExprNode" "Constructor" [observationConstructorReference field0; observationString field1; `List (List.map (fun item -> observationExprNode (fun item -> encodeA item) item) field2)]
  | AST.Match (field0, field1) -> observationUnion "ExprNode" "Match" [observationExprNode (fun item -> encodeA item) field0; `List (List.map (fun item -> observationMatchCaseNode (fun item -> encodeA item) item) field1)]
  | AST.ListLiteral field0 -> observationUnion "ExprNode" "ListLiteral" [`List (List.map (fun item -> observationExprNode (fun item -> encodeA item) item) field0)]
  | AST.Lambda (field0, field1, field2) -> observationUnion "ExprNode" "Lambda" [observationNonEmpty (fun item -> observationLambdaParameterNode (fun item -> encodeA item) item) field0; observationOption (fun item -> encodeA item) field1; observationExprNode (fun item -> encodeA item) field2]
  | AST.IndirectApply (field0, field1) -> observationUnion "ExprNode" "IndirectApply" [observationExprNode (fun item -> encodeA item) field0; observationNonEmpty (fun item -> observationExprNode (fun item -> encodeA item) item) field1]
  | AST.Closure (field0, field1) -> observationUnion "ExprNode" "Closure" [observationString field0; `List (List.map (fun item -> observationExprNode (fun item -> encodeA item) item) field1)]
  | AST.RuntimeError field0 -> observationUnion "ExprNode" "RuntimeError" [observationString field0]
  | AST.BoundaryRender (field0, field1) -> observationUnion "ExprNode" "BoundaryRender" [observationString field0; observationExprNode (fun item -> encodeA item) field1]
and observationMatchCaseNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.matchCaseNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "MatchCaseNode" [
  "Patterns", observationNonEmpty (fun item -> observationPattern item) value.AST.patterns;
  "Guard", observationOption (fun item -> observationExprNode (fun item -> encodeA item) item) value.AST.guard;
  "Body", observationExprNode (fun item -> encodeA item) value.AST.body;
]
and observationFunctionDefNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.functionDefNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "FunctionDefNode" [
  "Name", observationString value.AST.name;
  "TypeParams", `List (List.map (fun item -> observationString item) value.AST.typeParams);
  "Params", observationNonEmpty (fun item -> (let part0, part1 = item in observationTuple [observationString part0; encodeA part1])) value.AST.params;
  "ReturnType", encodeA value.AST.returnType;
  "Body", observationExprNode (fun item -> encodeA item) value.AST.body;
  "Recursion", observationOption (fun item -> observationRecursiveBindingInfo item) value.AST.recursion;
]
and observationVariantNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.variantNode -> Yojson.Basic.t = fun encodeA value -> observationRecord "VariantNode" [
  "Name", observationString value.AST.name;
  "Fields", `List (List.map (fun item -> encodeA item) value.AST.fields);
]
and observationTypeDefNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.typeDefNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.RecordDef (field0, field1, field2) -> observationUnion "TypeDefNode" "RecordDef" [observationString field0; `List (List.map (fun item -> observationString item) field1); `List (List.map (fun item -> (let part0, part1 = item in observationTuple [observationString part0; encodeA part1])) field2)]
  | AST.SumTypeDef (field0, field1, field2) -> observationUnion "TypeDefNode" "SumTypeDef" [observationString field0; `List (List.map (fun item -> observationString item) field1); `List (List.map (fun item -> observationVariantNode (fun item -> encodeA item) item) field2)]
  | AST.TypeAlias (field0, field1, field2) -> observationUnion "TypeDefNode" "TypeAlias" [observationString field0; `List (List.map (fun item -> observationString item) field1); encodeA field2]
and observationValueDefNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.valueDefNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.UncheckedValueDef (field0, field1) -> observationUnion "ValueDefNode" "UncheckedValueDef" [observationString field0; observationExprNode (fun item -> encodeA item) field1]
  | AST.CheckedValueDef (field0, field1, field2) -> observationUnion "ValueDefNode" "CheckedValueDef" [observationString field0; encodeA field1; observationExprNode (fun item -> encodeA item) field2]
and observationTopLevelNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.topLevelNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.FunctionDef field0 -> observationUnion "TopLevelNode" "FunctionDef" [observationFunctionDefNode (fun item -> encodeA item) field0]
  | AST.TypeDef field0 -> observationUnion "TopLevelNode" "TypeDef" [observationTypeDefNode (fun item -> encodeA item) field0]
  | AST.ValueDef field0 -> observationUnion "TopLevelNode" "ValueDef" [observationValueDefNode (fun item -> encodeA item) field0]
  | AST.Expression (field0, field1) -> observationUnion "TopLevelNode" "Expression" [`List (List.map (fun item -> observationString item) field0); observationExprNode (fun item -> encodeA item) field1]
and observationProgramNode : 'a. ('a -> Yojson.Basic.t) -> 'a AST.programNode -> Yojson.Basic.t = fun encodeA value -> match value with
  | AST.Program field0 -> observationUnion "ProgramNode" "Program" [`List (List.map (fun item -> observationTopLevelNode (fun item -> encodeA item) item) field0)]
and observationModuleFunc (value : AST.moduleFunc) = observationRecord "ModuleFunc" [
  "Name", observationString value.AST.name;
  "TypeParams", `List (List.map (fun item -> observationString item) value.AST.typeParams);
  "ParamTypes", `List (List.map (fun item -> observationSemanticType item) value.AST.paramTypes);
  "ReturnType", observationSemanticType value.AST.returnType;
]
and observationModuleDef (value : AST.moduleDef) = observationRecord "ModuleDef" [
  "Name", observationString value.AST.name;
  "Functions", `List (List.map (fun item -> observationModuleFunc item) value.AST.functions);
]
let semanticType = observationSemanticType
let expr = observationExprNode observationSemanticType
let typeDef = observationTypeDefNode observationSemanticType
