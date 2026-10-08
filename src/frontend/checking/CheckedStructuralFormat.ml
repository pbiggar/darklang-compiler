(* Typed structural formatting of the complete checked expression schema.
   Opaque identity descriptions come from AST's diagnostic boundary. *)
[@@@warning "-4"]

open! CheckedAST
open! StructuralValue

let unsigned value =
  Z.to_string
    (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)
     else Z.of_int64 value)

let observationScalar kind text =
  let suffix =
    match kind with
    | "int8" -> "y"
    | "uint8" -> "uy"
    | "int16" -> "s"
    | "uint16" -> "us"
    | "int64" -> "L"
    | "uint64" -> "UL"
    | "uint32" -> "u"
    | _ -> ""
  in
  if kind = "float64" then
    Scalar
      (FloatFormat.structural
         (Int64.float_of_bits (Int64.of_string ("0x" ^ text))))
  else Scalar (text ^ suffix)

let observationUnion _ case fields = Union (case, fields)
let observationTuple fields = Tuple fields
let observationRecord _ fields = Record fields
let observationString value = Text value
let observationInt value = Scalar (string_of_int value)

let observationOption encode = function
  | None -> Union ("None", [])
  | Some value -> Union ("Some", [ encode value ])

let opaque encode value = Scalar (StructuralFormat.format (encode value))
let observationBinding = opaque AST.DiagnosticFormatting.binding
let observationFunction = opaque AST.DiagnosticFormatting.func
let observationTypeId = opaque AST.DiagnosticFormatting.typ
let observationConstructorId = opaque AST.DiagnosticFormatting.constructor
let observationFieldId = opaque AST.DiagnosticFormatting.field
let observationScopeId = opaque AST.DiagnosticFormatting.scope
let observationGroupId = opaque AST.DiagnosticFormatting.group
let observationMemberId = opaque AST.DiagnosticFormatting.memberId
let privateBinding = AST.DiagnosticFormatting.binding
let privateFunction = AST.DiagnosticFormatting.func
let privateTypeId = AST.DiagnosticFormatting.typ
let privateConstructorId = AST.DiagnosticFormatting.constructor
let privateFieldId = AST.DiagnosticFormatting.field
let privateScopeId = AST.DiagnosticFormatting.scope
let privateGroupId = AST.DiagnosticFormatting.group
let privateMemberId = AST.DiagnosticFormatting.memberId

let rec privateNonEmpty :
    'a.
    ('a -> StructuralValue.value) -> 'a NonEmptyList.t -> StructuralValue.value
    =
 fun encode value ->
  observationRecord "NonEmptyList"
    [
      ("Head", encode value.NonEmptyList.head);
      ("Tail", Sequence (List.map encode value.NonEmptyList.tail));
    ]

and privateSemanticType (value : AST.semanticType) =
  match value with
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
  | AST.TFunction (field0, field1) ->
      observationUnion "SemanticType" "TFunction"
        [
          Sequence (List.map (fun item -> privateSemanticType item) field0);
          privateSemanticType field1;
        ]
  | AST.TTuple field0 ->
      observationUnion "SemanticType" "TTuple"
        [ Sequence (List.map (fun item -> privateSemanticType item) field0) ]
  | AST.TRecord (field0, field1) ->
      observationUnion "SemanticType" "TRecord"
        [
          observationString field0;
          Sequence (List.map (fun item -> privateSemanticType item) field1);
        ]
  | AST.TSum (field0, field1) ->
      observationUnion "SemanticType" "TSum"
        [
          observationString field0;
          Sequence (List.map (fun item -> privateSemanticType item) field1);
        ]
  | AST.TList field0 ->
      observationUnion "SemanticType" "TList" [ privateSemanticType field0 ]
  | AST.TStream field0 ->
      observationUnion "SemanticType" "TStream" [ privateSemanticType field0 ]
  | AST.TVar field0 ->
      observationUnion "SemanticType" "TVar" [ observationString field0 ]
  | AST.TInferenceVar (field0, field1) ->
      observationUnion "SemanticType" "TInferenceVar"
        [ observationString field0; observationString field1 ]
  | AST.TInternalRawPtr -> observationUnion "SemanticType" "TInternalRawPtr" []
  | AST.TDict (field0, field1) ->
      observationUnion "SemanticType" "TDict"
        [ privateSemanticType field0; privateSemanticType field1 ]

and privateBinOp (value : AST.binOp) =
  match value with
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

and privateUnaryOp (value : AST.unaryOp) =
  match value with
  | AST.Neg -> observationUnion "UnaryOp" "Neg" []
  | AST.Not -> observationUnion "UnaryOp" "Not" []
  | AST.BitNot -> observationUnion "UnaryOp" "BitNot" []

and privateRecursiveMemberKind (value : AST.recursiveMemberKind) =
  match value with
  | AST.TopLevelFunctionMember ->
      observationUnion "RecursiveMemberKind" "TopLevelFunctionMember" []
  | AST.NamedLocalFunctionMember ->
      observationUnion "RecursiveMemberKind" "NamedLocalFunctionMember" []
  | AST.DirectLambdaValueMember ->
      observationUnion "RecursiveMemberKind" "DirectLambdaValueMember" []

and privateRecursiveAvailability (value : AST.recursiveAvailability) =
  match value with
  | AST.OrdinaryBinding ->
      observationUnion "RecursiveAvailability" "OrdinaryBinding" []
  | AST.SelfRecursiveMember ->
      observationUnion "RecursiveAvailability" "SelfRecursiveMember" []
  | AST.MutualRecursiveMember ->
      observationUnion "RecursiveAvailability" "MutualRecursiveMember" []
  | AST.CompletedGroupMember ->
      observationUnion "RecursiveAvailability" "CompletedGroupMember" []
  | AST.ImportedGroupMember ->
      observationUnion "RecursiveAvailability" "ImportedGroupMember" []

and privateParsedRecursiveMember (value : AST.parsedRecursiveMember) =
  observationRecord "ParsedRecursiveMember"
    [
      ("Binding", privateBinding value.AST.binding);
      ("Boundary", privateScopeId value.AST.boundary);
      ("Member", privateMemberId value.AST.member);
      ("SourceName", observationString value.AST.sourceName);
      ("Kind", privateRecursiveMemberKind value.AST.kind);
    ]

and privateResolvedRecursiveMember (value : AST.resolvedRecursiveMember) =
  observationRecord "ResolvedRecursiveMember"
    [
      ("Parsed", privateParsedRecursiveMember value.AST.parsed);
      ("Group", privateGroupId value.AST.group);
      ("GroupIndex", observationInt value.AST.groupIndex);
      ("Availability", privateRecursiveAvailability value.AST.availability);
    ]

and privateLetPattern (value : letPattern) =
  match value with
  | LPUnit -> observationUnion "LetPattern" "LPUnit" []
  | LPWildcard -> observationUnion "LetPattern" "LPWildcard" []
  | LPVariable field0 ->
      observationUnion "LetPattern" "LPVariable" [ privateBinding field0 ]
  | LPTuple (field0, field1, field2) ->
      observationUnion "LetPattern" "LPTuple"
        [
          privateLetPattern field0;
          privateLetPattern field1;
          Sequence (List.map (fun item -> privateLetPattern item) field2);
        ]

and privateTupleElements :
    'a.
    ('a -> StructuralValue.value) -> 'a tupleElements -> StructuralValue.value =
 fun encodeA value ->
  observationRecord "TupleElements"
    [
      ("First", encodeA value.first);
      ("Second", encodeA value.second);
      ("Rest", Sequence (List.map (fun item -> encodeA item) value.rest));
    ]

and privateCheckedType value =
  observationUnion "CheckedType" "CheckedType"
    [ privateSemanticType (CheckedAST.semanticType value) ]

and privateRecursiveMember (value : recursiveMember) =
  observationRecord "RecursiveMember"
    [
      ("Resolved", privateResolvedRecursiveMember value.resolved);
      ("MonomorphicType", privateCheckedType value.monomorphicType);
    ]

and privatePattern (value : pattern) =
  match value with
  | PUnit -> observationUnion "Pattern" "PUnit" []
  | PWildcard -> observationUnion "Pattern" "PWildcard" []
  | PVariable field0 ->
      observationUnion "Pattern" "PVariable" [ privateBinding field0 ]
  | PConstructor (field0, field1) ->
      observationUnion "Pattern" "PConstructor"
        [
          privateConstructorId field0;
          Sequence (List.map (fun item -> privatePattern item) field1);
        ]
  | PInt64 field0 ->
      observationUnion "Pattern" "PInt64"
        [ observationScalar "int64" (Int64.to_string field0) ]
  | PBigInt field0 ->
      observationUnion "Pattern" "PBigInt"
        [ (fun x -> observationScalar "bigint" (Z.to_string x)) field0 ]
  | PInt128Literal field0 ->
      observationUnion "Pattern" "PInt128Literal"
        [ observationScalar "int128" (Z.to_string field0) ]
  | PInt8Literal field0 ->
      observationUnion "Pattern" "PInt8Literal"
        [ observationScalar "int8" (string_of_int field0) ]
  | PInt16Literal field0 ->
      observationUnion "Pattern" "PInt16Literal"
        [ observationScalar "int16" (string_of_int field0) ]
  | PInt32Literal field0 ->
      observationUnion "Pattern" "PInt32Literal"
        [ observationScalar "int32" (Int32.to_string field0) ]
  | PUInt8Literal field0 ->
      observationUnion "Pattern" "PUInt8Literal"
        [ observationScalar "uint8" (string_of_int field0) ]
  | PUInt16Literal field0 ->
      observationUnion "Pattern" "PUInt16Literal"
        [ observationScalar "uint16" (string_of_int field0) ]
  | PUInt32Literal field0 ->
      observationUnion "Pattern" "PUInt32Literal"
        [ observationScalar "uint32" (Int64.to_string field0) ]
  | PUInt64Literal field0 ->
      observationUnion "Pattern" "PUInt64Literal"
        [ observationScalar "uint64" (unsigned field0) ]
  | PUInt128Literal field0 ->
      observationUnion "Pattern" "PUInt128Literal"
        [ observationScalar "uint128" (Z.to_string field0) ]
  | PBool field0 ->
      observationUnion "Pattern" "PBool"
        [ (fun x -> Scalar (if x then "true" else "false")) field0 ]
  | PString field0 ->
      observationUnion "Pattern" "PString" [ observationString field0 ]
  | PChar field0 ->
      observationUnion "Pattern" "PChar" [ observationString field0 ]
  | PFloat field0 ->
      observationUnion "Pattern" "PFloat"
        [
          (fun x ->
            observationScalar "float64"
              (Printf.sprintf "%016Lx" (Int64.bits_of_float x)))
            field0;
        ]
  | PTuple field0 ->
      observationUnion "Pattern" "PTuple"
        [ Sequence (List.map (fun item -> privatePattern item) field0) ]
  | PList field0 ->
      observationUnion "Pattern" "PList"
        [ Sequence (List.map (fun item -> privatePattern item) field0) ]
  | PListCons (field0, field1) ->
      observationUnion "Pattern" "PListCons"
        [
          Sequence (List.map (fun item -> privatePattern item) field0);
          privatePattern field1;
        ]
  | POr field0 ->
      observationUnion "Pattern" "POr"
        [ privateNonEmpty (fun item -> privatePattern item) field0 ]

and privateLambdaParameter (value : lambdaParameter) =
  observationRecord "LambdaParameter"
    [
      ("Pattern", privateLetPattern value.pattern);
      ("Type", privateCheckedType value.typ);
    ]

and privateRecordReference (value : recordReference) =
  observationRecord "RecordReference"
    [
      ("TypeId", privateTypeId value.typeId);
      ( "TypeArgs",
        Sequence (List.map (fun item -> privateCheckedType item) value.typeArgs)
      );
    ]

and privateConstructorReference (value : constructorReference) =
  observationRecord "ConstructorReference"
    [
      ("TypeId", privateTypeId value.typeId);
      ("ConstructorId", privateConstructorId value.constructorId);
      ( "TypeArgs",
        Sequence (List.map (fun item -> privateCheckedType item) value.typeArgs)
      );
    ]

and privateRecordFields :
    'a.
    ('a -> StructuralValue.value) -> 'a recordFields -> StructuralValue.value =
 fun encodeA value ->
  observationUnion "RecordFields" "RecordFields"
    [
      Sequence
        (List.map
           (fun (field, value) ->
             observationTuple [ privateFieldId field; encodeA value ])
           (CheckedAST.recordFieldsInSourceOrder value));
    ]

and privateStringPart (value : stringPart) =
  match value with
  | StringText field0 ->
      observationUnion "StringPart" "StringText" [ observationString field0 ]
  | StringExpr field0 ->
      observationUnion "StringPart" "StringExpr" [ privateExpr field0 ]

and privateExpr (value : expr) =
  match value with
  | UnitLiteral -> observationUnion "Expr" "UnitLiteral" []
  | Int64Literal field0 ->
      observationUnion "Expr" "Int64Literal"
        [ observationScalar "int64" (Int64.to_string field0) ]
  | Int128Literal field0 ->
      observationUnion "Expr" "Int128Literal"
        [ observationScalar "int128" (Z.to_string field0) ]
  | Int8Literal field0 ->
      observationUnion "Expr" "Int8Literal"
        [ observationScalar "int8" (string_of_int field0) ]
  | Int16Literal field0 ->
      observationUnion "Expr" "Int16Literal"
        [ observationScalar "int16" (string_of_int field0) ]
  | Int32Literal field0 ->
      observationUnion "Expr" "Int32Literal"
        [ observationScalar "int32" (Int32.to_string field0) ]
  | UInt8Literal field0 ->
      observationUnion "Expr" "UInt8Literal"
        [ observationScalar "uint8" (string_of_int field0) ]
  | UInt16Literal field0 ->
      observationUnion "Expr" "UInt16Literal"
        [ observationScalar "uint16" (string_of_int field0) ]
  | UInt32Literal field0 ->
      observationUnion "Expr" "UInt32Literal"
        [ observationScalar "uint32" (Int64.to_string field0) ]
  | UInt64Literal field0 ->
      observationUnion "Expr" "UInt64Literal"
        [ observationScalar "uint64" (unsigned field0) ]
  | UInt128Literal field0 ->
      observationUnion "Expr" "UInt128Literal"
        [ observationScalar "uint128" (Z.to_string field0) ]
  | BigIntLiteral field0 ->
      observationUnion "Expr" "BigIntLiteral"
        [ (fun x -> observationScalar "bigint" (Z.to_string x)) field0 ]
  | BoolLiteral field0 ->
      observationUnion "Expr" "BoolLiteral"
        [ (fun x -> Scalar (if x then "true" else "false")) field0 ]
  | StringLiteral field0 ->
      observationUnion "Expr" "StringLiteral" [ observationString field0 ]
  | BlobLiteral field0 ->
      observationUnion "Expr" "BlobLiteral" [ observationString field0 ]
  | CharLiteral field0 ->
      observationUnion "Expr" "CharLiteral" [ observationString field0 ]
  | FloatLiteral field0 ->
      observationUnion "Expr" "FloatLiteral"
        [
          (fun x ->
            observationScalar "float64"
              (Printf.sprintf "%016Lx" (Int64.bits_of_float x)))
            field0;
        ]
  | InterpolatedString field0 ->
      observationUnion "Expr" "InterpolatedString"
        [ Sequence (List.map (fun item -> privateStringPart item) field0) ]
  | BinOp (field0, field1, field2) ->
      observationUnion "Expr" "BinOp"
        [ privateBinOp field0; privateExpr field1; privateExpr field2 ]
  | UnaryOp (field0, field1) ->
      observationUnion "Expr" "UnaryOp"
        [ privateUnaryOp field0; privateExpr field1 ]
  | Let (field0, field1, field2) ->
      observationUnion "Expr" "Let"
        [ privateLetPattern field0; privateExpr field1; privateExpr field2 ]
  | RecursiveLet (field0, field1, field2) ->
      observationUnion "Expr" "RecursiveLet"
        [
          privateRecursiveMember field0; privateExpr field1; privateExpr field2;
        ]
  | Local field0 -> observationUnion "Expr" "Local" [ privateBinding field0 ]
  | If (field0, field1, field2) ->
      observationUnion "Expr" "If"
        [ privateExpr field0; privateExpr field1; privateExpr field2 ]
  | Sequence (field0, field1) ->
      observationUnion "Expr" "Sequence"
        [ privateExpr field0; privateExpr field1 ]
  | Call (field0, field1) ->
      observationUnion "Expr" "Call"
        [
          privateFunction field0;
          privateNonEmpty (fun item -> privateExpr item) field1;
        ]
  | TypeApp (field0, field1, field2) ->
      observationUnion "Expr" "TypeApp"
        [
          privateFunction field0;
          Sequence (List.map (fun item -> privateCheckedType item) field1);
          privateNonEmpty (fun item -> privateExpr item) field2;
        ]
  | TupleLiteral field0 ->
      observationUnion "Expr" "TupleLiteral"
        [ privateTupleElements (fun item -> privateExpr item) field0 ]
  | TupleAccess (field0, field1) ->
      observationUnion "Expr" "TupleAccess"
        [ privateExpr field0; observationInt field1 ]
  | DictLiteral (field0, field1, field2) ->
      observationUnion "Expr" "DictLiteral"
        [
          privateCheckedType field0;
          privateCheckedType field1;
          Sequence
            (List.map
               (fun item ->
                 let part0, part1 = item in
                 observationTuple [ privateExpr part0; privateExpr part1 ])
               field2);
        ]
  | RecordLiteral (field0, field1) ->
      observationUnion "Expr" "RecordLiteral"
        [
          privateRecordReference field0;
          privateRecordFields (fun item -> privateExpr item) field1;
        ]
  | RecordUpdate (field0, field1) ->
      observationUnion "Expr" "RecordUpdate"
        [
          privateExpr field0;
          Sequence
            (List.map
               (fun item ->
                 let part0, part1 = item in
                 observationTuple [ privateFieldId part0; privateExpr part1 ])
               field1);
        ]
  | RecordAccess (field0, field1) ->
      observationUnion "Expr" "RecordAccess"
        [ privateExpr field0; privateFieldId field1 ]
  | Constructor (field0, field1) ->
      observationUnion "Expr" "Constructor"
        [
          privateConstructorReference field0;
          Sequence (List.map (fun item -> privateExpr item) field1);
        ]
  | Match (field0, field1) ->
      observationUnion "Expr" "Match"
        [
          privateExpr field0;
          privateNonEmpty (fun item -> privateMatchCase item) field1;
        ]
  | ListLiteral field0 ->
      observationUnion "Expr" "ListLiteral"
        [ Sequence (List.map (fun item -> privateExpr item) field0) ]
  | Lambda (field0, field1, field2) ->
      observationUnion "Expr" "Lambda"
        [
          privateNonEmpty (fun item -> privateLambdaParameter item) field0;
          observationOption (fun item -> privateCheckedType item) field1;
          privateExpr field2;
        ]
  | Apply (field0, field1) ->
      observationUnion "Expr" "Apply"
        [
          privateExpr field0;
          privateNonEmpty (fun item -> privateExpr item) field1;
        ]
  | IndirectApply (field0, field1) ->
      observationUnion "Expr" "IndirectApply"
        [
          privateExpr field0;
          privateNonEmpty (fun item -> privateExpr item) field1;
        ]
  | FuncRef field0 ->
      observationUnion "Expr" "FuncRef" [ privateFunction field0 ]
  | GenericFuncRef (id, args, typ) ->
      observationUnion "Expr" "GenericFuncRef"
        [
          privateFunction id;
          Sequence (List.map privateCheckedType args);
          privateCheckedType typ;
        ]
  | Closure (field0, field1) ->
      observationUnion "Expr" "Closure"
        [
          privateFunction field0;
          Sequence (List.map (fun item -> privateExpr item) field1);
        ]
  | RuntimeError field0 ->
      observationUnion "Expr" "RuntimeError" [ observationString field0 ]
  | BoundaryRender (field0, field1) ->
      observationUnion "Expr" "BoundaryRender"
        [ privateFunction field0; privateExpr field1 ]

and privateMatchCase (value : matchCase) =
  observationRecord "MatchCase"
    [
      ( "Patterns",
        privateNonEmpty (fun item -> privatePattern item) value.patterns );
      ("Guard", observationOption (fun item -> privateExpr item) value.guard);
      ("Body", privateExpr value.body);
    ]

let rec observationNonEmpty :
    'a.
    ('a -> StructuralValue.value) -> 'a NonEmptyList.t -> StructuralValue.value
    =
 fun encode value ->
  observationRecord "NonEmptyList"
    [
      ("Head", encode value.NonEmptyList.head);
      ("Tail", Sequence (List.map encode value.NonEmptyList.tail));
    ]

and observationBinOp (value : AST.binOp) =
  match value with
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

and observationUnaryOp (value : AST.unaryOp) =
  match value with
  | AST.Neg -> observationUnion "UnaryOp" "Neg" []
  | AST.Not -> observationUnion "UnaryOp" "Not" []
  | AST.BitNot -> observationUnion "UnaryOp" "BitNot" []

and observationRecursiveMemberKind (value : AST.recursiveMemberKind) =
  match value with
  | AST.TopLevelFunctionMember ->
      observationUnion "RecursiveMemberKind" "TopLevelFunctionMember" []
  | AST.NamedLocalFunctionMember ->
      observationUnion "RecursiveMemberKind" "NamedLocalFunctionMember" []
  | AST.DirectLambdaValueMember ->
      observationUnion "RecursiveMemberKind" "DirectLambdaValueMember" []

and observationRecursiveAvailability (value : AST.recursiveAvailability) =
  match value with
  | AST.OrdinaryBinding ->
      observationUnion "RecursiveAvailability" "OrdinaryBinding" []
  | AST.SelfRecursiveMember ->
      observationUnion "RecursiveAvailability" "SelfRecursiveMember" []
  | AST.MutualRecursiveMember ->
      observationUnion "RecursiveAvailability" "MutualRecursiveMember" []
  | AST.CompletedGroupMember ->
      observationUnion "RecursiveAvailability" "CompletedGroupMember" []
  | AST.ImportedGroupMember ->
      observationUnion "RecursiveAvailability" "ImportedGroupMember" []

and observationParsedRecursiveMember (value : AST.parsedRecursiveMember) =
  observationRecord "ParsedRecursiveMember"
    [
      ("Binding", observationBinding value.AST.binding);
      ("Boundary", observationScopeId value.AST.boundary);
      ("Member", observationMemberId value.AST.member);
      ("SourceName", observationString value.AST.sourceName);
      ("Kind", observationRecursiveMemberKind value.AST.kind);
    ]

and observationResolvedRecursiveMember (value : AST.resolvedRecursiveMember) =
  observationRecord "ResolvedRecursiveMember"
    [
      ("Parsed", observationParsedRecursiveMember value.AST.parsed);
      ("Group", observationGroupId value.AST.group);
      ("GroupIndex", observationInt value.AST.groupIndex);
      ("Availability", observationRecursiveAvailability value.AST.availability);
    ]

and observationLetPattern (value : letPattern) =
  match value with
  | LPUnit -> observationUnion "LetPattern" "LPUnit" []
  | LPWildcard -> observationUnion "LetPattern" "LPWildcard" []
  | LPVariable field0 ->
      observationUnion "LetPattern" "LPVariable" [ observationBinding field0 ]
  | LPTuple (field0, field1, field2) ->
      observationUnion "LetPattern" "LPTuple"
        [
          observationLetPattern field0;
          observationLetPattern field1;
          Sequence (List.map (fun item -> observationLetPattern item) field2);
        ]

and observationTupleElements :
    'a.
    ('a -> StructuralValue.value) -> 'a tupleElements -> StructuralValue.value =
 fun encodeA value ->
  observationRecord "TupleElements"
    [
      ("First", encodeA value.first);
      ("Second", encodeA value.second);
      ("Rest", Sequence (List.map (fun item -> encodeA item) value.rest));
    ]

and observationCheckedType value =
  Scalar (StructuralFormat.format (privateCheckedType value))

and observationRecursiveMember (value : recursiveMember) =
  observationRecord "RecursiveMember"
    [
      ("Resolved", observationResolvedRecursiveMember value.resolved);
      ("MonomorphicType", observationCheckedType value.monomorphicType);
    ]

and observationPattern (value : pattern) =
  match value with
  | PUnit -> observationUnion "Pattern" "PUnit" []
  | PWildcard -> observationUnion "Pattern" "PWildcard" []
  | PVariable field0 ->
      observationUnion "Pattern" "PVariable" [ observationBinding field0 ]
  | PConstructor (field0, field1) ->
      observationUnion "Pattern" "PConstructor"
        [
          observationConstructorId field0;
          Sequence (List.map (fun item -> observationPattern item) field1);
        ]
  | PInt64 field0 ->
      observationUnion "Pattern" "PInt64"
        [ observationScalar "int64" (Int64.to_string field0) ]
  | PBigInt field0 ->
      observationUnion "Pattern" "PBigInt"
        [ (fun x -> observationScalar "bigint" (Z.to_string x)) field0 ]
  | PInt128Literal field0 ->
      observationUnion "Pattern" "PInt128Literal"
        [ observationScalar "int128" (Z.to_string field0) ]
  | PInt8Literal field0 ->
      observationUnion "Pattern" "PInt8Literal"
        [ observationScalar "int8" (string_of_int field0) ]
  | PInt16Literal field0 ->
      observationUnion "Pattern" "PInt16Literal"
        [ observationScalar "int16" (string_of_int field0) ]
  | PInt32Literal field0 ->
      observationUnion "Pattern" "PInt32Literal"
        [ observationScalar "int32" (Int32.to_string field0) ]
  | PUInt8Literal field0 ->
      observationUnion "Pattern" "PUInt8Literal"
        [ observationScalar "uint8" (string_of_int field0) ]
  | PUInt16Literal field0 ->
      observationUnion "Pattern" "PUInt16Literal"
        [ observationScalar "uint16" (string_of_int field0) ]
  | PUInt32Literal field0 ->
      observationUnion "Pattern" "PUInt32Literal"
        [ observationScalar "uint32" (Int64.to_string field0) ]
  | PUInt64Literal field0 ->
      observationUnion "Pattern" "PUInt64Literal"
        [ observationScalar "uint64" (unsigned field0) ]
  | PUInt128Literal field0 ->
      observationUnion "Pattern" "PUInt128Literal"
        [ observationScalar "uint128" (Z.to_string field0) ]
  | PBool field0 ->
      observationUnion "Pattern" "PBool"
        [ (fun x -> Scalar (if x then "true" else "false")) field0 ]
  | PString field0 ->
      observationUnion "Pattern" "PString" [ observationString field0 ]
  | PChar field0 ->
      observationUnion "Pattern" "PChar" [ observationString field0 ]
  | PFloat field0 ->
      observationUnion "Pattern" "PFloat"
        [
          (fun x ->
            observationScalar "float64"
              (Printf.sprintf "%016Lx" (Int64.bits_of_float x)))
            field0;
        ]
  | PTuple field0 ->
      observationUnion "Pattern" "PTuple"
        [ Sequence (List.map (fun item -> observationPattern item) field0) ]
  | PList field0 ->
      observationUnion "Pattern" "PList"
        [ Sequence (List.map (fun item -> observationPattern item) field0) ]
  | PListCons (field0, field1) ->
      observationUnion "Pattern" "PListCons"
        [
          Sequence (List.map (fun item -> observationPattern item) field0);
          observationPattern field1;
        ]
  | POr field0 ->
      observationUnion "Pattern" "POr"
        [ observationNonEmpty (fun item -> observationPattern item) field0 ]

and observationLambdaParameter (value : lambdaParameter) =
  observationRecord "LambdaParameter"
    [
      ("Pattern", observationLetPattern value.pattern);
      ("Type", observationCheckedType value.typ);
    ]

and observationRecordReference (value : recordReference) =
  observationRecord "RecordReference"
    [
      ("TypeId", observationTypeId value.typeId);
      ( "TypeArgs",
        Sequence
          (List.map (fun item -> observationCheckedType item) value.typeArgs) );
    ]

and observationConstructorReference (value : constructorReference) =
  observationRecord "ConstructorReference"
    [
      ("TypeId", observationTypeId value.typeId);
      ("ConstructorId", observationConstructorId value.constructorId);
      ( "TypeArgs",
        Sequence
          (List.map (fun item -> observationCheckedType item) value.typeArgs) );
    ]

and observationRecordFields _encode value =
  Scalar (StructuralFormat.format (privateRecordFields privateExpr value))

and observationStringPart (value : stringPart) =
  match value with
  | StringText field0 ->
      observationUnion "StringPart" "StringText" [ observationString field0 ]
  | StringExpr field0 ->
      observationUnion "StringPart" "StringExpr" [ observationExpr field0 ]

and observationExpr (value : expr) =
  match value with
  | UnitLiteral -> observationUnion "Expr" "UnitLiteral" []
  | Int64Literal field0 ->
      observationUnion "Expr" "Int64Literal"
        [ observationScalar "int64" (Int64.to_string field0) ]
  | Int128Literal field0 ->
      observationUnion "Expr" "Int128Literal"
        [ observationScalar "int128" (Z.to_string field0) ]
  | Int8Literal field0 ->
      observationUnion "Expr" "Int8Literal"
        [ observationScalar "int8" (string_of_int field0) ]
  | Int16Literal field0 ->
      observationUnion "Expr" "Int16Literal"
        [ observationScalar "int16" (string_of_int field0) ]
  | Int32Literal field0 ->
      observationUnion "Expr" "Int32Literal"
        [ observationScalar "int32" (Int32.to_string field0) ]
  | UInt8Literal field0 ->
      observationUnion "Expr" "UInt8Literal"
        [ observationScalar "uint8" (string_of_int field0) ]
  | UInt16Literal field0 ->
      observationUnion "Expr" "UInt16Literal"
        [ observationScalar "uint16" (string_of_int field0) ]
  | UInt32Literal field0 ->
      observationUnion "Expr" "UInt32Literal"
        [ observationScalar "uint32" (Int64.to_string field0) ]
  | UInt64Literal field0 ->
      observationUnion "Expr" "UInt64Literal"
        [ observationScalar "uint64" (unsigned field0) ]
  | UInt128Literal field0 ->
      observationUnion "Expr" "UInt128Literal"
        [ observationScalar "uint128" (Z.to_string field0) ]
  | BigIntLiteral field0 ->
      observationUnion "Expr" "BigIntLiteral"
        [ (fun x -> observationScalar "bigint" (Z.to_string x)) field0 ]
  | BoolLiteral field0 ->
      observationUnion "Expr" "BoolLiteral"
        [ (fun x -> Scalar (if x then "true" else "false")) field0 ]
  | StringLiteral field0 ->
      observationUnion "Expr" "StringLiteral" [ observationString field0 ]
  | BlobLiteral field0 ->
      observationUnion "Expr" "BlobLiteral" [ observationString field0 ]
  | CharLiteral field0 ->
      observationUnion "Expr" "CharLiteral" [ observationString field0 ]
  | FloatLiteral field0 ->
      observationUnion "Expr" "FloatLiteral"
        [
          (fun x ->
            observationScalar "float64"
              (Printf.sprintf "%016Lx" (Int64.bits_of_float x)))
            field0;
        ]
  | InterpolatedString field0 ->
      observationUnion "Expr" "InterpolatedString"
        [ Sequence (List.map (fun item -> observationStringPart item) field0) ]
  | BinOp (field0, field1, field2) ->
      observationUnion "Expr" "BinOp"
        [
          observationBinOp field0;
          observationExpr field1;
          observationExpr field2;
        ]
  | UnaryOp (field0, field1) ->
      observationUnion "Expr" "UnaryOp"
        [ observationUnaryOp field0; observationExpr field1 ]
  | Let (field0, field1, field2) ->
      observationUnion "Expr" "Let"
        [
          observationLetPattern field0;
          observationExpr field1;
          observationExpr field2;
        ]
  | RecursiveLet (field0, field1, field2) ->
      observationUnion "Expr" "RecursiveLet"
        [
          observationRecursiveMember field0;
          observationExpr field1;
          observationExpr field2;
        ]
  | Local field0 ->
      observationUnion "Expr" "Local" [ observationBinding field0 ]
  | If (field0, field1, field2) ->
      observationUnion "Expr" "If"
        [
          observationExpr field0; observationExpr field1; observationExpr field2;
        ]
  | Sequence (field0, field1) ->
      observationUnion "Expr" "Sequence"
        [ observationExpr field0; observationExpr field1 ]
  | Call (field0, field1) ->
      observationUnion "Expr" "Call"
        [
          observationFunction field0;
          observationNonEmpty (fun item -> observationExpr item) field1;
        ]
  | TypeApp (field0, field1, field2) ->
      observationUnion "Expr" "TypeApp"
        [
          observationFunction field0;
          Sequence (List.map (fun item -> observationCheckedType item) field1);
          observationNonEmpty (fun item -> observationExpr item) field2;
        ]
  | TupleLiteral field0 ->
      observationUnion "Expr" "TupleLiteral"
        [ observationTupleElements (fun item -> observationExpr item) field0 ]
  | TupleAccess (field0, field1) ->
      observationUnion "Expr" "TupleAccess"
        [ observationExpr field0; observationInt field1 ]
  | DictLiteral (field0, field1, field2) ->
      observationUnion "Expr" "DictLiteral"
        [
          observationCheckedType field0;
          observationCheckedType field1;
          Sequence
            (List.map
               (fun item ->
                 let part0, part1 = item in
                 observationTuple
                   [ observationExpr part0; observationExpr part1 ])
               field2);
        ]
  | RecordLiteral (field0, field1) ->
      observationUnion "Expr" "RecordLiteral"
        [
          observationRecordReference field0;
          observationRecordFields (fun item -> observationExpr item) field1;
        ]
  | RecordUpdate (field0, field1) ->
      observationUnion "Expr" "RecordUpdate"
        [
          observationExpr field0;
          Sequence
            (List.map
               (fun item ->
                 let part0, part1 = item in
                 observationTuple
                   [ observationFieldId part0; observationExpr part1 ])
               field1);
        ]
  | RecordAccess (field0, field1) ->
      observationUnion "Expr" "RecordAccess"
        [ observationExpr field0; observationFieldId field1 ]
  | Constructor (field0, field1) ->
      observationUnion "Expr" "Constructor"
        [
          observationConstructorReference field0;
          Sequence (List.map (fun item -> observationExpr item) field1);
        ]
  | Match (field0, field1) ->
      observationUnion "Expr" "Match"
        [
          observationExpr field0;
          observationNonEmpty (fun item -> observationMatchCase item) field1;
        ]
  | ListLiteral field0 ->
      observationUnion "Expr" "ListLiteral"
        [ Sequence (List.map (fun item -> observationExpr item) field0) ]
  | Lambda (field0, field1, field2) ->
      observationUnion "Expr" "Lambda"
        [
          observationNonEmpty
            (fun item -> observationLambdaParameter item)
            field0;
          observationOption (fun item -> observationCheckedType item) field1;
          observationExpr field2;
        ]
  | Apply (field0, field1) ->
      observationUnion "Expr" "Apply"
        [
          observationExpr field0;
          observationNonEmpty (fun item -> observationExpr item) field1;
        ]
  | IndirectApply (field0, field1) ->
      observationUnion "Expr" "IndirectApply"
        [
          observationExpr field0;
          observationNonEmpty (fun item -> observationExpr item) field1;
        ]
  | FuncRef field0 ->
      observationUnion "Expr" "FuncRef" [ observationFunction field0 ]
  | GenericFuncRef (id, args, typ) ->
      observationUnion "Expr" "GenericFuncRef"
        [
          observationFunction id;
          Sequence (List.map observationCheckedType args);
          observationCheckedType typ;
        ]
  | Closure (field0, field1) ->
      observationUnion "Expr" "Closure"
        [
          observationFunction field0;
          Sequence (List.map (fun item -> observationExpr item) field1);
        ]
  | RuntimeError field0 ->
      observationUnion "Expr" "RuntimeError" [ observationString field0 ]
  | BoundaryRender (field0, field1) ->
      observationUnion "Expr" "BoundaryRender"
        [ observationFunction field0; observationExpr field1 ]

and observationMatchCase (value : matchCase) =
  observationRecord "MatchCase"
    [
      ( "Patterns",
        observationNonEmpty (fun item -> observationPattern item) value.patterns
      );
      ("Guard", observationOption (fun item -> observationExpr item) value.guard);
      ("Body", observationExpr value.body);
    ]

let value = observationExpr
let expr expression = StructuralFormat.format (value expression)
let toString expression = StructuralFormat.format (privateExpr expression)
let pattern value = StructuralFormat.format (privatePattern value)
