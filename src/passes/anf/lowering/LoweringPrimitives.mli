(* Resolve intrinsic calls and primitive source representations. *)
type variantLookup =
  (string * string list * int * AST.semanticType list) StringOrder.Map.t

type sumCase = {
  typeParams : string list;
  tag : int;
  fields : AST.semanticType list;
}

type sumRepresentationIndex = sumCase StringOrder.Map.t StringOrder.Map.t
type sumMetadata = { names : StringOrder.Set.t; cases : sumRepresentationIndex }

val eqHelperDispatchMarker : string

val materializeFunctionComparisonPlan :
  AST.bindingId -> AST.bindingId -> CheckedAST.expr list -> CheckedAST.expr

val canonicalBufferKindForType :
  AST.semanticType -> MemoryModel.canonicalBufferKind option

val materializeComparisonPlan :
  (string -> AST.functionId) ->
  AST.semanticType ->
  CheckedAST.expr list ->
  CheckedAST.expr

val sumRepresentationIndex : variantLookup -> sumRepresentationIndex
val canUseTransparentPayload : AST.semanticType -> bool

val transparentSumPayloadType :
  string ->
  AST.semanticType list ->
  sumRepresentationIndex ->
  AST.semanticType option

val canUseNullaryZeroForPayload : AST.semanticType -> bool

val nullablePointerSumPayloadType :
  string ->
  AST.semanticType list ->
  sumRepresentationIndex ->
  AST.semanticType option

val spareImmediateSumSentinel :
  string -> AST.semanticType list -> sumRepresentationIndex -> int64 option

val sumPayloadExpr :
  AST.semanticType -> ANF.atom -> sumRepresentationIndex -> ANF.cExpr

val sumTypeNamesFromVariantLookup : variantLookup -> StringOrder.Set.t
val sumMetadataFromVariantLookup : variantLookup -> sumMetadata
val mergeSumMetadata : sumMetadata -> sumMetadata -> sumMetadata

val tryFindRecordTypeNameById :
  AST.typeId -> CheckedAST.semanticMetadata -> string option

val tryFindSumTypeNameById :
  AST.typeId -> CheckedAST.semanticMetadata -> string option

val tryFindVariantForType :
  string ->
  AST.semanticType ->
  variantLookup ->
  (string * string list * int * AST.semanticType list) option

val tryFindVariantByTag :
  string ->
  int ->
  sumRepresentationIndex ->
  (string * string list * int * AST.semanticType list) option

val tryFindVariantByConstructorId :
  AST.typeId ->
  string ->
  AST.constructorId ->
  variantLookup ->
  (string * string list * int * AST.semanticType list) option

val tryFindVariantForTypeById :
  AST.constructorId ->
  AST.semanticType ->
  CheckedAST.semanticMetadata ->
  variantLookup ->
  (string * string list * int * AST.semanticType list) option

val constructorReferenceMatches :
  string ->
  string ->
  CheckedAST.constructorReference ->
  CheckedAST.semanticMetadata ->
  variantLookup ->
  bool

val int128ToCanonicalString : Z.t -> string
val uint128ToCanonicalString : Z.t -> string
val int128Construction : (string -> AST.functionId) -> Z.t -> ANF.cExpr
val uint128Construction : (string -> AST.functionId) -> Z.t -> ANF.cExpr

val int128LiteralComparison :
  (string -> AST.functionId) -> ANF.atom -> Z.t -> ANF.cExpr

val uint128LiteralComparison :
  (string -> AST.functionId) -> ANF.atom -> Z.t -> ANF.cExpr

val typeToString : AST.semanticType -> string
val patternLiteralToSizedInt : CheckedAST.pattern -> ANF.sizedInt option
val tryFileIntrinsic : string -> ANF.atom list -> ANF.cExpr option
val tryCliIntrinsic : string -> ANF.atom list -> ANF.cExpr option
val normalizeNullaryIntrinsicArgs : ANF.atom list -> ANF.atom list
val tryPresentationIntrinsic : string -> ANF.atom list -> ANF.cExpr option

val tryParseMangledTypeWithSumTypeNames :
  StringOrder.Set.t -> string -> (AST.semanticType, string) result

val tryParseMangledType :
  variantLookup -> string -> (AST.semanticType, string) result

val tryFloatIntrinsic : string -> ANF.atom list -> ANF.cExpr option
val tryCanonicalPrimitiveIntrinsic : string -> ANF.atom list -> ANF.cExpr option

val tryRawMemoryIntrinsic :
  (string -> AST.functionId) ->
  StringOrder.Set.t ->
  string ->
  ANF.atom list ->
  ANF.cExpr option

val tryRandomIntrinsic : string -> ANF.atom list -> ANF.cExpr option
val tryDateTimeIntrinsic : string -> ANF.atom list -> ANF.cExpr option
val isBuiltinUnwrapName : string -> bool
val isBuiltinTestRuntimeErrorName : string -> bool
val isSourceCrashName : string -> bool
val isRuntimeFailureName : string -> bool
val unwrapErrorPayloadToString : CheckedAST.expr -> string option
