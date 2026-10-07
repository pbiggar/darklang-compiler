(* ComparisonPlanning.mli - Statically typed equality, ordering, JSON, and Dict plans. *)
type internalTypeAppMarker = EqHelperDispatch

type internalTypeApp =
  | EqHelperDispatchTypeApp of AST.semanticType * AST.expr * AST.expr

type comparisonPlan =
  | EqualityComparison of AST.semanticType
  | OrderingComparison of AST.semanticType

val internalTypeAppMarkerName : internalTypeAppMarker -> string
val makeInternalTypeApp : internalTypeApp -> AST.expr
val tryDecodeInternalTypeApp : AST.expr -> internalTypeApp option
val sumTypeHasPayload : Types.variantLookup -> string -> bool

val canonicalEqualityType :
  Types.variantLookup -> AST.semanticType -> AST.semanticType

val validateJsonTargetType :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.indexedSumTypeRegistry ->
  AST.semanticType ->
  (unit, CheckingDiagnostics.typeError) result

val needsEqHelperForResolvedType :
  Types.variantLookup -> AST.semanticType -> bool

val eqHelperName : AST.semanticType -> string
val compareHelperName : AST.semanticType -> string
val chainAndExpr : AST.expr list -> AST.expr

val buildEqExprForType :
  Types.aliasRegistry ->
  Types.variantLookup ->
  AST.semanticType ->
  AST.expr ->
  AST.expr ->
  AST.expr

val canonicalSortableType :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  AST.semanticType ->
  bool

val dictKeyAdmissibleType :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  AST.semanticType ->
  bool

val validateDictKeyCall :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  string ->
  AST.semanticType list ->
  (unit, CheckingDiagnostics.typeError) result

val validateCanonicalSortableCall :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  string ->
  AST.semanticType list ->
  (unit, CheckingDiagnostics.typeError) result

val classifyComparison :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.indexedSumTypeRegistry ->
  AST.binOp ->
  AST.semanticType ->
  AST.semanticType ->
  (comparisonPlan, CheckingDiagnostics.typeError) result

val buildOrderingExprForType :
  AST.binOp -> AST.semanticType -> AST.expr -> AST.expr -> AST.expr
