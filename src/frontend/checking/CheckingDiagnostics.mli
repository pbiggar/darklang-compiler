(* CheckingDiagnostics.mli - Typing failures, source-compatible rendering, and inference identities. *)
type typeError =
  | TypeMismatch of AST.semanticType * AST.semanticType * string
  | IfBranchTypeMismatch of AST.semanticType * AST.semanticType
  | UndefinedVariable of string
  | UndefinedCallTarget of string
  | MissingTypeAnnotation of string
  | InvalidOperation of string * AST.semanticType list
  | IncompatibleEqualityOperands of AST.semanticType * AST.semanticType
  | IncompatibleOrderingOperands of AST.semanticType * AST.semanticType
  | PolymorphicRecursion of string
  | ResolutionFailure of NameResolution.resolutionError
  | GenericError of string

type aliasVisitState = AliasVisiting | AliasValidated

val makePartialParams :
  string -> AST.semanticType list -> (string * AST.semanticType) list

val toCallArgs : AST.expr list -> AST.expr NonEmptyList.t
val normalizeNullaryCallArgs : int -> AST.expr list -> AST.expr list

val toLambdaParams :
  (string * AST.semanticType) list -> AST.lambdaParameter NonEmptyList.t

val typeToString : AST.semanticType -> string
val typeToHelperIdentityString : AST.semanticType -> string
val typeErrorToString : typeError -> string
val withIndefiniteArticle : string -> string
val ifConditionTypeMismatchMessage : AST.expr -> AST.semanticType -> string
val interpolationTypeMismatchMessage : AST.expr -> AST.semanticType -> string
val substituteInterpolationLiteral : string -> AST.expr -> AST.expr -> AST.expr
val isBuiltinUnwrapName : string -> bool
val isRuntimeFailureName : string -> bool
val isBuiltinTestNanName : string -> bool
val isBuiltinTestInfinityName : string -> bool
val isBuiltinBlobEmptyName : string -> bool
val isNeverType : AST.semanticType -> bool
val isKnownFailureConstructorExpr : AST.expr -> bool
val isKnownUnwrapFailureExpr : AST.expr StringOrder.Map.t -> AST.expr -> bool
val isKnownCrashExpr : AST.expr StringOrder.Map.t -> AST.expr -> bool

val tryExtractKnownCrashMessage :
  AST.expr StringOrder.Map.t -> AST.expr -> string option

val tryFormatLiteralValue : AST.expr -> string option
val formatDeconstructionPattern : AST.pattern -> string
val formatLetDeconstructionPattern : AST.letPattern -> string
val inferredLetPatternType : string -> AST.letPattern -> AST.semanticType

val bindLetPatternTypes :
  AST.letPattern -> AST.semanticType -> (string * AST.semanticType) list option

val formatListLiteralForNoMatch : AST.expr list -> string
val formatPatternMismatchValue : AST.expr -> string option

val formatPatternMismatchError :
  AST.expr -> AST.semanticType -> AST.semanticType -> string option -> string

val formatLegacyParamTypeError :
  string ->
  int ->
  string ->
  AST.semanticType ->
  AST.semanticType ->
  AST.expr ->
  string

val inferenceVarForKey : string -> AST.semanticType

val freshenTypeParams :
  string option -> string list -> string list * string StringOrder.Map.t

val applyTypeVarRenaming :
  string StringOrder.Map.t -> AST.semanticType -> AST.semanticType
