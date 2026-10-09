(* Frozen structural layouts for whole-function ownership and lowering failures. *)
open StructuralValue
module A = AnalyzeFunctionOwnership
module S = A.Scheduler
module V = A.Verification
module E = ElaborateFunctionOwnership
module I = S.Inference
module U = I.Uniqueness
module M = S.Materialize

let integer value = Scalar (string_of_int value)
let id (HIR.ValueId value) = Union ("ValueId", [ integer value ])
let func = AST.DiagnosticFormatting.func
let unary name value = Union (name, [ value ])
let two name left right = Union (name, [ left; right ])

let site (site : OwnedIR.callSiteIdentity) =
  Record
    [ ("Caller", func site.OwnedIR.caller); ("Result", id site.OwnedIR.result) ]

let nonempty value =
  Record
    [
      ("Head", Text value.NonEmptyList.head);
      ( "Tail",
        Sequence (List.map (fun text -> Text text) value.NonEmptyList.tail) );
    ]

let grouping (OwnedFunctionGroups.DuplicateFunctionName value) =
  unary "DuplicateFunctionName" (func value)

let uniqueness = function
  | U.VariantLimitExceeded (actual, maximum) ->
      two "VariantLimitExceeded" (integer actual) (integer maximum)
  | U.RecursiveFunctionRequiresGroupInference value ->
      unary "RecursiveFunctionRequiresGroupInference" (func value)
  | U.NoVerifiedBoundary cause ->
      unary "NoVerifiedBoundary" (U.Ownership.errorValue id cause)
  | U.NoVerifiedFunctionGroup cause ->
      unary "NoVerifiedFunctionGroup" (U.Ownership.errorValue id cause)

let inference = function
  | I.FunctionGroupingFailed cause ->
      unary "FunctionGroupingFailed" (grouping cause)
  | I.DemandTargetMissing value -> unary "DemandTargetMissing" (func value)
  | I.GroupInferenceFailed (names, cause) ->
      two "GroupInferenceFailed" (nonempty names) (uniqueness cause)

let selection = function
  | SelectOwnershipVariants.DuplicateFunctionName name ->
      unary "DuplicateFunctionName" (Text name)
  | SelectOwnershipVariants.UnknownFunction name ->
      unary "UnknownFunction" (Text name)
  | SelectOwnershipVariants.InvalidUniqueArgumentIndex (name, index) ->
      two "InvalidUniqueArgumentIndex" (Text name) (integer index)
  | SelectOwnershipVariants.MissingEstablishedUniqueArgument (name, index) ->
      two "MissingEstablishedUniqueArgument" (Text name) (integer index)
  | SelectOwnershipVariants.InconsistentEstablishedBoundary name ->
      unary "InconsistentEstablishedBoundary" (Text name)

let materializedVerification = function
  | M.Verification.HIRVerificationFailed cause ->
      unary "HIRVerificationFailed" (VerifyHIR.errorValue cause)
  | M.Verification.OwnershipVerificationFailed cause ->
      unary "OwnershipVerificationFailed"
        (M.Verification.Ownership.errorValue id cause)

let verification = function
  | V.HIRVerificationFailed cause ->
      unary "HIRVerificationFailed" (VerifyHIR.errorValue cause)
  | V.OwnershipVerificationFailed cause ->
      unary "OwnershipVerificationFailed" (V.Ownership.errorValue id cause)

let schedulingVerification = function
  | S.Verification.HIRVerificationFailed cause ->
      unary "HIRVerificationFailed" (VerifyHIR.errorValue cause)
  | S.Verification.OwnershipVerificationFailed cause ->
      unary "OwnershipVerificationFailed"
        (S.Verification.Ownership.errorValue id cause)

let materialization = function
  | M.GroupingFailed cause -> unary "GroupingFailed" (grouping cause)
  | M.InvalidOriginalProgram cause ->
      unary "InvalidOriginalProgram" (materializedVerification cause)
  | M.MissingGroupMember name -> unary "MissingGroupMember" (Text name)
  | M.GroupMembershipMismatch name ->
      unary "GroupMembershipMismatch" (Text name)
  | M.BoundaryMismatch name -> unary "BoundaryMismatch" (Text name)
  | M.MissingCallSite value -> unary "MissingCallSite" (site value)
  | M.DuplicateCallSite value -> unary "DuplicateCallSite" (site value)
  | M.StaleCallSite value -> unary "StaleCallSite" (site value)
  | M.MixedRecursiveCandidate value ->
      unary "MixedRecursiveCandidate" (site value)
  | M.SymbolCollision name -> unary "SymbolCollision" (Text name)
  | M.InvalidMaterializedProgram cause ->
      unary "InvalidMaterializedProgram" (materializedVerification cause)

let scheduling = function
  | S.InvalidLimits limits ->
      unary "InvalidLimits"
        (Record
           [
             ( "MaxIterations",
               integer limits.ScheduleOwnershipVariants.maxIterations );
             ( "MaxGeneratedGroups",
               integer limits.ScheduleOwnershipVariants.maxGeneratedGroups );
             ( "MaxRewrittenCalls",
               integer limits.ScheduleOwnershipVariants.maxRewrittenCalls );
           ])
  | S.InvalidFunctionBoundary (value, cause) ->
      two "InvalidFunctionBoundary" (func value)
        (S.Ownership.errorValue id cause)
  | S.InferenceFailed cause -> unary "InferenceFailed" (inference cause)
  | S.CatalogFailed cause -> unary "CatalogFailed" (selection cause)
  | S.AnalysisFailed cause ->
      unary "AnalysisFailed" (schedulingVerification cause)
  | S.SelectionFailed (value, cause) ->
      two "SelectionFailed" (site value) (selection cause)
  | S.MaterializationFailed cause ->
      unary "MaterializationFailed" (materialization cause)
  | S.IterationLimitExceeded count ->
      unary "IterationLimitExceeded" (integer count)
  | S.GeneratedGroupLimitExceeded count ->
      unary "GeneratedGroupLimitExceeded" (integer count)
  | S.RewrittenCallLimitExceeded count ->
      unary "RewrittenCallLimitExceeded" (integer count)
  | S.MissingOriginalCall value -> unary "MissingOriginalCall" (site value)

let construction = function
  | ConstructHIRFunctions.CannotInferExpression (expression, cause) ->
      two "CannotInferExpression" (Text expression) (Text cause)
  | ConstructHIRFunctions.InconsistentCallSignature (name, value) ->
      two "InconsistentCallSignature" (Text name) (func value)

let elaboration = function
  | E.UnknownCallOwnership value -> unary "UnknownCallOwnership" (func value)
  | E.InconsistentCallParameters value ->
      unary "InconsistentCallParameters" (func value)
  | E.InvalidFunctionBoundary (value, cause) ->
      two "InvalidFunctionBoundary" (func value)
        (E.Ownership.errorValue id cause)

let analysisError error =
  StructuralFormat.format
    (match error with
    | A.HIRConstructionFailed cause ->
        unary "HIRConstructionFailed" (construction cause)
    | A.OwnershipElaborationFailed cause ->
        unary "OwnershipElaborationFailed" (elaboration cause)
    | A.OwnedHIRVerificationFailed cause ->
        unary "OwnedHIRVerificationFailed" (verification cause)
    | A.FunctionOwnershipVerificationFailed (value, cause) ->
        two "FunctionOwnershipVerificationFailed" (func value)
          (A.Ownership.errorValue id cause)
    | A.SpecializationSchedulingFailed cause ->
        unary "SpecializationSchedulingFailed" (scheduling cause))

let loweringError error =
  StructuralFormat.format
    (match error with
    | LowerOwnershipVariants.MissingSourceFunction value ->
        unary "MissingSourceFunction" (func value)
    | LowerOwnershipVariants.MissingSourceCalls (value, sites) ->
        two "MissingSourceCalls" (func value) (Sequence (List.map site sites))
    | LowerOwnershipVariants.InvalidOwnershipBoundary value ->
        unary "InvalidOwnershipBoundary" (func value))
