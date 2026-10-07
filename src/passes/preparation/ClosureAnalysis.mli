(* Closure environments, free variables, and inferred capture types. *)
module BindingSet : Set.S with type elt = AST.bindingId
module TypeListSet : Set.S with type elt = AST.semanticType list

module ComparisonMap :
  Map.S with type key = AST.functionId * AST.semanticType list

type liftState = {
  symbols : CheckedAST.symbols;
  counter : int;
  liftedFunctions : CheckedAST.functionDef list;
  comparisonFuncs : string ComparisonMap.t;
  comparableFunctionParams : TypeListSet.t;
  typeEnv : AST.semanticType CheckedAST.BindingIdMap.t;
  funcParams : AST.semanticType list FunctionIdMap.t;
  funcReturnTypes : AST.semanticType FunctionIdMap.t;
  genericFuncDefs : (string list * AST.semanticType) FunctionIdMap.t;
  typeReg : TypeRegistries.typeRegistry;
  variantLookup : LoweringPrimitives.variantLookup;
  recursiveSelf :
    (AST.bindingId
    * AST.bindingId
    * AST.semanticType
    * CheckedAST.recursiveMember)
    option;
}

val freshLiftedName : liftState -> string -> string * liftState

val matchPatternBindingTypes :
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.typeNameRegistry ->
  CheckedAST.pattern ->
  AST.semanticType ->
  AST.semanticType CheckedAST.BindingIdMap.t

val lambdaNeedsComparison :
  CheckedAST.lambdaParameter NonEmptyList.t -> liftState -> bool

val freeVars : CheckedAST.expr -> BindingSet.t -> BindingSet.t

val reconcileBranchTypes :
  AST.semanticType -> AST.semanticType -> AST.semanticType option

val simpleInferType :
  CheckedAST.expr ->
  AST.semanticType CheckedAST.BindingIdMap.t ->
  AST.semanticType list FunctionIdMap.t ->
  AST.semanticType FunctionIdMap.t ->
  (string list * AST.semanticType) FunctionIdMap.t ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.typeNameRegistry ->
  AST.semanticType option

val inferLambdaReturnType :
  CheckedAST.expr -> liftState -> (AST.semanticType, string) result
