(* SpecializationIdentity.fs - Name concrete generic instances and normalize typed parameters. *)
module FunctionSet : Set.S with type elt = AST.functionId
type genericFunctionArtifact = {symbols : CheckedAST.symbols; func : CheckedAST.functionDef; directDependencies : FunctionSet.t}
type genericFuncDefs = genericFunctionArtifact StringOrder.Map.t
type specKey = string * AST.semanticType list
module SpecMap : Map.S with type key = specKey
module SpecSet : Set.S with type elt = specKey
type specRegistry = string SpecMap.t
type specializationResult = {specializedFuncs : genericFunctionArtifact list; specRegistry : specRegistry; externalSpecs : SpecSet.t; symbols : CheckedAST.symbols}
val directDependencies : CheckedAST.expr -> FunctionSet.t
val extractGenericFuncDefs : CheckedAST.program -> genericFuncDefs
val importSpecializedFunctions : CheckedAST.symbols -> genericFunctionArtifact list -> CheckedAST.symbols * CheckedAST.functionDef list
val typeToMangledName : AST.semanticType -> string
val containsTypeVar : AST.semanticType -> bool
val specName : string -> AST.semanticType list -> string
val isGenericKeyIntrinsicName : string -> bool
val exprArgsToList : CheckedAST.expr NonEmptyList.t -> CheckedAST.expr list
val exprArgsFromList : CheckedAST.expr list -> CheckedAST.expr NonEmptyList.t
val paramsToList : (AST.bindingId * AST.semanticType) NonEmptyList.t -> (AST.bindingId * AST.semanticType) list
val lambdaParameterType : CheckedAST.lambdaParameter -> AST.semanticType
val letPatternBindingTypes : CheckedAST.letPattern -> AST.semanticType -> (AST.bindingId * AST.semanticType) list
val lambdaParameterBindings : CheckedAST.lambdaParameter -> (AST.bindingId * AST.semanticType) list
val lowerLambdaParameters : CheckedAST.symbols -> CheckedAST.lambdaParameter NonEmptyList.t -> CheckedAST.expr -> (AST.bindingId * AST.semanticType) list * CheckedAST.expr * CheckedAST.symbols
val paramsFromList : string -> (AST.bindingId * AST.semanticType) list -> (AST.bindingId * AST.semanticType) NonEmptyList.t
val normalizeSyntheticNullaryParams : CheckedAST.symbols -> (AST.bindingId * AST.semanticType) list -> (AST.bindingId * AST.semanticType) list
val normalizeSyntheticNullaryArgAtoms : AST.semanticType list -> CheckedAST.expr list -> 'atom list -> 'atom list
val unresolvedKeyIntrinsicTypeArgErrorExpr : AST.functionId -> string -> CheckedAST.expr
val wrapWithIgnoredArgEvaluations : CheckedAST.expr list -> CheckedAST.expr -> CheckedAST.expr
