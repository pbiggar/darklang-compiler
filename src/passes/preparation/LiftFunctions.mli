(* LiftFunctions.mli - Resolve lifted function references and program-level closure wrappers. *)
type liftStateWithFuncs = {
  state : ClosureAnalysis.liftState;
  funcParams : AST.semanticType list FunctionIdMap.t;
  generatedWrappers : (AST.functionId * AST.functionId) FunctionIdMap.t;
}

type functionCatalog = {
  params : AST.semanticType list FunctionIdMap.t;
  returnTypes : AST.semanticType FunctionIdMap.t;
  genericDefs : (string list * AST.semanticType) FunctionIdMap.t;
}

val liftLambdasInFunc :
  CheckedAST.functionDef ->
  ClosureAnalysis.liftState ->
  (CheckedAST.functionDef * ClosureAnalysis.liftState, string) result

val generateFuncWrapper :
  AST.functionId ->
  AST.semanticType list FunctionIdMap.t ->
  AST.semanticType FunctionIdMap.t ->
  liftStateWithFuncs ->
  (CheckedAST.functionDef * liftStateWithFuncs, string) result

val prepareLambdaLiftBaseTypes :
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.typeRegistry * LoweringPrimitives.variantLookup

val liftLambdasInProgram :
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  functionCatalog ->
  CheckedAST.program ->
  (CheckedAST.program, string) result

val collectFuncRefsInExpr :
  CheckedAST.expr ->
  AST.semanticType list FunctionIdMap.t ->
  AST.functionId list

val replaceFuncRefsWithWrappers :
  (AST.functionId * AST.functionId) FunctionIdMap.t ->
  CheckedAST.topLevel ->
  CheckedAST.topLevel

val replaceInExpr :
  (AST.functionId * AST.functionId) FunctionIdMap.t ->
  CheckedAST.expr ->
  CheckedAST.expr
