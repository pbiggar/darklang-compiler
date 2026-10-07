(* Inline lexical lambda bindings before closure conversion. *)
type lambdaEnv = CheckedAST.expr CheckedAST.BindingIdMap.t
val varOccursInExpr : AST.bindingId -> CheckedAST.expr -> bool
val inlineLambdas : CheckedAST.expr -> lambdaEnv -> CheckedAST.expr
val inlineLambdasInFunc : CheckedAST.functionDef -> CheckedAST.functionDef
val inlineLambdasInProgram : CheckedAST.program -> CheckedAST.program
