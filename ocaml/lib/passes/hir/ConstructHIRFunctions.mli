(* ConstructFunctions.fs - Normalize checked functions into structured semantic HIR. *)
type scalarLiteral = UnitLiteral | Int8Literal of int | Int16Literal of int | Int32Literal of int32 | Int64Literal of int64 | UInt8Literal of int | UInt16Literal of int | UInt32Literal of int64 | UInt64Literal of int64 | BoolLiteral of bool | FloatLiteral of float
type primitive = Literal of HIR.value * scalarLiteral | Unary of HIR.value * AST.unaryOp * HIR.value | Binary of HIR.value * AST.binOp * HIR.value * HIR.value | FreshManaged of HIR.value * HIR.operand | ListTransform of HIR.value * HIR.value * HIR.operand
type block
type constructionError = CannotInferExpression of string * string | InconsistentCallSignature of string * AST.functionId
type callContracts = {externalSignature : AST.functionId -> HIR.functionSignature option; contract : AST.functionId -> (HIR.functionCall -> HIR.primitiveContract) option}
val body : block -> (primitive, block) HIR.operation HIR.block
val primitiveContract : primitive -> HIR.primitiveContract
val verificationDialect : callContracts -> (primitive, block) VerifyHIR.dialect
val constructFunction : (AST.semanticType CheckedAST.BindingIdMap.t -> CheckedAST.expr -> (AST.semanticType, string) result) -> (CheckedAST.expr -> ClosureAnalysis.BindingSet.t) -> callContracts -> CheckedAST.functionDef -> (block HIR.functionDef, constructionError) result
val constructFunctions : (AST.semanticType CheckedAST.BindingIdMap.t -> CheckedAST.expr -> (AST.semanticType, string) result) -> (CheckedAST.expr -> ClosureAnalysis.BindingSet.t) -> callContracts -> CheckedAST.functionDef list -> (block HIR.functionDef list, constructionError) result
val constructFunctionsWithOpaqueFallback : string FunctionIdMap.t -> (AST.semanticType CheckedAST.BindingIdMap.t -> CheckedAST.expr -> (AST.semanticType, string) result) -> (CheckedAST.expr -> ClosureAnalysis.BindingSet.t) -> callContracts -> CheckedAST.functionDef list -> (block HIR.functionDef list, constructionError) result
