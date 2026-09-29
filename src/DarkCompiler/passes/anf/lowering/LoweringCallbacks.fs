// LoweringCallbacks.fs - Typed recursive entry points shared by expression-family handlers.

module LoweringCallbacks

open LoweringPrimitives
open TypeRegistries

type ExpressionLowerer = SumMetadata -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.VarGen, string>
type AtomLowerer = SumMetadata -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string>
type BoundAtomLowerer = SumMetadata -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string>
