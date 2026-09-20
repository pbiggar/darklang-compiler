// LoweringCallbacks.fs - Typed recursive entry points shared by expression-family handlers.

module LoweringCallbacks

open LoweringPrimitives
open TypeRegistries

type ExpressionLowerer = Set<string> -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.VarGen, string>
type AtomLowerer = Set<string> -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string>
type BoundAtomLowerer = Set<string> -> TypeNameRegistry -> Set<AST.FunctionId> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> FunctionNameRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string>
