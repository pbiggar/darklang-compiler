// LoweringCallbacks.fs - Typed recursive entry points shared by expression-family handlers.

module LoweringCallbacks

open LoweringPrimitives
open TypeRegistries

type ExpressionLowerer = Set<string> -> Set<string> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.VarGen, string>
type AtomLowerer = Set<string> -> Set<string> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> AST.ModuleRegistry -> Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string>
type BoundAtomLowerer = Set<string> -> Set<string> -> CheckedAST.Expr -> ANF.VarGen -> VarEnv -> TypeRegistry -> VariantLookup -> FunctionRegistry -> AST.ModuleRegistry -> Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string>
