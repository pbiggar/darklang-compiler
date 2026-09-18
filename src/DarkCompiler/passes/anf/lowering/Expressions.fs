// Expressions.fs - Tie recursive ANF lowering handlers and list-region selection together.

module LoweringExpressions

open ANF
open LoweringPrimitives
open TypeRegistries
open ClosureAnalysis
open LiftExpressions
open LiftFunctions
open LoweringTypeInference
open ANFContinuations

let rec toANFCore (sumTypeNames: Set<string>) (inertScopes: Set<string>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    let infer localTypes value =
        let types = Map.fold (fun types name typ -> Map.add name typ types) (typeEnvFromVarEnv env) localTypes
        inferTypeCore sumTypeNames value types typeReg variantLookup funcReg moduleRegistry
    match ExtractListRegions.tryExtract inertScopes (typeEnvFromVarEnv env) infer (fun value -> freeVars value Set.empty) expr with
    | Some region ->
        let lower value vg environment = toANFUnplannedCore sumTypeNames inertScopes value vg environment typeReg variantLookup funcReg moduleRegistry
        ListLiveness.verifyFunctional region |> Result.bind (fun () ->
            region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership |> LowerListRegions.lower lower env varGen)
    | None -> toANFUnplannedCore sumTypeNames inertScopes expr varGen env typeReg variantLookup funcReg moduleRegistry

and private toANFUnplannedCore (sumTypeNames: Set<string>) (inertScopes: Set<string>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    ExpressionLowering.lowerExpression toANFCore toAtomCore toANFBoundAtomCore sumTypeNames inertScopes expr varGen env typeReg variantLookup funcReg moduleRegistry

and toAtomCore (sumTypeNames: Set<string>) (inertScopes: Set<string>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    AtomLowering.lowerAtom toANFCore toAtomCore toANFBoundAtomCore sumTypeNames inertScopes expr varGen env typeReg variantLookup funcReg moduleRegistry

and toANFBoundAtomCore (sumTypeNames: Set<string>) (inertScopes: Set<string>)
    (expr: CheckedAST.Expr)
    (varGen: ANF.VarGen)
    (env: VarEnv)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string> =
    match toAtomCore sumTypeNames inertScopes expr varGen env typeReg variantLookup funcReg moduleRegistry with
    | Ok (atom, bindings, vg1) ->
        // Keep existing atom lowering behavior unchanged when toAtom succeeds:
        // do not introduce extra temp ids in the common path.
        Ok (wrapBindings bindings (ANF.Return atom), atom, vg1)
    | Error _ ->
        let (boundVar, vg1) = ANF.freshVar varGen
        toANFCore sumTypeNames inertScopes expr vg1 env typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun (exprA, vg2) ->
            let boundExpr =
                bindReturns exprA (fun atom ->
                    ANF.Let (boundVar, ANF.Atom atom, ANF.Return (ANF.Var boundVar)))
            (boundExpr, ANF.Var boundVar, vg2))
