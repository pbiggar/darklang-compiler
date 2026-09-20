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

let rec toANFCore (sumTypeNames: Set<string>) (typeNames: TypeNameRegistry) (inertScopes: Set<AST.FunctionId>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (functionNames: FunctionNameRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    let functionIds = functionNames |> Map.toSeq |> Seq.map (fun (id, name) -> name, id) |> Map.ofSeq
    let resolveFunction name =
        Map.tryFind name functionIds
        |> Option.defaultWith (fun () -> Crash.crash $"List-region helper '{name}' is absent from registries")
    let infer localTypes value =
        let types = Map.fold (fun types name typ -> Map.add name typ types) (typeEnvFromVarEnv env) localTypes
        inferTypeCore sumTypeNames typeNames value types typeReg variantLookup funcReg functionNames moduleRegistry
    match ExtractListRegions.tryExtract inertScopes functionNames (typeEnvFromVarEnv env) infer (fun value -> freeVars value Set.empty) expr with
    | Some region ->
        let lower value vg environment = toANFUnplannedCore sumTypeNames typeNames inertScopes value vg environment typeReg variantLookup funcReg functionNames moduleRegistry
        ListLiveness.verifyFunctional region |> Result.bind (fun () ->
            region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership |> LowerListRegions.lower resolveFunction lower env varGen)
    | None -> toANFUnplannedCore sumTypeNames typeNames inertScopes expr varGen env typeReg variantLookup funcReg functionNames moduleRegistry

and private toANFUnplannedCore (sumTypeNames: Set<string>) (typeNames: TypeNameRegistry) (inertScopes: Set<AST.FunctionId>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (functionNames: FunctionNameRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    ExpressionLowering.lowerExpression toANFCore toAtomCore toANFBoundAtomCore sumTypeNames typeNames inertScopes expr varGen env typeReg variantLookup funcReg functionNames moduleRegistry

and toAtomCore (sumTypeNames: Set<string>) (typeNames: TypeNameRegistry) (inertScopes: Set<AST.FunctionId>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (functionNames: FunctionNameRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    AtomLowering.lowerAtom toANFCore toAtomCore toANFBoundAtomCore sumTypeNames typeNames inertScopes expr varGen env typeReg variantLookup funcReg functionNames moduleRegistry

and toANFBoundAtomCore (sumTypeNames: Set<string>) (typeNames: TypeNameRegistry) (inertScopes: Set<AST.FunctionId>)
    (expr: CheckedAST.Expr)
    (varGen: ANF.VarGen)
    (env: VarEnv)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (functionNames: FunctionNameRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string> =
    match toAtomCore sumTypeNames typeNames inertScopes expr varGen env typeReg variantLookup funcReg functionNames moduleRegistry with
    | Ok (atom, bindings, vg1) ->
        // Keep existing atom lowering behavior unchanged when toAtom succeeds:
        // do not introduce extra temp ids in the common path.
        Ok (wrapBindings bindings (ANF.Return atom), atom, vg1)
    | Error _ ->
        let lowerWithBranchLocalBinding () =
            let (boundVar, vg1) = ANF.freshVar varGen
            toANFCore sumTypeNames typeNames inertScopes expr vg1 env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.map (fun (exprA, vg2) ->
                let boundExpr =
                    bindReturns exprA (fun atom ->
                        ANF.Let (boundVar, ANF.Atom atom, ANF.Return (ANF.Var boundVar)))
                (boundExpr, ANF.Var boundVar, vg2))
        match expr with
        | CheckedAST.Match _ ->
            inferTypeCore
                sumTypeNames
                typeNames
                expr
                (typeEnvFromVarEnv env)
                typeReg
                variantLookup
                funcReg
                functionNames
                moduleRegistry
            |> Result.bind (fun resultType ->
                if isSupportedJoinArgumentType resultType then
                    let (boundVar, vg1) = ANF.freshVar varGen
                    toANFCore sumTypeNames typeNames inertScopes expr vg1 env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (exprA, vg2) ->
                        // Pattern-bound generic values can retain a TVar in the recovered
                        // TypeMap even though checking established the match result type.
                        // Give each return path an explicit boundary type before it jumps.
                        let rec returnsToTypedJumps expression vg =
                            match expression with
                            | ANF.Return atom ->
                                let (typedResult, vg1) = ANF.freshVar vg
                                (ANF.Let (
                                    typedResult,
                                    ANF.TypedAtom (atom, resultType),
                                    ANF.Jump (boundVar, ANF.Var typedResult)
                                 ), vg1)
                            | ANF.Jump _ -> (expression, vg)
                            | ANF.Join (parameter, continuation, entry) ->
                                let (continuation', vg1) = returnsToTypedJumps continuation vg
                                let (entry', vg2) = returnsToTypedJumps entry vg1
                                (ANF.Join (parameter, continuation', entry'), vg2)
                            | ANF.Let (id, cexpr, rest) ->
                                let (rest', vg1) = returnsToTypedJumps rest vg
                                (ANF.Let (id, cexpr, rest'), vg1)
                            | ANF.If (condition, thenBranch, elseBranch) ->
                                let (thenBranch', vg1) = returnsToTypedJumps thenBranch vg
                                let (elseBranch', vg2) = returnsToTypedJumps elseBranch vg1
                                (ANF.If (condition, thenBranch', elseBranch'), vg2)
                        let (entry, vg3) = returnsToTypedJumps exprA vg2
                        let boundExpr =
                            ANF.Join (
                                { Id = boundVar; Type = resultType },
                                ANF.Return (ANF.Var boundVar),
                                entry
                            )
                        (boundExpr, ANF.Var boundVar, vg3))
                else lowerWithBranchLocalBinding ())
        | _ -> lowerWithBranchLocalBinding ()
