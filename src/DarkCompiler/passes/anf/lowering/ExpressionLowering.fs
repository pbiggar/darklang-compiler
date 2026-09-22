// ExpressionLowering.fs - Lower expressions while delegating recursive children through typed callbacks.

module ExpressionLowering

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution
open ClosureAnalysis
open LiftExpressions
open LiftFunctions
open LoweringOperators
open LoweringTypeInference
open LoweringAggregates
open ANFContinuations
open LoweringCallbacks

let lowerExpression (toANFCore: ExpressionLowerer) (toAtomCore: AtomLowerer) (toANFBoundAtomCore: BoundAtomLowerer) (functionIds: FunctionIdRegistry) (sumTypeNames: Set<string>) (typeNames: TypeNameRegistry) (inertScopes: Set<AST.FunctionId>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (functionNames: FunctionNameRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    let fieldIndex id =
        tryFindFieldIndex id typeNames
        |> Option.defaultWith (fun () -> Crash.crash "Checked field identity is absent from layout metadata")
    let constructorTag id =
        tryFindConstructorTag id typeNames
        |> Option.defaultWith (fun () -> Crash.crash "Checked constructor identity is absent from layout metadata")
    let functionId name =
        Map.tryFind name functionIds
        |> Option.defaultWith (fun () ->
            Crash.crash $"Expression lowering function '{name}' is absent from registries")
    let functionNameIs id expected =
        Map.tryFind id functionNames
        |> Option.orElseWith (fun () -> Map.tryFind id funcReg |> Option.map fst)
        |> Option.contains expected
    match expr with
    | CheckedAST.RecursiveLet _ -> Error "RecursiveLet must be lowered during lambda lifting"
    | CheckedAST.DictLiteral (_, _, []) ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 0L)), varGen)
    | CheckedAST.DictLiteral _ -> Error "Non-empty DictLiteral must be lowered during generic specialization"
    | CheckedAST.BoundaryRender (renderer, value) ->
        toANFCore sumTypeNames typeNames inertScopes value varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.map (fun (valueExpr, varGen1) ->
            let (renderedVar, varGen2) = ANF.freshVar varGen1
            let renderedExpr =
                bindReturns valueExpr (fun valueAtom ->
                    ANF.Let (
                        renderedVar,
                        ANF.Call (renderer, [valueAtom]),
                        ANF.Return (ANF.Var renderedVar)
                    ))
            (renderedExpr, varGen2))
    | CheckedAST.RuntimeError message ->
        let (runtimeErrorVar, varGen1) = ANF.freshVar varGen
        Ok (ANF.Let (runtimeErrorVar, ANF.RuntimeError message, ANF.Return ANF.UnitLiteral), varGen1)
    | CheckedAST.UnitLiteral ->
        // Unit literal becomes return of unit value (represented as 0)
        Ok (ANF.Return (ANF.UnitLiteral), varGen)

    | CheckedAST.Int64Literal n ->
        // Integer literal (default Int64)
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 n)), varGen)

    | CheckedAST.Int128Literal n ->
        let (resultVar, varGen1) = ANF.freshVar varGen
        Ok (ANF.Let (resultVar, int128Construction functionId n, ANF.Return (ANF.Var resultVar)), varGen1)

    | CheckedAST.BigIntLiteral n ->
        let smallMin = -(System.Numerics.BigInteger.One <<< 62)
        let smallMax = (System.Numerics.BigInteger.One <<< 62) - System.Numerics.BigInteger.One
        let (resultVar, varGen1) = ANF.freshVar varGen
        let construction =
            if n >= smallMin && n <= smallMax then
                let taggedWord = int64 (n * 2I + 1I)
                ANF.TypedAtom (ANF.IntLiteral (ANF.Int64 taggedWord), AST.TInt)
            else
                ANF.Call (functionId "Darklang.Stdlib.Int.__value", [ANF.StringLiteral (n.ToString())])
        Ok (ANF.Let (resultVar, construction, ANF.Return (ANF.Var resultVar)), varGen1)

    | CheckedAST.Int8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int8 n)), varGen)

    | CheckedAST.Int16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int16 n)), varGen)

    | CheckedAST.Int32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int32 n)), varGen)

    | CheckedAST.UInt8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt8 n)), varGen)

    | CheckedAST.UInt16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt16 n)), varGen)

    | CheckedAST.UInt32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt32 n)), varGen)

    | CheckedAST.UInt64Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt64 n)), varGen)

    | CheckedAST.UInt128Literal n ->
        let (resultVar, varGen1) = ANF.freshVar varGen
        Ok (ANF.Let (resultVar, uint128Construction functionId n, ANF.Return (ANF.Var resultVar)), varGen1)

    | CheckedAST.BoolLiteral b ->
        // Boolean literal becomes return
        Ok (ANF.Return (ANF.BoolLiteral b), varGen)

    | CheckedAST.StringLiteral s ->
        // String literal becomes return
        Ok (ANF.Return (ANF.StringLiteral (s.Normalize(System.Text.NormalizationForm.FormC))), varGen)

    | CheckedAST.BlobLiteral bytes ->
        Ok (ANF.Return (ANF.StringLiteral bytes), varGen)

    | CheckedAST.CharLiteral s ->
        // Char literal becomes return (stored as string, same runtime representation)
        Ok (ANF.Return (ANF.StringLiteral (s.Normalize(System.Text.NormalizationForm.FormC))), varGen)

    | CheckedAST.FloatLiteral f ->
        // Float literal becomes return
        Ok (ANF.Return (ANF.FloatLiteral f), varGen)

    | CheckedAST.Local id ->
        match Map.tryFind id env with
        | Some (tempId, _) -> Ok (ANF.Return (ANF.Var tempId), varGen)
        | None -> Error "Undefined local binding identity"

    | CheckedAST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')

    | CheckedAST.Closure (funcName, captures) ->
        // Closure: allocate closure tuple with function address and captured values
        // Convert each capture expression to an atom
        let rec convertCaptures (caps: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | CheckedAST.FuncRef funcName :: rest ->
                convertCaptures rest vg ((ANF.FuncRef funcName, []) :: acc)
            | cap :: rest ->
                toAtomCore sumTypeNames typeNames inertScopes cap vg env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Generate ClosureAlloc: allocate closure tuple
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            let finalExpr = ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId))
            let exprWithBindings = wrapBindings allBindings finalExpr
            (exprWithBindings, varGen2))

    | CheckedAST.Let (pattern, value, body) ->
        // Evaluate the RHS in the incoming environment, then prepare every
        // projection before exposing any binder to the continuation.
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames typeNames value typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun valueType ->
            if not (letPatternAcceptsType pattern valueType) then
                // Type checking has replaced the continuation with the binding
                // mismatch error. Preserve every RHS effect, then transition
                // directly to that error without preparing any projection.
                toANFCore sumTypeNames typeNames inertScopes value varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (valueExpr, varGen1) ->
                    toANFCore sumTypeNames typeNames inertScopes body varGen1 env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (failureExpr, varGen2) ->
                        (bindReturns valueExpr (fun _ -> failureExpr), varGen2)))
            else
              let compileContinuation valueAtom valueBindings varGen1 =
                match pattern with
                | CheckedAST.LPVariable name ->
                    let (bindingId, varGen2) = ANF.freshVar varGen1
                    let env' = Map.add name (bindingId, valueType) env
                    toANFCore sumTypeNames typeNames inertScopes body varGen2 env' typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (bodyExpr, varGen3) ->
                        let transition =
                            ANF.Let (bindingId, ANF.Atom valueAtom, bodyExpr)
                            |> wrapBindings valueBindings
                        (transition, varGen3))
                | CheckedAST.LPUnit | CheckedAST.LPWildcard ->
                    toANFCore sumTypeNames typeNames inertScopes body varGen1 env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (bodyExpr, varGen2) ->
                        (wrapBindings valueBindings bodyExpr, varGen2))
                | CheckedAST.LPTuple _ ->
                    let (rootId, varGen2) = ANF.freshVar varGen1
                    lowerLetPatternBindings pattern (ANF.Var rootId) valueType env [] varGen2
                    |> Result.bind (fun (env', patternBindingsRev, varGen3) ->
                        toANFCore sumTypeNames typeNames inertScopes body varGen3 env' typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.map (fun (bodyExpr, varGen4) ->
                            let transition =
                                wrapBindings (List.rev patternBindingsRev) bodyExpr
                                |> fun continuation -> ANF.Let (rootId, ANF.Atom valueAtom, continuation)
                                |> wrapBindings valueBindings
                            (transition, varGen4)))

              // Try toAtom first; if it fails for complex expressions like Match, use toANF
              match toAtomCore sumTypeNames typeNames inertScopes value varGen env typeReg variantLookup funcReg functionNames moduleRegistry with
              | Ok (valueAtom, valueBindings, varGen1) ->
                  compileContinuation valueAtom valueBindings varGen1
              | Error _ ->
                  // Complex expression (like Match) - compile with toANF and transform returns
                  toANFCore sumTypeNames typeNames inertScopes value varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                  |> Result.bind (fun (valueExpr, varGen2) ->
                      let (rootId, varGen3) = ANF.freshVar varGen2
                      lowerLetPatternBindings pattern (ANF.Var rootId) valueType env [] varGen3
                      |> Result.bind (fun (env', patternBindingsRev, varGen4) ->
                          toANFCore sumTypeNames typeNames inertScopes body varGen4 env' typeReg variantLookup funcReg functionNames moduleRegistry
                          |> Result.map (fun (bodyExpr, varGen5) ->
                              let patternContinuation =
                                  wrapBindings (List.rev patternBindingsRev) bodyExpr
                              let rec transformReturns anfExpr =
                                  match anfExpr with
                                  | ANF.Return atom -> ANF.Let (rootId, ANF.Atom atom, patternContinuation)
                                  | ANF.Jump _ -> anfExpr
                                  | ANF.Join (parameter, continuation, entry) ->
                                      ANF.Join (parameter, transformReturns continuation, transformReturns entry)
                                  | ANF.Let (id, cexpr, rest) -> ANF.Let (id, cexpr, transformReturns rest)
                                  | ANF.If (cond, thenBr, elseBr) ->
                                      ANF.If (cond, transformReturns thenBr, transformReturns elseBr)
                              (transformReturns valueExpr, varGen5)))))

    | CheckedAST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: use operand type to select float vs integer path
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames typeNames innerExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun innerType ->
            match innerType with
            | AST.TFloat64 ->
                match innerExpr with
                | CheckedAST.FloatLiteral f ->
                    // Constant-fold negative float literals at compile time
                    Ok (ANF.Return (ANF.FloatLiteral (-f)), varGen)
                | _ ->
                    toANFBoundAtomCore sumTypeNames typeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (innerSetup, innerAtom, varGen1) ->
                        let (tempVar, varGen2) = ANF.freshVar varGen1
                        let cexpr = ANF.FloatNeg innerAtom
                        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                        (bindReturns innerSetup (fun _ -> finalExpr), varGen2))
            | AST.TInt64 ->
                match innerExpr with
                | CheckedAST.Int64Literal n when n = System.Int64.MinValue ->
                    // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
                    // When negated, it should remain INT64_MIN (mathematically correct)
                    Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue)), varGen)
                | _ ->
                    let zeroExpr = CheckedAST.Int64Literal 0L
                    toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TInt ->
                toANFCore sumTypeNames typeNames inertScopes
                    (CheckedAST.BinOp (AST.Sub, CheckedAST.BigIntLiteral System.Numerics.BigInteger.Zero, innerExpr))
                    varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TInt128 ->
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, CheckedAST.Int128Literal System.Int128.Zero, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TInt32 ->
                let zeroExpr = CheckedAST.Int32Literal 0l
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TInt16 ->
                let zeroExpr = CheckedAST.Int16Literal 0s
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TInt8 ->
                let zeroExpr = CheckedAST.Int8Literal 0y
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TUInt64 ->
                let zeroExpr = CheckedAST.UInt64Literal 0UL
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TUInt32 ->
                let zeroExpr = CheckedAST.UInt32Literal 0ul
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TUInt16 ->
                let zeroExpr = CheckedAST.UInt16Literal 0us
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TUInt8 ->
                let zeroExpr = CheckedAST.UInt8Literal 0uy
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | AST.TUInt128 ->
                toANFCore sumTypeNames typeNames inertScopes (CheckedAST.BinOp (AST.Sub, CheckedAST.UInt128Literal System.UInt128.Zero, innerExpr)) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            | _ ->
                Error $"Negation requires numeric operand, got {innerType}")

    | CheckedAST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom and apply Not
        toANFBoundAtomCore sumTypeNames typeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry |> Result.map (fun (innerSetup, innerAtom, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            (bindReturns innerSetup (fun _ -> finalExpr), varGen2))

    | CheckedAST.UnaryOp (AST.BitNot, innerExpr) ->
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames typeNames innerExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun innerType ->
            toANFBoundAtomCore sumTypeNames typeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.map (fun (innerSetup, innerAtom, varGen1) ->
                let (tempVar, varGen2) = ANF.freshVar varGen1
                let cexpr =
                    match innerType with
                    | AST.TInt -> ANF.Call (functionId "Darklang.Stdlib.Int.bitwiseNot", [innerAtom])
                    | AST.TInt128 -> ANF.Call (functionId "Darklang.Stdlib.Int128.bitwiseNot", [innerAtom])
                    | AST.TUInt128 -> ANF.Call (functionId "Darklang.Stdlib.UInt128.bitwiseNot", [innerAtom])
                    | _ -> ANF.UnaryPrim (ANF.BitNot, innerAtom)
                let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                (bindReturns innerSetup (fun _ -> finalExpr), varGen2)))

    | CheckedAST.BinOp (AST.StringConcat, left, right) ->
        let rec collectParts expr acc =
            match expr with
            | CheckedAST.BinOp (AST.StringConcat, nestedLeft, nestedRight) ->
                collectParts nestedLeft (collectParts nestedRight acc)
            | part -> part :: acc

        let rec lowerParts parts vg expressions atoms =
            match parts with
            | [] -> Ok (List.rev expressions, List.rev atoms, vg)
            | part :: rest ->
                toANFBoundAtomCore sumTypeNames typeNames inertScopes part vg env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (partExpr, partAtom, nextVg) ->
                    let representableAtom =
                        match partAtom with
                        | ANF.UnitLiteral ->
                            // RuntimeError uses Unit as its unreachable ANF return.
                            // Concat still needs a representation-valid operand for codegen.
                            ANF.StringLiteral ""
                        | _ -> partAtom
                    lowerParts rest nextVg (partExpr :: expressions) (representableAtom :: atoms))

        lowerParts (collectParts left (collectParts right [])) varGen [] []
        |> Result.map (fun (partExprs, partAtoms, varGen1) ->
            let nonemptyAtoms =
                partAtoms |> List.filter (function ANF.StringLiteral "" -> false | _ -> true)
            let sequence result =
                List.foldBack
                    (fun partExpr continuation -> bindReturns partExpr (fun _ -> continuation))
                    partExprs
                    result
            match nonemptyAtoms with
            | [] -> (sequence (ANF.Return (ANF.StringLiteral "")), varGen1)
            | [singleAtom] -> (sequence (ANF.Return singleAtom), varGen1)
            | firstAtom :: secondAtom :: remainingAtoms ->
                let (rawId, varGen2) = ANF.freshVar varGen1
                let (resultId, varGen3) = ANF.freshVar varGen2
                let fused =
                    ANF.Let (
                        rawId,
                        ANF.StringConcat (firstAtom, secondAtom, remainingAtoms),
                        ANF.Let (
                            resultId,
                            ANF.Call (functionId "Darklang.Stdlib.String.__normalizeAfterConcat", [ANF.Var rawId]),
                            ANF.Return (ANF.Var resultId)))
                (sequence fused, varGen3))

    | CheckedAST.BinOp (op, left, right) ->
        toANFBoundAtomCore sumTypeNames typeNames inertScopes left varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun (leftExpr, leftAtom, varGen1) ->
            toANFBoundAtomCore sumTypeNames typeNames inertScopes right varGen1 env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.bind (fun (rightExpr, rightAtom, varGen2) ->
                let typeEnv = typeEnvFromVarEnv env
                let buildCoreExpr () : Result<ANF.AExpr * ANF.VarGen, string> =
                    match op with
                    | AST.Eq | AST.Neq ->
                        // Infer type of left operand to check if structural comparison is needed
                        match inferTypeCore sumTypeNames typeNames left typeEnv typeReg variantLookup funcReg functionNames moduleRegistry with
                        | Ok operandType when isCompoundType operandType ->
                            // Generate structural equality
                            let (eqBindings, eqResultAtom, varGen3) =
                                generateStructuralEquality functionId leftAtom rightAtom operandType varGen2 typeReg variantLookup
                            // For Neq, negate the result
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                    (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                                else
                                    (eqResultAtom, eqBindings, varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | Ok AST.TInt ->
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let cexpr = ANF.Call (functionId "Darklang.Stdlib.Int.__equals", [leftAtom; rightAtom])
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                    (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                                else
                                    (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | Ok operandType when canonicalBufferKindForType operandType |> Option.isSome ->
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let kind =
                                canonicalBufferKindForType operandType
                                |> Option.defaultWith (fun () -> Crash.crash $"Expected canonical buffer type, got {operandType}")
                            let cexpr = ANF.CanonicalBufferEq (kind, leftAtom, rightAtom)
                            // For Neq, negate the result
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                    (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                                else
                                    (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | (Ok AST.TInt128 as wideType)
                        | (Ok AST.TUInt128 as wideType) ->
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let equalsName =
                                match wideType with
                                | Ok AST.TInt128 -> "Darklang.Stdlib.Int128.__equals"
                                | Ok AST.TUInt128 -> "Darklang.Stdlib.UInt128.__equals"
                                | _ -> Crash.crash "128-bit equality dispatch lost its operand type"
                            let cexpr = ANF.Call (functionId equalsName, [leftAtom; rightAtom])
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    (ANF.Var negVar, [(tempVar, cexpr); (negVar, ANF.UnaryPrim (ANF.Not, ANF.Var tempVar))], vg)
                                else (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | _ ->
                            // Primitive type or type inference failed - use simple comparison
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                            Ok (ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar)), varGen3)
                    | AST.StringConcat ->
                        Crash.crash "StringConcat must be lowered as a fused tree"
                    // Arithmetic, bitwise, and comparison operators - use simple primitive
                    | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow
                    | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                    | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                    | AST.And | AST.Or ->
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr =
                            match inferTypeCore sumTypeNames typeNames left typeEnv typeReg variantLookup funcReg functionNames moduleRegistry with
                            | Ok operandType ->
                                match integerFunctionForBinOp functionId operandType op with
                                | Some funcName -> ANF.Call (funcName, [leftAtom; rightAtom])
                                | None -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                            | _ -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        Ok (ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar)), varGen3)

                buildCoreExpr ()
                |> Result.map (fun (coreExpr, varGen3) ->
                    let withRight = bindReturns rightExpr (fun _ -> coreExpr)
                    let withLeft = bindReturns leftExpr (fun _ -> withRight)
                    (withLeft, varGen3))))

    | CheckedAST.If (cond, thenBranch, elseBranch) ->
        toANFBoundAtomCore sumTypeNames typeNames inertScopes cond varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun (condSetup, condAtom, varGen1) ->
            toANFCore sumTypeNames typeNames inertScopes thenBranch varGen1 env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.bind (fun (thenExpr, varGen2) ->
                toANFCore sumTypeNames typeNames inertScopes elseBranch varGen2 env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.map (fun (elseExpr, varGen3) ->
                    let ifExpr = ANF.If (condAtom, thenExpr, elseExpr)
                    (bindReturns condSetup (fun _ -> ifExpr), varGen3))))

    | CheckedAST.Sequence (first, next) ->
        // Preserve source order and let the final expression carry the value.
        // bindReturns also prevents the tail from running after a failing head.
        toANFCore sumTypeNames typeNames inertScopes first varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun (firstExpr, varGen1) ->
            toANFCore sumTypeNames typeNames inertScopes next varGen1 env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.map (fun (nextExpr, varGen2) ->
                (bindReturns firstExpr (fun _ -> nextExpr), varGen2)))

    | CheckedAST.Call (funcName, args)
        when functionNameIs funcName "Builtin.unwrap" ->
        let argList = exprArgsToList args
        match argList with
        | [argExpr] ->
            let typeEnv = typeEnvFromVarEnv env
            inferTypeCore sumTypeNames typeNames argExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.bind (fun argType ->
                let lookupVariantInfo (expectedTypeName: string) (variantName: string) : Result<int * AST.SemanticType list, string> =
                    match Map.tryFind variantName variantLookup with
                    | Some (typeName, _, tag, fieldTypes) when typeName = expectedTypeName ->
                        Ok (tag, fieldTypes)
                    | Some (typeName, _, _, _) ->
                        Error $"Builtin.unwrap expected variant {variantName} in {expectedTypeName}, got {typeName}"
                    | None ->
                        Error $"Builtin.unwrap could not find variant tag for {expectedTypeName}.{variantName}"

                let tryConstructorPayload expectedTypeName expectedVariantName expression =
                    match Map.tryFind expectedVariantName variantLookup, expression with
                    | Some (typeName, _, tag, _), CheckedAST.Constructor (reference, [payload])
                        when typeName = expectedTypeName
                             && Map.tryFind reference.TypeId typeNames.TypeNames = Some expectedTypeName
                             && tryFindConstructorTag reference.ConstructorId typeNames = Some tag ->
                        Some payload
                    | _ -> None

                let buildUnwrapExpr (successTag: int) (payloadType: AST.SemanticType) (failureMessage: string) : Result<ANF.AExpr * ANF.VarGen, string> =
                    toAtomCore sumTypeNames typeNames inertScopes argExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.map (fun (argAtom0, argBindings0, vg1) ->
                        let (argAtom, argBindings, vg2) =
                            match argAtom0 with
                            | ANF.Var _ -> (argAtom0, argBindings0, vg1)
                            | _ ->
                                let (argVar, vg') = ANF.freshVar vg1
                                (ANF.Var argVar, argBindings0 @ [(argVar, ANF.Atom argAtom0)], vg')

                        let (tagVar, vg3) = ANF.freshVar vg2
                        let (isSuccessVar, vg4) = ANF.freshVar vg3
                        let tagBindings = [
                            (tagVar, ANF.TupleGet (argAtom, 0))
                            (isSuccessVar, ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 successTag))))
                        ]

                        let normalizedPayloadType =
                            if containsTypeVar payloadType then AST.TUnit else payloadType

                        let (payloadVar, vg5) = ANF.freshVar vg4
                        let (typedPayloadVar, vg6) = ANF.freshVar vg5
                        let thenBranch =
                            ANF.Let (
                                payloadVar,
                                ANF.TupleGet (argAtom, 1),
                                ANF.Let (
                                    typedPayloadVar,
                                    ANF.TypedAtom (ANF.Var payloadVar, normalizedPayloadType),
                                    ANF.Return (ANF.Var typedPayloadVar)
                                )
                            )

                        let (printVar, vg7) = ANF.freshVar vg6
                        let elseBranch =
                            ANF.Let (
                                printVar,
                                ANF.RuntimeError failureMessage,
                                ANF.Return ANF.UnitLiteral
                            )

                        let ifExpr = ANF.If (ANF.Var isSuccessVar, thenBranch, elseBranch)
                        let finalExpr = wrapBindings (argBindings @ tagBindings) ifExpr
                        (finalExpr, vg7))

                match argType with
                | AST.TSum ("Darklang.Stdlib.Option.Option", [valueType]) ->
                    lookupVariantInfo "Darklang.Stdlib.Option.Option" "Some"
                    |> Result.bind (fun (successTag, _) ->
                        buildUnwrapExpr successTag valueType "Cannot unwrap None")
                | AST.TSum ("Darklang.Stdlib.Option.Option", []) ->
                    lookupVariantInfo "Darklang.Stdlib.Option.Option" "Some"
                    |> Result.bind (fun (successTag, fieldTypes) ->
                        let payloadTypeResult =
                            match tryConstructorPayload "Darklang.Stdlib.Option.Option" "Some" argExpr with
                            | Some payloadExpr ->
                                inferTypeCore sumTypeNames typeNames payloadExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                            | _ ->
                                match fieldTypes with
                                | [payloadType] -> Ok payloadType
                                | _ -> Ok AST.TUnit
                        payloadTypeResult
                        |> Result.bind (fun payloadType ->
                            buildUnwrapExpr successTag payloadType "Cannot unwrap None"))
                | AST.TSum ("Darklang.Stdlib.Result.Result", [okType; _]) ->
                    lookupVariantInfo "Darklang.Stdlib.Result.Result" "Ok"
                    |> Result.bind (fun (successTag, _) ->
                        let failureMessage =
                            match tryConstructorPayload "Darklang.Stdlib.Result.Result" "Error" argExpr with
                            | Some payloadExpr ->
                                match unwrapErrorPayloadToString payloadExpr with
                                | Some payloadText -> $"Cannot unwrap Error: {payloadText}"
                                | None -> "Cannot unwrap Error"
                            | _ ->
                                "Cannot unwrap Error"
                        buildUnwrapExpr successTag okType failureMessage)
                | AST.TSum ("Darklang.Stdlib.Result.Result", []) ->
                    lookupVariantInfo "Darklang.Stdlib.Result.Result" "Ok"
                    |> Result.bind (fun (successTag, fieldTypes) ->
                        let payloadTypeResult =
                            match tryConstructorPayload "Darklang.Stdlib.Result.Result" "Ok" argExpr with
                            | Some payloadExpr ->
                                inferTypeCore sumTypeNames typeNames payloadExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                            | _ ->
                                match fieldTypes with
                                | [payloadType] -> Ok payloadType
                                | _ -> Ok AST.TUnit
                        let failureMessage =
                            match tryConstructorPayload "Darklang.Stdlib.Result.Result" "Error" argExpr with
                            | Some payloadExpr ->
                                match unwrapErrorPayloadToString payloadExpr with
                                | Some payloadText -> $"Cannot unwrap Error: {payloadText}"
                                | None -> "Cannot unwrap Error"
                            | _ ->
                                "Cannot unwrap Error"
                        payloadTypeResult
                        |> Result.bind (fun payloadType ->
                            buildUnwrapExpr successTag payloadType failureMessage))
                | _ ->
                    Error $"Internal error: Builtin.unwrap should have been typechecked as Option/Result, got {typeToString argType}")
        | _ ->
            Error $"Internal error: Builtin.unwrap should have exactly 1 argument, got {List.length argList}"

    | CheckedAST.Call (funcName, args)
        when functionNameIs funcName "Builtin.testRuntimeError"
             || functionNameIs funcName "Builtin.crash" ->
        let argList = exprArgsToList args
        match argList with
        | [messageExpr] ->
            match unwrapErrorPayloadToString messageExpr with
            | Some messageText ->
                let fullMessage = $"Uncaught exception: {messageText}"
                let (runtimeErrorVar, varGen1) = ANF.freshVar varGen
                let runtimeErrorExpr = ANF.RuntimeError fullMessage
                Ok (ANF.Let (runtimeErrorVar, runtimeErrorExpr, ANF.Return ANF.UnitLiteral), varGen1)
            | None ->
                toAtomCore sumTypeNames typeNames inertScopes messageExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.map (fun (messageAtom, messageBindings, varGen1) ->
                    let (fullMessageVar, varGen2) = ANF.freshVar varGen1
                    let (runtimeErrorVar, varGen3) = ANF.freshVar varGen2
                    let errorExpr =
                        ANF.Let (
                            fullMessageVar,
                            ANF.StringConcat (ANF.StringLiteral "Uncaught exception: ", messageAtom, []),
                            ANF.Let (
                                runtimeErrorVar,
                                ANF.RuntimeErrorString (ANF.Var fullMessageVar),
                                ANF.Return ANF.UnitLiteral
                            )
                        )
                    (wrapBindings messageBindings errorExpr, varGen3))
        | _ ->
            Error $"Internal error: {funcName} should have exactly 1 argument, got {List.length argList}"

    | CheckedAST.Call (funcName, args) ->
        // Function call: convert all arguments to atoms
        // If an argument is a function reference, wrap it in a trivial closure for uniform calling convention
        let argExprList = exprArgsToList args
        let displayName =
            Map.tryFind funcName functionNames
            |> Option.orElseWith (fun () -> Map.tryFind funcName funcReg |> Option.map fst)
            |> Option.defaultWith (fun () ->
                Crash.crash
                    $"Function identity {AST.functionIdValue funcName} is absent from lowering registries")

        let wrapFuncRefInClosure (argExpr: ANF.AExpr) (atom: ANF.Atom) (vg: ANF.VarGen) : ANF.AExpr * ANF.Atom * ANF.VarGen =
            match atom with
            | ANF.FuncRef fnName ->
                // Function reference needs to be wrapped in a closure.
                let (closureId, vg') = ANF.freshVar vg
                let closureExpr = ANF.Let (closureId, ANF.ClosureAlloc (fnName, []), ANF.Return (ANF.Var closureId))
                let wrappedExpr = bindReturns argExpr (fun _ -> closureExpr)
                (wrappedExpr, ANF.Var closureId, vg')
            | _ ->
                (argExpr, atom, vg)

        let rec convertArgs
            (argExprs: CheckedAST.Expr list)
            (vg: ANF.VarGen)
            (accExprs: ANF.AExpr list)
            (accAtoms: ANF.Atom list)
            : Result<ANF.AExpr list * ANF.Atom list * ANF.VarGen, string> =
            match argExprs with
            | [] ->
                Ok (List.rev accExprs, List.rev accAtoms, vg)
            | arg :: rest ->
                toANFBoundAtomCore sumTypeNames typeNames inertScopes arg vg env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (argExpr, argAtom, vg') ->
                    // Wrap function references in closures for uniform calling convention.
                    let (wrappedExpr, wrappedAtom, vg'') = wrapFuncRefInClosure argExpr argAtom vg'
                    convertArgs rest vg'' (wrappedExpr :: accExprs) (wrappedAtom :: accAtoms))

        // Regular function call (including module functions like Stdlib.Int64.add)
        convertArgs argExprList varGen [] []
        |> Result.bind (fun (argSetupExprs, argAtoms, varGen1) ->
            // Bind call result to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let withArgSetups (finalExpr: ANF.AExpr) =
                List.foldBack
                    (fun argExpr acc -> bindReturns argExpr (fun _ -> acc))
                    argSetupExprs
                    finalExpr
            // Check if funcName is a variable (indirect call) or a defined function (direct call)
            // Not a variable - check explicit presentation effects first.
            match tryPresentationIntrinsic displayName argAtoms with
            | Some intrinsicExpr ->
                let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                Ok (withArgSetups finalExpr, varGen2)
            | None ->
            match tryCliIntrinsic displayName (normalizeNullaryIntrinsicArgs argAtoms) with
            | Some intrinsicExpr ->
                let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                Ok (withArgSetups finalExpr, varGen2)
            | None ->
            // Check if it's a file intrinsic.
            match tryFileIntrinsic displayName argAtoms with
            | Some intrinsicExpr ->
                let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                Ok (withArgSetups finalExpr, varGen2)
            | None ->
                // Check if it's a raw memory intrinsic
                match tryRawMemoryIntrinsic functionId sumTypeNames displayName argAtoms with
                | Some intrinsicExpr ->
                    // Raw memory intrinsic call
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                match tryCanonicalPrimitiveIntrinsic displayName argAtoms with
                | Some intrinsicExpr ->
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                // Check if it's a Float intrinsic
                match tryFloatIntrinsic displayName argAtoms with
                | Some intrinsicExpr ->
                    // Float intrinsic call
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                // Check if it's a random intrinsic
                match tryRandomIntrinsic displayName argAtoms with
                | Some intrinsicExpr ->
                    // Random intrinsic call
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                // Check if it's a DateTime intrinsic.
                match tryDateTimeIntrinsic displayName argAtoms with
                | Some intrinsicExpr ->
                    // DateTime intrinsic call.
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                // Check if it's a defined function
                match Map.tryFind funcName funcReg with
                | Some (_, AST.TFunction (paramTypes, _)) ->
                    // Direct call to defined function
                    let normalizedArgAtoms = normalizeSyntheticNullaryArgAtoms paramTypes argExprList argAtoms
                    let callExpr = ANF.Call (funcName, normalizedArgAtoms)
                    let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | Some _ ->
                    // Preserve existing behavior for malformed registry entries.
                    let callExpr = ANF.Call (funcName, argAtoms)
                    let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                    // Unknown function - could be error or forward reference
                    // For now, assume it's a valid function (will fail at link time if not)
                    let callExpr = ANF.Call (funcName, argAtoms)
                    let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2))

    | CheckedAST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"

    | CheckedAST.TupleLiteral elements ->
        // Convert all elements to bound atoms so tuple elements can include expressions
        // that cannot be lowered directly with toAtom (for example Builtin.testRuntimeError).
        let rec convertElements
            (elems: CheckedAST.Expr list)
            (vg: ANF.VarGen)
            (accExprs: ANF.AExpr list)
            (accAtoms: ANF.Atom list)
            : Result<ANF.AExpr list * ANF.Atom list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accExprs, List.rev accAtoms, vg)
            | elem :: rest ->
                toANFBoundAtomCore sumTypeNames typeNames inertScopes elem vg env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (elemExpr, elemAtom, vg') ->
                    convertElements rest vg' (elemExpr :: accExprs) (elemAtom :: accAtoms))

        convertElements elements varGen [] []
        |> Result.map (fun (elemExprs, elemAtoms, varGen1) ->
            // Create TupleAlloc and bind to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let tupleExpr = ANF.TupleAlloc elemAtoms
            let tupleAllocExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
            let exprWithSetups =
                List.foldBack
                    (fun elemExpr acc -> bindReturns elemExpr (fun _ -> acc))
                    elemExprs
                    tupleAllocExpr

            (exprWithSetups, varGen2))

    | CheckedAST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toANFBoundAtomCore sumTypeNames typeNames inertScopes tupleExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.map (fun (tupleSetup, tupleAtom, varGen1) ->
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let getExpr = ANF.TupleGet (tupleAtom, index)
            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
            (bindReturns tupleSetup (fun _ -> finalExpr), varGen2))

    | CheckedAST.RecordLiteral (reference, fields) ->
        let typeName =
            match tryFindRecordTypeNameById reference.TypeId typeNames with
            | Some name -> name
            | None -> Crash.crash "Resolved record type identity is absent from the lowering registry"
        // Evaluate fields in source order, then place their already-computed atoms
        // into the record's declaration-order layout.
        let recordInfo =
            match Map.tryFind typeName typeReg with
            | Some info -> info
            | None -> Crash.crash $"Record type '{typeName}' not found in typeReg"
        let fieldCount = List.length recordInfo.Fields

        let rec convertFields remaining vg acc =
            match remaining with
            | [] -> Ok (List.rev acc, vg)
            | (fieldId, fieldExpr) :: rest ->
                toANFBoundAtomCore sumTypeNames typeNames inertScopes fieldExpr vg env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun (setupExpr, fieldAtom, vg') ->
                    convertFields rest vg' ((fieldId, setupExpr, fieldAtom) :: acc))

        convertFields fields varGen []
        |> Result.map (fun (convertedFields, varGen1) ->
            let atomByIndex =
                convertedFields
                |> List.map (fun (fieldId, _, atom) -> (fieldIndex fieldId, atom))
                |> Map.ofList
            let orderedAtoms =
                [0 .. fieldCount - 1]
                |> List.map (fun fieldIndex ->
                    match Map.tryFind fieldIndex atomByIndex with
                    | Some atom -> atom
                    | None -> Crash.crash $"Record literal '{typeName}' is missing field slot {fieldIndex} after type checking")
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let allocation =
                ANF.Let (
                    resultVar,
                    ANF.RecordAlloc (recordDescriptor typeName reference.TypeArgs recordInfo, orderedAtoms),
                    ANF.Return (ANF.Var resultVar)
                )
            let withSourceOrderEvaluation =
                convertedFields
                |> List.map (fun (_, setupExpr, _) -> setupExpr)
                |> List.foldBack (fun setupExpr body -> bindReturns setupExpr (fun _ -> body)) <| allocation
            (withSourceOrderEvaluation, varGen2))

    | CheckedAST.RecordUpdate (recordExpr, updates) ->
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames typeNames recordExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | Some recordInfo ->
                    let typeFields = recordInfo.Fields
                    toANFBoundAtomCore sumTypeNames typeNames inertScopes recordExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.bind (fun (recordSetup, recordAtom, varGen1) ->
                        let rec convertUpdates remaining vg acc =
                            match remaining with
                            | [] -> Ok (List.rev acc, vg)
                            | (fieldId, updateExpr) :: rest ->
                                toANFBoundAtomCore sumTypeNames typeNames inertScopes updateExpr vg env typeReg variantLookup funcReg functionNames moduleRegistry
                                |> Result.bind (fun (setupExpr, updateAtom, vg') ->
                                    convertUpdates rest vg' ((fieldId, setupExpr, updateAtom) :: acc))

                        convertUpdates updates varGen1 []
                        |> Result.map (fun (convertedUpdates, varGen2) ->
                            let updatesByIndex =
                                convertedUpdates
                                |> List.map (fun (fieldId, _, atom) -> (fieldIndex fieldId, atom))
                                |> Map.ofList

                            let (fieldAtoms, projectionBindings, varGen3) =
                                typeFields
                                |> List.mapi (fun index (fieldName, _) -> (index, fieldName))
                                |> List.fold (fun (atoms, bindings, vg) (index, fieldName) ->
                                    match Map.tryFind index updatesByIndex with
                                    | Some atom -> (atom :: atoms, bindings, vg)
                                    | None ->
                                        let (fieldVar, vg') = ANF.freshVar vg
                                        (ANF.Var fieldVar :: atoms,
                                         (
                                             fieldVar,
                                             ANF.RecordGet (
                                                 recordDescriptor
                                                     typeName
                                                     typeArgs
                                                     recordInfo,
                                                 recordAtom,
                                                 index
                                             )
                                         ) :: bindings,
                                         vg')) ([], [], varGen2)

                            let (resultVar, varGen4) = ANF.freshVar varGen3
                            let allocation =
                                ANF.Let (
                                    resultVar,
                                    ANF.RecordClone (
                                        recordDescriptor
                                            typeName
                                            typeArgs
                                            recordInfo,
                                        recordAtom,
                                        List.rev fieldAtoms
                                    ),
                                    ANF.Return (ANF.Var resultVar)
                                )
                            let withProjections = wrapBindings (List.rev projectionBindings) allocation
                            let withUpdates =
                                convertedUpdates
                                |> List.map (fun (_, setupExpr, _) -> setupExpr)
                                |> List.foldBack (fun setupExpr body -> bindReturns setupExpr (fun _ -> body)) <| withProjections
                            let withRecord = bindReturns recordSetup (fun _ -> withUpdates)
                            (withRecord, varGen4)))
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error "Cannot use record update syntax on non-record type")

    | CheckedAST.RecordAccess (recordExpr, fieldName) ->
        // Projection is type-directed so the nominal descriptor and keyed slot
        // always agree, including after aliases and generic substitution.
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames typeNames recordExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, typeArgs) ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some recordInfo ->
                    match List.tryItem (fieldIndex fieldName) recordInfo.Fields with
                    | Some _ ->
                        let index = fieldIndex fieldName
                        toANFBoundAtomCore sumTypeNames typeNames inertScopes recordExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.map (fun (recordSetup, recordAtom, varGen1) ->
                            let (resultVar, varGen2) = ANF.freshVar varGen1
                            let getExpr =
                                ANF.RecordGet (
                                    recordDescriptor
                                        typeName
                                        typeArgs
                                        recordInfo,
                                    recordAtom,
                                    index
                                )
                            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                            (bindReturns recordSetup (fun _ -> finalExpr), varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | CheckedAST.Constructor (constructorReference, fields) ->
        match tryFindSumTypeNameById constructorReference.TypeId typeNames with
        | None -> Error "Resolved constructor type identity is absent from the lowering registry"
        | Some constructorTypeName ->
            let tag = constructorTag constructorReference.ConstructorId
            match tryFindVariantByTag constructorTypeName tag variantLookup with
            | None ->
                Error $"Unknown constructor tag: {tag}"
            | Some (typeName, typeParams, tag, variantFieldTypes) ->
                // Check if ANY variant in this type has a payload
                // If so, all variants must be heap-allocated for consistency
                // Note: We get typeName from variantLookup, not from AST (which may be empty)
                let typeHasPayloadVariants =
                    variantLookup
                    |> Map.exists (fun _ (tName, _, _, variantFields) ->
                        tName = typeName && not (List.isEmpty variantFields))

                let boxedDescriptor () =
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
                    |> Result.bind (function
                        | AST.TSum (inferredName, typeArgs) when inferredName = typeName ->
                            boxedSumDescriptor typeName typeParams typeArgs variantFieldTypes
                        | inferredType ->
                            Error $"Constructor '{typeName}' inferred unexpected type '{inferredType}'")

                match fields with
                | [] when not typeHasPayloadVariants ->
                    // Pure enum type (no payloads anywhere): return tag as an integer
                    Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 (int64 tag))), varGen)
                | [] ->
                    // No payload but type has other variants with payloads
                    // Heap-allocate as [tag, 0] for uniform 2-element structure
                    // This enables consistent structural equality comparison
                    boxedDescriptor ()
                    |> Result.map (fun descriptor ->
                        let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                        let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                        let (resultVar, varGen1) = ANF.freshVar varGen
                        let allocation = ANF.RecordAlloc (descriptor, [tagAtom; dummyPayload])
                        let finalExpr = ANF.Let (resultVar, allocation, ANF.Return (ANF.Var resultVar))
                        (finalExpr, varGen1))
                | _ ->
                    // Variant with payload: allocate [tag, payload] on heap
                    let payloadExpr =
                        match fields with
                        | [field] -> field
                        | _ -> CheckedAST.TupleLiteral fields
                    boxedDescriptor ()
                    |> Result.bind (fun descriptor ->
                        toANFBoundAtomCore sumTypeNames typeNames inertScopes payloadExpr varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.map (fun (payloadSetupExpr, payloadAtom, varGen1) ->
                            let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                            let (resultVar, varGen2) = ANF.freshVar varGen1
                            let allocation = ANF.RecordAlloc (descriptor, [tagAtom; payloadAtom])
                            let finalExpr = ANF.Let (resultVar, allocation, ANF.Return (ANF.Var resultVar))
                            let exprWithPayloadEvaluation = bindReturns payloadSetupExpr (fun _ -> finalExpr)
                            (exprWithPayloadEvaluation, varGen2)))

    | CheckedAST.ListLiteral elements ->
        // Compile list literal as SkewList
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        // Tag a raw pointer as a list value without routing through Stdlib wrappers.
        // Keep a typed binding so RC/type inference still treats the result as List<a>.
        let tagRawPtrAsList (listNode: AST.SemanticType) (tag: int64) (ptrVar: ANF.TempId) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (taggedRawVar, vg1) = ANF.freshVar vg
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 tag))
            let (taggedVar, vg2) = ANF.freshVar vg1
            let typedExpr = ANF.TypedAtom (ANF.Var taggedRawVar, listNode)
            (ANF.Var taggedVar, bindings @ [(taggedRawVar, tagExpr); (taggedVar, typedExpr)], vg2)

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (elemType: AST.SemanticType) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (setRcVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 16L))
            let setExpr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), elemAtom, elemType)
            let setRcExpr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), ANF.IntLiteral (ANF.Int64 1L))
            let vg4 = vg3
            let bindings4 = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (setRcVar, setRcExpr)]
            let leafListType = AST.TList elemType
            tagRawPtrAsList leafListType 5L ptrVar vg4 bindings4

        // Helper to create a SINGLE node containing a TreeNode
        let allocSingle (listNode: AST.SemanticType) (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (setRcVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 16L))
            let setExpr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, listNode)
            let setRcExpr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), ANF.IntLiteral (ANF.Int64 1L))
            let bindings1 = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (setRcVar, setRcExpr)]
            tagRawPtrAsList listNode 1L ptrVar vg3 bindings1

        // Helper to create a DEEP node
        let allocDeep (listNode: AST.SemanticType) (measure: int) (prefixNodes: ANF.Atom list) (middle: ANF.Atom) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let prefixCount = List.length prefixNodes
            let suffixCount = List.length suffixNodes
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 104L))  // 12 fields * 8 bytes + refcount

            // Build all the set operations
            let setAt offset value valueType vg bindings =
                let (setVar, vg') = ANF.freshVar vg
                let setExpr =
                    match valueType with
                    | Some slotType -> ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value, slotType)
                    | None -> ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value)
                (vg', bindings @ [(setVar, setExpr)])

            let (vg2, bindings2) = setAt 0 (ANF.IntLiteral (ANF.Int64 (int64 measure))) None vg1 (bindings @ [(ptrVar, allocExpr)])
            let (vg3, bindings3) = setAt 8 (ANF.IntLiteral (ANF.Int64 (int64 prefixCount))) None vg2 bindings2

            // Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
            let rec setPrefix nodes offset vg bindings =
                match nodes with
                | [] -> (vg, bindings)
                | n :: rest ->
                    let (vg', bindings') = setAt offset n (Some listNode) vg bindings
                    setPrefix rest (offset + 8) vg' bindings'
            let (vg4, bindings4) = setPrefix prefixNodes 16 vg3 bindings3

            // Set middle at offset 48 (type-uniform: another SkewList of nodes)
            let (vg5, bindings5) = setAt 48 middle (Some listNode) vg4 bindings4

            // Set suffix count at offset 56
            let (vg6, bindings6) = setAt 56 (ANF.IntLiteral (ANF.Int64 (int64 suffixCount))) None vg5 bindings5

            // Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
            let (vg7, bindings7) = setPrefix suffixNodes 64 vg6 bindings6

            // Set refcount at offset 96
            let (vg8, bindings8) = setAt 96 (ANF.IntLiteral (ANF.Int64 1L)) None vg7 bindings7

            // Tag with DEEP (2)
            tagRawPtrAsList listNode 2L ptrVar vg8 bindings8

        // Build SkewList nodes for middle spines without using pushBack.
        let emptyTree = ANF.IntLiteral (ANF.Int64 0L)

        let nodeAtom (node: ANF.Atom, _measure: int) = node
        let nodeMeasure (_node: ANF.Atom, measure: int) = measure

        // Helper to create a NODE2 (tag 3): [child0:8][child1:8][measure:8]
        let allocNode2 (listNode: AST.SemanticType) (left: ANF.Atom * int) (right: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 32L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom left, listNode)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom right, listNode)
            let measure = nodeMeasure left + nodeMeasure right
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), ANF.IntLiteral (ANF.Int64 (int64 measure)))
            let (setRcVar, vg5) = ANF.freshVar vg4
            let setRcExpr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), ANF.IntLiteral (ANF.Int64 1L))
            let bindings1 =
                bindings
                @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (setRcVar, setRcExpr)]
            let (taggedNode, bindings2, vg6) = tagRawPtrAsList listNode 3L ptrVar vg5 bindings1
            ((taggedNode, measure), bindings2, vg6)

        // Helper to create a NODE3 (tag 4): [child0:8][child1:8][child2:8][measure:8]
        let allocNode3 (listNode: AST.SemanticType) (first: ANF.Atom * int) (second: ANF.Atom * int) (third: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 40L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom first, listNode)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom second, listNode)
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), nodeAtom third, listNode)
            let measure = nodeMeasure first + nodeMeasure second + nodeMeasure third
            let (set3Var, vg5) = ANF.freshVar vg4
            let set3Expr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), ANF.IntLiteral (ANF.Int64 (int64 measure)))
            let (setRcVar, vg6) = ANF.freshVar vg5
            let setRcExpr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 32L), ANF.IntLiteral (ANF.Int64 1L))
            let bindings1 =
                bindings
                @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (set3Var, set3Expr); (setRcVar, setRcExpr)]
            let (taggedNode, bindings2, vg7) = tagRawPtrAsList listNode 4L ptrVar vg6 bindings1
            ((taggedNode, measure), bindings2, vg7)

        let splitAt count nodes =
            let rec loop remaining acc rest =
                match remaining, rest with
                | 0, _ -> Ok (List.rev acc, rest)
                | _, [] -> Error "List literal: not enough nodes for split"
                | n, x :: xs -> loop (n - 1) (x :: acc) xs
            loop count [] nodes

        let groupSizes nodeCount =
            if nodeCount < 2 then
                Error "List literal: middle spine needs at least 2 nodes"
            else
                match nodeCount % 3 with
                | 0 -> Ok (List.replicate (nodeCount / 3) 3)
                | 1 ->
                    if nodeCount < 4 then
                        Error "List literal: invalid middle spine size"
                    else
                        Ok (2 :: 2 :: List.replicate ((nodeCount - 4) / 3) 3)
                | _ ->
                    Ok (2 :: List.replicate ((nodeCount - 2) / 3) 3)

        let rec buildGroupedNodes listNode sizes nodes vg bindings acc =
            match sizes with
            | [] -> Ok (List.rev acc, bindings, vg)
            | size :: rest ->
                splitAt size nodes
                |> Result.bind (fun (group, remaining) ->
                    match size, group with
                    | 2, [a; b] ->
                        let (nodeInfo, bindings1, vg1) = allocNode2 listNode a b vg bindings
                        buildGroupedNodes listNode rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | 3, [a; b; c] ->
                        let (nodeInfo, bindings1, vg1) = allocNode3 listNode a b c vg bindings
                        buildGroupedNodes listNode rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | _ ->
                        Error $"List literal: unexpected group size {size}")

        let rec buildTree (listNode: AST.SemanticType) (nodes: (ANF.Atom * int) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let nodeCount = List.length nodes
            match nodes with
            | [] -> Ok (emptyTree, bindings, vg)
            | [single] ->
                let (resultAtom, resultBindings, vg1) = allocSingle listNode (nodeAtom single) vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | first :: rest when nodeCount <= 5 ->
                let totalMeasure = nodes |> List.sumBy nodeMeasure
                let prefixNodes = [nodeAtom first]
                let suffixNodes = rest |> List.map nodeAtom
                let (resultAtom, resultBindings, vg1) = allocDeep listNode totalMeasure prefixNodes emptyTree suffixNodes vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | _ ->
                splitAt 2 nodes
                |> Result.bind (fun (prefixNodes, rest) ->
                    let restLength = List.length rest
                    let middleCount = restLength - 2
                    splitAt middleCount rest
                    |> Result.bind (fun (middleNodes, suffixNodes) ->
                        groupSizes (List.length middleNodes)
                        |> Result.bind (fun sizes ->
                            buildGroupedNodes listNode sizes middleNodes vg bindings []
                            |> Result.bind (fun (groupedMiddle, bindings1, vg1) ->
                                buildTree listNode groupedMiddle vg1 bindings1
                                |> Result.map (fun (middleTree, bindings2, vg2) ->
                                    let totalMeasure = nodes |> List.sumBy nodeMeasure
                                    let prefixAtoms = prefixNodes |> List.map nodeAtom
                                    let suffixAtoms = suffixNodes |> List.map nodeAtom
                                    let (resultAtom, resultBindings, vg3) =
                                        allocDeep listNode totalMeasure prefixAtoms middleTree suffixAtoms vg2 bindings2
                                    (resultAtom, resultBindings, vg3))))))

        if List.isEmpty elements then
            // Empty list is EMPTY (represented as 0)
            Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 0L)), varGen)
        else
            let typeEnv = typeEnvFromVarEnv env

            // Evaluate elements in source order before constructing the list.
            let rec convertElements (elems: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.AExpr * ANF.Atom * AST.SemanticType) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    inferTypeCore sumTypeNames typeNames e typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.bind (fun elemType ->
                        toANFBoundAtomCore sumTypeNames typeNames inertScopes e vg env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.bind (fun (setupExpr, atom, vg') ->
                            convertElements rest vg' ((setupExpr, atom, elemType) :: acc)))

            convertElements elements varGen []
            |> Result.bind (fun (convertedElements, varGen1) ->
                let elemAtoms = convertedElements |> List.map (fun (_, atom, elemType) -> (atom, elemType))

                let listType =
                    match elemAtoms with
                    | (_, elemType) :: _ -> AST.TList elemType
                    | [] -> AST.TList (AST.TVar "a")
                let (resultAtom, resultBindings, varGen2) =
                    buildSkewListLiteral listType elemAtoms varGen1 []
                let (typedResultVar, varGen3) = ANF.freshVar varGen2
                let finalExpr =
                    ANF.Let (
                        typedResultVar,
                        ANF.TypedAtom (resultAtom, listType),
                        ANF.Return (ANF.Var typedResultVar)
                    )
                let allocation = wrapBindings resultBindings finalExpr
                let exprWithElements =
                    convertedElements
                    |> List.map (fun (setupExpr, _, _) -> setupExpr)
                    |> List.foldBack (fun setupExpr continuation -> bindReturns setupExpr (fun _ -> continuation)) <| allocation
                Ok (exprWithElements, varGen3))

    | CheckedAST.Match (scrutinee, cases) ->
        PatternLowering.lowerMatch toANFCore toAtomCore toANFBoundAtomCore functionIds sumTypeNames typeNames inertScopes scrutinee cases varGen env typeReg variantLookup funcReg functionNames moduleRegistry

    | CheckedAST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        // $"Hello {name}!" → "Hello " ++ name ++ "!"
        let partToExpr (part: CheckedAST.StringPart) : CheckedAST.Expr =
            match part with
            | CheckedAST.StringText s -> CheckedAST.StringLiteral s
            | CheckedAST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.Return (ANF.StringLiteral ""), varGen)
        | [single] ->
            // Single part → convert directly
            toANFCore sumTypeNames typeNames inertScopes (partToExpr single) varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        | first :: rest ->
            // Multiple parts → fold with StringConcat
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    CheckedAST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toANFCore sumTypeNames typeNames inertScopes desugared varGen env typeReg variantLookup funcReg functionNames moduleRegistry

    | CheckedAST.Lambda (_parameters, _, _body) ->
        // Lambda in expression position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | CheckedAST.IndirectApply (func, args) ->
        toAtomCore sumTypeNames typeNames inertScopes func varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun (funcAtom, funcBindings, varGen1) ->
            let rec convertArgs remaining vg acc =
                match remaining with
                | [] -> Ok (List.rev acc, vg)
                | arg :: rest ->
                    toAtomCore sumTypeNames typeNames inertScopes arg vg env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.bind (fun (argAtom, argBindings, vg') ->
                        convertArgs rest vg' ((argAtom, argBindings) :: acc))
            convertArgs (exprArgsToList args) varGen1 []
            |> Result.map (fun (argResults, varGen2) ->
                let argAtoms = argResults |> List.map fst
                let bindings = funcBindings @ (argResults |> List.collect snd)
                let (resultId, varGen3) = ANF.freshVar varGen2
                let resultExpr =
                    ANF.Let (resultId, ANF.IndirectCall (funcAtom, argAtoms), ANF.Return (ANF.Var resultId))
                (wrapBindings bindings resultExpr, varGen3)))

    | CheckedAST.Apply (func, args) ->
        // Apply a function expression to arguments
        // For now, only support immediate application of lambdas
        let argsList = exprArgsToList args
        match func with
        | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
            // Immediate application becomes one non-recursive let per binder.
            let parameterList = AST.NonEmptyList.toList parameters
            if List.length argsList <> List.length parameterList then
                Error $"Expected {List.length parameterList} arguments, got {List.length argsList}"
            else
                // Build nested let bindings: let p1 = arg1 in let p2 = arg2 in ... body
                let rec buildLets (ps: CheckedAST.LambdaParameter list) (as': CheckedAST.Expr list) : CheckedAST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | parameter :: restPs, argExpr :: restAs ->
                        CheckedAST.Let (parameter.Pattern, argExpr, buildLets restPs restAs)
                    | _ -> body  // Should not happen due to length check
                let desugared = buildLets parameterList argsList
                toANFCore sumTypeNames typeNames inertScopes desugared varGen env typeReg variantLookup funcReg functionNames moduleRegistry
        | CheckedAST.Local name ->
            // Calling a variable that might hold a closure
            match Map.tryFind name env with
            | Some (tempId, _) ->
                // Variable exists - treat as closure call
                let rec convertArgs (remaining: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtomCore sumTypeNames typeNames inertScopes arg vg env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs argsList varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    // Generate closure call
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Return (ANF.Var resultId), varGen2)
                    |> Result.map (fun (expr, vg) ->
                        (wrapBindings finalBindings expr, vg)))
            | None ->
                Error $"Cannot apply variable '{name}' as function - variable not in scope"

        | CheckedAST.Apply (_, _) ->
            // Nested application: (fun x -> fun y -> ...)(a)(b)(c)...
            // Flatten all nested applies first, then desugar from innermost out
            let rec flattenApplies expr argLists =
                match expr with
                | CheckedAST.Apply (innerFunc, innerArgs) ->
                    flattenApplies innerFunc (exprArgsToList innerArgs :: argLists)
                | other -> (other, argLists)

            let (baseFunc, allArgLists) = flattenApplies func [argsList]
            // allArgLists is a list of arg lists, from innermost to outermost
            // e.g., for f(1)(2)(3), we get ([1], [2], [3])

            match baseFunc with
            | CheckedAST.Lambda _ ->
                // Desugar all nested lambda applications at once
                let rec desugaAll (currentFunc: CheckedAST.Expr) (remainingArgLists: CheckedAST.Expr list list) : CheckedAST.Expr =
                    match remainingArgLists with
                    | [] -> currentFunc
                    | currentArgs :: restArgLists ->
                        match currentFunc with
                        | CheckedAST.Lambda (lambdaParams, returnAnnotation, body) ->
                            let lambdaParamList = AST.NonEmptyList.toList lambdaParams
                            if List.length currentArgs <> List.length lambdaParamList then
                                // Will error later, just wrap in Apply for now
                                desugaAll (CheckedAST.Apply (currentFunc, exprArgsFromList currentArgs)) restArgLists
                            else
                                // Desugar: let p1 = a1 in let p2 = a2 in ... body
                                let rec buildLets (ps: CheckedAST.LambdaParameter list) (as': CheckedAST.Expr list) : CheckedAST.Expr =
                                    match ps, as' with
                                    | [], [] -> body
                                    | parameter :: restPs, argExpr :: restAs ->
                                        CheckedAST.Let (parameter.Pattern, argExpr, buildLets restPs restAs)
                                    | _ -> body
                                let desugared = buildLets lambdaParamList currentArgs
                                desugaAll desugared restArgLists
                        | CheckedAST.Let (name, value, innerBody) ->
                            // Float let out: Apply(let x = v in body, args) → let x = v in Apply(body, args)
                            CheckedAST.Let (name, value, desugaAll innerBody (currentArgs :: restArgLists))
                        | _ ->
                            // Non-lambda function - wrap remaining in Apply
                            let applied = CheckedAST.Apply (currentFunc, exprArgsFromList currentArgs)
                            desugaAll applied restArgLists

                let desugared = desugaAll baseFunc allArgLists
                toANFCore sumTypeNames typeNames inertScopes desugared varGen env typeReg variantLookup funcReg functionNames moduleRegistry

            | _ ->
                // Base function is not a lambda - use toAtom which handles nested applies
                // Reconstruct the full nested apply, then delegate to toAtom
                let rec applyAll (currentExpr: CheckedAST.Expr) (remainingArgLists: CheckedAST.Expr list list) : CheckedAST.Expr =
                    match remainingArgLists with
                    | [] -> currentExpr
                    | currentArgs :: rest ->
                        applyAll (CheckedAST.Apply (currentExpr, exprArgsFromList currentArgs)) rest
                let fullApply = applyAll baseFunc allArgLists

                toAtomCore sumTypeNames typeNames inertScopes fullApply varGen env typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.map (fun (resultAtom, bindings, vg) ->
                    (wrapBindings bindings (ANF.Return resultAtom), vg))

        | CheckedAST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) → let x = v in Apply(body, args)
            // Float the let binding out
            toANFCore sumTypeNames typeNames inertScopes (CheckedAST.Let (letName, letValue, CheckedAST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg functionNames moduleRegistry

        | CheckedAST.Closure (funcName, captures) ->
            // Closure being called directly - convert to ClosureCall
            // First, convert captures to atoms
            let rec convertCaptures (caps: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | CheckedAST.FuncRef funcName :: rest ->
                    convertCaptures rest vg ((ANF.FuncRef funcName, []) :: acc)
                | cap :: rest ->
                    toAtomCore sumTypeNames typeNames inertScopes cap vg env typeReg variantLookup funcReg functionNames moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                // Allocate closure
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                // Convert args
                let rec convertArgs
                    (remaining: CheckedAST.Expr list)
                    (vg: ANF.VarGen)
                    (acc: (ANF.AExpr * ANF.Atom) list)
                    =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toANFBoundAtomCore sumTypeNames typeNames inertScopes arg vg env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.bind (fun (argExpr, argAtom, vg') ->
                            convertArgs rest vg' ((argExpr, argAtom) :: acc))
                convertArgs argsList varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map snd
                    // Generate closure call
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let callExpr = ANF.Let (resultId, closureCall, ANF.Return (ANF.Var resultId))
                    let withArguments =
                        argResults
                        |> List.map fst
                        |> List.foldBack (fun argExpr continuation -> bindReturns argExpr (fun _ -> continuation)) <| callExpr
                    let withClosure = ANF.Let (closureId, closureAlloc, withArguments)
                    Ok (wrapBindings captureBindings withClosure, varGen4)))

        | _ ->
            // General function-expression application (for example record field access):
            // evaluate function expression to a closure value, then invoke it.
            toAtomCore sumTypeNames typeNames inertScopes func varGen env typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.bind (fun (funcAtom, funcBindings, varGen1) ->
                let rec convertArgs
                    (remaining: CheckedAST.Expr list)
                    (vg: ANF.VarGen)
                    (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list)
                    =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtomCore sumTypeNames typeNames inertScopes arg vg env typeReg variantLookup funcReg functionNames moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))

                convertArgs argsList varGen1 []
                |> Result.map (fun (argResults, varGen2) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    let (resultId, varGen3) = ANF.freshVar varGen2
                    let closureCall = ANF.ClosureCall (funcAtom, argAtoms)
                    let allBindings = funcBindings @ argBindings @ [(resultId, closureCall)]
                    (wrapBindings allBindings (ANF.Return (ANF.Var resultId)), varGen3)))
