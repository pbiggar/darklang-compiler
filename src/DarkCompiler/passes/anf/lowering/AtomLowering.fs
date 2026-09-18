// AtomLowering.fs - Lower atom-producing expressions and their ordered binding prefixes.

module AtomLowering

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
open LoweringCallbacks

let lowerAtom (toANFCore: ExpressionLowerer) (toAtomCore: AtomLowerer) (toANFBoundAtomCore: BoundAtomLowerer) (sumTypeNames: Set<string>) (inertScopes: Set<string>) (expr: CheckedAST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match expr with
    | CheckedAST.RecursiveLet _ -> Error "RecursiveLet must be lowered during lambda lifting"
    | CheckedAST.DictLiteral (_, _, []) ->
        Ok (ANF.IntLiteral (ANF.Int64 0L), [], varGen)
    | CheckedAST.DictLiteral _ -> Error "Non-empty DictLiteral must be lowered during generic specialization"
    | CheckedAST.BoundaryRender _ ->
        Error "BoundaryRender must be lowered through toANF"
    | CheckedAST.RuntimeError _ ->
        Error "Compiler-generated RuntimeError must be lowered through toANF"
    | CheckedAST.UnitLiteral ->
        Ok (ANF.UnitLiteral, [], varGen)

    | CheckedAST.Int64Literal n ->
        Ok (ANF.IntLiteral (ANF.Int64 n), [], varGen)

    | CheckedAST.Int128Literal n ->
        let (resultVar, varGen1) = ANF.freshVar varGen
        Ok (ANF.Var resultVar, [(resultVar, int128Construction n)], varGen1)

    | CheckedAST.BigIntLiteral n ->
        Ok (ANF.StringLiteral (n.ToString()), [], varGen)

    | CheckedAST.Int8Literal n ->
        Ok (ANF.IntLiteral (ANF.Int8 n), [], varGen)

    | CheckedAST.Int16Literal n ->
        Ok (ANF.IntLiteral (ANF.Int16 n), [], varGen)

    | CheckedAST.Int32Literal n ->
        Ok (ANF.IntLiteral (ANF.Int32 n), [], varGen)

    | CheckedAST.UInt8Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt8 n), [], varGen)

    | CheckedAST.UInt16Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt16 n), [], varGen)

    | CheckedAST.UInt32Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt32 n), [], varGen)

    | CheckedAST.UInt64Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt64 n), [], varGen)

    | CheckedAST.UInt128Literal n ->
        let (resultVar, varGen1) = ANF.freshVar varGen
        Ok (ANF.Var resultVar, [(resultVar, uint128Construction n)], varGen1)

    | CheckedAST.BoolLiteral b ->
        Ok (ANF.BoolLiteral b, [], varGen)

    | CheckedAST.StringLiteral s ->
        Ok (ANF.StringLiteral (s.Normalize(System.Text.NormalizationForm.FormC)), [], varGen)

    | CheckedAST.CharLiteral s ->
        // Char literal uses same representation as string
        Ok (ANF.StringLiteral (s.Normalize(System.Text.NormalizationForm.FormC)), [], varGen)

    | CheckedAST.FloatLiteral f ->
        Ok (ANF.FloatLiteral f, [], varGen)

    | CheckedAST.Local id ->
        match Map.tryFind id env with
        | Some (tempId, _) -> Ok (ANF.Var tempId, [], varGen)
        | None -> Error "Undefined local binding identity"

    | CheckedAST.NamedValue name ->
        if isBuiltinTestNanName name then
            Ok (ANF.FloatLiteral System.Double.NaN, [], varGen)
        else if isBuiltinTestInfinityName name then
            Ok (ANF.FloatLiteral System.Double.PositiveInfinity, [], varGen)
        else if isBuiltinBlobEmptyName name then
            Ok (ANF.StringLiteral "", [], varGen)
        else if name = "Darklang.LanguageTools.PackageManager.PickContext.empty" then
            toAtomCore sumTypeNames inertScopes
                (CheckedAST.RecordLiteral (
                    { TypeName = "Darklang.LanguageTools.PackageManager.PickContext"; TypeArgs = [] },
                    [("currentModule", CheckedAST.ListLiteral [])]
                ))
                varGen env typeReg variantLookup funcReg moduleRegistry
        else if name = "Darklang.Stdlib.List.empty" || name = "Darklang.Stdlib.List.empty_v0" then
            Ok (ANF.IntLiteral (ANF.Int64 0L), [], varGen)
        else
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some (_, resolvedName) ->
                let (closureId, varGen') = ANF.freshVar varGen
                let closureAlloc = ANF.ClosureAlloc (resolvedName, [])
                Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
            | None ->
                // Check if it's a function reference (function name used as value)
                match tryLookupResolved name funcReg with
                | Some (_, resolvedName) ->
                    let (closureId, varGen') = ANF.freshVar varGen
                    let closureAlloc = ANF.ClosureAlloc (resolvedName, [])
                    Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
                | None ->
                    Error $"Undefined named value: {name}"

    | CheckedAST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')

    | CheckedAST.Closure (funcName, captures) ->
        // Closure in atom position: convert captures and create ClosureAlloc binding
        let rec convertCaptures (caps: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | CheckedAST.FuncRef funcName :: rest ->
                convertCaptures rest vg ((ANF.FuncRef funcName, []) :: acc)
            | cap :: rest ->
                toAtomCore sumTypeNames inertScopes cap vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Create binding for ClosureAlloc
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            (ANF.Var closureId, allBindings @ [(closureId, closureAlloc)], varGen2))

    | CheckedAST.Let (pattern, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            if not (letPatternAcceptsType pattern valueType) then
                Error "Binding mismatch requires control-flow lowering"
            else
              toAtomCore sumTypeNames inertScopes value varGen env typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                match pattern with
                | CheckedAST.LPVariable name ->
                    let (bindingId, varGen2) = ANF.freshVar varGen1
                    let env' = Map.add name (bindingId, valueType) env
                    toAtomCore sumTypeNames inertScopes body varGen2 env' typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                        (bodyAtom,
                         valueBindings @ [(bindingId, ANF.Atom valueAtom)] @ bodyBindings,
                         varGen3))
                | CheckedAST.LPUnit | CheckedAST.LPWildcard ->
                    toAtomCore sumTypeNames inertScopes body varGen1 env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyAtom, bodyBindings, varGen2) ->
                        (bodyAtom, valueBindings @ bodyBindings, varGen2))
                | CheckedAST.LPTuple _ ->
                    let (rootId, varGen2) = ANF.freshVar varGen1
                    lowerLetPatternBindings pattern (ANF.Var rootId) valueType env [] varGen2
                    |> Result.bind (fun (env', patternBindingsRev, varGen3) ->
                        toAtomCore sumTypeNames inertScopes body varGen3 env' typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyAtom, bodyBindings, varGen4) ->
                            let allBindings =
                                valueBindings
                                @ [(rootId, ANF.Atom valueAtom)]
                                @ (List.rev patternBindingsRev)
                                @ bodyBindings
                            (bodyAtom, allBindings, varGen4)))))

    | CheckedAST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: use operand type to select float vs integer path
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames innerExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun innerType ->
            match innerType with
            | AST.TFloat64 ->
                match innerExpr with
                | CheckedAST.FloatLiteral f ->
                    // Constant-fold negative float literals at compile time
                    Ok (ANF.FloatLiteral (-f), [], varGen)
                | _ ->
                    toAtomCore sumTypeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
                        let (tempVar, varGen2) = ANF.freshVar varGen1
                        let cexpr = ANF.FloatNeg innerAtom
                        let allBindings = innerBindings @ [(tempVar, cexpr)]
                        (ANF.Var tempVar, allBindings, varGen2))
            | AST.TInt64 ->
                match innerExpr with
                | CheckedAST.Int64Literal n when n = System.Int64.MinValue ->
                    // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
                    // When negated, it should remain INT64_MIN (mathematically correct)
                    Ok (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue), [], varGen)
                | _ ->
                    let zeroExpr = CheckedAST.Int64Literal 0L
                    toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt ->
                toAtomCore sumTypeNames inertScopes
                    (CheckedAST.BinOp (AST.Sub, CheckedAST.BigIntLiteral System.Numerics.BigInteger.Zero, innerExpr))
                    varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt128 ->
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, CheckedAST.Int128Literal System.Int128.Zero, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt32 ->
                let zeroExpr = CheckedAST.Int32Literal 0l
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt16 ->
                let zeroExpr = CheckedAST.Int16Literal 0s
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt8 ->
                let zeroExpr = CheckedAST.Int8Literal 0y
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TUInt64 ->
                let zeroExpr = CheckedAST.UInt64Literal 0UL
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TUInt32 ->
                let zeroExpr = CheckedAST.UInt32Literal 0ul
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TUInt16 ->
                let zeroExpr = CheckedAST.UInt16Literal 0us
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TUInt8 ->
                let zeroExpr = CheckedAST.UInt8Literal 0uy
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TUInt128 ->
                toAtomCore sumTypeNames inertScopes (CheckedAST.BinOp (AST.Sub, CheckedAST.UInt128Literal System.UInt128.Zero, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                Error $"Negation requires numeric operand, got {innerType}")

    | CheckedAST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom, create binding
        toAtomCore sumTypeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | CheckedAST.UnaryOp (AST.BitNot, innerExpr) ->
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames innerExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun innerType ->
            toAtomCore sumTypeNames inertScopes innerExpr varGen env typeReg variantLookup funcReg moduleRegistry
            |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
                let (tempVar, varGen2) = ANF.freshVar varGen1
                let cexpr =
                    match innerType with
                    | AST.TInt -> ANF.Call ("Darklang.Stdlib.Int.bitwiseNot", [innerAtom])
                    | AST.TInt128 -> ANF.Call ("Darklang.Stdlib.Int128.bitwiseNot", [innerAtom])
                    | AST.TUInt128 -> ANF.Call ("Darklang.Stdlib.UInt128.bitwiseNot", [innerAtom])
                    | _ -> ANF.UnaryPrim (ANF.BitNot, innerAtom)
                (ANF.Var tempVar, innerBindings @ [(tempVar, cexpr)], varGen2)))

    | CheckedAST.BinOp (AST.StringConcat, left, right) ->
        let rec collectParts expr acc =
            match expr with
            | CheckedAST.BinOp (AST.StringConcat, nestedLeft, nestedRight) ->
                collectParts nestedLeft (collectParts nestedRight acc)
            | part -> part :: acc

        let rec lowerParts parts vg bindingGroups atoms =
            match parts with
            | [] -> Ok (List.rev atoms, bindingGroups |> List.rev |> List.concat, vg)
            | part :: rest ->
                toAtomCore sumTypeNames inertScopes part vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (partAtom, partBindings, nextVg) ->
                    lowerParts rest nextVg (partBindings :: bindingGroups) (partAtom :: atoms))

        lowerParts (collectParts left (collectParts right [])) varGen [] []
        |> Result.map (fun (partAtoms, partBindings, varGen1) ->
            let nonemptyAtoms =
                partAtoms |> List.filter (function ANF.StringLiteral "" -> false | _ -> true)
            match nonemptyAtoms with
            | [] -> (ANF.StringLiteral "", partBindings, varGen1)
            | [singleAtom] -> (singleAtom, partBindings, varGen1)
            | firstAtom :: secondAtom :: remainingAtoms ->
                let (rawId, varGen2) = ANF.freshVar varGen1
                let (resultId, varGen3) = ANF.freshVar varGen2
                let bindings =
                    partBindings
                    @ [ (rawId, ANF.StringConcat (firstAtom, secondAtom, remainingAtoms))
                        (resultId, ANF.Call ("Darklang.Stdlib.String.__normalizeAfterConcat", [ANF.Var rawId])) ]
                (ANF.Var resultId, bindings, varGen3))

    | CheckedAST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtomCore sumTypeNames inertScopes left varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtomCore sumTypeNames inertScopes right varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (rightAtom, rightBindings, varGen2) ->
                // Check if this is an equality comparison on compound types
                let typeEnv = typeEnvFromVarEnv env
                match op with
                | AST.Eq | AST.Neq ->
                    match inferTypeCore sumTypeNames left typeEnv typeReg variantLookup funcReg moduleRegistry with
                    | Ok operandType when isCompoundType operandType ->
                        // Generate structural equality
                        let (eqBindings, eqResultAtom, varGen3) =
                            generateStructuralEquality leftAtom rightAtom operandType varGen2 typeReg variantLookup
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                            else
                                (eqResultAtom, eqBindings, varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | Ok AST.TInt ->
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Call ("Darklang.Stdlib.Int.__equals", [leftAtom; rightAtom])
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                            else
                                (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
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
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | (Ok AST.TInt128 as wideType)
                    | (Ok AST.TUInt128 as wideType) ->
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let equalsName =
                            match wideType with
                            | Ok AST.TInt128 -> "Darklang.Stdlib.Int128.__equals"
                            | Ok AST.TUInt128 -> "Darklang.Stdlib.UInt128.__equals"
                            | _ -> Crash.crash "128-bit equality dispatch lost its operand type"
                        let cexpr = ANF.Call (equalsName, [leftAtom; rightAtom])
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                (ANF.Var negVar, [(tempVar, cexpr); (negVar, ANF.UnaryPrim (ANF.Not, ANF.Var tempVar))], vg)
                            else (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                        Ok (finalAtom, leftBindings @ rightBindings @ finalBindings, varGen4)
                    | _ ->
                        // Primitive type - simple comparison
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen3)
                | AST.StringConcat ->
                    Crash.crash "StringConcat must be lowered as a fused tree"
                // Arithmetic, bitwise, and comparison operators - use simple primitive
                | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow
                | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                | AST.And | AST.Or ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr =
                        match inferTypeCore sumTypeNames left typeEnv typeReg variantLookup funcReg moduleRegistry with
                        | Ok operandType ->
                            match integerFunctionForBinOp operandType op with
                            | Some funcName -> ANF.Call (funcName, [leftAtom; rightAtom])
                            | None -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        | _ -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                    let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen3)))

    | CheckedAST.If (condExpr, thenExpr, elseExpr) ->
        // IfValue selects atoms, but any bindings execute before it. Branches
        // with bindings must use full control-flow lowering to remain lazy.
        toAtomCore sumTypeNames inertScopes condExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtomCore sumTypeNames inertScopes thenExpr varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtomCore sumTypeNames inertScopes elseExpr varGen2 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
                    if List.isEmpty thenBindings && List.isEmpty elseBindings then
                        // Create a temporary for the result
                        let (tempVar, varGen4) = ANF.freshVar varGen3
                        // Create an IfValue CExpr
                        let ifCExpr = ANF.IfValue (condAtom, thenAtom, elseAtom)
                        // Return temp as atom with all bindings
                        let allBindings = condBindings @ thenBindings @ elseBindings @ [(tempVar, ifCExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen4)
                    else
                        Error "If expression requires lazy branch lowering")))

    | CheckedAST.Sequence _ ->
        Error "Sequence expression requires ordered lowering"

    | CheckedAST.Call (funcName, args) ->
        if isBuiltinUnwrapName funcName then
            Error "Internal error: Builtin.unwrap should be lowered via toANF, not toAtom"
        elif isRuntimeFailureName funcName then
            let argList = exprArgsToList args
            match argList with
            | [messageExpr] ->
                match unwrapErrorPayloadToString messageExpr with
                | Some messageText ->
                    let (runtimeErrorVar, varGen1) = ANF.freshVar varGen
                    Ok (
                        ANF.UnitLiteral,
                        [(runtimeErrorVar, ANF.RuntimeError $"Uncaught exception: {messageText}")],
                        varGen1
                    )
                | None ->
                    toAtomCore sumTypeNames inertScopes messageExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (messageAtom, messageBindings, varGen1) ->
                        let (fullMessageVar, varGen2) = ANF.freshVar varGen1
                        let (runtimeErrorVar, varGen3) = ANF.freshVar varGen2
                        (
                            ANF.UnitLiteral,
                            messageBindings
                            @ [ (fullMessageVar,
                                 ANF.StringConcat (ANF.StringLiteral "Uncaught exception: ", messageAtom, []))
                                (runtimeErrorVar, ANF.RuntimeErrorString (ANF.Var fullMessageVar)) ],
                            varGen3
                        ))
            | _ ->
                Error $"Internal error: {funcName} should have exactly 1 argument, got {List.length argList}"
        else
            // Function call in atom position: convert all arguments to atoms
            let argExprList = exprArgsToList args

            let rec convertArgs (argExprs: CheckedAST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match argExprs with
                | [] -> Ok (List.rev accAtoms, accBindings, vg)
                | arg :: rest ->
                    toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (argAtom, argBindings, vg') ->
                        convertArgs rest vg' (argAtom :: accAtoms) (accBindings @ argBindings))

            convertArgs argExprList varGen [] []
            |> Result.bind (fun (argAtoms, argBindings, varGen1) ->
                // Create a temporary for the call result
                let (tempVar, varGen2) = ANF.freshVar varGen1
                // Check if funcName is a variable (indirect call) or a defined function (direct call)
                // Not a variable - check explicit presentation effects first.
                match tryPresentationIntrinsic funcName argAtoms with
                | Some intrinsicExpr ->
                    let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen2)
                | None ->
                    match tryCliIntrinsic funcName (normalizeNullaryIntrinsicArgs argAtoms) with
                    | Some intrinsicExpr ->
                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2)
                    | None ->
                    // Check if it's a file intrinsic.
                    match tryFileIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2)
                    | None ->
                    // Check if it's a raw memory intrinsic
                    match tryRawMemoryIntrinsic sumTypeNames funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Raw memory intrinsic call
                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2)
                    | None ->
                        match tryCanonicalPrimitiveIntrinsic funcName argAtoms with
                        | Some intrinsicExpr ->
                            let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2)
                        | None ->
                        // Check if it's a Float intrinsic
                        match tryFloatIntrinsic funcName argAtoms with
                        | Some intrinsicExpr ->
                            // Float intrinsic call
                            let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2)
                        | None ->
                            // Check if it's a random intrinsic
                            match tryRandomIntrinsic funcName argAtoms with
                            | Some intrinsicExpr ->
                                // Random intrinsic call
                                let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                Ok (ANF.Var tempVar, allBindings, varGen2)
                            | None ->
                                // Check if it's a DateTime intrinsic.
                                match tryDateTimeIntrinsic funcName argAtoms with
                                | Some intrinsicExpr ->
                                    // DateTime intrinsic call.
                                    let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                    Ok (ANF.Var tempVar, allBindings, varGen2)
                                | None ->
                                        // Assume it's a defined function (direct call)
                                        let callArgAtoms =
                                            match Map.tryFind funcName funcReg with
                                            | Some (AST.TFunction (paramTypes, _)) ->
                                                normalizeSyntheticNullaryArgAtoms paramTypes argExprList argAtoms
                                            | _ ->
                                                argAtoms
                                        let callCExpr = ANF.Call (funcName, callArgAtoms)
                                        let allBindings = argBindings @ [(tempVar, callCExpr)]
                                        Ok (ANF.Var tempVar, allBindings, varGen2))

    | CheckedAST.TypeApp (_, _, _) ->
        // Placeholder: Generic instantiation not yet implemented
        Error "TypeApp (generic instantiation) not yet implemented in toAtom"

    | CheckedAST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: CheckedAST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtomCore sumTypeNames inertScopes elem vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create a temporary for the tuple
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let tupleCExpr = ANF.TupleAlloc elemAtoms
            // Return temp as atom with all bindings
            let allBindings = elemBindings @ [(tempVar, tupleCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | CheckedAST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtomCore sumTypeNames inertScopes tupleExpr varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let getCExpr = ANF.TupleGet (tupleAtom, index)
            // Return temp as atom with all bindings
            let allBindings = tupleBindings @ [(tempVar, getCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | CheckedAST.RecordLiteral (reference, fields) ->
        let typeName = reference.TypeName
        // Evaluate field expressions in source order, independently of the
        // declaration-order tuple layout used for the record value.
        let fieldOrder =
            match Map.tryFind typeName typeReg with
            | Some recordInfo -> recordInfo.Fields |> List.map fst
            | None -> Crash.crash $"Record type '{typeName}' not found in typeReg"

        let rec convertFields remaining vg acc =
            match remaining with
            | [] -> Ok (List.rev acc, vg)
            | (fieldName, fieldExpr) :: rest ->
                toAtomCore sumTypeNames inertScopes fieldExpr vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (fieldAtom, fieldBindings, vg') ->
                    convertFields rest vg' ((fieldName, fieldAtom, fieldBindings) :: acc))

        convertFields fields varGen []
        |> Result.map (fun (convertedFields, varGen1) ->
            let atomByName =
                convertedFields
                |> List.map (fun (fieldName, atom, _) -> (fieldName, atom))
                |> Map.ofList
            let orderedAtoms =
                fieldOrder
                |> List.map (fun fieldName ->
                    match Map.tryFind fieldName atomByName with
                    | Some atom -> atom
                    | None -> Crash.crash $"Record literal '{typeName}' is missing field '{fieldName}' after type checking")
            let sourceBindings =
                convertedFields |> List.collect (fun (_, _, bindings) -> bindings)
            let (tempVar, varGen2) = ANF.freshVar varGen1
            (ANF.Var tempVar,
             sourceBindings
             @ [(tempVar,
                 ANF.RecordAlloc (
                    recordDescriptor reference (Map.find typeName typeReg),
                    orderedAtoms
                 ))],
             varGen2))

    | CheckedAST.RecordUpdate (recordExpr, updates) ->
        // Evaluate the record once, then updates in source order, before
        // projecting untouched fields and allocating the layout tuple.
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | Some recordInfo ->
                    let typeFields = recordInfo.Fields
                    toAtomCore sumTypeNames inertScopes recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                        let rec convertUpdates remaining vg acc =
                            match remaining with
                            | [] -> Ok (List.rev acc, vg)
                            | (fieldName, updateExpr) :: rest ->
                                toAtomCore sumTypeNames inertScopes updateExpr vg env typeReg variantLookup funcReg moduleRegistry
                                |> Result.bind (fun (updateAtom, updateBindings, vg') ->
                                    convertUpdates rest vg' ((fieldName, updateAtom, updateBindings) :: acc))

                        convertUpdates updates varGen1 []
                        |> Result.map (fun (convertedUpdates, varGen2) ->
                            let updateAtoms =
                                convertedUpdates
                                |> List.map (fun (fieldName, atom, _) -> (fieldName, atom))
                                |> Map.ofList
                            let updateBindings =
                                convertedUpdates |> List.collect (fun (_, _, bindings) -> bindings)
                            let (fieldAtoms, projectionBindings, varGen3) =
                                typeFields
                                |> List.mapi (fun index (fieldName, _) -> (index, fieldName))
                                |> List.fold (fun (atoms, bindings, vg) (index, fieldName) ->
                                    match Map.tryFind fieldName updateAtoms with
                                    | Some atom -> (atom :: atoms, bindings, vg)
                                    | None ->
                                        let (fieldVar, vg') = ANF.freshVar vg
                                        (ANF.Var fieldVar :: atoms,
                                         bindings
                                         @ [(
                                             fieldVar,
                                             ANF.RecordGet (
                                                 recordDescriptor
                                                     {
                                                         TypeName = typeName
                                                         TypeArgs = typeArgs
                                                     }
                                                     recordInfo,
                                                 recordAtom,
                                                 index
                                             )
                                         )],
                                         vg')) ([], [], varGen2)
                            let (resultVar, varGen4) = ANF.freshVar varGen3
                            (ANF.Var resultVar,
                             recordBindings
                             @ updateBindings
                             @ projectionBindings
                             @ [(
                                 resultVar,
                                 ANF.RecordClone (
                                     recordDescriptor
                                         {
                                             TypeName = typeName
                                             TypeArgs = typeArgs
                                         }
                                         recordInfo,
                                     recordAtom,
                                     List.rev fieldAtoms
                                 )
                             )],
                             varGen4)))
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error "Cannot use record update syntax on non-record type")

    | CheckedAST.RecordAccess (recordExpr, fieldName) ->
        // Projection is type-directed so the nominal descriptor and keyed slot
        // always agree, including after aliases and generic substitution.
        let typeEnv = typeEnvFromVarEnv env
        inferTypeCore sumTypeNames recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, typeArgs) ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some recordInfo ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) recordInfo.Fields with
                    | Some index ->
                        toAtomCore sumTypeNames inertScopes recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                            let (tempVar, varGen2) = ANF.freshVar varGen1
                            let getCExpr =
                                ANF.RecordGet (
                                    recordDescriptor
                                        {
                                            TypeName = typeName
                                            TypeArgs = typeArgs
                                        }
                                        recordInfo,
                                    recordAtom,
                                    index
                                )
                            let allBindings = recordBindings @ [(tempVar, getCExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | CheckedAST.Constructor (constructorTypeName, variantName, fields) ->
        match tryFindVariant constructorTypeName variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, _, tag, _) ->
            // Check if ANY variant in this type has a payload
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, _, variantFields) ->
                    tName = typeName && not (List.isEmpty variantFields))

            match fields with
            | [] when not typeHasPayloadVariants ->
                // Pure enum type: return tag as an integer (no bindings needed)
                Ok (ANF.IntLiteral (ANF.Int64 (int64 tag)), [], varGen)
            | [] ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag, 0] for uniform 2-element structure
                // This enables consistent structural equality comparison
                let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                let (tempVar, varGen1) = ANF.freshVar varGen
                let tupleCExpr = ANF.TupleAlloc [tagAtom; dummyPayload]
                Ok (ANF.Var tempVar, [(tempVar, tupleCExpr)], varGen1)
            | _ ->
                // Variant with payload: allocate [tag, payload] on heap
                let payloadExpr =
                    match fields with
                    | [field] -> field
                    | _ -> CheckedAST.TupleLiteral fields
                toAtomCore sumTypeNames inertScopes payloadExpr varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | CheckedAST.ListLiteral elements ->
        // Compile list literal as SkewList in atom position
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        let listNode = AST.TList (AST.TVar "a")
        let listNodeType = Some listNode

        // Tag a raw pointer as a list value without routing through Stdlib wrappers.
        // Keep a typed binding so RC/type inference still treats the result as List<a>.
        let tagRawPtrAsList (listNode: AST.Type) (tag: int64) (ptrVar: ANF.TempId) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (taggedRawVar, vg1) = ANF.freshVar vg
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 tag))
            let (taggedVar, vg2) = ANF.freshVar vg1
            let typedExpr = ANF.TypedAtom (ANF.Var taggedRawVar, listNode)
            (ANF.Var taggedVar, bindings @ [(taggedRawVar, tagExpr); (taggedVar, typedExpr)], vg2)

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (elemType: AST.Type) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
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
        let allocSingle (listNode: AST.Type) (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (setRcVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 16L))
            let setExpr = ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, listNode)
            let setRcExpr = ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), ANF.IntLiteral (ANF.Int64 1L))
            let bindings1 = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (setRcVar, setRcExpr)]
            tagRawPtrAsList listNode 1L ptrVar vg3 bindings1

        // Helper to create a DEEP node
        let allocDeep (listNode: AST.Type) (measure: int) (prefixNodes: ANF.Atom list) (middle: ANF.Atom) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
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
        let allocNode2 (listNode: AST.Type) (left: ANF.Atom * int) (right: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
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
        let allocNode3 (listNode: AST.Type) (first: ANF.Atom * int) (second: ANF.Atom * int) (third: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
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

        let rec buildTree (listNode: AST.Type) (nodes: (ANF.Atom * int) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
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
            Ok (ANF.IntLiteral (ANF.Int64 0L), [], varGen)
        else
            let typeEnv = typeEnvFromVarEnv env

            // Convert all elements to atoms first
            let rec convertElements (elems: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * AST.Type * (ANF.TempId * ANF.CExpr) list) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    inferTypeCore sumTypeNames e typeEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun elemType ->
                        toAtomCore sumTypeNames inertScopes e vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (atom, bindings, vg') ->
                            convertElements rest vg' ((atom, elemType, bindings) :: acc)))

            convertElements elements varGen []
            |> Result.bind (fun (atomsWithBindings, varGen1) ->
                // Flatten all element bindings
                let elemBindings = atomsWithBindings |> List.collect (fun (_, _, bindings) -> bindings)
                let elemAtoms = atomsWithBindings |> List.map (fun (atom, elemType, _) -> (atom, elemType))

                // Create LEAF nodes for all elements. Each leaf is independent,
                // so collect per-leaf bindings separately to avoid repeatedly
                // appending to the growing element-binding prefix for large lists.
                let listType =
                    match elemAtoms with
                    | (_, elemType) :: _ -> AST.TList elemType
                    | [] -> AST.TList (AST.TVar "a")

                let (resultAtom, resultBindings, varGen2) =
                    buildSkewListLiteral listType elemAtoms varGen1 elemBindings
                Ok (resultAtom, resultBindings, varGen2))

    | CheckedAST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        let partToExpr (part: CheckedAST.StringPart) : CheckedAST.Expr =
            match part with
            | CheckedAST.StringText s -> CheckedAST.StringLiteral s
            | CheckedAST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.StringLiteral "", [], varGen)
        | [single] ->
            // Single part → convert directly
            toAtomCore sumTypeNames inertScopes (partToExpr single) varGen env typeReg variantLookup funcReg moduleRegistry
        | first :: rest ->
            // Multiple parts → desugar to StringConcat and convert
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    CheckedAST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toAtomCore sumTypeNames inertScopes desugared varGen env typeReg variantLookup funcReg moduleRegistry

    | CheckedAST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANFCore sumTypeNames inertScopes (CheckedAST.Match (scrutinee, cases)) varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (matchExpr, varGen1) ->
            // The match compiles to an if-else chain that returns a value
            // We need to extract that value into a temp variable
            // For now, just return an error - complex match in atom position needs more work
            Error "Match expressions in atom position not yet supported (use let binding)")

    | CheckedAST.Lambda (_parameters, _, _body) ->
        // Lambda in atom position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | CheckedAST.IndirectApply (func, args) ->
        toAtomCore sumTypeNames inertScopes func varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (funcAtom, funcBindings, varGen1) ->
            let rec convertArgs remaining vg acc =
                match remaining with
                | [] -> Ok (List.rev acc, vg)
                | arg :: rest ->
                    toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (argAtom, argBindings, vg') ->
                        convertArgs rest vg' ((argAtom, argBindings) :: acc))
            convertArgs (exprArgsToList args) varGen1 []
            |> Result.map (fun (argResults, varGen2) ->
                let argAtoms = argResults |> List.map fst
                let bindings = funcBindings @ (argResults |> List.collect snd)
                let (resultId, varGen3) = ANF.freshVar varGen2
                (ANF.Var resultId, bindings @ [(resultId, ANF.IndirectCall (funcAtom, argAtoms))], varGen3)))

    | CheckedAST.Apply (func, args) ->
        // Apply in atom position - convert via toANF and extract result
        let argsList = exprArgsToList args
        match func with
        | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
            // Immediate application: desugar to let bindings
            let parameterList = AST.NonEmptyList.toList parameters
            if List.length argsList <> List.length parameterList then
                Error $"Expected {List.length parameterList} arguments, got {List.length argsList}"
            else
                let rec buildLets (ps: CheckedAST.LambdaParameter list) (as': CheckedAST.Expr list) : CheckedAST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | parameter :: restPs, argExpr :: restAs ->
                        CheckedAST.Let (parameter.Pattern, argExpr, buildLets restPs restAs)
                    | _ -> body
                let desugared = buildLets parameterList argsList
                toAtomCore sumTypeNames inertScopes desugared varGen env typeReg variantLookup funcReg moduleRegistry

        | CheckedAST.Apply (innerFunc, innerArgs) ->
            // Nested application in atom position: (fun x -> fun y -> ...)(a)(b)
            let innerArgsList = exprArgsToList innerArgs
            match innerFunc with
            | CheckedAST.Lambda (innerParams, returnAnnotation, innerBody) ->
                let innerParamList = AST.NonEmptyList.toList innerParams
                if List.length innerArgsList <> List.length innerParamList then
                    Error $"Inner lambda expects {List.length innerParamList} arguments, got {List.length innerArgsList}"
                else
                    let rec buildLets (ps: CheckedAST.LambdaParameter list) (as': CheckedAST.Expr list) : CheckedAST.Expr =
                        match ps, as' with
                        | [], [] -> innerBody
                        | parameter :: restPs, argExpr :: restAs ->
                            CheckedAST.Let (parameter.Pattern, argExpr, buildLets restPs restAs)
                        | _ -> innerBody
                    let desugaredInner = buildLets innerParamList innerArgsList
                    toAtomCore sumTypeNames inertScopes (CheckedAST.Apply (desugaredInner, args)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                // Inner is complex - evaluate inner, then call as closure
                toAtomCore sumTypeNames inertScopes (CheckedAST.Apply (innerFunc, innerArgs)) varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (closureAtom, closureBindings, varGen1) ->
                    let rec convertArgs (remaining: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                        match remaining with
                        | [] -> Ok (List.rev acc, vg)
                        | arg :: rest ->
                            toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                            |> Result.bind (fun (argAtom, argBindings, vg') ->
                                convertArgs rest vg' ((argAtom, argBindings) :: acc))
                    convertArgs argsList varGen1 []
                    |> Result.bind (fun (argResults, varGen2) ->
                        let argAtoms = argResults |> List.map fst
                        let argBindings = argResults |> List.collect snd
                        let (resultId, varGen3) = ANF.freshVar varGen2
                        let closureCall = ANF.ClosureCall (closureAtom, argAtoms)
                        let allBindings = closureBindings @ argBindings @ [(resultId, closureCall)]
                        Ok (ANF.Var resultId, allBindings, varGen3)))

        | CheckedAST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) in atom position
            // Float the let out and recurse
            toAtomCore sumTypeNames inertScopes (CheckedAST.Let (letName, letValue, CheckedAST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg moduleRegistry

        | CheckedAST.Local name ->
            // Variable call in atom position - treat as closure call
            match Map.tryFind name env with
            | Some (tempId, _) ->
                let rec convertArgs (remaining: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs argsList varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, finalBindings, varGen2))
            | None ->
                Error $"Cannot apply variable '{name}' as function in atom position - variable not in scope"

        | CheckedAST.Closure (funcName, captures) ->
            // Closure call in atom position
            let rec convertCaptures (caps: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | CheckedAST.FuncRef funcName :: rest ->
                    convertCaptures rest vg ((ANF.FuncRef funcName, []) :: acc)
                | cap :: rest ->
                    toAtomCore sumTypeNames inertScopes cap vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                let rec convertArgs (remaining: CheckedAST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs argsList varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let allBindings = captureBindings @ [(closureId, closureAlloc)] @ argBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, allBindings, varGen4)))

        | _ ->
            // General function-expression application in atom position.
            toAtomCore sumTypeNames inertScopes func varGen env typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (funcAtom, funcBindings, varGen1) ->
                let rec convertArgs
                    (remaining: CheckedAST.Expr list)
                    (vg: ANF.VarGen)
                    (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list)
                    : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list) list * ANF.VarGen, string> =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtomCore sumTypeNames inertScopes arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))

                convertArgs argsList varGen1 []
                |> Result.map (fun (argResults, varGen2) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    let (resultId, varGen3) = ANF.freshVar varGen2
                    let closureCall = ANF.ClosureCall (funcAtom, argAtoms)
                    let allBindings = funcBindings @ argBindings @ [(resultId, closureCall)]
                    (ANF.Var resultId, allBindings, varGen3)))
