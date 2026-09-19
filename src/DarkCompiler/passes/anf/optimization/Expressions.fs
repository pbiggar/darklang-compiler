// Expressions.fs - Propagate facts and common expressions through lexical ANF control flow.

module ANFExpressionOptimization

open ANF
open ANFConstants
open ANFEffects
open ANFSubstitution

type OptimizeAExprResult = {
    Expr: AExpr
    Changed: bool
    Uses: Set<TempId>
}

type private ScalarUnaryCSEOp =
    | PrimitiveUnary of UnaryOp
    | FloatSqrtOp
    | FloatAbsOp
    | FloatNegOp
    | Int64ToFloatOp
    | FloatToInt64Op
    | FloatToBitsOp

type private CSEKey =
    | BinaryValue of BinOp * Atom * Atom
    | UnaryValue of ScalarUnaryCSEOp * Atom
    | ConditionalValue of cond:Atom * thenValue:Atom * elseValue:Atom
    | TupleProjection of tuple:Atom * index:int
    | RecordProjection of descriptor:RecordDescriptor * record:Atom * index:int

type private CSEnv = Map<CSEKey, TempId>

let private isCommutativeBinOp (op: BinOp) : bool =
    match op with
    | Add
    | Mul
    | Eq
    | Neq
    | And
    | Or
    | BitAnd
    | BitOr
    | BitXor -> true
    | Sub
    | Div
    | Mod
    | Lt
    | Gt
    | Lte
    | Gte
    | Shl
    | Shr -> false

let private binaryCSEKey (op: BinOp) (left: Atom) (right: Atom) : CSEKey =
    match op with
    // Canonicalize relational comparisons to their less-than spelling so
    // reversing both the operator and operands produces the same CSE key.
    | Gt -> BinaryValue (Lt, right, left)
    | Gte -> BinaryValue (Lte, right, left)
    | _ when isCommutativeBinOp op && compare right left < 0 ->
        BinaryValue (op, right, left)
    | _ -> BinaryValue (op, left, right)

let private isRecordProjectionCSEType (fieldType: AST.Type) : bool =
    match fieldType with
    | AST.TInt64 | AST.TInt32 | AST.TInt16 | AST.TInt8
    | AST.TUInt64 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8
    | AST.TBool | AST.TChar | AST.TDateTime -> true
    | AST.TInt128 | AST.TInt
    | AST.TUInt128
    | AST.TFloat64
    | AST.TString
    | AST.TBlob
    | AST.TUnit
    | AST.TRuntimeError
    | AST.TFunction _
    | AST.TTuple _
    | AST.TRecord _
    | AST.TSum _
    | AST.TList _
    | AST.TStream _
    | AST.TVar _
    | AST.TRawPtr
    | AST.TDict _ -> false

/// Return a value-numbering key only when merging two evaluations preserves
/// allocation identity, mutable-memory observations, and ownership semantics.
/// Keep this match exhaustive so every new CExpr case requires an explicit CSE
/// decision instead of silently falling through a permissive purity test.
let private tryCSEKey (cexpr: CExpr) : CSEKey option =
    match cexpr with
    | Prim (op, left, right) -> Some (binaryCSEKey op left right)
    | UnaryPrim (op, atom) -> Some (UnaryValue (PrimitiveUnary op, atom))
    | IfValue (cond, thenValue, elseValue) ->
        Some (ConditionalValue (cond, thenValue, elseValue))
    | FloatSqrt atom -> Some (UnaryValue (FloatSqrtOp, atom))
    | FloatAbs atom -> Some (UnaryValue (FloatAbsOp, atom))
    | FloatNeg atom -> Some (UnaryValue (FloatNegOp, atom))
    | Int64ToFloat atom -> Some (UnaryValue (Int64ToFloatOp, atom))
    | FloatToInt64 atom -> Some (UnaryValue (FloatToInt64Op, atom))
    | FloatToBits atom -> Some (UnaryValue (FloatToBitsOp, atom))
    | TupleGet (tuple, index) -> Some (TupleProjection (tuple, index))
    | RecordGet (descriptor, record, index) ->
        match List.tryItem index descriptor.Fields with
        | Some (_, fieldType) when isRecordProjectionCSEType fieldType ->
            Some (RecordProjection (descriptor, record, index))
        | Some _ -> None
        | None ->
            Crash.crash
                $"ANF CSE: invalid field index {index} for record {descriptor.RuntimeTypeName}"
    | Atom _
    | TypedAtom _
    | Call _
    | BorrowedCall _
    | TailCall _
    | IndirectCall _
    | IndirectTailCall _
    | ClosureAlloc _
    | ClosureCall _
    | ClosureTailCall _
    | TupleAlloc _
    | RecordAlloc _
    | RecordClone _
    | RecordReuse _
    | StringConcat _
    | CanonicalBufferEq _
    | RefCountInc _
    | RefCountDec _
    | Print _
    | StdoutWrite _
    | StdinReadLine
    | RuntimeError _
    | RuntimeErrorString _
    | FileReadText _
    | FileExists _
    | FileWriteText _
    | FileAppendText _
    | FileDelete _
    | FileSetExecutable _
    | FileWriteFromPtr _
    | RawAlloc _
    | MappedAlloc _
    | RawFree _
    | MappedFree _
    | RawGet _
    | RawTake _
    | RawGetByte _
    | RawWriteWord _
    | RawWriteByte _
    | RawSlotInit _
    | StringToRawPtr _
    | RawPtrToString _
    | RawPtrToInt128 _
    | RawPtrToUInt128 _
    | BlobToRawPtr _
    | RawPtrToBlob _
    | DictToRawPtr _
    | RawPtrToDict _
    | ListToRawPtr _
    | FixedBlockToRawPtr _
    | RawPtrToList _
    | RefCountIncString _
    | RefCountDecString _
    | RefCountIncBlob _
    | RefCountDecBlob _
    | RandomInt64
    | DateTimeNow
    | Sleep _
    | CliNative _
    | FloatToString _ -> None

let private tryAbsorbedAtom (outer: Atom) (nestedLeft: Atom) (nestedRight: Atom) : Atom option =
    if outer = nestedLeft || outer = nestedRight then Some outer
    else None

let rec private aExprUsesTemp (tid: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump (_, atom) -> atomUsesTemp tid atom
    | Join (parameter, continuation, entry) ->
        aExprUsesTemp tid entry || (parameter.Id <> tid && aExprUsesTemp tid continuation)
    | Return atom -> atomUsesTemp tid atom
    | Let (_, cexpr, body) ->
        cexprUsesTemp tid cexpr || aExprUsesTemp tid body
    | If (cond, thenBranch, elseBranch) ->
        atomUsesTemp tid cond
        || aExprUsesTemp tid thenBranch
        || aExprUsesTemp tid elseBranch

/// Replace uses of one branch-local binding with a shared binding. The bound
/// TempId is removed from the substitution when crossing a shadowing Let so
/// this remains correct for hand-built ANF as well as globally fresh output.
let rec private replaceTempUses (sourceTid: TempId) (replacement: Atom) (expr: AExpr) : AExpr =
    let substitution = Map.ofList [(sourceTid, replacement)]

    match expr with
    | Jump (target, atom) -> Jump (target, substAtom substitution atom)
    | Join (parameter, continuation, entry) ->
        let body = if parameter.Id = sourceTid then continuation else replaceTempUses sourceTid replacement continuation
        Join (parameter, body, replaceTempUses sourceTid replacement entry)
    | Return atom -> Return (substAtom substitution atom)
    | Let (tid, cexpr, body) ->
        let cexpr' = substCExpr substitution cexpr
        let body' =
            if tid = sourceTid then body
            else replaceTempUses sourceTid replacement body
        Let (tid, cexpr', body')
    | If (cond, thenBranch, elseBranch) ->
        If (
            substAtom substitution cond,
            replaceTempUses sourceTid replacement thenBranch,
            replaceTempUses sourceTid replacement elseBranch
        )

let rec private aExprMustPreserveEvaluation (context: OptimizeContext) (expr: AExpr) : bool =
    match expr with
    | Jump _ -> false
    | Join (_, continuation, entry) ->
        aExprMustPreserveEvaluation context continuation || aExprMustPreserveEvaluation context entry
    | Return _ -> false
    | Let (_, cexpr, body) ->
        mustPreserveEvaluation context cexpr || aExprMustPreserveEvaluation context body
    | If (_, thenBranch, elseBranch) ->
        aExprMustPreserveEvaluation context thenBranch
        || aExprMustPreserveEvaluation context elseBranch

/// Hoist before the binding that computes a local condition so the shared
/// expression does not separate a comparison from its branch during lowering.
let private tryHoistSharedLeadingBranchBinding
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (expr: AExpr)
    : AExpr option =
    let sharedIf
        (cond: Atom)
        (thenTid: TempId)
        (thenBody: AExpr)
        (elseTid: TempId)
        (elseBody: AExpr)
        : AExpr =
        let elseBody' = replaceTempUses elseTid (Var thenTid) elseBody
        If (cond, thenBody, elseBody')

    if not options.EnableCSE then
        None
    else
        match expr with
        | Let (
            condTid,
            condCExpr,
            If (
                Var ifCondTid,
                Let (thenTid, thenCExpr, thenBody),
                Let (elseTid, elseCExpr, elseBody)
            )
          )
            when ifCondTid = condTid
                 && thenCExpr = elseCExpr
                 && not (mustPreserveEvaluation context condCExpr)
                 && not (mustPreserveEvaluation context thenCExpr)
                 && not (aExprMustPreserveEvaluation context thenBody)
                 && not (aExprMustPreserveEvaluation context elseBody)
                 && not (cexprUsesTemp condTid thenCExpr) ->
            let conditional = sharedIf (Var condTid) thenTid thenBody elseTid elseBody
            Some (Let (thenTid, thenCExpr, Let (condTid, condCExpr, conditional)))
        | _ -> None

let private tryComplementIntegerComparison (op: BinOp) : BinOp option =
    match op with
    | Eq -> Some Neq
    | Neq -> Some Eq
    | Lt -> Some Gte
    | Gt -> Some Lte
    | Lte -> Some Gt
    | Gte -> Some Lt
    | _ -> None

let private trySimplifyAdjacentLet (typeEnv: TypeEnv) (tid: TempId) (cexpr: CExpr) (body: AExpr) : AExpr option =
    match cexpr, body with
    | Call (fromInt64Id, [nativeIndex]),
      Let (
          resultTid,
          Call (getByteAtId, [value; Var indexTid]),
          resultBody
      )
        when fromInt64Id = AST.functionIdForName "Darklang.Stdlib.Int.fromInt64"
             && getByteAtId = AST.functionIdForName "Darklang.Stdlib.String.getByteAt"
             && indexTid = tid
             && not (aExprUsesTemp tid resultBody) ->
        Some (
            Let (
                resultTid,
                Call (
                    AST.functionIdForName "Darklang.Stdlib.String.__getByteAtInt64",
                    [value; nativeIndex]
                ),
                resultBody
            )
        )
    | UnaryPrim (Not, source), If (Var conditionTid, thenBranch, elseBranch)
        when conditionTid = tid
             && not (aExprUsesTemp tid thenBranch)
             && not (aExprUsesTemp tid elseBranch) ->
        Some (If (source, elseBranch, thenBranch))
    | Prim (op, left, right), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid
             && isIntegerAtom typeEnv left
             && isIntegerAtom typeEnv right
             && not (aExprUsesTemp tid notBody) ->
        // Ordered integer comparisons have exact complements. Float relations
        // do not: both x < NaN and x >= NaN are false.
        tryComplementIntegerComparison op
        |> Option.map (fun complement -> Let (notTid, Prim (complement, left, right), notBody))
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, other, Var negatedTid), resultBody)
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, Var negatedTid, other), resultBody)
        when negatedTid = tid
             && not (atomUsesTemp tid other)
             && not (aExprUsesTemp tid resultBody)
             && isInt64Atom typeEnv negated
             && isInt64Atom typeEnv other ->
        Some (Let (resultTid, Prim (Sub, other, negated), resultBody))
    | Prim (Add, source, IntLiteral (Int64 a)),
      Let (addTid, Prim (Add, Var sourceTid, IntLiteral (Int64 b)), addBody)
        when sourceTid = tid ->
        // Keep the inner binding for this rewrite; the recursive optimization
        // removes it only when the reassociated expression was its final use.
        let combined = IntLiteral (Int64 (a + b))
        Some (Let (tid, cexpr, Let (addTid, Prim (Add, source, combined), addBody)))
    | Prim (Mul, source, IntLiteral (Int64 a)),
      Let (multiplyTid, Prim (Mul, Var sourceTid, IntLiteral (Int64 b)), multiplyBody)
        when sourceTid = tid ->
        // Int64 multiplication is associative modulo 2^64. Keeping the inner
        // binding here lets the recursive liveness pass remove it only when
        // the reassociated expression was its final use.
        let combined = IntLiteral (Int64 (a * b))
        Some (Let (tid, cexpr, Let (multiplyTid, Prim (Mul, source, combined), multiplyBody)))
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody)
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody)
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody)
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody)
        when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        // Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x.
        // Retain the product until recursive liveness cleanup proves it dead.
        let combined = IntLiteral (Int64 (coefficient + 1L))
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Add, source, cancelled),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Add, source, remaining),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && source = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv remaining ->
        Some (Let (tid, cexpr, Let (resultTid, Atom remaining, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, outerCancelled, Var intermediateTid), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | UnaryPrim (Not, source), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (BitNot, source), Let (notTid, UnaryPrim (BitNot, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (Neg, source), Let (negTid, UnaryPrim (Neg, Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatNeg source, Let (negTid, FloatNeg (Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatAbs source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | FloatNeg source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | _ -> None

let private trySimplifyBoolComplement (tid: TempId) (cexpr: CExpr) (body: AExpr) : AExpr option =
    let replacementForBoolOp op =
        match op with
        | And -> Some (BoolLiteral false)
        | Or -> Some (BoolLiteral true)
        | _ -> None

    match cexpr, body with
    | UnaryPrim (Not, source), Let (boolTid, Prim (op, Var sourceTid, Var notTid), boolBody)
    | UnaryPrim (Not, source), Let (boolTid, Prim (op, Var notTid, Var sourceTid), boolBody)
        when notTid = tid ->
        match source with
        | Var originalTid when originalTid = sourceTid ->
            replacementForBoolOp op
            |> Option.map (fun replacement -> Let (boolTid, Atom replacement, boolBody))
        | _ -> None
    | _ -> None

let private trySimplifyInt64BitwiseComplement
    (typeEnv: TypeEnv)
    (tid: TempId)
    (cexpr: CExpr)
    (body: AExpr)
    : AExpr option =
    let replacementForBitwiseOp op =
        match op with
        | BitAnd -> Some (IntLiteral (Int64 0L))
        | BitOr
        | BitXor -> Some (IntLiteral (Int64 -1L))
        | _ -> None

    match cexpr, body with
    | UnaryPrim (BitNot, source), Let (bitwiseTid, Prim (op, Var sourceTid, Var notTid), bitwiseBody)
    | UnaryPrim (BitNot, source), Let (bitwiseTid, Prim (op, Var notTid, Var sourceTid), bitwiseBody)
        when notTid = tid && isInt64Atom typeEnv source ->
        match source with
        | Var originalTid when originalTid = sourceTid ->
            replacementForBitwiseOp op
            |> Option.map (fun replacement ->
                let foldedBody = Let (bitwiseTid, Atom replacement, bitwiseBody)
                if aExprUsesTemp tid bitwiseBody then
                    Let (tid, cexpr, foldedBody)
                else
                    foldedBody)
        | _ -> None
    | _ -> None

/// Optimize an AExpr, returning optimized expression, change flag, and used TempIds
let rec private optimizeAExprWithUses
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (env: ConstEnv)
    (typeEnv: TypeEnv)
    (tupleEnv: TupleEnv)
    (cseEnv: CSEnv)
    (aexpr: AExpr)
    : OptimizeAExprResult =
    match tryHoistSharedLeadingBranchBinding context options aexpr with
    | Some replacement ->
        let replacementResult =
            optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement
        { replacementResult with Changed = true }
    | None ->
        optimizeAExprWithoutBranchHoisting context options env typeEnv tupleEnv cseEnv aexpr

and private optimizeAExprWithoutBranchHoisting
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (env: ConstEnv)
    (typeEnv: TypeEnv)
    (tupleEnv: TupleEnv)
    (cseEnv: CSEnv)
    (aexpr: AExpr)
    : OptimizeAExprResult =
    match aexpr with
    | Jump (target, atom) ->
        let atom' = substAtom env atom
        { Expr = Jump (target, atom'); Changed = atom' <> atom; Uses = addAtomUse atom' Set.empty }
    | Join (parameter, continuation, entry) ->
        let body = optimizeAExprWithUses context options (Map.remove parameter.Id env) (Map.add parameter.Id parameter.Type typeEnv) tupleEnv cseEnv continuation
        let entry' = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv entry
        { Expr = Join (parameter, body.Expr, entry'.Expr)
          Changed = body.Changed || entry'.Changed
          Uses = Set.union (Set.remove parameter.Id body.Uses) entry'.Uses }
    | Return atom ->
        let atom' = substAtom env atom
        {
            Expr = Return atom'
            Changed = atom' <> atom
            Uses = addAtomUse atom' Set.empty
        }

    | Let (tid, cexpr, body) ->
        // Optimize the CExpr
        let (cexpr', cexprChanged) = optimizeCExpr options env typeEnv tupleEnv cexpr
        let (cexpr'', cseChanged, cseEnv') =
            if options.EnableCSE then
                match tryCSEKey cexpr' with
                | Some key ->
                    match Map.tryFind key cseEnv with
                    | Some existingTid -> (Atom (Var existingTid), true, cseEnv)
                    | None -> (cexpr', false, Map.add key tid cseEnv)
                | None -> (cexpr', false, cseEnv)
            else (cexpr', false, cseEnv)

        // Check for copy propagation: if cexpr is just an Atom, substitute it
        let (env', skipBinding) =
            match cexpr'' with
            | Atom a when options.EnableCopyProp && not (mustPreserveEvaluation context cexpr'') ->
                // Copy propagation: don't emit binding, just substitute
                (Map.add tid a env, true)
            | Atom (IntLiteral _ | BoolLiteral _ | FloatLiteral _ | StringLiteral _ | UnitLiteral as constAtom)
                when options.EnableConstProp ->
                // Constant propagation
                (Map.add tid constAtom env, false)
            | _ ->
                (env, false)

        // Optimize the body
        let tupleEnv' =
            match cexpr'' with
            | TupleAlloc elements ->
                let forwardableElements =
                    elements
                    |> List.indexed
                    |> List.choose (fun (index, element) ->
                        if canForwardTupleElement context typeEnv element then
                            Some (index, element)
                        else
                            None)
                    |> Map.ofList
                Map.add tid forwardableElements tupleEnv
            | _ -> tupleEnv

        let bodyResult = optimizeAExprWithUses context options env' typeEnv tupleEnv' cseEnv' body

        // Dead code elimination: discard an unused binding only when evaluating
        // it is not required for effects or ownership bookkeeping.
        let usesInBody = bodyResult.Uses
        let isDead =
            options.EnableDCE
            && not (Set.contains tid usesInBody)
            && not (mustPreserveEvaluation context cexpr'')
        let usesInBodyWithoutTid = Set.remove tid usesInBody

        let adjacentSimplification =
            if options.EnableConstFolding then
                trySimplifyAdjacentLet typeEnv tid cexpr'' bodyResult.Expr
                |> Option.orElseWith (fun () -> trySimplifyBoolComplement tid cexpr'' bodyResult.Expr)
                |> Option.orElseWith (fun () ->
                    trySimplifyInt64BitwiseComplement typeEnv tid cexpr'' bodyResult.Expr)
            else
                None

        match adjacentSimplification with
        | Some replacement ->
            let replacementResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement
            { replacementResult with Changed = true }
        | None when skipBinding ->
            // Copy propagation: skip this binding entirely
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        | _ when isDead ->
            // Dead code elimination
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        | _ ->
            let uses = addCExprUses cexpr'' usesInBodyWithoutTid
            {
                Expr = Let (tid, cexpr'', bodyResult.Expr)
                Changed = cexprChanged || cseChanged || bodyResult.Changed
                Uses = uses
            }

    | If (cond, thenBranch, elseBranch) ->
        let cond' = substAtom env cond

        // Fold constant conditions
        match cond' with
        | BoolLiteral true when options.EnableConstFolding ->
            let thenResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv thenBranch
            {
                Expr = thenResult.Expr
                Changed = true
                Uses = thenResult.Uses
            }
        | BoolLiteral false when options.EnableConstFolding ->
            let elseResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv elseBranch
            {
                Expr = elseResult.Expr
                Changed = true
                Uses = elseResult.Uses
            }
        | _ ->
            let thenResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv thenBranch
            let elseResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv elseBranch
            if options.EnableConstFolding && thenResult.Expr = Return (BoolLiteral true) && elseResult.Expr = Return (BoolLiteral false) then
                {
                    Expr = Return cond'
                    Changed = true
                    Uses = addAtomUse cond' Set.empty
                }
            elif options.EnableConstFolding && thenResult.Expr = elseResult.Expr then
                {
                    Expr = thenResult.Expr
                    Changed = true
                    Uses = thenResult.Uses
                }
            else
                let uses = Set.union thenResult.Uses elseResult.Uses |> addAtomUse cond'
                {
                    Expr = If (cond', thenResult.Expr, elseResult.Expr)
                    Changed = cond' <> cond || thenResult.Changed || elseResult.Changed
                    Uses = uses
                }

/// Optimize an AExpr
let optimizeAExpr (context: OptimizeContext) (options: OptimizeOptions) (env: ConstEnv) (typeEnv: TypeEnv) (aexpr: AExpr) : AExpr * bool =
    let result = optimizeAExprWithUses context options env typeEnv Map.empty Map.empty aexpr
    (result.Expr, result.Changed)

/// Optimize a function using the stable type metadata for its parameters.
let optimizeFunction (context: OptimizeContext) (options: OptimizeOptions) (typeEnv: TypeEnv) (func: Function) : Function * bool =
    // Initialize env with function parameters (they're not constants)
    let env = Map.empty
    let (body', changed) = optimizeAExpr context options env typeEnv func.Body
    ({ func with Body = body' }, changed)

/// Optimize until fixed point
let optimizeToFixedPoint (context: OptimizeContext) (options: OptimizeOptions) (func: Function) (maxIterations: int) : Function =
    let typeEnv =
        func.TypedParams
        |> List.map (fun param -> (param.Id, param.Type))
        |> Map.ofList

    let rec optimize (func: Function) (remainingIterations: int) : Function =
        if remainingIterations <= 0 then func
        else
            let (func', changed) = optimizeFunction context options typeEnv func
            if changed then
                optimize func' (remainingIterations - 1)
            else
                func'

    optimize func maxIterations

let rec private collectAExprTempIds (expr: AExpr) (tempIds: Set<TempId>) : Set<TempId> =
    match expr with
    | Jump (target, atom) -> tempIds |> Set.add target |> addAtomUse atom
    | Join (parameter, continuation, entry) ->
        tempIds |> Set.add parameter.Id |> collectAExprTempIds continuation |> collectAExprTempIds entry
    | Return atom -> addAtomUse atom tempIds
    | Let (tid, cexpr, body) ->
        tempIds
        |> Set.add tid
        |> addCExprUses cexpr
        |> collectAExprTempIds body
    | If (cond, thenBranch, elseBranch) ->
        tempIds
        |> addAtomUse cond
        |> collectAExprTempIds thenBranch
        |> collectAExprTempIds elseBranch

/// Count uses of a local closure while rejecting every use that is not a call
/// through that exact closure value. A positive result proves the allocation
/// neither escapes nor reaches storage or an unknown callee.
let rec private countKnownClosureCalls (closureId: TempId) (expr: AExpr) : int option =
    let combine left right =
        match left, right with
        | Some leftCount, Some rightCount -> Some (leftCount + rightCount)
        | _ -> None

    let classifyCExpr cexpr =
        match cexpr with
        | ClosureCall (Var calledId, args) when calledId = closureId && not (atomsUseTemp closureId args) ->
            Some 1
        | ClosureTailCall (Var calledId, args) when calledId = closureId && not (atomsUseTemp closureId args) ->
            Some 1
        | _ when not (cexprUsesTemp closureId cexpr) ->
            Some 0
        | _ ->
            None

    match expr with
    | Jump (_, atom)
    | Return atom ->
        if atomUsesTemp closureId atom then None else Some 0
    | Let (boundId, cexpr, body) ->
        match classifyCExpr cexpr with
        | None -> None
        | Some callCount when boundId = closureId -> Some callCount
        | Some callCount ->
            countKnownClosureCalls closureId body
            |> Option.map (fun bodyCount -> callCount + bodyCount)
    | Join (parameter, continuation, entry) ->
        combine (countKnownClosureCalls closureId entry)
            (if parameter.Id = closureId then Some 0 else countKnownClosureCalls closureId continuation)
    | If (condition, thenBranch, elseBranch) ->
        if atomUsesTemp closureId condition then
            None
        else
            combine
                (countKnownClosureCalls closureId thenBranch)
                (countKnownClosureCalls closureId elseBranch)

/// Replace proven calls through one capture-free local closure with calls to
/// its lifted target. The unused hidden closure argument remains explicit so
/// the lifted function's established ABI does not change.
let rec private rewriteKnownCaptureFreeCalls
    (closureId: TempId)
    (funcName: AST.FunctionId)
    (expr: AExpr)
    : AExpr =
    let rewriteCExpr cexpr =
        match cexpr with
        | ClosureCall (Var calledId, args) when calledId = closureId ->
            Call (funcName, UnitLiteral :: args)
        | ClosureTailCall (Var calledId, args) when calledId = closureId ->
            TailCall (funcName, UnitLiteral :: args)
        | _ -> cexpr

    match expr with
    | Jump _ | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        let body' =
            if boundId = closureId then body
            else rewriteKnownCaptureFreeCalls closureId funcName body
        Let (boundId, rewriteCExpr cexpr, body')
    | Join (parameter, continuation, entry) ->
        let body = if parameter.Id = closureId then continuation else rewriteKnownCaptureFreeCalls closureId funcName continuation
        Join (parameter, body, rewriteKnownCaptureFreeCalls closureId funcName entry)
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            rewriteKnownCaptureFreeCalls closureId funcName thenBranch,
            rewriteKnownCaptureFreeCalls closureId funcName elseBranch)

/// Eliminate only capture-free closure allocations whose complete lexical use
/// set consists of one or more known calls.
let rec internal devirtualizeCaptureFreeClosures (expr: AExpr) : AExpr =
    match expr with
    | Jump _ | Return _ -> expr
    | Let (closureId, ClosureAlloc (funcName, []), body) ->
        let body' = devirtualizeCaptureFreeClosures body
        match countKnownClosureCalls closureId body' with
        | Some callCount when callCount > 0 ->
            rewriteKnownCaptureFreeCalls closureId funcName body'
        | _ ->
            Let (closureId, ClosureAlloc (funcName, []), body')
    | Let (tempId, cexpr, body) ->
        Let (tempId, cexpr, devirtualizeCaptureFreeClosures body)
    | Join (parameter, continuation, entry) ->
        Join (parameter, devirtualizeCaptureFreeClosures continuation, devirtualizeCaptureFreeClosures entry)
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            devirtualizeCaptureFreeClosures thenBranch,
            devirtualizeCaptureFreeClosures elseBranch)

let internal freshVarGenForProgram (Program (functions, mainExpr)) : VarGen =
    let tempIds =
        functions
        |> List.fold
            (fun tempIds func ->
                func.TypedParams
                |> List.fold (fun ids param -> Set.add param.Id ids) tempIds
                |> collectAExprTempIds func.Body)
            Set.empty
        |> collectAExprTempIds mainExpr

    match
        tempIds
        |> Set.fold
            (fun greatest (TempId tempId) ->
                match greatest with
                | None -> Some tempId
                | Some greatestId -> Some (max greatestId tempId))
            None
    with
    | None -> initialVarGen
    | Some greatestId -> VarGen (greatestId + 1)
