// 2.3_ANF_Optimize.fs - ANF Optimization Pass
//
// Performs optimizations on ANF before reference counting:
// - Constant folding: evaluate constant expressions at compile time
// - Constant propagation: replace variable uses with constant definitions
// - Dead code elimination: remove unused bindings
// - Beta reduction: eliminate trivial bindings (copy propagation)
//
// These optimizations run in a loop until no more changes occur.

module ANF_Optimize

open ANF

/// Environment mapping TempIds to their constant values (for propagation)
type ConstEnv = Map<TempId, Atom>

/// Check if an atom is a constant (literal or known value)
let isConstant (atom: Atom) : bool =
    match atom with
    | UnitLiteral | IntLiteral _ | BoolLiteral _ | StringLiteral _ | FloatLiteral _ -> true
    | Var _ | FuncRef _ -> false

/// Try to get a constant from an atom (resolving through env)
let resolveAtom (env: ConstEnv) (atom: Atom) : Atom =
    match atom with
    | Var tid ->
        match Map.tryFind tid env with
        | Some constAtom -> constAtom
        | None -> atom
    | _ -> atom

/// Fold a binary operation on constants
/// Only folds Int64 for now - other integer types need proper overflow handling at runtime
let foldBinOp (op: BinOp) (left: Atom) (right: Atom) : CExpr option =
    match op, left, right with
    // Int64 arithmetic (unchecked - overflow wraps)
    | Add, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a + b))))
    | Sub, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a - b))))
    | Mul, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a * b))))
    | Div, IntLiteral (Int64 a), IntLiteral (Int64 b) when b <> 0L -> Some (Atom (IntLiteral (Int64 (a / b))))
    | Mod, IntLiteral (Int64 a), IntLiteral (Int64 b) when b <> 0L -> Some (Atom (IntLiteral (Int64 (a % b))))

    // Float arithmetic
    | Add, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a + b)))
    | Sub, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a - b)))
    | Mul, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a * b)))
    | Div, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a / b)))

    // Int64 comparisons
    | Eq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a = b)))
    | Neq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <> b)))
    | Lt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a < b)))
    | Gt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a > b)))
    | Lte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <= b)))
    | Gte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a >= b)))

    // Boolean comparisons
    | Eq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a <> b)))

    // Boolean operations
    | And, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a && b)))
    | Or, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a || b)))

    // String comparisons
    | Eq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a <> b)))

    // Algebraic identities (strength reduction) - only for Int64
    | Add, IntLiteral (Int64 0L), x -> Some (Atom x)
    | Add, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Sub, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Mul, IntLiteral (Int64 1L), x -> Some (Atom x)
    | Mul, x, IntLiteral (Int64 1L) -> Some (Atom x)
    | Mul, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | Mul, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
    | Div, x, IntLiteral (Int64 1L) -> Some (Atom x)

    // Short-circuit boolean
    | And, BoolLiteral false, _ -> Some (Atom (BoolLiteral false))
    | And, _, BoolLiteral false -> Some (Atom (BoolLiteral false))
    | And, BoolLiteral true, x -> Some (Atom x)
    | And, x, BoolLiteral true -> Some (Atom x)
    | Or, BoolLiteral true, _ -> Some (Atom (BoolLiteral true))
    | Or, _, BoolLiteral true -> Some (Atom (BoolLiteral true))
    | Or, BoolLiteral false, x -> Some (Atom x)
    | Or, x, BoolLiteral false -> Some (Atom x)

    | _ -> None

/// Fold a unary operation on constants
let foldUnaryOp (op: UnaryOp) (src: Atom) : CExpr option =
    match op, src with
    // Int64 negation (unchecked - INT64_MIN wraps to itself)
    | Neg, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (-n))))
    | Neg, FloatLiteral f -> Some (Atom (FloatLiteral (-f)))
    | Not, BoolLiteral b -> Some (Atom (BoolLiteral (not b)))
    | _ -> None

/// Check if a CExpr has side effects
let hasSideEffects (cexpr: CExpr) : bool =
    match cexpr with
    | Atom _ -> false
    | Prim _ -> false
    | UnaryPrim _ -> false
    | IfValue _ -> false
    | TupleAlloc _ -> false
    | TupleGet _ -> false
    // These have side effects
    | Call _ -> true
    | TailCall _ -> true
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true
    | StringConcat _ -> true  // Allocates memory
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawSet _ -> true    // Memory mutation
    | RawSetByte _ -> true  // Memory mutation (byte)
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | IntToFloat _ -> false // Pure conversion
    | FloatToInt _ -> false // Pure conversion
    | StringHash _ -> false  // Pure
    | StringEq _ -> false    // Pure
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RandomInt64 -> true   // Reads from OS random source

/// Collect all TempIds used in an atom
let collectAtomUses (atom: Atom) : Set<TempId> =
    match atom with
    | Var tid -> Set.singleton tid
    | _ -> Set.empty

/// Collect all TempIds used in a CExpr
let collectCExprUses (cexpr: CExpr) : Set<TempId> =
    match cexpr with
    | Atom a -> collectAtomUses a
    | Prim (_, left, right) -> Set.union (collectAtomUses left) (collectAtomUses right)
    | UnaryPrim (_, src) -> collectAtomUses src
    | IfValue (cond, thenVal, elseVal) ->
        Set.unionMany [collectAtomUses cond; collectAtomUses thenVal; collectAtomUses elseVal]
    | Call (_, args) -> args |> List.map collectAtomUses |> Set.unionMany
    | TailCall (_, args) -> args |> List.map collectAtomUses |> Set.unionMany
    | IndirectCall (func, args) ->
        Set.unionMany ((collectAtomUses func) :: (args |> List.map collectAtomUses))
    | IndirectTailCall (func, args) ->
        Set.unionMany ((collectAtomUses func) :: (args |> List.map collectAtomUses))
    | ClosureAlloc (_, captures) -> captures |> List.map collectAtomUses |> Set.unionMany
    | ClosureCall (closure, args) ->
        Set.unionMany ((collectAtomUses closure) :: (args |> List.map collectAtomUses))
    | ClosureTailCall (closure, args) ->
        Set.unionMany ((collectAtomUses closure) :: (args |> List.map collectAtomUses))
    | TupleAlloc elems -> elems |> List.map collectAtomUses |> Set.unionMany
    | TupleGet (tuple, _) -> collectAtomUses tuple
    | StringConcat (left, right) -> Set.union (collectAtomUses left) (collectAtomUses right)
    | RefCountInc (atom, _) -> collectAtomUses atom
    | RefCountDec (atom, _) -> collectAtomUses atom
    | Print (atom, _) -> collectAtomUses atom
    | FileReadText path -> collectAtomUses path
    | FileExists path -> collectAtomUses path
    | FileWriteText (path, content) -> Set.union (collectAtomUses path) (collectAtomUses content)
    | FileAppendText (path, content) -> Set.union (collectAtomUses path) (collectAtomUses content)
    | FileDelete path -> collectAtomUses path
    | FileSetExecutable path -> collectAtomUses path
    | FileWriteFromPtr (path, ptr, length) -> Set.unionMany [collectAtomUses path; collectAtomUses ptr; collectAtomUses length]
    | RawAlloc numBytes -> collectAtomUses numBytes
    | RawFree ptr -> collectAtomUses ptr
    | RawGet (ptr, byteOffset) -> Set.union (collectAtomUses ptr) (collectAtomUses byteOffset)
    | RawGetByte (ptr, byteOffset) -> Set.union (collectAtomUses ptr) (collectAtomUses byteOffset)
    | RawSet (ptr, byteOffset, value) -> Set.unionMany [collectAtomUses ptr; collectAtomUses byteOffset; collectAtomUses value]
    | RawSetByte (ptr, byteOffset, value) -> Set.unionMany [collectAtomUses ptr; collectAtomUses byteOffset; collectAtomUses value]
    | FloatSqrt atom -> collectAtomUses atom
    | FloatAbs atom -> collectAtomUses atom
    | FloatNeg atom -> collectAtomUses atom
    | IntToFloat atom -> collectAtomUses atom
    | FloatToInt atom -> collectAtomUses atom
    | StringHash str -> collectAtomUses str
    | StringEq (left, right) -> Set.union (collectAtomUses left) (collectAtomUses right)
    | RefCountIncString str -> collectAtomUses str
    | RefCountDecString str -> collectAtomUses str
    | RandomInt64 -> Set.empty  // No atoms

/// Collect all TempIds used in an AExpr
let rec collectAExprUses (aexpr: AExpr) : Set<TempId> =
    match aexpr with
    | Return atom -> collectAtomUses atom
    | Let (_, cexpr, body) ->
        Set.union (collectCExprUses cexpr) (collectAExprUses body)
    | If (cond, thenBranch, elseBranch) ->
        Set.unionMany [collectAtomUses cond; collectAExprUses thenBranch; collectAExprUses elseBranch]

/// Substitute atom in another atom
let substAtom (env: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var tid -> Map.tryFind tid env |> Option.defaultValue atom
    | _ -> atom

/// Substitute atoms in CExpr
let substCExpr (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    let s = substAtom env
    match cexpr with
    | Atom a -> Atom (s a)
    | Prim (op, left, right) -> Prim (op, s left, s right)
    | UnaryPrim (op, src) -> UnaryPrim (op, s src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (s cond, s thenVal, s elseVal)
    | Call (name, args) -> Call (name, List.map s args)
    | TailCall (name, args) -> TailCall (name, List.map s args)
    | IndirectCall (func, args) -> IndirectCall (s func, List.map s args)
    | IndirectTailCall (func, args) -> IndirectTailCall (s func, List.map s args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, List.map s captures)
    | ClosureCall (closure, args) -> ClosureCall (s closure, List.map s args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (s closure, List.map s args)
    | TupleAlloc elems -> TupleAlloc (List.map s elems)
    | TupleGet (tuple, idx) -> TupleGet (s tuple, idx)
    | StringConcat (left, right) -> StringConcat (s left, s right)
    | RefCountInc (atom, size) -> RefCountInc (s atom, size)
    | RefCountDec (atom, size) -> RefCountDec (s atom, size)
    | Print (atom, t) -> Print (s atom, t)
    | FileReadText path -> FileReadText (s path)
    | FileExists path -> FileExists (s path)
    | FileWriteText (path, content) -> FileWriteText (s path, s content)
    | FileAppendText (path, content) -> FileAppendText (s path, s content)
    | FileDelete path -> FileDelete (s path)
    | FileSetExecutable path -> FileSetExecutable (s path)
    | FileWriteFromPtr (path, ptr, length) -> FileWriteFromPtr (s path, s ptr, s length)
    | RawAlloc numBytes -> RawAlloc (s numBytes)
    | RawFree ptr -> RawFree (s ptr)
    | RawGet (ptr, byteOffset) -> RawGet (s ptr, s byteOffset)
    | RawGetByte (ptr, byteOffset) -> RawGetByte (s ptr, s byteOffset)
    | RawSet (ptr, byteOffset, value) -> RawSet (s ptr, s byteOffset, s value)
    | RawSetByte (ptr, byteOffset, value) -> RawSetByte (s ptr, s byteOffset, s value)
    | FloatSqrt atom -> FloatSqrt (s atom)
    | FloatAbs atom -> FloatAbs (s atom)
    | FloatNeg atom -> FloatNeg (s atom)
    | IntToFloat atom -> IntToFloat (s atom)
    | FloatToInt atom -> FloatToInt (s atom)
    | StringHash str -> StringHash (s str)
    | StringEq (left, right) -> StringEq (s left, s right)
    | RefCountIncString str -> RefCountIncString (s str)
    | RefCountDecString str -> RefCountDecString (s str)
    | RandomInt64 -> RandomInt64

/// Optimize a CExpr with constant folding
let optimizeCExpr (env: ConstEnv) (cexpr: CExpr) : CExpr * bool =
    // First, substitute known constants
    let cexpr' = substCExpr env cexpr

    // Then try to fold
    match cexpr' with
    | Prim (op, left, right) ->
        match foldBinOp op left right with
        | Some folded -> (folded, true)
        | None -> (cexpr', cexpr' <> cexpr)

    | UnaryPrim (op, src) ->
        match foldUnaryOp op src with
        | Some folded -> (folded, true)
        | None -> (cexpr', cexpr' <> cexpr)

    | IfValue (BoolLiteral true, thenVal, _) -> (Atom thenVal, true)
    | IfValue (BoolLiteral false, _, elseVal) -> (Atom elseVal, true)

    | _ -> (cexpr', cexpr' <> cexpr)

/// Optimize an AExpr
let rec optimizeAExpr (env: ConstEnv) (aexpr: AExpr) : AExpr * bool =
    match aexpr with
    | Return atom ->
        let atom' = substAtom env atom
        (Return atom', atom' <> atom)

    | Let (tid, cexpr, body) ->
        // Optimize the CExpr
        let (cexpr', cexprChanged) = optimizeCExpr env cexpr

        // Check for beta reduction: if cexpr is just an Atom, substitute it
        let (env', cexpr'', skipBinding) =
            match cexpr' with
            | Atom a when not (hasSideEffects cexpr') ->
                // Beta reduction: don't emit binding, just substitute
                (Map.add tid a env, cexpr', true)
            | Atom (IntLiteral _ | BoolLiteral _ | FloatLiteral _ | StringLiteral _ | UnitLiteral as constAtom) ->
                // Constant propagation
                (Map.add tid constAtom env, cexpr', false)
            | _ ->
                (env, cexpr', false)

        // Optimize the body
        let (body', bodyChanged) = optimizeAExpr env' body

        // Dead code elimination: if tid is not used in body and cexpr has no side effects
        let usesInBody = collectAExprUses body'
        let isDead = not (Set.contains tid usesInBody) && not (hasSideEffects cexpr')

        if skipBinding then
            // Beta reduction: skip this binding entirely
            (body', true)
        elif isDead then
            // Dead code elimination
            (body', true)
        else
            (Let (tid, cexpr', body'), cexprChanged || bodyChanged)

    | If (cond, thenBranch, elseBranch) ->
        let cond' = substAtom env cond

        // Fold constant conditions
        match cond' with
        | BoolLiteral true ->
            let (thenBranch', _) = optimizeAExpr env thenBranch
            (thenBranch', true)
        | BoolLiteral false ->
            let (elseBranch', _) = optimizeAExpr env elseBranch
            (elseBranch', true)
        | _ ->
            let (thenBranch', thenChanged) = optimizeAExpr env thenBranch
            let (elseBranch', elseChanged) = optimizeAExpr env elseBranch
            (If (cond', thenBranch', elseBranch'), cond' <> cond || thenChanged || elseChanged)

/// Optimize a function
let optimizeFunction (func: Function) : Function * bool =
    // Initialize env with function parameters (they're not constants)
    let env = Map.empty
    let (body', changed) = optimizeAExpr env func.Body
    ({ func with Body = body' }, changed)

/// Optimize until fixed point
let rec optimizeToFixedPoint (func: Function) (maxIterations: int) : Function =
    if maxIterations <= 0 then func
    else
        let (func', changed) = optimizeFunction func
        if changed then
            optimizeToFixedPoint func' (maxIterations - 1)
        else
            func'

/// Optimize a program
let optimizeProgram (program: Program) : Program =
    let (Program (functions, mainExpr)) = program

    // Optimize all functions
    let functions' = functions |> List.map (fun f -> optimizeToFixedPoint f 10)

    // Optimize main expression
    let (mainExpr', _) = optimizeAExpr Map.empty mainExpr
    let mainFunc = { Name = "__main__"; Params = []; Body = mainExpr' }
    let mainOptimized = optimizeToFixedPoint mainFunc 10

    Program (functions', mainOptimized.Body)
