// 3.5_MIR_Optimize.fs - MIR/SSA Optimization Pass
//
// Performs optimizations on MIR in SSA form:
// - Dead Code Elimination (DCE): remove unused instructions
// - Copy Propagation: eliminate trivial moves and phis
// - Constant Propagation: propagate known constant values
// - CFG Simplification: remove empty blocks, merge blocks
//
// These optimizations leverage SSA form where each variable is defined exactly once.

module MIR_Optimize

open MIR
open Output

/// Check if an instruction has side effects (must be preserved even if unused)
let hasSideEffects (instr: Instr) : bool =
    match instr with
    | Mov _ -> false
    | BinOp _ -> false
    | UnaryOp _ -> false
    | Phi _ -> false
    | HeapLoad _ -> false
    // These have side effects
    | Call _ -> true  // Function calls may have side effects
    | TailCall _ -> true  // Tail calls have side effects
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true  // Indirect tail calls have side effects
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true  // Closure tail calls have side effects
    | HeapAlloc _ -> true  // Allocates memory
    | HeapStore _ -> true  // Writes to memory
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
    | RawSet _ -> true    // Writes to memory
    | RawSetByte _ -> true  // Writes to memory (byte)
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | IntToFloat _ -> false // Pure conversion
    | FloatToInt _ -> false // Pure conversion
    | StringHash _ -> false  // Pure
    | StringEq _ -> false    // Pure
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RandomInt64 _ -> true  // Syscall
    | FloatToString _ -> false  // Pure conversion (allocates but no visible side effect)
    | CoverageHit _ -> true  // Must not be eliminated (tracking side effect)

/// Get the destination VReg of an instruction (if any)
let getInstrDest (instr: Instr) : VReg option =
    match instr with
    | Mov (dest, _, _) -> Some dest
    | BinOp (dest, _, _, _, _) -> Some dest
    | UnaryOp (dest, _, _) -> Some dest
    | Call (dest, _, _, _, _) -> Some dest
    | TailCall _ -> None  // Tail calls don't return here
    | IndirectCall (dest, _, _, _, _) -> Some dest
    | IndirectTailCall _ -> None  // Indirect tail calls don't return here
    | ClosureAlloc (dest, _, _) -> Some dest
    | ClosureCall (dest, _, _) -> Some dest
    | ClosureTailCall _ -> None  // Closure tail calls don't return here
    | HeapAlloc (dest, _) -> Some dest
    | HeapLoad (dest, _, _, _) -> Some dest
    | StringConcat (dest, _, _) -> Some dest
    | FileReadText (dest, _) -> Some dest
    | FileExists (dest, _) -> Some dest
    | FileWriteText (dest, _, _) -> Some dest
    | FileAppendText (dest, _, _) -> Some dest
    | FileDelete (dest, _) -> Some dest
    | FileSetExecutable (dest, _) -> Some dest
    | FileWriteFromPtr (dest, _, _, _) -> Some dest
    | Phi (dest, _, _) -> Some dest
    | RawAlloc (dest, _) -> Some dest
    | RawGet (dest, _, _, _) -> Some dest
    | RawGetByte (dest, _, _) -> Some dest
    | FloatSqrt (dest, _) -> Some dest
    | FloatAbs (dest, _) -> Some dest
    | FloatNeg (dest, _) -> Some dest
    | IntToFloat (dest, _) -> Some dest
    | FloatToInt (dest, _) -> Some dest
    | StringHash (dest, _) -> Some dest
    | StringEq (dest, _, _) -> Some dest
    | HeapStore _ -> None
    | RefCountInc _ -> None
    | RefCountDec _ -> None
    | Print _ -> None
    | RawFree _ -> None
    | RawSet _ -> None
    | RawSetByte _ -> None
    | RefCountIncString _ -> None
    | RefCountDecString _ -> None
    | RandomInt64 dest -> Some dest
    | FloatToString (dest, _) -> Some dest
    | CoverageHit _ -> None

/// Get all VRegs used by an instruction
let getInstrUses (instr: Instr) : Set<VReg> =
    let fromOperand op =
        match op with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty

    match instr with
    | Mov (_, src, _) -> fromOperand src
    | BinOp (_, _, left, right, _) -> Set.union (fromOperand left) (fromOperand right)
    | UnaryOp (_, _, src) -> fromOperand src
    | Call (_, _, args, _, _) -> args |> List.map fromOperand |> Set.unionMany
    | TailCall (_, args, _, _) -> args |> List.map fromOperand |> Set.unionMany
    | IndirectCall (_, func, args, _, _) -> Set.unionMany ((fromOperand func) :: (args |> List.map fromOperand))
    | IndirectTailCall (func, args, _, _) -> Set.unionMany ((fromOperand func) :: (args |> List.map fromOperand))
    | ClosureAlloc (_, _, captures) -> captures |> List.map fromOperand |> Set.unionMany
    | ClosureCall (_, closure, args) -> Set.unionMany ((fromOperand closure) :: (args |> List.map fromOperand))
    | ClosureTailCall (closure, args) -> Set.unionMany ((fromOperand closure) :: (args |> List.map fromOperand))
    | HeapAlloc _ -> Set.empty
    | HeapStore (addr, _, src, _) -> Set.add addr (fromOperand src)
    | HeapLoad (_, addr, _, _) -> Set.singleton addr
    | StringConcat (_, left, right) -> Set.union (fromOperand left) (fromOperand right)
    | RefCountInc (addr, _) -> Set.singleton addr
    | RefCountDec (addr, _) -> Set.singleton addr
    | Print (src, _) -> fromOperand src
    | FileReadText (_, path) -> fromOperand path
    | FileExists (_, path) -> fromOperand path
    | FileWriteText (_, path, content) -> Set.union (fromOperand path) (fromOperand content)
    | FileAppendText (_, path, content) -> Set.union (fromOperand path) (fromOperand content)
    | FileDelete (_, path) -> fromOperand path
    | FileSetExecutable (_, path) -> fromOperand path
    | FileWriteFromPtr (_, path, ptr, length) -> Set.unionMany [fromOperand path; fromOperand ptr; fromOperand length]
    | Phi (_, sources, _) -> sources |> List.map (fun (op, _) -> fromOperand op) |> Set.unionMany
    | RawAlloc (_, numBytes) -> fromOperand numBytes
    | RawFree ptr -> fromOperand ptr
    | RawGet (_, ptr, byteOffset, _) -> Set.union (fromOperand ptr) (fromOperand byteOffset)
    | RawGetByte (_, ptr, byteOffset) -> Set.union (fromOperand ptr) (fromOperand byteOffset)
    | RawSet (ptr, byteOffset, value, _) -> Set.unionMany [fromOperand ptr; fromOperand byteOffset; fromOperand value]
    | RawSetByte (ptr, byteOffset, value) -> Set.unionMany [fromOperand ptr; fromOperand byteOffset; fromOperand value]
    | FloatSqrt (_, src) -> fromOperand src
    | FloatAbs (_, src) -> fromOperand src
    | FloatNeg (_, src) -> fromOperand src
    | IntToFloat (_, src) -> fromOperand src
    | FloatToInt (_, src) -> fromOperand src
    | StringHash (_, str) -> fromOperand str
    | StringEq (_, left, right) -> Set.union (fromOperand left) (fromOperand right)
    | RefCountIncString str -> fromOperand str
    | RefCountDecString str -> fromOperand str
    | RandomInt64 _ -> Set.empty  // No operand uses
    | FloatToString (_, value) -> fromOperand value
    | CoverageHit _ -> Set.empty  // No operand uses

/// Get VRegs used by terminator
let getTerminatorUses (term: Terminator) : Set<VReg> =
    match term with
    | Ret op ->
        match op with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty
    | Branch (cond, _, _) ->
        match cond with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty
    | Jump _ -> Set.empty

/// Collect all uses in a CFG
let collectAllUses (cfg: CFG) : Set<VReg> =
    cfg.Blocks
    |> Map.fold (fun uses _ block ->
        let instrUses = block.Instrs |> List.map getInstrUses |> Set.unionMany
        let termUses = getTerminatorUses block.Terminator
        Set.unionMany [uses; instrUses; termUses]
    ) Set.empty

/// Dead Code Elimination
/// Remove instructions whose destinations are never used (unless they have side effects)
let eliminateDeadCode (cfg: CFG) : CFG * bool =
    let allUses = collectAllUses cfg

    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match getInstrDest instr with
                    | Some dest when not (Set.contains dest allUses) && not (hasSideEffects instr) ->
                        // Dead instruction - remove it
                        (acc', true)
                    | _ ->
                        // Keep instruction
                        (acc' @ [instr], ch')
                ) ([], false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Copy Propagation
/// Replace uses of copy destinations with their sources
/// For: dest = src, replace all uses of dest with src
type CopyMap = Map<VReg, Operand>

let buildCopyMap (cfg: CFG) : CopyMap =
    // First, collect all phi destinations - these should not be copy propagated
    let phiDests =
        cfg.Blocks
        |> Map.fold (fun dests _ block ->
            block.Instrs
            |> List.fold (fun d instr ->
                match instr with
                | Phi (dest, _, _) -> Set.add dest d
                | _ -> d
            ) dests
        ) Set.empty

    cfg.Blocks
    |> Map.fold (fun copies _ block ->
        block.Instrs
        |> List.fold (fun m instr ->
            match instr with
            | Mov (dest, Register src, _) when dest <> src ->
                // Don't add if dest is a phi destination or already in map
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Mov (dest, (IntConst _ as src), _)
            | Mov (dest, (BoolConst _ as src), _) ->
                // Constant propagation: track constant moves too
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest src m
            | Phi (dest, [(Register src, _)], _) when dest <> src ->
                // Trivial phi with single register source
                if Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Phi (dest, sources, _) ->
                // Check if all sources are the same register
                if Map.containsKey dest m then m
                else
                    match sources with
                    | (Register firstSrc, _) :: rest ->
                        if rest |> List.forall (fun (s, _) -> s = Register firstSrc) then
                            if dest <> firstSrc then
                                Map.add dest (Register firstSrc) m
                            else
                                m
                        else
                            m
                    | _ -> m
            | _ -> m
        ) copies
    ) Map.empty

/// Transitively resolve a copy chain (with cycle detection)
let resolveCopy (copies: CopyMap) (op: Operand) : Operand =
    let rec resolve visited op' =
        match op' with
        | Register vreg ->
            if Set.contains vreg visited then
                // Cycle detected, stop here
                op'
            else
                match Map.tryFind vreg copies with
                | Some resolvedOp -> resolve (Set.add vreg visited) resolvedOp
                | None -> op'
        | _ -> op'
    resolve Set.empty op

/// Apply copy propagation to an operand
let propagateCopyOperand (copies: CopyMap) (op: Operand) : Operand =
    resolveCopy copies op

/// Apply copy propagation to an instruction
let propagateCopyInstr (copies: CopyMap) (instr: Instr) : Instr =
    let p = propagateCopyOperand copies
    match instr with
    | Mov (dest, src, vt) -> Mov (dest, p src, vt)
    | BinOp (dest, op, left, right, opType) -> BinOp (dest, op, p left, p right, opType)
    | UnaryOp (dest, op, src) -> UnaryOp (dest, op, p src)
    | Call (dest, name, args, argTypes, retType) -> Call (dest, name, List.map p args, argTypes, retType)
    | TailCall (name, args, argTypes, retType) -> TailCall (name, List.map p args, argTypes, retType)
    | IndirectCall (dest, func, args, argTypes, retType) -> IndirectCall (dest, p func, List.map p args, argTypes, retType)
    | IndirectTailCall (func, args, argTypes, retType) -> IndirectTailCall (p func, List.map p args, argTypes, retType)
    | ClosureAlloc (dest, name, captures) -> ClosureAlloc (dest, name, List.map p captures)
    | ClosureCall (dest, closure, args) -> ClosureCall (dest, p closure, List.map p args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (p closure, List.map p args)
    | HeapAlloc (dest, size) -> HeapAlloc (dest, size)
    | HeapStore (addr, offset, src, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapStore (addr', offset, p src, vt)
    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapLoad (dest, addr', offset, vt)
    | StringConcat (dest, left, right) -> StringConcat (dest, p left, p right)
    | RefCountInc (addr, size) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountInc (addr', size)
    | RefCountDec (addr, size) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountDec (addr', size)
    | Print (src, vt) -> Print (p src, vt)
    | FileReadText (dest, path) -> FileReadText (dest, p path)
    | FileExists (dest, path) -> FileExists (dest, p path)
    | FileWriteText (dest, path, content) -> FileWriteText (dest, p path, p content)
    | FileAppendText (dest, path, content) -> FileAppendText (dest, p path, p content)
    | FileDelete (dest, path) -> FileDelete (dest, p path)
    | FileSetExecutable (dest, path) -> FileSetExecutable (dest, p path)
    | FileWriteFromPtr (dest, path, ptr, length) -> FileWriteFromPtr (dest, p path, p ptr, p length)
    // Don't propagate copies into phi sources - phis are merge points and their
    // sources represent values flowing from specific predecessor blocks
    | Phi (dest, sources, valueType) -> Phi (dest, sources, valueType)
    | RawAlloc (dest, numBytes) -> RawAlloc (dest, p numBytes)
    | RawFree ptr -> RawFree (p ptr)
    | RawGet (dest, ptr, byteOffset, valueType) -> RawGet (dest, p ptr, p byteOffset, valueType)
    | RawGetByte (dest, ptr, byteOffset) -> RawGetByte (dest, p ptr, p byteOffset)
    | RawSet (ptr, byteOffset, value, valueType) -> RawSet (p ptr, p byteOffset, p value, valueType)
    | RawSetByte (ptr, byteOffset, value) -> RawSetByte (p ptr, p byteOffset, p value)
    | FloatSqrt (dest, src) -> FloatSqrt (dest, p src)
    | FloatAbs (dest, src) -> FloatAbs (dest, p src)
    | FloatNeg (dest, src) -> FloatNeg (dest, p src)
    | IntToFloat (dest, src) -> IntToFloat (dest, p src)
    | FloatToInt (dest, src) -> FloatToInt (dest, p src)
    | StringHash (dest, str) -> StringHash (dest, p str)
    | StringEq (dest, left, right) -> StringEq (dest, p left, p right)
    | RefCountIncString str -> RefCountIncString (p str)
    | RefCountDecString str -> RefCountDecString (p str)
    | RandomInt64 dest -> RandomInt64 dest
    | FloatToString (dest, value) -> FloatToString (dest, p value)
    | CoverageHit exprId -> CoverageHit exprId

/// Apply copy propagation to terminator
let propagateCopyTerminator (copies: CopyMap) (term: Terminator) : Terminator =
    let p = propagateCopyOperand copies
    match term with
    | Ret op -> Ret (p op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (p cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Apply copy propagation to CFG
let applyCopyPropagation (cfg: CFG) : CFG * bool =
    let copies = buildCopyMap cfg

    if Map.isEmpty copies then
        (cfg, false)
    else
        // Track if any actual changes were made
        let mutable changed = false

        let blocks' =
            cfg.Blocks
            |> Map.map (fun _ block ->
                let instrs' =
                    block.Instrs
                    |> List.map (fun instr ->
                        let instr' = propagateCopyInstr copies instr
                        if instr' <> instr then changed <- true
                        instr'
                    )
                let term' = propagateCopyTerminator copies block.Terminator
                if term' <> block.Terminator then changed <- true
                { block with Instrs = instrs'; Terminator = term' }
            )
        ({ cfg with Blocks = blocks' }, changed)

/// CFG Simplification: Remove empty blocks (just a jump)
let simplifyEmptyBlocks (cfg: CFG) : CFG * bool =
    // Find blocks that only contain a Jump
    let emptyBlocks =
        cfg.Blocks
        |> Map.filter (fun label block ->
            label <> cfg.Entry &&  // Don't remove entry block
            List.isEmpty block.Instrs &&
            match block.Terminator with
            | Jump _ -> true
            | _ -> false
        )
        |> Map.map (fun _ block ->
            match block.Terminator with
            | Jump target -> target
            | _ -> crash "Expected Jump"
        )

    if Map.isEmpty emptyBlocks then
        (cfg, false)
    else
        // Redirect jumps through empty blocks
        let redirectLabel label =
            Map.tryFind label emptyBlocks |> Option.defaultValue label

        let blocks' =
            cfg.Blocks
            |> Map.filter (fun label _ -> not (Map.containsKey label emptyBlocks))
            |> Map.map (fun _ block ->
                let term' =
                    match block.Terminator with
                    | Jump target -> Jump (redirectLabel target)
                    | Branch (cond, trueLabel, falseLabel) ->
                        Branch (cond, redirectLabel trueLabel, redirectLabel falseLabel)
                    | Ret op -> Ret op

                // Also update phi sources
                let instrs' =
                    block.Instrs
                    |> List.map (fun instr ->
                        match instr with
                        | Phi (dest, sources, valueType) ->
                            let sources' = sources |> List.map (fun (op, lbl) -> (op, redirectLabel lbl))
                            Phi (dest, sources', valueType)
                        | other -> other
                    )

                { block with Instrs = instrs'; Terminator = term' }
            )

        ({ cfg with Blocks = blocks' }, true)

/// Truncate a 64-bit value to the appropriate integer type width
/// This ensures proper overflow/wraparound behavior for smaller integer types
let truncateToType (value: int64) (opType: AST.Type) : int64 =
    match opType with
    | AST.TInt8 -> int64 (int8 value)      // Truncate to signed 8-bit
    | AST.TInt16 -> int64 (int16 value)    // Truncate to signed 16-bit
    | AST.TInt32 -> int64 (int32 value)    // Truncate to signed 32-bit
    | AST.TUInt8 -> int64 (uint8 value)    // Truncate to unsigned 8-bit
    | AST.TUInt16 -> int64 (uint16 value)  // Truncate to unsigned 16-bit
    | AST.TUInt32 -> int64 (uint32 value)  // Truncate to unsigned 32-bit
    | _ -> value                            // Int64/UInt64 and other types: no truncation

/// Constant Folding for MIR
/// Evaluate operations on constants at compile time
let tryFoldBinOp (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : Operand option =
    match op, left, right with
    // Integer arithmetic - apply truncation for proper overflow behavior
    | Add, IntConst a, IntConst b -> Some (IntConst (truncateToType (a + b) opType))
    | Sub, IntConst a, IntConst b -> Some (IntConst (truncateToType (a - b) opType))
    | Mul, IntConst a, IntConst b -> Some (IntConst (truncateToType (a * b) opType))
    // Division: avoid divide by zero and INT64_MIN / -1 overflow
    | Div, IntConst a, IntConst b when b <> 0L && not (a = System.Int64.MinValue && b = -1L) -> Some (IntConst (truncateToType (a / b) opType))
    | Mod, IntConst a, IntConst b when b <> 0L -> Some (IntConst (truncateToType (a % b) opType))

    // Comparisons
    | Eq, IntConst a, IntConst b -> Some (BoolConst (a = b))
    | Neq, IntConst a, IntConst b -> Some (BoolConst (a <> b))
    | Lt, IntConst a, IntConst b -> Some (BoolConst (a < b))
    | Gt, IntConst a, IntConst b -> Some (BoolConst (a > b))
    | Lte, IntConst a, IntConst b -> Some (BoolConst (a <= b))
    | Gte, IntConst a, IntConst b -> Some (BoolConst (a >= b))

    // Boolean operations
    | And, BoolConst a, BoolConst b -> Some (BoolConst (a && b))
    | Or, BoolConst a, BoolConst b -> Some (BoolConst (a || b))

    // Algebraic identities
    | Add, IntConst 0L, x -> Some x
    | Add, x, IntConst 0L -> Some x
    | Sub, x, IntConst 0L -> Some x
    | Sub, x, y when x = y -> Some (IntConst 0L)  // x - x = 0
    | Mul, IntConst 1L, x -> Some x
    | Mul, x, IntConst 1L -> Some x
    | Mul, IntConst 0L, _ -> Some (IntConst 0L)
    | Mul, _, IntConst 0L -> Some (IntConst 0L)
    | Mul, IntConst -1L, x -> None  // Could transform to Neg, but need instruction change
    | Mul, x, IntConst -1L -> None  // Could transform to Neg
    | Div, x, IntConst 1L -> Some x
    | Div, x, y when x = y && y <> IntConst 0L -> Some (IntConst 1L)  // x / x = 1 (if x != 0)
    | Mod, _, IntConst 1L -> Some (IntConst 0L)  // x % 1 = 0
    | Mod, x, y when x = y && y <> IntConst 0L -> Some (IntConst 0L)  // x % x = 0 (if x != 0)

    // Bitwise identities
    | BitAnd, IntConst 0L, _ -> Some (IntConst 0L)
    | BitAnd, _, IntConst 0L -> Some (IntConst 0L)
    | BitAnd, IntConst -1L, x -> Some x  // -1 = all bits set
    | BitAnd, x, IntConst -1L -> Some x
    | BitAnd, x, y when x = y -> Some x  // x & x = x
    | BitOr, IntConst 0L, x -> Some x
    | BitOr, x, IntConst 0L -> Some x
    | BitOr, IntConst -1L, _ -> Some (IntConst -1L)
    | BitOr, _, IntConst -1L -> Some (IntConst -1L)
    | BitOr, x, y when x = y -> Some x  // x | x = x
    | BitXor, IntConst 0L, x -> Some x
    | BitXor, x, IntConst 0L -> Some x
    | BitXor, x, y when x = y -> Some (IntConst 0L)  // x ^ x = 0

    // Shift identities
    | Shl, x, IntConst 0L -> Some x  // x << 0 = x
    | Shr, x, IntConst 0L -> Some x  // x >> 0 = x
    | Shl, IntConst 0L, _ -> Some (IntConst 0L)  // 0 << n = 0
    | Shr, IntConst 0L, _ -> Some (IntConst 0L)  // 0 >> n = 0

    // Boolean short-circuit
    | And, BoolConst false, _ -> Some (BoolConst false)
    | And, _, BoolConst false -> Some (BoolConst false)
    | And, BoolConst true, x -> Some x
    | And, x, BoolConst true -> Some x
    | Or, BoolConst true, _ -> Some (BoolConst true)
    | Or, _, BoolConst true -> Some (BoolConst true)
    | Or, BoolConst false, x -> Some x
    | Or, x, BoolConst false -> Some x

    | _ -> None

/// Common Subexpression Elimination (CSE)
/// Detect identical computations and replace with reference to first result

/// Expression key for CSE - represents a pure computation
type ExprKey =
    | BinExpr of BinOp * Operand * Operand * AST.Type
    | UnaryExpr of UnaryOp * Operand

/// Check if a binary operation is commutative (order of operands doesn't matter)
let isCommutative (op: BinOp) : bool =
    match op with
    | Add | Mul | And | Or | Eq | Neq | BitAnd | BitOr | BitXor -> true
    | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false

/// Normalize operand order for commutative operations (for consistent hashing)
let normalizeOperands (op: BinOp) (left: Operand) (right: Operand) : Operand * Operand =
    if isCommutative op then
        // Use structural comparison to ensure consistent ordering
        if compare left right <= 0 then (left, right) else (right, left)
    else
        (left, right)

/// Build expression key for a BinOp
let makeBinExprKey (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : ExprKey =
    let (l, r) = normalizeOperands op left right
    BinExpr (op, l, r, opType)

/// Build expression key for a UnaryOp
let makeUnaryExprKey (op: UnaryOp) (src: Operand) : ExprKey =
    UnaryExpr (op, src)

/// Apply CSE to a CFG
/// Note: This is a local CSE within each basic block (not global)
let applyCSE (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            // For each block, track expressions we've seen
            let (instrs', _, instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', exprMap: Map<ExprKey, VReg>, ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        let key = makeBinExprKey op left right opType
                        match Map.tryFind key exprMap with
                        | Some prevDest ->
                            // Found a previous computation - replace with copy
                            let copy = Mov (dest, Register prevDest, None)
                            (acc' @ [copy], exprMap, true)
                        | None ->
                            // New expression - add to map
                            let exprMap' = Map.add key dest exprMap
                            (acc' @ [instr], exprMap', ch')
                    | UnaryOp (dest, op, src) ->
                        let key = makeUnaryExprKey op src
                        match Map.tryFind key exprMap with
                        | Some prevDest ->
                            // Found a previous computation - replace with copy
                            let copy = Mov (dest, Register prevDest, None)
                            (acc' @ [copy], exprMap, true)
                        | None ->
                            // New expression - add to map
                            let exprMap' = Map.add key dest exprMap
                            (acc' @ [instr], exprMap', ch')
                    | _ ->
                        (acc' @ [instr], exprMap, ch')
                ) ([], Map.empty, false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Try to fold a unary operation on a constant
let tryFoldUnaryOp (op: UnaryOp) (src: Operand) : Operand option =
    match op, src with
    | Neg, IntConst n -> Some (IntConst (-n))
    | Not, BoolConst b -> Some (BoolConst (not b))
    | _ -> None

/// Apply constant folding to a CFG
let applyConstantFolding (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        match tryFoldBinOp op left right opType with
                        | Some result ->
                            (acc' @ [Mov (dest, result, None)], true)
                        | None ->
                            (acc' @ [instr], ch')
                    | UnaryOp (dest, op, src) ->
                        match tryFoldUnaryOp op src with
                        | Some result ->
                            (acc' @ [Mov (dest, result, None)], true)
                        | None ->
                            (acc' @ [instr], ch')
                    | _ ->
                        (acc' @ [instr], ch')
                ) ([], false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Run all optimizations until fixed point
let optimizeCFG (cfg: CFG) : CFG =
    let (cfg1, _) = applyConstantFolding cfg
    let (cfg2, _) = applyCSE cfg1
    let (cfg3, _) = applyCopyPropagation cfg2
    // Run constant folding again after copy propagation
    // This catches cases like: v1 = -127; v2 = v1 - 2
    // After copy prop: v2 = IntConst(-127) - IntConst(2) -> can fold
    let (cfg4, _) = applyConstantFolding cfg3
    let (cfg5, _) = eliminateDeadCode cfg4
    cfg5

/// Optimize a function
let optimizeFunction (func: Function) : Function =
    let cfg' = optimizeCFG func.CFG
    { func with CFG = cfg' }

/// Optimize a program
let optimizeProgram (program: Program) : Program =
    let (Program (functions, strings, floats, variants, records)) = program
    let functions' = functions |> List.map optimizeFunction
    Program (functions', strings, floats, variants, records)
