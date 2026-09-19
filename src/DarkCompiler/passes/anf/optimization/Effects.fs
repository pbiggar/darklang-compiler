// Effects.fs - Describe ANF evaluation effects and temporary uses.

module ANFEffects

open MemoryModel
open MemoryPlanning
open ANF
open ANFConstants

let private typeNeedsTypedAtomDceProtection (context: OptimizeContext) (typ: AST.Type) : bool =
    typ
    |> rcShapeOfTypeWithSums context.TypeReg context.RecordTypeParams context.SumShapeReg
    |> rcShapeNeedsOwnedScopeRelease

/// Projection bindings carry borrowing information into RC insertion, so only
/// bypass them when the selected element cannot own managed memory.
let internal canForwardTupleElement (context: OptimizeContext) (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | UnitLiteral
    | IntLiteral _
    | BoolLiteral _
    | StringLiteral _
    | FloatLiteral _
    | FuncRef _ -> true
    | Var tid ->
        Map.tryFind tid typeEnv
        |> Option.exists (typeNeedsTypedAtomDceProtection context >> not)

/// Whether evaluating a CExpr must be retained when its result is unused.
/// This is deliberately weaker than CSE eligibility: allocations and mutable
/// reads can be discarded, but two executions cannot necessarily be merged.
let internal mustPreserveEvaluation (context: OptimizeContext) (cexpr: CExpr) : bool =
    match cexpr with
    | Atom _ -> false
    | TypedAtom (_, typ) ->
        // Some internal lowerings materialize ownership only after tagging a
        // raw pointer with its heap type. Dropping that marker before RC
        // insertion can orphan the allocation even though the cast itself is
        // computationally pure.
        typeNeedsTypedAtomDceProtection context typ
    | Prim _ -> false
    | UnaryPrim _ -> false
    | IfValue _ -> false
    | TupleAlloc _ -> false
    | TupleGet _ -> false
    | RecordAlloc _ -> false
    | RecordGet _ -> false
    | RecordClone _ -> false
    | RecordReuse _ -> true
    // These have side effects
    | Call _ -> true
    | BorrowedCall _ -> true
    | TailCall _ -> true
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true
    | StringConcat _ -> true  // Allocates memory
    | CanonicalBufferEq _ -> false
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | StdoutWrite _ -> true
    | StdinReadLine -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | MappedAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | MappedFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawTake _ -> true   // Ownership transfer; paired with clearing the source slot
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawWriteWord _ -> true    // Memory mutation
    | RawWriteByte _ -> true  // Memory mutation (byte)
    | RawSlotInit _ -> true  // Memory mutation plus possible ownership edge
    | StringToRawPtr _ -> false
    | RawPtrToString _ -> false
    | BlobToRawPtr _ -> false
    | RawPtrToBlob _ -> false
    | RawPtrToInt128 _ -> false
    | RawPtrToUInt128 _ -> false
    | DictToRawPtr _ -> false
    | RawPtrToDict _ -> false
    | ListToRawPtr _ -> false
    | FixedBlockToRawPtr _ -> false
    | RawPtrToList _ -> false
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RefCountIncBlob _ -> true    // Mutates refcount
    | RefCountDecBlob _ -> true    // Mutates refcount
    | RandomInt64 -> true   // Reads from OS random source
    | DateTimeNow -> true       // Reads current time (syscall)
    | Sleep _ -> true           // Blocks the current process
    | CliNative _ -> true
    | FloatToString _ -> false  // Its allocation is unobservable when the result is unused
    | RuntimeError _ -> true
    | RuntimeErrorString _ -> true

/// Add the TempId used by an atom to an existing liveness set.
let internal addAtomUse (atom: Atom) (uses: Set<TempId>) : Set<TempId> =
    match atom with
    | Var tid -> Set.add tid uses
    | _ -> uses

let private addAtomUses (atoms: Atom list) (uses: Set<TempId>) : Set<TempId> =
    List.fold (fun uses atom -> addAtomUse atom uses) uses atoms

let atomUsesTemp (tid: TempId) (atom: Atom) : bool =
    match atom with
    | Var usedTid -> usedTid = tid
    | _ -> false

let atomsUseTemp (tid: TempId) (atoms: Atom list) : bool =
    List.exists (atomUsesTemp tid) atoms

/// Add every TempId used by a CExpr to an existing liveness set.
let internal addCExprUses (cexpr: CExpr) (uses: Set<TempId>) : Set<TempId> =
    match cexpr with
    | Atom a -> addAtomUse a uses
    | TypedAtom (a, _) -> addAtomUse a uses
    | Prim (_, left, right) -> uses |> addAtomUse left |> addAtomUse right
    | UnaryPrim (_, src) -> addAtomUse src uses
    | IfValue (cond, thenVal, elseVal) ->
        uses |> addAtomUse cond |> addAtomUse thenVal |> addAtomUse elseVal
    | Call (_, args) -> addAtomUses args uses
    | BorrowedCall (_, args) -> addAtomUses args uses
    | TailCall (_, args) -> addAtomUses args uses
    | IndirectCall (func, args) ->
        uses |> addAtomUse func |> addAtomUses args
    | IndirectTailCall (func, args) ->
        uses |> addAtomUse func |> addAtomUses args
    | ClosureAlloc (_, captures) -> addAtomUses captures uses
    | ClosureCall (closure, args) ->
        uses |> addAtomUse closure |> addAtomUses args
    | ClosureTailCall (closure, args) ->
        uses |> addAtomUse closure |> addAtomUses args
    | TupleAlloc elems -> addAtomUses elems uses
    | TupleGet (tuple, _) -> addAtomUse tuple uses
    | RecordAlloc (_, fields) -> addAtomUses fields uses
    | RecordGet (_, record, _) -> addAtomUse record uses
    | RecordClone (_, record, fields)
    | RecordReuse (_, record, fields) ->
        uses |> addAtomUse record |> addAtomUses fields
    | StringConcat (first, second, remaining) ->
        uses |> addAtomUses (first :: second :: remaining)
    | CanonicalBufferEq (_, left, right) -> uses |> addAtomUse left |> addAtomUse right
    | RefCountInc (atom, _, _, _) -> addAtomUse atom uses
    | RefCountDec (atom, _, _, _) -> addAtomUse atom uses
    | Print (atom, _) -> addAtomUse atom uses
    | StdoutWrite (atom, _) -> addAtomUse atom uses
    | StdinReadLine -> uses
    | FileReadText path -> addAtomUse path uses
    | FileExists path -> addAtomUse path uses
    | FileWriteText (path, content) -> uses |> addAtomUse path |> addAtomUse content
    | FileAppendText (path, content) -> uses |> addAtomUse path |> addAtomUse content
    | FileDelete path -> addAtomUse path uses
    | FileSetExecutable path -> addAtomUse path uses
    | FileWriteFromPtr (path, ptr, length) ->
        uses |> addAtomUse path |> addAtomUse ptr |> addAtomUse length
    | RawAlloc numBytes -> addAtomUse numBytes uses
    | MappedAlloc numBytes -> addAtomUse numBytes uses
    | RawFree ptr -> addAtomUse ptr uses
    | MappedFree ptr -> addAtomUse ptr uses
    | RawGet (ptr, byteOffset, _) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawTake (ptr, byteOffset, _) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawGetByte (ptr, byteOffset) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawWriteWord (ptr, byteOffset, value) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | RawWriteByte (ptr, byteOffset, value) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | RawSlotInit (ptr, byteOffset, value, _) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | StringToRawPtr value -> addAtomUse value uses
    | RawPtrToString ptr -> addAtomUse ptr uses
    | BlobToRawPtr value -> addAtomUse value uses
    | RawPtrToBlob ptr -> addAtomUse ptr uses
    | RawPtrToInt128 ptr -> addAtomUse ptr uses
    | RawPtrToUInt128 ptr -> addAtomUse ptr uses
    | DictToRawPtr dict -> addAtomUse dict uses
    | RawPtrToDict (ptr, tag, _) -> uses |> addAtomUse ptr |> addAtomUse tag
    | ListToRawPtr list -> addAtomUse list uses
    | FixedBlockToRawPtr value -> addAtomUse value uses
    | RawPtrToList (ptr, tag, _) -> uses |> addAtomUse ptr |> addAtomUse tag
    | FloatSqrt atom -> addAtomUse atom uses
    | FloatAbs atom -> addAtomUse atom uses
    | FloatNeg atom -> addAtomUse atom uses
    | Int64ToFloat atom -> addAtomUse atom uses
    | FloatToInt64 atom -> addAtomUse atom uses
    | FloatToBits atom -> addAtomUse atom uses
    | RefCountIncString str -> addAtomUse str uses
    | RefCountDecString str -> addAtomUse str uses
    | RefCountIncBlob bytes -> addAtomUse bytes uses
    | RefCountDecBlob bytes -> addAtomUse bytes uses
    | RandomInt64 -> uses  // No atoms
    | DateTimeNow -> uses      // No atoms
    | Sleep delayMs -> addAtomUse delayMs uses
    | CliNative (_, args) -> addAtomUses args uses
    | FloatToString atom -> addAtomUse atom uses
    | RuntimeError _ -> uses
    | RuntimeErrorString atom -> addAtomUse atom uses

/// Complete operand interface for lexical-scope verification.
let cexprTempUses (cexpr: CExpr) : Set<TempId> = addCExprUses cexpr Set.empty

/// Test whether a CExpr uses a TempId without constructing a liveness set.
let cexprUsesTemp (tid: TempId) (cexpr: CExpr) : bool =
    let used = atomUsesTemp tid
    let anyUsed = atomsUseTemp tid

    match cexpr with
    | Atom atom
    | TypedAtom (atom, _)
    | UnaryPrim (_, atom)
    | TupleGet (atom, _)
    | RecordGet (_, atom, _)
    | RefCountInc (atom, _, _, _)
    | RefCountDec (atom, _, _, _)
    | Print (atom, _)
    | FileReadText atom
    | FileExists atom
    | FileDelete atom
    | FileSetExecutable atom
    | RawAlloc atom
    | MappedAlloc atom
    | RawFree atom
    | MappedFree atom
    | StringToRawPtr atom
    | RawPtrToString atom
    | BlobToRawPtr atom
    | RawPtrToBlob atom
    | RawPtrToInt128 atom
    | RawPtrToUInt128 atom
    | DictToRawPtr atom
    | ListToRawPtr atom
    | FixedBlockToRawPtr atom
    | FloatSqrt atom
    | FloatAbs atom
    | FloatNeg atom
    | Int64ToFloat atom
    | FloatToInt64 atom
    | FloatToBits atom
    | RefCountIncString atom
    | RefCountDecString atom
    | RefCountIncBlob atom
    | RefCountDecBlob atom
    | FloatToString atom -> used atom
    | Sleep atom -> used atom
    | StdoutWrite (atom, _) -> used atom
    | StringConcat (first, second, remaining) -> anyUsed (first :: second :: remaining)
    | Prim (_, left, right)
    | CanonicalBufferEq (_, left, right)
    | FileWriteText (left, right)
    | FileAppendText (left, right)
    | RawGet (left, right, _)
    | RawTake (left, right, _)
    | RawGetByte (left, right)
    | RawPtrToDict (left, right, _)
    | RawPtrToList (left, right, _) -> used left || used right
    | IfValue (first, second, third)
    | FileWriteFromPtr (first, second, third)
    | RawWriteWord (first, second, third)
    | RawWriteByte (first, second, third)
    | RawSlotInit (first, second, third, _) ->
        used first || used second || used third
    | Call (_, atoms)
    | BorrowedCall (_, atoms)
    | TailCall (_, atoms)
    | ClosureAlloc (_, atoms)
    | TupleAlloc atoms
    | RecordAlloc (_, atoms) -> anyUsed atoms
    | RecordClone (_, record, fields)
    | RecordReuse (_, record, fields) -> used record || anyUsed fields
    | CliNative (_, atoms) -> anyUsed atoms
    | IndirectCall (first, rest)
    | IndirectTailCall (first, rest)
    | ClosureCall (first, rest)
    | ClosureTailCall (first, rest) -> used first || anyUsed rest
    | RandomInt64
    | DateTimeNow
    | StdinReadLine
    | RuntimeError _ -> false
    | RuntimeErrorString atom -> used atom
