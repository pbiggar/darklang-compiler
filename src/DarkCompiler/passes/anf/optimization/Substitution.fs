// Substitution.fs - Substitute ANF atoms and simplify individual expressions.

module ANFSubstitution

open MemoryModel
open ANF
open ANFConstants

/// Substitute atom in another atom
let substAtom (env: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var tid -> Map.tryFind tid env |> Option.defaultValue atom
    | _ -> atom

/// Substitute operands in one pass, preserving the original list when no atom
/// changes and sharing the untouched suffix after the final replacement.
let rec private substAtoms (env: Map<TempId, Atom>) (atoms: Atom list) : Atom list =
    match atoms with
    | [] -> atoms
    | atom :: rest ->
        let atom' = substAtom env atom
        let rest' = substAtoms env rest
        if atom' = atom && obj.ReferenceEquals (rest', rest) then
            atoms
        else
            atom' :: rest'

/// Substitute atoms in CExpr
let private substCExprValue (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    let s = substAtom env
    match cexpr with
    | Atom a -> Atom (s a)
    | TypedAtom (a, t) -> TypedAtom (s a, t)
    | Prim (op, left, right) -> Prim (op, s left, s right)
    | UnaryPrim (op, src) -> UnaryPrim (op, s src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (s cond, s thenVal, s elseVal)
    | Call (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else Call (name, args')
    | BorrowedCall (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else BorrowedCall (name, args')
    | TailCall (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else TailCall (name, args')
    | IndirectCall (func, args) ->
        let func' = s func
        let args' = substAtoms env args
        if func' = func && obj.ReferenceEquals (args', args) then cexpr else IndirectCall (func', args')
    | IndirectTailCall (func, args) ->
        let func' = s func
        let args' = substAtoms env args
        if func' = func && obj.ReferenceEquals (args', args) then cexpr else IndirectTailCall (func', args')
    | ClosureAlloc (name, captures) ->
        let captures' = substAtoms env captures
        if obj.ReferenceEquals (captures', captures) then cexpr else ClosureAlloc (name, captures')
    | ClosureCall (closure, args) ->
        let closure' = s closure
        let args' = substAtoms env args
        if closure' = closure && obj.ReferenceEquals (args', args) then cexpr else ClosureCall (closure', args')
    | ClosureTailCall (closure, args) ->
        let closure' = s closure
        let args' = substAtoms env args
        if closure' = closure && obj.ReferenceEquals (args', args) then cexpr else ClosureTailCall (closure', args')
    | TupleAlloc elems ->
        let elems' = substAtoms env elems
        if obj.ReferenceEquals (elems', elems) then cexpr else TupleAlloc elems'
    | TupleGet (tuple, idx) -> TupleGet (s tuple, idx)
    | RecordAlloc (descriptor, fields) ->
        let fields' = substAtoms env fields
        if obj.ReferenceEquals (fields', fields) then cexpr else RecordAlloc (descriptor, fields')
    | RecordGet (descriptor, record, idx) -> RecordGet (descriptor, s record, idx)
    | RecordClone (descriptor, record, fields) ->
        RecordClone (descriptor, s record, substAtoms env fields)
    | RecordReuse (sourceDescriptor, targetDescriptor, record, fields) ->
        RecordReuse (sourceDescriptor, targetDescriptor, s record, substAtoms env fields)
    | StringConcat (first, second, remaining) ->
        StringConcat (s first, s second, List.map s remaining)
    | CanonicalBufferEq (kind, left, right) -> CanonicalBufferEq (kind, s left, s right)
    | RefCountInc (atom, size, kind, sourceType) -> RefCountInc (s atom, size, kind, sourceType)
    | RefCountDec (atom, size, kind, sourceType) -> RefCountDec (s atom, size, kind, sourceType)
    | Print (atom, t) -> Print (s atom, t)
    | StdoutWrite (atom, appendNewline) -> StdoutWrite (s atom, appendNewline)
    | StdinReadLine -> StdinReadLine
    | FileReadBlob path -> FileReadBlob (s path)
    | FileExists path -> FileExists (s path)
    | FileWriteBlob (path, content) -> FileWriteBlob (s path, s content)
    | FileAppendText (path, content) -> FileAppendText (s path, s content)
    | FileDelete path -> FileDelete (s path)
    | FileCreateDirectory path -> FileCreateDirectory (s path)
    | FileSetExecutable path -> FileSetExecutable (s path)
    | FileWriteFromPtr (path, ptr, length) -> FileWriteFromPtr (s path, s ptr, s length)
    | RawAlloc numBytes -> RawAlloc (s numBytes)
    | MappedAlloc numBytes -> MappedAlloc (s numBytes)
    | RawFree ptr -> RawFree (s ptr)
    | MappedFree ptr -> MappedFree (s ptr)
    | RawGet (ptr, byteOffset, valueType) -> RawGet (s ptr, s byteOffset, valueType)
    | RawTake (ptr, byteOffset, valueType) -> RawTake (s ptr, s byteOffset, valueType)
    | RawGetByte (ptr, byteOffset) -> RawGetByte (s ptr, s byteOffset)
    | RawWriteWord (ptr, byteOffset, value) -> RawWriteWord (s ptr, s byteOffset, s value)
    | RawWriteByte (ptr, byteOffset, value) -> RawWriteByte (s ptr, s byteOffset, s value)
    | RawSlotInit (ptr, byteOffset, value, valueType) -> RawSlotInit (s ptr, s byteOffset, s value, valueType)
    | StringToRawPtr value -> StringToRawPtr (s value)
    | RawPtrToString ptr -> RawPtrToString (s ptr)
    | BlobToRawPtr value -> BlobToRawPtr (s value)
    | RawPtrToBlob ptr -> RawPtrToBlob (s ptr)
    | RawPtrToInt128 ptr -> RawPtrToInt128 (s ptr)
    | RawPtrToUInt128 ptr -> RawPtrToUInt128 (s ptr)
    | DictToRawPtr dict -> DictToRawPtr (s dict)
    | RawPtrToDict (ptr, tag, dictType) -> RawPtrToDict (s ptr, s tag, dictType)
    | ListToRawPtr list -> ListToRawPtr (s list)
    | FixedBlockToRawPtr value -> FixedBlockToRawPtr (s value)
    | RawPtrToList (ptr, tag, listType) -> RawPtrToList (s ptr, s tag, listType)
    | FloatSqrt atom -> FloatSqrt (s atom)
    | FloatAbs atom -> FloatAbs (s atom)
    | FloatNeg atom -> FloatNeg (s atom)
    | Int64ToFloat atom -> Int64ToFloat (s atom)
    | FloatToInt64 atom -> FloatToInt64 (s atom)
    | FloatToBits atom -> FloatToBits (s atom)
    | RefCountIncString str -> RefCountIncString (s str)
    | RefCountDecString str -> RefCountDecString (s str)
    | RefCountIncBlob bytes -> RefCountIncBlob (s bytes)
    | RefCountDecBlob bytes -> RefCountDecBlob (s bytes)
    | RefCountIncInt value -> RefCountIncInt (s value)
    | RefCountDecInt value -> RefCountDecInt (s value)
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (s delayMs)
    | CliNative (operation, args) -> CliNative (operation, List.map s args)
    | FloatToString atom -> FloatToString (s atom)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (s atom)

/// Substitute atoms in a CExpr, preserving the original value when there is no
/// substitution environment.
let substCExpr (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    if Map.isEmpty env then
        cexpr
    else
        substCExprValue env cexpr

/// Substitute atoms while reporting list-bearing no-op expressions by identity,
/// avoiding a structural comparison of their operands.
let private substCExprWithChange (env: Map<TempId, Atom>) (cexpr: CExpr) : struct (CExpr * bool) =
    if Map.isEmpty env then
        struct (cexpr, false)
    else
        let cexpr' = substCExprValue env cexpr
        let changed =
            match cexpr with
            | Call _
            | BorrowedCall _
            | TailCall _
            | IndirectCall _
            | IndirectTailCall _
            | ClosureAlloc _
            | ClosureCall _
            | ClosureTailCall _
            | TupleAlloc _ -> not (obj.ReferenceEquals (cexpr', cexpr))
            | _ -> cexpr' <> cexpr
        struct (cexpr', changed)

/// Optimize a CExpr with constant folding
let optimizeCExpr (options: OptimizeOptions) (env: ConstEnv) (typeEnv: TypeEnv) (tupleEnv: TupleEnv) (cexpr: CExpr) : CExpr * bool =
    // First, substitute known constants
    let struct (cexpr', substitutionChanged) = substCExprWithChange env cexpr

    let tryConstFold () =
        if options.EnableConstFolding then
            match cexpr' with
            | Prim (op, left, right) ->
                match foldBinOp op left right with
                | Some folded -> Some folded
                | None -> None
            | UnaryPrim (op, src) -> foldUnaryOp op src
            | FloatNeg (FloatLiteral f) -> Some (Atom (FloatLiteral (-f)))
            | FloatAbs (FloatLiteral f) -> Some (Atom (FloatLiteral (abs f)))
            | FloatSqrt (FloatLiteral f) -> Some (Atom (FloatLiteral (sqrt f)))
            | Int64ToFloat (IntLiteral (Int64 n)) -> Some (Atom (FloatLiteral (float n)))
            | FloatToInt64 (FloatLiteral f) ->
                tryTruncateFloatToInt64 f
                |> Option.map (fun n -> Atom (IntLiteral (Int64 n)))
            | FloatToBits (FloatLiteral f) ->
                Some (Atom (IntLiteral (UInt64 (System.BitConverter.DoubleToUInt64Bits f))))
            | StringConcat (first, second, remaining) ->
                let parts = first :: second :: remaining
                if parts |> List.forall (function StringLiteral _ -> true | _ -> false) then
                    parts
                    |> List.choose (function StringLiteral value -> Some value | _ -> None)
                    |> String.concat ""
                    |> StringLiteral
                    |> Atom
                    |> Some
                else
                    match parts |> List.filter (function StringLiteral "" -> false | _ -> true) with
                    | [single] -> Some (Atom single)
                    | _ -> None
            | Call (id, [StringLiteral left; StringLiteral right])
                when id = AST.functionIdForName "Darklang.Stdlib.String.__appendNormalized" ->
                let normalized = (left + right).Normalize(System.Text.NormalizationForm.FormC)
                Some (Atom (StringLiteral normalized))
            | Call (id, [StringLiteral value])
                when id = AST.functionIdForName "Darklang.Stdlib.String.__normalizeAfterConcat" ->
                Some (Atom (StringLiteral (value.Normalize(System.Text.NormalizationForm.FormC))))
            | Call (id, [left; StringLiteral ""])
                when id = AST.functionIdForName "Darklang.Stdlib.String.__appendNormalized" ->
                Some (Atom left)
            | Call (id, [StringLiteral ""; right])
                when id = AST.functionIdForName "Darklang.Stdlib.String.__appendNormalized" ->
                Some (Atom right)
            | TupleGet (Var tupleTid, index) ->
                Map.tryFind tupleTid tupleEnv
                |> Option.bind (Map.tryFind index)
                |> Option.map Atom
            | CanonicalBufferEq (_, StringLiteral left, StringLiteral right) ->
                Some (Atom (BoolLiteral (left = right)))
            | CanonicalBufferEq (_, Var leftTid, Var rightTid) when leftTid = rightTid ->
                Some (Atom (BoolLiteral true))
            | IfValue (BoolLiteral true, thenVal, _) -> Some (Atom thenVal)
            | IfValue (BoolLiteral false, _, elseVal) -> Some (Atom elseVal)
            | IfValue (_, thenVal, elseVal) when thenVal = elseVal -> Some (Atom thenVal)
            | _ -> None
        else
            None

    match tryConstFold () with
    | Some folded -> (folded, true)
    | None ->
        if options.EnableStrengthReduction then
            match cexpr' with
            | Prim (op, left, right) ->
                match tryStrengthReduce typeEnv op left right with
                | Some reduced -> (reduced, true)
                | None -> (cexpr', substitutionChanged)
            | _ -> (cexpr', substitutionChanged)
        else
            (cexpr', substitutionChanged)
