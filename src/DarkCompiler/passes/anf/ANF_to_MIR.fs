// ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into MIR with Control Flow Graph (CFG).
//
// Algorithm:
// - Converts ANF expressions into MIR CFG with basic blocks
// - Maps ANF temporary variables to MIR virtual registers
// - Converts ANF If expressions into conditional branches with basic blocks
// - Each basic block has a label, instructions, and a terminator
//
// Example (with if):
//   if x then 10 else 20
//   →
//   entry:
//     branch x, then_block, else_block
//   then_block:
//     v0 <- 10
//     jump join_block
//   else_block:
//     v1 <- 20
//     jump join_block
//   join_block:
//     v2 <- phi(v0, v1)  // (simplified - actual implementation uses registers)
//     ret v2

module ANF_to_MIR

open ResultList

/// Helper to create VariantInfo record
let private mkVariantInfo (name: string) (tag: int) (fields: AST.Type list) : MIR.VariantInfo =
    let payload =
        match fields with
        | [] -> None
        | [field] -> Some field
        | _ -> Some (AST.TTuple fields)
    { MIR.VariantInfo.Name = name; MIR.VariantInfo.Tag = tag; MIR.VariantInfo.Payload = payload }

/// Helper to create TypeVariants record
let private mkTypeVariants (typeParams: string list) (variants: MIR.VariantInfo list) : MIR.TypeVariants =
    { MIR.TypeVariants.TypeParams = typeParams; MIR.TypeVariants.Variants = variants }

/// Helper to create RecordField record
let private mkRecordField (name: string) (typ: AST.Type) : MIR.RecordField =
    { MIR.RecordField.Name = name; MIR.RecordField.Type = typ }

/// Build VariantRegistry from VariantLookup
/// VariantLookup: variantName -> (typeName, typeParams, tagIndex, payloadType)
/// VariantRegistry: typeName -> TypeVariants (with named record types)
let buildVariantRegistry (variantLookup: LoweringPrimitives.VariantLookup) : MIR.VariantRegistry =
    let entries = variantLookup |> Map.toList
    let canonicalEntries =
        entries
        |> List.choose (fun (variantName, (typeName, typeParams, tagIndex, payloadType)) ->
        // Qualified entries are canonical and cannot be overwritten by a
        // same-named case from another nominal type. Short entries exist only
        // for source resolution.
            let prefix = $"{typeName}."
            if variantName.StartsWith(prefix) then
                Some (typeName, typeParams, (variantName.Substring(prefix.Length), tagIndex, payloadType))
            else
                None)
    let canonicalTypes = canonicalEntries |> List.map (fun (typeName, _, _) -> typeName) |> Set.ofList
    let fixtureOnlyShortEntries =
        entries
        |> List.choose (fun (variantName, (typeName, typeParams, tagIndex, payloadType)) ->
            if Set.contains typeName canonicalTypes then None
            else Some (typeName, typeParams, (variantName, tagIndex, payloadType)))
    canonicalEntries @ fixtureOnlyShortEntries
    |> List.groupBy (fun (typeName, _, _) -> typeName)
    |> List.map (fun (typeName, entries) ->
        match entries with
        | [] -> Crash.crash $"ANF_to_MIR: variant group for type '{typeName}' had no variants"
        | (_, typeParams, _) :: rest ->
            let hasInconsistentTypeParams =
                rest |> List.exists (fun (_, otherTypeParams, _) -> otherTypeParams <> typeParams)

            if hasInconsistentTypeParams then
                Crash.crash $"ANF_to_MIR: inconsistent type parameters in variant registry for type: {typeName}"
            else
                let variants =
                    entries
                    |> List.map (fun (_, _, (name, tag, payload)) -> mkVariantInfo name tag payload)
                    |> List.sortBy (fun v -> v.Tag)
                (typeName, mkTypeVariants typeParams variants))
    |> Map.ofList

/// Build RecordRegistry from TypeReg
/// TypeReg: typeName -> (fieldName, fieldType) list
/// RecordRegistry: typeName -> RecordField list
let buildRecordRegistry (typeReg: Map<string, (string * AST.Type) list>) : MIR.RecordRegistry =
    typeReg
    |> Map.map (fun _typeName fields ->
        fields |> List.map (fun (name, typ) -> mkRecordField name typ))

/// Convert ANF.BinOp to MIR.BinOp
let convertBinOp (op: ANF.BinOp) : MIR.BinOp =
    match op with
    | ANF.Add -> MIR.Add
    | ANF.Sub -> MIR.Sub
    | ANF.Mul -> MIR.Mul
    | ANF.Div -> MIR.Div
    | ANF.Mod -> MIR.Mod
    | ANF.Shl -> MIR.Shl
    | ANF.Shr -> MIR.Shr
    | ANF.BitAnd -> MIR.BitAnd
    | ANF.BitOr -> MIR.BitOr
    | ANF.BitXor -> MIR.BitXor
    | ANF.Eq -> MIR.Eq
    | ANF.Neq -> MIR.Neq
    | ANF.Lt -> MIR.Lt
    | ANF.Gt -> MIR.Gt
    | ANF.Lte -> MIR.Lte
    | ANF.Gte -> MIR.Gte
    | ANF.And -> MIR.And
    | ANF.Or -> MIR.Or

/// Convert ANF.UnaryOp to MIR.UnaryOp
let convertUnaryOp (op: ANF.UnaryOp) : MIR.UnaryOp =
    match op with
    | ANF.Neg -> MIR.Neg
    | ANF.Not -> MIR.Not
    | ANF.BitNot -> MIR.BitNot

let convertCliOperation (operation: ANF.CliOperation) : MIR.CliOperation =
    match operation with
    | ANF.Execute -> MIR.Execute
    | ANF.RunProcess -> MIR.RunProcess
    | ANF.HostOS -> MIR.HostOS
    | ANF.HostArchitecture -> MIR.HostArchitecture
    | ANF.Hostname -> MIR.Hostname
    | ANF.GetEnv -> MIR.GetEnv
    | ANF.GetEnvironmentPacked -> MIR.GetEnvironmentPacked
    | ANF.SetEnv -> MIR.SetEnv
    | ANF.UnsetEnv -> MIR.UnsetEnv
    | ANF.DirectoryCurrent -> MIR.DirectoryCurrent
    | ANF.DirectoryListPacked -> MIR.DirectoryListPacked
    | ANF.FileIsDirectory -> MIR.FileIsDirectory
    | ANF.GetArgv -> MIR.GetArgv
    | ANF.Kill -> MIR.Kill
    | ANF.GetPid -> MIR.GetPid
    | ANF.GetUid -> MIR.GetUid
    | ANF.CpuCount -> MIR.CpuCount
    | ANF.SpawnProcess -> MIR.SpawnProcess
    | ANF.ProcessIO -> MIR.ProcessIO
    | ANF.TerminateProcess -> MIR.TerminateProcess

/// Precomputed descriptions for primitive ops (avoids formatting on hot path)
let private binOpDescription (op: ANF.BinOp) : string =
    match op with
    | ANF.Add -> "Prim Add"
    | ANF.Sub -> "Prim Sub"
    | ANF.Mul -> "Prim Mul"
    | ANF.Div -> "Prim Div"
    | ANF.Mod -> "Prim Mod"
    | ANF.Shl -> "Prim Shl"
    | ANF.Shr -> "Prim Shr"
    | ANF.BitAnd -> "Prim BitAnd"
    | ANF.BitOr -> "Prim BitOr"
    | ANF.BitXor -> "Prim BitXor"
    | ANF.Eq -> "Prim Eq"
    | ANF.Neq -> "Prim Neq"
    | ANF.Lt -> "Prim Lt"
    | ANF.Gt -> "Prim Gt"
    | ANF.Lte -> "Prim Lte"
    | ANF.Gte -> "Prim Gte"
    | ANF.And -> "Prim And"
    | ANF.Or -> "Prim Or"

/// Precomputed descriptions for unary ops (avoids formatting on hot path)
let private unaryOpDescription (op: ANF.UnaryOp) : string =
    match op with
    | ANF.Neg -> "UnaryPrim Neg"
    | ANF.Not -> "UnaryPrim Not"
    | ANF.BitNot -> "UnaryPrim BitNot"

/// Append a list of instructions (in order) to a reversed instruction list
let private appendInstrsRev (instrs: MIR.Instr list) (revInstrs: MIR.Instr list) : MIR.Instr list =
    (List.rev instrs) @ revInstrs

/// Build a dense type lookup array for TempIds up to maxId
let private buildTypeById (maxId: int) (typeMap: ANF.TypeMap) : AST.Type option array =
    if maxId < 0 then
        [||]
    else
        let arr = Array.create (maxId + 1) None
        typeMap
        |> Map.iter (fun (ANF.TempId id) typ ->
            if id >= 0 && id <= maxId then
                arr.[id] <- Some typ)
        arr

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Find the maximum TempId in an atom (returns -1 if no TempId)
let maxTempIdInAtom (atom: ANF.Atom) : int =
    match atom with
    | ANF.Var (ANF.TempId id) -> id
    | _ -> -1

/// Find the maximum TempId across atoms without allocating an intermediate list
let private maxTempIdInAtoms (atoms: ANF.Atom list) : int =
    atoms |> List.fold (fun maxId atom -> max maxId (maxTempIdInAtom atom)) -1

let private maxTempIdWithAtoms (first: ANF.Atom) (rest: ANF.Atom list) : int =
    max (maxTempIdInAtom first) (maxTempIdInAtoms rest)

/// Find the maximum TempId in a CExpr
let maxTempIdInCExpr (cexpr: ANF.CExpr) : int =
    match cexpr with
    | ANF.Atom atom -> maxTempIdInAtom atom
    | ANF.TypedAtom (atom, _) -> maxTempIdInAtom atom
    | ANF.Prim (_, left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.UnaryPrim (_, atom) -> maxTempIdInAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAtom thenVal) (maxTempIdInAtom elseVal))
    | ANF.Call (_, args)
    | ANF.BorrowedCall (_, args) ->
        maxTempIdInAtoms args
    | ANF.TailCall (_, args) ->
        maxTempIdInAtoms args
    | ANF.IndirectCall (func, args) ->
        maxTempIdWithAtoms func args
    | ANF.IndirectTailCall (func, args) ->
        maxTempIdWithAtoms func args
    | ANF.TupleAlloc atoms ->
        maxTempIdInAtoms atoms
    | ANF.TupleGet (tuple, _) -> maxTempIdInAtom tuple
    | ANF.RecordAlloc (_, fields) -> maxTempIdInAtoms fields
    | ANF.RecordGet (_, record, _) -> maxTempIdInAtom record
    | ANF.RecordClone (_, record, fields)
    | ANF.RecordReuse (_, _, record, fields) -> maxTempIdWithAtoms record fields
    | ANF.StringConcat (first, second, remaining) ->
        maxTempIdInAtoms (first :: second :: remaining)
    | ANF.CanonicalBufferEq (_, left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.RefCountInc (atom, _, _, _) -> maxTempIdInAtom atom
    | ANF.RefCountDec (atom, _, _, _) -> maxTempIdInAtom atom
    | ANF.Print (atom, _) -> maxTempIdInAtom atom
    | ANF.StdoutWrite (atom, _) -> maxTempIdInAtom atom
    | ANF.StdinReadLine -> -1
    | ANF.RuntimeError _ -> -1
    | ANF.RuntimeErrorString atom -> maxTempIdInAtom atom
    | ANF.ClosureAlloc (_, captures) ->
        maxTempIdInAtoms captures
    | ANF.ClosureCall (closure, args) ->
        maxTempIdWithAtoms closure args
    | ANF.ClosureTailCall (closure, args) ->
        maxTempIdWithAtoms closure args
    | ANF.FileReadBlob path -> maxTempIdInAtom path
    | ANF.FileExists path -> maxTempIdInAtom path
    | ANF.FileWriteBlob (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileAppendText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileDelete path -> maxTempIdInAtom path
    | ANF.FileCreateDirectory path -> maxTempIdInAtom path
    | ANF.FileSetExecutable path -> maxTempIdInAtom path
    | ANF.FileWriteFromPtr (path, ptr, length) -> max (maxTempIdInAtom path) (max (maxTempIdInAtom ptr) (maxTempIdInAtom length))
    | ANF.RawAlloc numBytes -> maxTempIdInAtom numBytes
    | ANF.MappedAlloc numBytes -> maxTempIdInAtom numBytes
    | ANF.RawFree ptr -> maxTempIdInAtom ptr
    | ANF.MappedFree ptr -> maxTempIdInAtom ptr
    | ANF.RawGet (ptr, offset, _)
    | ANF.RawTake (ptr, offset, _) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
    | ANF.RawGetByte (ptr, offset) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
    | ANF.RawWriteWord (ptr, offset, value) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.RawWriteByte (ptr, offset, value) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.RawSlotInit (ptr, offset, value, _) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.StringToRawPtr value -> maxTempIdInAtom value
    | ANF.RawPtrToString ptr -> maxTempIdInAtom ptr
    | ANF.BlobToRawPtr value -> maxTempIdInAtom value
    | ANF.RawPtrToBlob ptr -> maxTempIdInAtom ptr
    | ANF.RawPtrToInt128 ptr -> maxTempIdInAtom ptr
    | ANF.RawPtrToUInt128 ptr -> maxTempIdInAtom ptr
    | ANF.DictToRawPtr dict -> maxTempIdInAtom dict
    | ANF.RawPtrToDict (ptr, tag, _) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom tag)
    | ANF.ListToRawPtr list -> maxTempIdInAtom list
    | ANF.FixedBlockToRawPtr value -> maxTempIdInAtom value
    | ANF.RawPtrToList (ptr, tag, _) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom tag)
    | ANF.FloatSqrt atom -> maxTempIdInAtom atom
    | ANF.FloatAbs atom -> maxTempIdInAtom atom
    | ANF.FloatNeg atom -> maxTempIdInAtom atom
    | ANF.Int64ToFloat atom -> maxTempIdInAtom atom
    | ANF.FloatToInt64 atom -> maxTempIdInAtom atom
    | ANF.FloatToBits atom -> maxTempIdInAtom atom
    | ANF.RefCountIncString str -> maxTempIdInAtom str
    | ANF.RefCountDecString str -> maxTempIdInAtom str
    | ANF.RefCountIncBlob bytes -> maxTempIdInAtom bytes
    | ANF.RefCountDecBlob bytes -> maxTempIdInAtom bytes
    | ANF.RefCountIncInt value -> maxTempIdInAtom value
    | ANF.RefCountDecInt value -> maxTempIdInAtom value
    | ANF.RandomInt64 -> -1  // No atoms, so no TempIds
    | ANF.DateTimeNow -> -1      // No atoms, so no TempIds
    | ANF.Sleep delayMs -> maxTempIdInAtom delayMs
    | ANF.CliNative (_, args) -> maxTempIdInAtoms args
    | ANF.FloatToString atom -> maxTempIdInAtom atom

/// Find the maximum TempId in an AExpr
let rec maxTempIdInAExpr (expr: ANF.AExpr) : int =
    match expr with
    | ANF.Let (ANF.TempId id, cexpr, body) ->
        max id (max (maxTempIdInCExpr cexpr) (maxTempIdInAExpr body))
    | ANF.Return atom -> maxTempIdInAtom atom
    | ANF.Jump (ANF.TempId target, atom) -> max target (maxTempIdInAtom atom)
    | ANF.Join (parameter, continuation, entry) ->
        let (ANF.TempId id) = parameter.Id
        max id (max (maxTempIdInAExpr continuation) (maxTempIdInAExpr entry))
    | ANF.If (cond, thenBranch, elseBranch) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAExpr thenBranch) (maxTempIdInAExpr elseBranch))

/// Find the maximum TempId in a function
let maxTempIdInFunction (func: ANF.Function) : int =
    let paramMax =
        func.TypedParams
        |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
        |> List.fold max -1
    max paramMax (maxTempIdInAExpr func.Body)

/// Find the maximum TempId in an ANF program
let maxTempIdInProgram (program: ANF.Program) : int =
    let (ANF.Program (functions, mainExpr)) = program
    let funcMax =
        functions
        |> List.map maxTempIdInFunction
        |> List.fold max -1
    let mainMax = maxTempIdInAExpr mainExpr
    max funcMax mainMax

/// Helper to check if an atom is a float value
let isFloatAtom (floatRegs: Set<int>) (atom: ANF.Atom) : bool =
    match atom with
    | ANF.FloatLiteral _ -> true
    | ANF.Var (ANF.TempId id) -> Set.contains id floatRegs
    | _ -> false

/// Helper to check if a CExpr produces a float value
/// returnTypeReg: map from function name to return type (for checking Call results)
let cexprProducesFloat (floatRegs: Set<int>) (returnTypeReg: Map<AST.FunctionId, AST.Type>) (cexpr: ANF.CExpr) : bool =
    match cexpr with
    | ANF.Prim (op, left, right) ->
        // Comparisons and boolean ops always produce Bool, not Float
        match op with
        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
        | ANF.And | ANF.Or -> false
        // Arithmetic ops produce float if either operand is float
        | ANF.Add | ANF.Sub | ANF.Mul | ANF.Div | ANF.Mod
        | ANF.Shl | ANF.Shr | ANF.BitAnd | ANF.BitOr | ANF.BitXor ->
            isFloatAtom floatRegs left || isFloatAtom floatRegs right
    | ANF.FloatSqrt _ | ANF.FloatAbs _ | ANF.FloatNeg _ | ANF.Int64ToFloat _ -> true
    | ANF.Atom atom -> isFloatAtom floatRegs atom
    | ANF.IfValue (_, thenAtom, _) ->
        // IfValue produces a float if either branch produces a float
        // (then and else should have the same type, so we check then)
        isFloatAtom floatRegs thenAtom
    | ANF.Call (funcName, _)
    | ANF.BorrowedCall (funcName, _)
    | ANF.TailCall (funcName, _) ->
        // Check if the called function returns a float
        match Map.tryFind funcName returnTypeReg with
        | Some AST.TFloat64 -> true
        | _ -> false
    | ANF.IndirectCall _ | ANF.IndirectTailCall _ ->
        // Indirect calls - we don't know the return type, assume not float
        false
    | ANF.ClosureCall _ | ANF.ClosureTailCall _ ->
        // Closure calls - we don't know the return type, assume not float
        false
    | _ -> false

/// Build a map from function name to return type for all functions
/// ANF functions are already typed, so their declared return types are authoritative.
/// externalReturnTypes: return types for functions not in `functions` (e.g., specialized functions compiled elsewhere)
let buildReturnTypeReg
    (functions: ANF.Function list)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.Type>)
    : Map<AST.FunctionId, AST.Type> =
    let externalById =
        externalReturnTypes
        |> Map.map (fun _ (_, typ) -> typ)
    functions
    |> List.fold
        (fun returnTypes anfFunc -> Map.add anfFunc.Id anfFunc.ReturnType returnTypes)
        externalById

/// Return type for monomorphized intrinsics not tracked in the return type registry
let tryGetIntrinsicReturnType (funcName: string) : AST.Type option =
    if funcName = "Builtin.pmFindValuesByValueType" then
        Some (AST.TList (AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])))
    elif funcName = "Builtin.pmGetLocationsByValue" then
        Some (AST.TList (AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])))
    elif funcName.StartsWith("__raw_get_") then
        Crash.crash $"ANF_to_MIR: monomorphized raw_get return type missing from registry: {funcName}"
    elif funcName.StartsWith("__raw_take_") then
        Crash.crash $"ANF_to_MIR: monomorphized raw_take return type missing from registry: {funcName}"
    elif funcName.StartsWith("__stream_to_rawptr_") then Some AST.TRawPtr
    elif funcName.StartsWith("__raw_slot_init_") then Some AST.TUnit
    elif funcName.StartsWith("__hash_") then Some AST.TInt64
    elif funcName.StartsWith("__key_eq_") then Some AST.TBool
    elif funcName.StartsWith("__empty_dict_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__dict_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_to_rawptr_") then Some AST.TRawPtr
    elif funcName.StartsWith("__rawptr_to_dict_") then Some (AST.TDict (AST.TVar "k", AST.TVar "v"))
    elif funcName.StartsWith("__list_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__list_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__list_to_rawptr_") then Some AST.TRawPtr
    elif funcName.StartsWith("__rawptr_to_list_") then Some (AST.TList (AST.TVar "a"))
    else None

/// CFG builder state - includes lookups to avoid mutable module-level state
/// which would cause race conditions in parallel test execution
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    Joins: Map<ANF.TempId, MIR.Label * AST.Type>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
    TypeById: AST.Type option array
    // Fresh MIR registers start above this function's source TempIds and must use ExtraTypeMap.
    SourceTempIdMax: int
    ExtraTypeMap: Map<ANF.TempId, AST.Type>
    TypeReg: Map<string, (string * AST.Type) list>
    ReturnTypeReg: Map<AST.FunctionId, AST.Type>  // Function identity -> return type
    FunctionNames: Map<AST.FunctionId, string>
    FuncId: AST.FunctionId
    FuncName: string  // For generating unique labels per function
    ParamRegs: MIR.VReg list  // Parameter VRegs for self-recursive tail call loop optimization
    FloatRegs: Set<int>  // VReg IDs that hold float values
    ClosureFuncs: Map<ANF.TempId, AST.FunctionId>  // Closure temp -> function identity for return type lookup
    // Coverage support
    EnableCoverage: bool
    ExprIdGen: ANF.ExprIdGen
    CoverageMapping: ANF.CoverageMapping
}

/// Lookup a TempId by raw integer id, checking extra types for newly created regs
let private tryFindTypeById (builder: CFGBuilder) (id: int) : AST.Type option =
    match Map.tryFind (ANF.TempId id) builder.ExtraTypeMap with
    | Some typ -> Some typ
    | None when id >= 0 && id <= builder.SourceTempIdMax && id < builder.TypeById.Length ->
        builder.TypeById.[id]
    | None -> None

/// Lookup a TempId, checking extra types for newly created regs
let private tryFindType (builder: CFGBuilder) (tempId: ANF.TempId) : AST.Type option =
    let (ANF.TempId id) = tempId
    tryFindTypeById builder id

/// Convert ANF Atom to MIR Operand using lookups from builder
/// Returns Error if float/string lookup fails (internal invariant violation)
let atomToOperand (builder: CFGBuilder) (atom: ANF.Atom) : Result<MIR.Operand, string> =
    match atom with
    | ANF.UnitLiteral -> Ok (MIR.Int64Const 0L)  // Unit is represented as 0
    | ANF.IntLiteral n -> Ok (MIR.Int64Const (ANF.sizedIntToInt64 n))
    | ANF.BoolLiteral b -> Ok (MIR.BoolConst b)
    | ANF.FloatLiteral f -> Ok (MIR.FloatSymbol f)
    | ANF.StringLiteral s -> Ok (MIR.StringSymbol s)
    | ANF.Var tempId -> Ok (MIR.Register (tempToVReg tempId))
    | ANF.FuncRef funcName -> Ok (MIR.FuncAddr funcName)

let private rcKindToMIR (kind: MemoryModel.RcKind) : MIR.RcKind =
    match kind with
    | MemoryModel.GenericHeap -> MIR.GenericHeap
    | MemoryModel.StreamHeap -> MIR.StreamHeap
    | MemoryModel.TaggedList -> MIR.TaggedList
    | MemoryModel.DictHeap -> MIR.DictHeap
    | MemoryModel.ClosureHeap -> MIR.ClosureHeap

/// Get the type of an ANF Atom (for generating type-specific instructions)
let atomType (builder: CFGBuilder) (atom: ANF.Atom) : AST.Type =
    match atom with
    | ANF.UnitLiteral -> AST.TUnit
    | ANF.IntLiteral n -> ANF.sizedIntToType n  // Use the actual type from SizedInt
    | ANF.BoolLiteral _ -> AST.TBool
    | ANF.StringLiteral _ -> AST.TString
    | ANF.FloatLiteral _ -> AST.TFloat64
    | ANF.Var (ANF.TempId id) ->
        // Check if this VReg is known to hold a float
        let result =
            if Set.contains id builder.FloatRegs then AST.TFloat64
            else
                match tryFindTypeById builder id with
                | Some t -> t
                | None ->
                    // TypeMap is populated by RefCountInsertion. If we reach
                    // here, a later pass created a TempId without tracking it.
                    Crash.crash $"atomType: unknown type for TempId {id} - TempId created after RefCountInsertion?"
        result
    | ANF.FuncRef _ -> AST.TInt64  // Function addresses are pointer-sized

/// Get the operand type for a binary operation (checks both operands)
/// If either operand is float, the operation is float
let binOpType (builder: CFGBuilder) (leftAtom: ANF.Atom) (rightAtom: ANF.Atom) : AST.Type =
    let leftType = atomType builder leftAtom
    let rightType = atomType builder rightAtom
    match leftType, rightType with
    | AST.TFloat64, _ | _, AST.TFloat64 -> AST.TFloat64
    | _ -> leftType

/// Resolve the result type for a closure call.
/// A closure temp may still have its concrete allocation target in ClosureFuncs;
/// higher-order values passed through parameters or containers are resolved from
/// the already-required ANF result temp type.
let private closureCallReturnType (builder: CFGBuilder) (resultTempId: ANF.TempId) (closure: ANF.Atom) : AST.Type =
    let resultTempType () =
        match tryFindType builder resultTempId with
        | Some (AST.TFunction (_, retType)) -> retType
        | Some t -> t
        | None -> Crash.crash $"ClosureCall: Return type not found for {resultTempId}"

    match closure with
    | ANF.Var closureId ->
        match Map.tryFind closureId builder.ClosureFuncs with
        | Some funcName ->
            match Map.tryFind funcName builder.ReturnTypeReg with
            | Some t -> t
            | None -> resultTempType ()
        | None -> resultTempType ()
    | _ -> resultTempType ()

let private directCallReturnType (builder: CFGBuilder) (funcName: AST.FunctionId) : AST.Type =
    match Map.tryFind funcName builder.ReturnTypeReg with
    | Some t -> t
    | None ->
        let displayName = Map.tryFind funcName builder.FunctionNames
        match displayName |> Option.bind tryGetIntrinsicReturnType with
        | Some t -> t
        | None when displayName |> Option.exists (fun name -> name.StartsWith("__dark_eq_")) -> AST.TBool
        | None when displayName |> Option.exists (fun name -> name.StartsWith("Builtin.pmEvaluateValue_")) ->
            let name = displayName |> Option.defaultWith (fun () -> Crash.crash "Package evaluator identity lost its display name")
            let suffix = name.Substring("Builtin.pmEvaluateValue_".Length)
            let resultType =
                match suffix with
                | "i8" -> AST.TInt8
                | "i16" -> AST.TInt16
                | "i32" -> AST.TInt32
                | "i64" -> AST.TInt64
                | "i128" -> AST.TInt128
                | "int" -> AST.TInt
                | "u8" -> AST.TUInt8
                | "u16" -> AST.TUInt16
                | "u32" -> AST.TUInt32
                | "u64" -> AST.TUInt64
                | "u128" -> AST.TUInt128
                | "bool" -> AST.TBool
                | "f64" -> AST.TFloat64
                | "str" -> AST.TString
                | "blob" -> AST.TBlob
                | "char" -> AST.TChar
                | "datetime" -> AST.TDateTime
                | "unit" -> AST.TUnit
                | nominal when Map.containsKey nominal builder.TypeReg -> AST.TRecord (nominal, [])
                | nominal -> AST.TSum (nominal, [])
            AST.TSum ("Darklang.Stdlib.Option.Option", [resultType])
        | None ->
            let renderedName = displayName |> Option.defaultValue "<missing>"
            Crash.crash
                $"ANF_to_MIR: Return type not found for function identity {AST.functionIdValue funcName} ({renderedName})"

let private tupleGetDestType
    (builder: CFGBuilder)
    (tempId: ANF.TempId)
    (tupleGetAliasType: AST.Type option)
    (tupleId: ANF.TempId)
    (index: int)
    : AST.Type option =
    match tupleGetAliasType with
    | Some AST.TFloat64 -> Some AST.TFloat64
    | _ ->
        match tryFindType builder tempId with
        | Some AST.TFloat64 -> Some AST.TFloat64
        | _ ->
            match tryFindType builder tupleId with
            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                let elemType = List.item index elemTypes
                if elemType = AST.TFloat64 then Some AST.TFloat64 else None
            | Some (AST.TList elemType) ->
                // List is a Cons cell: (tag, head, tail) - index 1 is head
                if index = 1 && elemType = AST.TFloat64 then Some AST.TFloat64 else None
            | Some (AST.TFunction (_, AST.TList elemType)) ->
                // Function returning list - extract list element type.
                if index = 1 && elemType = AST.TFloat64 then Some AST.TFloat64 else None
            | Some (AST.TSum (_typeName, typeArgs)) ->
                // Sum type: [tag:8][payload:8], index 1 is payload.
                match index, typeArgs with
                | 1, [singleType] when singleType = AST.TFloat64 -> Some AST.TFloat64
                | _ -> None
            | _ -> None

let private inferSimpleCExprDestType
    (builder: CFGBuilder)
    (tempId: ANF.TempId)
    (tupleGetAliasType: AST.Type option)
    (cexpr: ANF.CExpr)
    : AST.Type option =
    match cexpr with
    | ANF.Atom atom -> Some (atomType builder atom)
    | ANF.TypedAtom (_, aType) -> Some aType
    | ANF.Prim (op, leftAtom, rightAtom) ->
        match op with
        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
        | ANF.And | ANF.Or -> Some AST.TBool
        | _ -> Some (binOpType builder leftAtom rightAtom)
    | ANF.CanonicalBufferEq _ -> Some AST.TBool
    | ANF.UnaryPrim (op, atom) ->
        match op with
        | ANF.Not -> Some AST.TBool
        | _ -> Some (atomType builder atom)
    | ANF.Call (funcName, _)
    | ANF.BorrowedCall (funcName, _) ->
        Some (directCallReturnType builder funcName)
    | ANF.IndirectCall (func, _) ->
        match atomType builder func with
        | AST.TFunction (_, retType) -> Some retType
        | AST.TRawPtr | AST.TInt64 -> Some AST.TBool
        | other -> Crash.crash $"IndirectCall: Expected TFunction type for func, got {other}"
    | ANF.ClosureCall (closure, _) -> Some (closureCallReturnType builder tempId closure)
    | ANF.TupleGet (ANF.Var tupleId, index) ->
        tupleGetDestType builder tempId tupleGetAliasType tupleId index
    | ANF.RecordAlloc (descriptor, _)
    | ANF.RecordClone (descriptor, _, _) ->
        Some descriptor.ValueType
    | ANF.RecordReuse (_, descriptor, _, _) ->
        Some descriptor.ValueType
    | ANF.RecordGet (descriptor, _, index) ->
        descriptor.Fields |> List.tryItem index |> Option.map snd
    | ANF.StringToRawPtr _
    | ANF.BlobToRawPtr _
    | ANF.DictToRawPtr _
    | ANF.ListToRawPtr _ -> Some AST.TRawPtr
    | ANF.FixedBlockToRawPtr _ -> Some AST.TRawPtr
    | ANF.RawPtrToString _ -> Some AST.TString
    | ANF.RawPtrToBlob _ -> Some AST.TBlob
    | ANF.RawPtrToInt128 _ -> Some AST.TInt128
    | ANF.RawPtrToUInt128 _ -> Some AST.TUInt128
    | ANF.RawPtrToDict (_, _, dictType) -> Some dictType
    | ANF.RawPtrToList (_, _, listType) -> Some listType
    | ANF.FloatSqrt _
    | ANF.FloatAbs _
    | ANF.FloatNeg _
    | ANF.Int64ToFloat _ -> Some AST.TFloat64
    | ANF.FloatToInt64 _ -> Some AST.TInt64
    | ANF.FloatToBits _ -> Some AST.TUInt64
    | _ -> None

/// Get the type of an MIR operand (for generating type-specific instructions)
let operandType (builder: CFGBuilder) (operand: MIR.Operand) : AST.Type =
    match operand with
    | MIR.Int64Const _ -> AST.TInt64
    | MIR.BoolConst _ -> AST.TBool
    | MIR.FloatSymbol _ -> AST.TFloat64
    | MIR.StringSymbol _ -> AST.TString
    | MIR.FuncAddr _ -> AST.TInt64  // Function addresses are pointer-sized
    | MIR.Register (MIR.VReg id) ->
        // Check if this VReg is known to hold a float or has a tracked type
        if Set.contains id builder.FloatRegs then AST.TFloat64
        else
            match tryFindTypeById builder id with
            | Some t -> t
            | None -> Crash.crash $"operandType: missing type for v{id}"

/// Generate description for a CExpr (for coverage mapping)
let cexprDescription (cexpr: ANF.CExpr) : string =
    match cexpr with
    | ANF.Atom _ -> "Atom"
    | ANF.TypedAtom _ -> "TypedAtom"
    | ANF.Prim (op, _, _) -> binOpDescription op
    | ANF.UnaryPrim (op, _) -> unaryOpDescription op
    | ANF.IfValue _ -> "IfValue"
    | ANF.Call (name, _) -> System.String.Concat("Call ", name)
    | ANF.BorrowedCall (name, _) -> System.String.Concat("BorrowedCall ", name)
    | ANF.TailCall (name, _) -> System.String.Concat("TailCall ", name)
    | ANF.IndirectCall _ -> "IndirectCall"
    | ANF.IndirectTailCall _ -> "IndirectTailCall"
    | ANF.ClosureAlloc (name, _) -> System.String.Concat("ClosureAlloc ", name)
    | ANF.ClosureCall _ -> "ClosureCall"
    | ANF.ClosureTailCall _ -> "ClosureTailCall"
    | ANF.TupleAlloc _ -> "TupleAlloc"
    | ANF.TupleGet _ -> "TupleGet"
    | ANF.RecordAlloc (descriptor, _) -> System.String.Concat("RecordAlloc ", descriptor.RuntimeTypeName)
    | ANF.RecordGet (descriptor, _, _) -> System.String.Concat("RecordGet ", descriptor.RuntimeTypeName)
    | ANF.RecordClone (descriptor, _, _) -> System.String.Concat("RecordClone ", descriptor.RuntimeTypeName)
    | ANF.RecordReuse (_, descriptor, _, _) -> System.String.Concat("RecordReuse ", descriptor.RuntimeTypeName)
    | ANF.StringConcat _ -> "StringConcat"
    | ANF.CanonicalBufferEq _ -> "CanonicalBufferEq"
    | ANF.RefCountInc _ -> "RefCountInc"
    | ANF.RefCountDec _ -> "RefCountDec"
    | ANF.Print _ -> "Print"
    | ANF.StdoutWrite _ -> "StdoutWrite"
    | ANF.StdinReadLine -> "StdinReadLine"
    | ANF.RuntimeError _ -> "RuntimeError"
    | ANF.RuntimeErrorString _ -> "RuntimeErrorString"
    | ANF.FileReadBlob _ -> "FileReadBlob"
    | ANF.FileExists _ -> "FileExists"
    | ANF.FileWriteBlob _ -> "FileWriteBlob"
    | ANF.FileAppendText _ -> "FileAppendText"
    | ANF.FileDelete _ -> "FileDelete"
    | ANF.FileCreateDirectory _ -> "FileCreateDirectory"
    | ANF.FileSetExecutable _ -> "FileSetExecutable"
    | ANF.FileWriteFromPtr _ -> "FileWriteFromPtr"
    | ANF.FloatSqrt _ -> "FloatSqrt"
    | ANF.FloatAbs _ -> "FloatAbs"
    | ANF.FloatNeg _ -> "FloatNeg"
    | ANF.Int64ToFloat _ -> "Int64ToFloat"
    | ANF.FloatToInt64 _ -> "FloatToInt64"
    | ANF.FloatToBits _ -> "FloatToBits"
    | ANF.RawAlloc _ -> "RawAlloc"
    | ANF.MappedAlloc _ -> "MappedAlloc"
    | ANF.RawFree _ -> "RawFree"
    | ANF.MappedFree _ -> "MappedFree"
    | ANF.RawGet _ -> "RawGet"
    | ANF.RawTake _ -> "RawTake"
    | ANF.RawGetByte _ -> "RawGetByte"
    | ANF.RawWriteWord _ -> "RawWriteWord"
    | ANF.RawWriteByte _ -> "RawWriteByte"
    | ANF.RawSlotInit _ -> "RawSlotInit"
    | ANF.StringToRawPtr _ -> "StringToRawPtr"
    | ANF.RawPtrToString _ -> "RawPtrToString"
    | ANF.BlobToRawPtr _ -> "BlobToRawPtr"
    | ANF.RawPtrToBlob _ -> "RawPtrToBlob"
    | ANF.RawPtrToInt128 _ -> "RawPtrToInt128"
    | ANF.RawPtrToUInt128 _ -> "RawPtrToUInt128"
    | ANF.DictToRawPtr _ -> "DictToRawPtr"
    | ANF.RawPtrToDict _ -> "RawPtrToDict"
    | ANF.ListToRawPtr _ -> "ListToRawPtr"
    | ANF.FixedBlockToRawPtr _ -> "FixedBlockToRawPtr"
    | ANF.RawPtrToList _ -> "RawPtrToList"
    | ANF.RefCountIncString _ -> "RefCountIncString"
    | ANF.RefCountDecString _ -> "RefCountDecString"
    | ANF.RefCountIncBlob _ -> "RefCountIncBlob"
    | ANF.RefCountDecBlob _ -> "RefCountDecBlob"
    | ANF.RefCountIncInt _ -> "RefCountIncInt"
    | ANF.RefCountDecInt _ -> "RefCountDecInt"
    | ANF.RandomInt64 -> "RandomInt64"
    | ANF.DateTimeNow -> "DateTimeNow"
    | ANF.Sleep _ -> "Sleep"
    | ANF.CliNative (operation, _) -> $"CliNative {operation}"
    | ANF.FloatToString _ -> "FloatToString"

/// Generate coverage instrumentation for an expression
/// Returns: (CoverageHit instruction option, updated builder with new ExprId)
let withCoverage (builder: CFGBuilder) (cexpr: ANF.CExpr) : MIR.Instr list * CFGBuilder =
    if builder.EnableCoverage then
        let (exprId, exprIdGen') = ANF.freshExprId builder.ExprIdGen
        let description = System.String.Concat(builder.FuncName, ": ", cexprDescription cexpr)
        let mapping' = ANF.addCoverageEntry exprId description builder.CoverageMapping
        let builder' = { builder with ExprIdGen = exprIdGen'; CoverageMapping = mapping' }
        ([MIR.CoverageHit exprId], builder')
    else
        ([], builder)

/// Collect cleanup operations that must run before a self-tailcall loop jump.
/// Expected shape after TailCallDetection:
///   Let(callTmp, TailCall(...), Let(_, RefCountDec..., ... Return(callTmp)))
let rec collectSelfTailCallCleanup
    (builder: CFGBuilder)
    (callTempId: ANF.TempId)
    (expr: ANF.AExpr)
    : Result<MIR.Instr list, string> =
    match expr with
    | ANF.Return (ANF.Var tid) when tid = callTempId ->
        Ok []
    | ANF.Let (_, ANF.RefCountDec (ANF.Var tid, payloadSize, kind, sourceType), rest) ->
        collectSelfTailCallCleanup builder callTempId rest
        |> Result.map (fun instrs -> MIR.RefCountDec (tempToVReg tid, payloadSize, rcKindToMIR kind, sourceType) :: instrs)
    | ANF.Let (_, ANF.RefCountDec (_, _, _, _), _) ->
        Error "Internal error: RefCountDec in self-tailcall cleanup on non-variable"
    | ANF.Let (_, ANF.RefCountDecString strAtom, rest) ->
        atomToOperand builder strAtom
        |> Result.bind (fun strOp ->
            collectSelfTailCallCleanup builder callTempId rest
            |> Result.map (fun instrs -> MIR.RefCountDecString strOp :: instrs))
    | ANF.Let (_, ANF.RefCountDecBlob bytesAtom, rest) ->
        atomToOperand builder bytesAtom
        |> Result.bind (fun bytesOp ->
            collectSelfTailCallCleanup builder callTempId rest
            |> Result.map (fun instrs -> MIR.RefCountDecBlob bytesOp :: instrs))
    | ANF.Let (_, ANF.RefCountDecInt valueAtom, rest) ->
        atomToOperand builder valueAtom
        |> Result.bind (fun valueOp ->
            collectSelfTailCallCleanup builder callTempId rest
            |> Result.map (fun instrs -> MIR.RefCountDecInt valueOp :: instrs))
    | _ ->
        Error $"Internal error: unexpected expression after self tailcall in {builder.FuncName}"

/// RC insertion places owned loop-state releases immediately before the call
/// so tail-call validation can account for them. Pull that contiguous suffix
/// back out before lowering: arguments must be captured, and overlaps retained,
/// before any obsolete state is released.
let collectPreSelfTailCallCleanup
    (instrsRev: MIR.Instr list)
    : MIR.Instr list * MIR.Instr list =
    let isCleanup instr =
        match instr with
        | MIR.RefCountDec _
        | MIR.RefCountDecString _
        | MIR.RefCountDecBlob _ -> true
        | _ -> false

    let rec loop cleanup remaining =
        match remaining with
        | instr :: rest when isCleanup instr -> loop (instr :: cleanup) rest
        | _ -> (cleanup, remaining)

    loop [] instrsRev

/// Transfer cleanup-owned edges that also occur in the next argument vector.
/// The first destination adopts the existing edge, so its decrement disappears;
/// only additional destinations require retains.
let transferOverlappingArgOwnership
    (argOperands: MIR.Operand list)
    (cleanupInstrs: MIR.Instr list)
    (existingInstrsRev: MIR.Instr list)
    : MIR.Instr list * MIR.Instr list =
    let decInfos =
        cleanupInstrs
        |> List.choose (fun instr ->
            match instr with
            | MIR.RefCountDec (vreg, payloadSize, kind, sourceType) -> Some (vreg, (payloadSize, kind, sourceType))
            | _ -> None)
        |> Map.ofList

    let aliasMap =
        existingInstrsRev
        |> List.fold (fun map instr ->
            match instr with
            | MIR.Mov (dest, MIR.Register src, _) ->
                Map.add dest src map
            | _ ->
                map)
            Map.empty

    let rec findCleanupTargetAlias
        (vreg: MIR.VReg)
        (visited: Set<MIR.VReg>)
        : (MIR.VReg * (int * MIR.RcKind * MemoryModel.RcMetadata option)) option =
        if Set.contains vreg visited then
            None
        else
            match Map.tryFind vreg decInfos with
            | Some decInfo ->
                Some (vreg, decInfo)
            | None ->
                match Map.tryFind vreg aliasMap with
                | Some next ->
                    findCleanupTargetAlias next (Set.add vreg visited)
                | None ->
                    None

    let overlapCounts =
        argOperands
        |> List.fold (fun counts argOp ->
            match argOp with
            | MIR.Register vreg ->
                match findCleanupTargetAlias vreg Set.empty with
                | Some (targetVReg, _) ->
                    counts
                    |> Map.change targetVReg (fun count ->
                        Some (Option.defaultValue 0 count + 1))
                | _ ->
                    counts
            | _ ->
                counts)
            Map.empty

    let overlapIncs =
        cleanupInstrs
        |> List.collect (fun instr ->
            match instr with
            | MIR.RefCountDec (vreg, payloadSize, kind, sourceType) ->
                let additionalEdges =
                    Map.tryFind vreg overlapCounts
                    |> Option.defaultValue 0
                    |> fun count -> max 0 (count - 1)
                List.replicate
                    additionalEdges
                    (MIR.RefCountInc (vreg, payloadSize, kind, sourceType))
            | _ -> [])

    let cleanupAfterTransfers =
        cleanupInstrs
        |> List.filter (fun instr ->
            match instr with
            | MIR.RefCountDec (vreg, _, _, _) ->
                not (Map.containsKey vreg overlapCounts)
            | _ -> true)

    (overlapIncs, cleanupAfterTransfers)

/// Only value-producing exits may be redirected into an enclosing value join.
/// A terminal transfer has no result register or patchable return block.
type ExprExit =
    | Returned of value: MIR.Operand * block: MIR.Label
    | Terminated

/// Redirect a value exit without inspecting a label's spelling or fabricating
/// an operand for a path that cannot reach the continuation.
let private redirectReturn resultReg joinLabel exit (builder: CFGBuilder) =
    match exit with
    | Terminated -> Ok builder
    | Returned (operand, label) ->
        match Map.tryFind label builder.Blocks with
        | Some ({ Terminator = MIR.Ret _ } as block) ->
            let operandType = operandType builder operand
            let redirected = {
                block with
                    Instrs = block.Instrs @ [MIR.Mov (resultReg, operand, Some operandType)]
                    Terminator = MIR.Jump joinLabel
            }
            Ok { builder with Blocks = Map.add label redirected builder.Blocks }
        | Some _ -> Error "ANF to MIR: value exit does not end in a return"
        | None -> Error "ANF to MIR: value exit block is missing"

/// Convert ANF sequencing to CFG once, whether at function scope or in a branch.
/// Returned blocks are complete CFG blocks; an enclosing join may redirect only
/// the returned exit. Terminal transfers are never patched.
let rec convertExpr
    (resultType: AST.Type)
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrsRev: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<ExprExit * CFGBuilder, string> =

    match expr with
    | ANF.Jump (target, value) ->
        match Map.tryFind target builder.Joins with
        | None -> Error $"ANF to MIR: jump target {target} is not in lexical scope"
        | Some (label, typ) ->
            atomToOperand builder value
            |> Result.map (fun operand ->
                let block = {
                    MIR.Label = currentLabel
                    MIR.Instrs = List.rev currentInstrsRev @ [MIR.Mov (tempToVReg target, operand, Some typ)]
                    MIR.Terminator = MIR.Jump label
                }
                Terminated, { builder with Blocks = Map.add currentLabel block builder.Blocks })
    | ANF.Join (parameter, continuation, entry) ->
        let label, labels = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
        let entryBuilder = {
            builder with
                LabelGen = labels
                Joins = Map.add parameter.Id (label, parameter.Type) builder.Joins
        }
        convertExpr resultType entry currentLabel currentInstrsRev entryBuilder
        |> Result.bind (fun (entryExit, afterEntry) ->
            match entryExit with
            | Returned _ -> Error "ANF to MIR: a join entry must transfer control, not return a function value"
            | Terminated ->
                let continuationBuilder = {
                    afterEntry with
                        Joins = builder.Joins
                        ExtraTypeMap = Map.add parameter.Id parameter.Type afterEntry.ExtraTypeMap
                }
                convertExpr resultType continuation label [] continuationBuilder)
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            let block = {
                MIR.Label = currentLabel
                MIR.Instrs = List.rev currentInstrsRev
                MIR.Terminator = MIR.Ret operand
            }
            let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
            Ok (Returned (operand, currentLabel), builder'))

    // Self-recursive tail call: emit arg capture + cleanup + param update + Jump to loop header
    // This must come before the general Let case to take precedence
    // Phi nodes carry type info, so this works for both int and float parameters.
    | ANF.Let (callTempId, ANF.TailCall (funcName, args), rest) when funcName = builder.FuncId ->
        collectSelfTailCallCleanup builder callTempId rest
        |> Result.bind (fun postCallCleanupInstrs ->
            let argTypes = args |> List.map (atomType builder)
            args
            |> List.map (atomToOperand builder)
            |> sequenceResults
            |> Result.bind (fun argOperands ->
                let loopLabel = MIR.Label $"{builder.FuncName}_body"
                // To handle register swaps correctly (e.g., swapInt(b, a, n-1)),
                // we need temps only when an argument directly references a parameter.
                //
                // Example where temps are needed: args = [b, a] for params [a, b]
                //   - Arg 0 is param b (VReg 1), needs capture before a is overwritten
                //   - Arg 1 is param a (VReg 0), needs capture before b is overwritten
                //
                // Example where temps are NOT needed: args = [n-1, acc+n] for params [n, acc]
                //   - Arg 0 is a computed temp (VReg 10xxx), not a direct param reference
                //   - Arg 1 is a computed temp (VReg 10xxx), not a direct param reference
                //
                // We only need temps if ANY argument is a direct param reference AND
                // that param will be written to by another assignment.
                let paramSet = builder.ParamRegs |> Set.ofList
                let argReferencesParam (op: MIR.Operand) =
                    match op with
                    | MIR.Register vreg -> Set.contains vreg paramSet
                    | _ -> false
                let needsTemps = argOperands |> List.exists argReferencesParam

                let (captureInstrs, assignInstrs, regGen') =
                    if needsTemps then
                        // Use temps to avoid swap issues
                        let (tempRegs, rg) =
                            argOperands
                            |> List.fold (fun (tempsRev, rg) _ ->
                                let (temp, rg') = MIR.freshReg rg
                                (temp :: tempsRev, rg')
                            ) ([], builder.RegGen)
                            |> fun (tempsRev, rg) -> (List.rev tempsRev, rg)
                        // First capture all arg values into temps
                        let captures =
                            List.zip3 tempRegs argOperands argTypes
                            |> List.map (fun (temp, argOp, argType) ->
                                MIR.Mov (temp, argOp, Some argType))
                        // Then assign temps to params
                        let assigns =
                            List.zip3 builder.ParamRegs tempRegs argTypes
                            |> List.map (fun (paramReg, temp, argType) ->
                                MIR.Mov (paramReg, MIR.Register temp, Some argType))
                        (captures, assigns, rg)
                    else
                        // No temps needed - just assign directly
                        let assigns =
                            List.zip3 builder.ParamRegs argOperands argTypes
                            |> List.map (fun (paramReg, argOp, argType) ->
                                MIR.Mov (paramReg, argOp, Some argType))
                        ([], assigns, builder.RegGen)

                let (preCallCleanupInstrs, instrsBeforeCleanupRev) =
                    collectPreSelfTailCallCleanup currentInstrsRev
                let cleanupBeforeTransfers =
                    preCallCleanupInstrs @ postCallCleanupInstrs
                let (overlapArgIncs, cleanupInstrs) =
                    transferOverlappingArgOwnership
                        argOperands
                        cleanupBeforeTransfers
                        instrsBeforeCleanupRev

                // Create block with accumulated instructions + arg capture + overlap incs + cleanup + param assignments + Jump
                let instrsRev =
                    instrsBeforeCleanupRev
                    |> appendInstrsRev captureInstrs
                    |> appendInstrsRev overlapArgIncs
                    |> appendInstrsRev cleanupInstrs
                    |> appendInstrsRev assignInstrs
                let block = {
                    MIR.Label = currentLabel
                    MIR.Instrs = List.rev instrsRev
                    MIR.Terminator = MIR.Jump loopLabel
                }
                let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks; RegGen = regGen' }
                Ok (Terminated, builder')))

    | ANF.Let (tempId, cexpr, rest) ->
        // Let binding: handle based on cexpr type
        let destReg = tempToVReg tempId
        let tupleGetAliasType =
            match cexpr, rest with
            | ANF.TupleGet _, ANF.Let (_, ANF.TypedAtom (ANF.Var sourceId, aliasType), _)
            | ANF.RecordGet _, ANF.Let (_, ANF.TypedAtom (ANF.Var sourceId, aliasType), _)
                when sourceId = tempId -> Some aliasType
            | _ -> None

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow blocks
            // 1. End current block with branch on condition
            // 2. Create then-block (assigns thenAtom to destReg, jumps to join)
            // 3. Create else-block (assigns elseAtom to destReg, jumps to join)
            // 4. Create join-block (continues with rest)

            // Add coverage instrumentation for the IfValue expression
            let (coverageInstrs, builderWithCoverage) = withCoverage builder cexpr

            atomToOperand builderWithCoverage condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builderWithCoverage thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builderWithCoverage elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName builderWithCoverage.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName labelGen2

                        // Current block ends with branch (after coverage hit)
                        let instrsRev = appendInstrsRev coverageInstrs currentInstrsRev
                        let currentBlock = {
                            MIR.Label = currentLabel
                            MIR.Instrs = List.rev instrsRev
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Determine the type of the if result (then/else should have same type)
                        let bindingType = atomType builderWithCoverage thenAtom

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp, Some bindingType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp, Some bindingType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        let builderWithBlocks = {
                            builderWithCoverage with
                                Blocks = builderWithCoverage.Blocks
                                         |> Map.add currentLabel currentBlock
                                         |> Map.add thenLabel thenBlock
                                         |> Map.add elseLabel elseBlock
                                LabelGen = labelGen3
                        }
                        let (ANF.TempId destId) = tempId
                        let builder' =
                            if bindingType = AST.TFloat64 then
                                { builderWithBlocks with
                                    FloatRegs = Set.add destId builderWithBlocks.FloatRegs }
                            else
                                builderWithBlocks

                        // Continue with rest in join block (no instructions yet)
                        convertExpr resultType rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: add instruction(s) to current block, continue
            // Track if dest is float type for later builder update
            let destType = inferSimpleCExprDestType builder tempId tupleGetAliasType cexpr
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    let aType = atomType builder atom
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.TypedAtom (atom, aType) ->
                    // Use the explicit type annotation (for pattern matching with correct types)
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    let opType = binOpType builder leftAtom rightAtom
                    // Comparison and boolean ops produce Bool, not the operand type
                    let resultType =
                        match op with
                        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
                        | ANF.And | ANF.Or -> AST.TBool
                        | _ -> opType
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp, opType)]))
                | ANF.UnaryPrim (op, atom) ->
                    let atomTy = atomType builder atom
                    let resultType =
                        match op with
                        | ANF.Not -> AST.TBool
                        | _ -> atomTy
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        match op with
                        | ANF.Not ->
                            [MIR.UnaryOp (destReg, convertUnaryOp op, operand)]
                        | ANF.Neg ->
                            // Use typed subtraction so sized integers are truncated correctly downstream.
                            [MIR.BinOp (destReg, MIR.Sub, MIR.Int64Const 0L, operand, atomTy)]
                        | ANF.BitNot ->
                            // x XOR -1 is equivalent to bitwise-not and preserves integer width via operandType.
                            [MIR.BinOp (destReg, MIR.BitXor, operand, MIR.Int64Const -1L, atomTy)])
                | ANF.Call (funcName, args)
                | ANF.BorrowedCall (funcName, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType = directCallReturnType builder funcName
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectCall (func, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | AST.TRawPtr | AST.TInt64 -> AST.TBool
                        | other -> Crash.crash $"IndirectCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectCall (destReg, funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureAlloc (funcName, captures) ->
                    // Allocate closure: (func_addr, cap1, cap2, ...)
                    let numSlots = 1 + List.length captures  // func_ptr + captures
                    let sizeBytes = numSlots * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store function pointer at offset 0 (always int/pointer type)
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName, None)
                    // Store captured values at offsets 8, 16, ... tracking value type for floats
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        let capType = atomType builder cap
                        let valueType = if capType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    let argTypes = args |> List.map (atomType builder)
                    let returnType = closureCallReturnType builder tempId closure
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands, argTypes, returnType)]))
                | ANF.TailCall (funcName, args) ->
                    // Non-self-recursive tail call (self-recursive handled specially above)
                    // Emits TailCall instruction with full epilogue + branch
                    let argTypes = args |> List.map (atomType builder)
                    let returnType = directCallReturnType builder funcName
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.TailCall (funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectTailCall (func, args) ->
                    // Indirect tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | AST.TRawPtr | AST.TInt64 -> AST.TBool
                        | other -> Crash.crash $"IndirectTailCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectTailCall (funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureTailCall (closure, args) ->
                    // Closure tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureTailCall (closureOp, argOperands, argTypes)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset, tracking value type for float handling
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        let elemType = atomType builder elem
                        let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        let loadType =
                            match destType with
                            | Some AST.TFloat64 -> Some AST.TFloat64
                            | _ -> None
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8, loadType)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.RecordAlloc (descriptor, fields)
                | ANF.RecordClone (descriptor, _, fields) ->
                    let allocInstr = MIR.HeapAlloc (destReg, List.length fields * 8)
                    fields
                    |> List.mapi (fun index field -> (index, field))
                    |> List.map (fun (index, field) ->
                        let fieldType = atomType builder field
                        let valueType = if fieldType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder field
                        |> Result.map (fun operand ->
                            MIR.HeapStore (destReg, index * 8, operand, valueType)))
                    |> sequenceResults
                    |> Result.map (fun stores -> allocInstr :: stores)
                | ANF.RecordReuse (_, descriptor, recordAtom, fields) ->
                    match recordAtom with
                    | ANF.Var sourceId ->
                        let recordType = descriptor.ValueType
                        fields
                        |> List.mapi (fun index field -> (index, field))
                        |> List.map (fun (index, field) ->
                            let fieldType = atomType builder field
                            let valueType = if fieldType = AST.TFloat64 then Some AST.TFloat64 else None
                            atomToOperand builder field
                            |> Result.map (fun operand ->
                                MIR.HeapStore (destReg, index * 8, operand, valueType)))
                        |> sequenceResults
                        |> Result.map (fun stores ->
                            MIR.Mov (destReg, MIR.Register (tempToVReg sourceId), Some recordType) :: stores)
                    | _ ->
                        Error "Internal error: Record reuse on non-variable (ANF invariant violated)"
                | ANF.RecordGet (_, recordAtom, index) ->
                    match recordAtom with
                    | ANF.Var tid ->
                        let recordReg = tempToVReg tid
                        let loadType =
                            match destType with
                            | Some AST.TFloat64 -> Some AST.TFloat64
                            | _ -> None
                        Ok [MIR.HeapLoad (destReg, recordReg, index * 8, loadType)]
                    | _ ->
                        Error "Internal error: Record access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize, kind, sourceType) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize, rcKindToMIR kind, sourceType)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize, kind, sourceType) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize, rcKindToMIR kind, sourceType)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.StdoutWrite (atom, appendNewline) ->
                    let (MIR.VReg effectId) = destReg
                    atomToOperand builder atom
                    |> Result.map (fun op ->
                        [ MIR.StdoutWrite (effectId, op, appendNewline)
                          MIR.Mov (destReg, MIR.Int64Const 0L, Some AST.TUnit) ])
                | ANF.StdinReadLine ->
                    Ok [MIR.StdinReadLine destReg]
                | ANF.RuntimeError message ->
                    Ok [MIR.RuntimeError message]
                | ANF.RuntimeErrorString message ->
                    atomToOperand builder message
                    |> Result.map (fun operand -> [MIR.RuntimeErrorString operand])
                | ANF.StringConcat (firstAtom, secondAtom, remainingAtoms) ->
                    ResultList.mapResults
                        (atomToOperand builder)
                        (firstAtom :: secondAtom :: remainingAtoms)
                    |> Result.map (function
                        | firstOp :: secondOp :: remainingOps ->
                            [MIR.StringConcat (destReg, firstOp, secondOp, remainingOps)]
                        | _ -> Crash.crash "StringConcat lost its required operands")
                | ANF.CanonicalBufferEq (kind, leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.CanonicalBufferEq (destReg, kind, leftOp, rightOp)]))
                | ANF.FileReadBlob pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadBlob (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteBlob (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteBlob (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))
                | ANF.FileDelete pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileDelete (destReg, pathOp)])
                | ANF.FileCreateDirectory pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileCreateDirectory (destReg, pathOp)])
                | ANF.FileSetExecutable pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileSetExecutable (destReg, pathOp)])
                | ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder ptrAtom
                        |> Result.bind (fun ptrOp ->
                            atomToOperand builder lengthAtom
                            |> Result.map (fun lengthOp ->
                                [MIR.FileWriteFromPtr (destReg, pathOp, ptrOp, lengthOp)])))
                | ANF.RawAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.RawAlloc (destReg, numBytesOp)])
                | ANF.MappedAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.MappedAlloc (destReg, numBytesOp)])
                | ANF.RawFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawFree ptrOp])
                | ANF.MappedFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.MappedFree ptrOp])
                | ANF.RawGet (ptrAtom, offsetAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp, valueType)]))
                | ANF.RawTake (ptrAtom, offsetAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp, valueType)]))
                | ANF.RawGetByte (ptrAtom, offsetAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGetByte (destReg, ptrOp, offsetOp)]))
                | ANF.RawWriteWord (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawWriteWord (ptrOp, offsetOp, valueOp)])))
                | ANF.RawWriteByte (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawWriteByte (ptrOp, offsetOp, valueOp)])))
                | ANF.RawSlotInit (ptrAtom, offsetAtom, valueAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSlotInit (ptrOp, offsetOp, valueOp, valueType)])))
                | ANF.StringToRawPtr valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.StringToRawPtr (destReg, valueOp)])
                | ANF.RawPtrToString ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawPtrToString (destReg, ptrOp)])
                | ANF.BlobToRawPtr valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.BlobToRawPtr (destReg, valueOp)])
                | ANF.RawPtrToBlob ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawPtrToBlob (destReg, ptrOp)])
                | ANF.RawPtrToInt128 ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.Mov (destReg, ptrOp, Some AST.TInt128)])
                | ANF.RawPtrToUInt128 ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.Mov (destReg, ptrOp, Some AST.TUInt128)])
                | ANF.DictToRawPtr dictAtom ->
                    atomToOperand builder dictAtom
                    |> Result.map (fun dictOp -> [MIR.DictToRawPtr (destReg, dictOp)])
                | ANF.RawPtrToDict (ptrAtom, tagAtom, _dictType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder tagAtom
                        |> Result.map (fun tagOp -> [MIR.RawPtrToDict (destReg, ptrOp, tagOp)]))
                | ANF.ListToRawPtr listAtom ->
                    atomToOperand builder listAtom
                    |> Result.map (fun listOp -> [MIR.ListToRawPtr (destReg, listOp)])
                | ANF.FixedBlockToRawPtr valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.Mov (destReg, valueOp, Some AST.TRawPtr)])
                | ANF.RawPtrToList (ptrAtom, tagAtom, _listType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder tagAtom
                        |> Result.map (fun tagOp -> [MIR.RawPtrToList (destReg, ptrOp, tagOp)]))
                | ANF.FloatSqrt atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatSqrt (destReg, op)])
                | ANF.FloatAbs atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatAbs (destReg, op)])
                | ANF.FloatNeg atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatNeg (destReg, op)])
                | ANF.Int64ToFloat atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Int64ToFloat (destReg, op)])
                | ANF.FloatToInt64 atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToInt64 (destReg, op)])
                | ANF.FloatToBits atom ->
                    // FloatToBits copies float bits to UInt64 (produces integer, not float)
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToBits (destReg, op)])
                | ANF.RefCountIncString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountIncString strOp])
                | ANF.RefCountDecString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountDecString strOp])
                | ANF.RefCountIncBlob bytesAtom ->
                    atomToOperand builder bytesAtom
                    |> Result.map (fun bytesOp -> [MIR.RefCountIncBlob bytesOp])
                | ANF.RefCountDecBlob bytesAtom ->
                    atomToOperand builder bytesAtom
                    |> Result.map (fun bytesOp -> [MIR.RefCountDecBlob bytesOp])
                | ANF.RefCountIncInt valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.RefCountIncInt valueOp])
                | ANF.RefCountDecInt valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.RefCountDecInt valueOp])
                | ANF.RandomInt64 ->
                    Ok [MIR.RandomInt64 destReg]
                | ANF.DateTimeNow ->
                    Ok [MIR.DateTimeNow destReg]
                | ANF.Sleep delayMs ->
                    let (MIR.VReg effectId) = destReg
                    atomToOperand builder delayMs
                    |> Result.map (fun delay -> [MIR.Sleep (effectId, destReg, delay)])
                | ANF.CliNative (operation, args) ->
                    ResultList.mapResults (atomToOperand builder) args
                    |> Result.map (fun operands -> [MIR.CliNative (destReg, convertCliOperation operation, operands)])
                | ANF.FloatToString valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.FloatToString (destReg, valueOp)])

            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                // Add coverage instrumentation if enabled
                let (coverageInstrs, builderWithCoverage) = withCoverage builder cexpr
                let builderWithClosure =
                    match cexpr with
                    | ANF.ClosureAlloc (funcName, _) ->
                        { builderWithCoverage with ClosureFuncs = Map.add tempId funcName builderWithCoverage.ClosureFuncs }
                    | _ -> builderWithCoverage
                let newInstrsRev = appendInstrsRev (coverageInstrs @ instrs) currentInstrsRev
                // Update FloatRegs if this dest is a float
                let (MIR.VReg destId) = destReg
                let builderWithType =
                    match destType with
                    | Some typ ->
                        { builderWithClosure with
                            ExtraTypeMap = Map.add tempId typ builderWithClosure.ExtraTypeMap }
                    | None -> builderWithClosure
                let builder' =
                    match destType with
                    | Some AST.TFloat64 ->
                        { builderWithType with FloatRegs = Set.add destId builderWithType.FloatRegs }
                    | _ ->
                        builderWithType
                convertExpr resultType rest currentLabel newInstrsRev builder'

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->
            let thenLabel, labelGen1 = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
            let elseLabel, labelGen2 = MIR.freshLabelWithPrefix builder.FuncName labelGen1
            let joinLabel, labelGen3 = MIR.freshLabelWithPrefix builder.FuncName labelGen2
            let resultReg, regGen = MIR.freshReg builder.RegGen
            let currentBlock = {
                MIR.Label = currentLabel
                MIR.Instrs = List.rev currentInstrsRev
                MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
            }
            let branched = {
                builder with
                    Blocks = Map.add currentLabel currentBlock builder.Blocks
                    LabelGen = labelGen3
                    RegGen = regGen
            }
            convertExpr resultType thenBranch thenLabel [] branched
            |> Result.bind (fun (thenExit, afterThen) ->
                convertExpr resultType elseBranch elseLabel [] afterThen
                |> Result.bind (fun (elseExit, afterElse) ->
                    match thenExit, elseExit with
                    | Terminated, Terminated -> Ok (Terminated, afterElse)
                    | _ ->
                        redirectReturn resultReg joinLabel thenExit afterElse
                        |> Result.bind (redirectReturn resultReg joinLabel elseExit)
                        |> Result.map (fun redirected ->
                            let result = MIR.Register resultReg
                            let joinBlock = {
                                MIR.Label = joinLabel
                                MIR.Instrs = []
                                MIR.Terminator = MIR.Ret result
                            }
                            let (MIR.VReg resultId) = resultReg
                            let joined = {
                                redirected with
                                    Blocks = Map.add joinLabel joinBlock redirected.Blocks
                                    ExtraTypeMap = Map.add (ANF.TempId resultId) resultType redirected.ExtraTypeMap
                                    FloatRegs =
                                        if resultType = AST.TFloat64 then Set.add resultId redirected.FloatRegs
                                        else redirected.FloatRegs
                            }
                            Returned (result, joinLabel), joined))))

/// Convert an ANF function to a MIR function
/// Each function gets its own RegGen starting from (maxTempId + 1) for deterministic VReg assignment.
/// This ensures the same function always produces identical MIR regardless of compilation context.
let convertANFFunction
    (anfFunc: ANF.Function)
    (typeMap: ANF.TypeMap)
    (typeById: AST.Type option array)
    (typeReg: Map<string, (string * AST.Type) list>)
    (returnTypeReg: Map<AST.FunctionId, AST.Type>)
    (functionNames: Map<AST.FunctionId, string>)
    (enableCoverage: bool)
    : Result<MIR.Function, string> =
    let convertCore () : Result<MIR.Function, string> =
        // Calculate RegGen for THIS function only
        // freshReg must generate VRegs that don't conflict with TempId-derived VRegs.
        // tempToVReg (TempId n) → VReg n, so freshReg must start past the max TempId used.
        let paramMax =
            anfFunc.TypedParams
            |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
            |> List.fold max -1

        // Initialize FloatRegs with float parameter IDs (types are now bundled in TypedParams)
        let floatParamIds =
            anfFunc.TypedParams
            |> List.filter (fun tp -> tp.Type = AST.TFloat64)
            |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
            |> Set.ofList

        // Convert ANF parameter TempIds to MIR VRegs
        // Must use tempToVReg to preserve the TempId values, not fresh VRegs,
        // because the body uses Var (TempId n) which converts to VReg n
        let paramVRegs = anfFunc.TypedParams |> List.map (fun tp -> tempToVReg tp.Id)

        // Get parameter types from TypedParams (types are now bundled)
        let paramTypes = anfFunc.TypedParams |> List.map (fun tp -> tp.Type)

        let bodyMaxId = maxTempIdInAExpr anfFunc.Body
        let maxId = max paramMax bodyMaxId
        let regGen = MIR.RegGen (maxId + 1)

        // Create initial builder
        let functionParamTypes =
            anfFunc.TypedParams
            |> List.fold (fun types param -> Map.add param.Id param.Type types) Map.empty

        let initialBuilder = {
            RegGen = regGen
            Joins = Map.empty
            LabelGen = MIR.initialLabelGen
            Blocks = Map.empty
            TypeById = typeById
            SourceTempIdMax = maxId
            ExtraTypeMap = functionParamTypes
            TypeReg = typeReg
            ReturnTypeReg = returnTypeReg
            FunctionNames = functionNames
            FuncId = anfFunc.Id
            FuncName = anfFunc.Name
            ParamRegs = paramVRegs  // For self-recursive tail call loop optimization
            FloatRegs = floatParamIds
            ClosureFuncs = Map.empty
            EnableCoverage = enableCoverage
            ExprIdGen = ANF.initialExprIdGen
            CoverageMapping = ANF.emptyCoverageMapping
        }

        // Create entry label for CFG (internal to function body)
        let entryLabel = MIR.Label $"{anfFunc.Name}_body"

        // Parameter retains at the start of ANF are one-time function setup.
        // Keep them in the true entry block so self-tailcall backedges enter
        // after setup rather than retaining the loop accumulator each iteration.
        let paramIds = anfFunc.TypedParams |> List.map (fun param -> param.Id) |> Set.ofList
        let rec splitLeadingParamRetains
            (expr: ANF.AExpr)
            : MIR.Instr list * ANF.AExpr =
            match expr with
            | ANF.Let (_, ANF.RefCountInc (ANF.Var tempId, payloadSize, kind, sourceType), body)
                when Set.contains tempId paramIds ->
                let (remainingRetains, loopBody) = splitLeadingParamRetains body
                (MIR.RefCountInc (tempToVReg tempId, payloadSize, rcKindToMIR kind, sourceType)
                 :: remainingRetains,
                 loopBody)
            | ANF.Let (_, ANF.RefCountIncString (ANF.Var tempId), body)
                when Set.contains tempId paramIds ->
                let (remainingRetains, loopBody) = splitLeadingParamRetains body
                (MIR.RefCountIncString (MIR.Register (tempToVReg tempId)) :: remainingRetains, loopBody)
            | ANF.Let (_, ANF.RefCountIncBlob (ANF.Var tempId), body)
                when Set.contains tempId paramIds ->
                let (remainingRetains, loopBody) = splitLeadingParamRetains body
                (MIR.RefCountIncBlob (MIR.Register (tempToVReg tempId)) :: remainingRetains, loopBody)
            | ANF.Let (_, ANF.RefCountIncInt (ANF.Var tempId), body)
                when Set.contains tempId paramIds ->
                let (remainingRetains, loopBody) = splitLeadingParamRetains body
                (MIR.RefCountIncInt (MIR.Register (tempToVReg tempId)) :: remainingRetains, loopBody)
            | _ ->
                ([], expr)
        let (entryRetains, loopBody) = splitLeadingParamRetains anfFunc.Body

        // For self-recursive functions, we need a separate entry block that jumps to the body.
        // This allows the body to be a proper loop header with two predecessors:
        // 1. The entry block (first call with initial param values)
        // 2. The recursive block (back-edge with updated param values)
        // This structure enables SSA to insert phi nodes at the loop header.
        let trueEntryLabel = MIR.Label $"{anfFunc.Name}_entry"
        let entryBlock = {
            MIR.Label = trueEntryLabel
            // Params are implicitly defined here by the calling convention.
            MIR.Instrs = entryRetains
            MIR.Terminator = MIR.Jump entryLabel
        }

        // Convert function body to CFG
        match convertExpr anfFunc.ReturnType loopBody entryLabel [] initialBuilder with
        | Error err -> Error err
        | Ok (_, finalBuilder) ->

        // Add the entry block to the CFG
        let allBlocks = Map.add trueEntryLabel entryBlock finalBuilder.Blocks

        let cfg = {
            MIR.Entry = trueEntryLabel
            MIR.Blocks = allBlocks
        }

        // Create TypedMIRParams by zipping VRegs with types
        let typedMIRParams : MIR.TypedMIRParam list =
            List.zip paramVRegs paramTypes
            |> List.map (fun (reg, typ) -> { Reg = reg; Type = typ })

        let mirFunc = {
            MIR.Id = anfFunc.Id
            MIR.Name = anfFunc.Name
            MIR.TypedParams = typedMIRParams
            MIR.ReturnType = anfFunc.ReturnType
            MIR.CFG = cfg
            MIR.FloatRegs = finalBuilder.FloatRegs
        }

        Ok mirFunc

    convertCore ()

/// Convert ANF program to MIR program
/// mainExprType: the type of the main expression (used for _start's return type)
/// variantLookup: mapping from variant names to type info (for enum printing)
/// typeReg: mapping from record type names to field info (for record printing, converted to RecordRegistry)
/// externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
/// Each function gets its own RegGen for deterministic VReg assignment.
let toMIR
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (mainExprType: AST.Type)
    (variantLookup: LoweringPrimitives.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.Type>)
    : Result<MIR.Program, string> =
    let (ANF.Program (functions, mainExpr)) = program
    // TypeMap spans the whole program, so materialize its dense lookup once and
    // retain a per-function source bound when sharing it with each CFG builder.
    let typeById = buildTypeById (maxTempIdInProgram program) typeMap

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeReg = buildReturnTypeReg functions externalReturnTypes
    let functionNames =
        (externalReturnTypes |> Map.toList)
        |> List.map (fun (id, (name, _)) -> id, name)
        |> List.append (functions |> List.map (fun func -> func.Id, func.Name))
        |> Map.ofList
    let startId =
        functionNames
        |> Map.toSeq
        |> Seq.tryPick (fun (id, name) -> if name = "_start" then Some id else None)
        |> Option.defaultValue (AST.functionId 0)
    // Phase 2: Convert all functions to MIR
    // Each function gets its own RegGen starting from (maxTempId + 1) for deterministic compilation
    match
        mapResults
            (fun anfFunc -> convertANFFunction anfFunc typeMap typeById typeReg returnTypeReg functionNames enableCoverage)
            functions
    with
    | Error err -> Error err
    | Ok mirFuncs ->

    // Convert main expression to a synthetic "_start" function
    // _start gets its own RegGen based on the main expression's TempIds
    let startMaxId = maxTempIdInAExpr mainExpr
    let startRegGen = MIR.RegGen (startMaxId + 1)
    let entryLabel = MIR.Label "_start_body"
    let initialBuilder = {
        RegGen = startRegGen
        Joins = Map.empty
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        TypeById = typeById
        SourceTempIdMax = startMaxId
        ExtraTypeMap = Map.empty
        TypeReg = typeReg
        ReturnTypeReg = returnTypeReg
        FunctionNames = functionNames
        FuncId = startId
        FuncName = "_start"
        ParamRegs = []  // _start has no params
        FloatRegs = Set.empty
        ClosureFuncs = Map.empty
        EnableCoverage = enableCoverage
        ExprIdGen = ANF.initialExprIdGen
        CoverageMapping = ANF.emptyCoverageMapping
    }
    match convertExpr mainExprType mainExpr entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->
    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }
    // Use the passed mainExprType for _start's return type
    // This is needed for proper float handling in the Ret terminator
    let startFunc = {
        MIR.Id = startId
        MIR.Name = "_start"
        MIR.TypedParams = []
        MIR.ReturnType = mainExprType
        MIR.CFG = cfg
        MIR.FloatRegs = finalBuilder.FloatRegs
    }
    let allFuncs = mirFuncs @ [startFunc]
    let variantRegistry = buildVariantRegistry variantLookup
    // Build recordRegistry from typeRegForRecords (converts tuples to RecordField records)
    let recordRegistry = buildRecordRegistry typeRegForRecords
    Ok (MIR.Program (allFuncs, variantRegistry, recordRegistry))

/// Convert ANF program to MIR (functions only, no _start)
/// Use for stdlib where there's no real main expression to convert.
/// Returns just the function list, variant registry, and record registry without wrapping in MIR.Program.
/// externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
/// Each function gets its own RegGen for deterministic VReg assignment.
let private toMIRFunctionsOnlyInternal
    (phaseRecorder: (string -> float -> unit) option)
    (projectedRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (variantLookup: LoweringPrimitives.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (knownFunctionNames: Map<AST.FunctionId, string>)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.Type>)
    : Result<MIR.Function list * MIR.VariantRegistry * MIR.RecordRegistry, string> =
    let startPhase () =
        phaseRecorder |> Option.map (fun _ -> System.Diagnostics.Stopwatch.StartNew())
    let recordPhase name timer =
        match phaseRecorder, timer with
        | Some record, Some (timer: System.Diagnostics.Stopwatch) ->
            timer.Stop()
            record name timer.Elapsed.TotalMilliseconds
        | _ -> ()

    let (ANF.Program (functions, _mainExpr)) = program
    // Avoid rescanning the global TypeMap for every function conversion.
    let typeLookupTimer = startPhase ()
    let typeById = buildTypeById (maxTempIdInProgram program) typeMap
    recordPhase "ANF -> MIR Type Lookup Preparation" typeLookupTimer

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeTimer = startPhase ()
    let returnTypeReg = buildReturnTypeReg functions externalReturnTypes
    let functionNames =
        (externalReturnTypes |> Map.toList)
        |> List.map (fun (id, (name, _)) -> id, name)
        |> List.append (functions |> List.map (fun func -> func.Id, func.Name))
        |> List.fold (fun names (id, name) -> Map.add id name names) knownFunctionNames
    recordPhase "ANF -> MIR Return Type Preparation" returnTypeTimer

    // Phase 2: Convert all functions to MIR (skip main/_start)
    // Each function gets its own RegGen starting from (maxTempId + 1) for deterministic compilation
    let conversionTimer = startPhase ()
    match
        mapResults
            (fun anfFunc -> convertANFFunction anfFunc typeMap typeById typeReg returnTypeReg functionNames enableCoverage)
            functions
    with
    | Error err -> Error err
    | Ok mirFuncs ->
        recordPhase "ANF -> MIR Function Conversion" conversionTimer
        let registryTimer = startPhase ()
        let variantRegistry, recordRegistry =
            match projectedRegistries with
            | Some registries -> registries
            | None ->
                (buildVariantRegistry variantLookup,
                 buildRecordRegistry typeRegForRecords)
        recordPhase "ANF -> MIR Registry Projection" registryTimer
        Ok (mirFuncs, variantRegistry, recordRegistry)

let toMIRFunctionsOnly
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (variantLookup: LoweringPrimitives.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.Type>)
    : Result<MIR.Function list * MIR.VariantRegistry * MIR.RecordRegistry, string> =
    toMIRFunctionsOnlyInternal
        None
        None
        program
        typeMap
        typeReg
        variantLookup
        typeRegForRecords
        enableCoverage
        Map.empty
        externalReturnTypes

let toMIRFunctionsOnlyWithTrace
    (phaseRecorder: (string -> float -> unit) option)
    (projectedRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (variantLookup: LoweringPrimitives.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (functionNames: Map<AST.FunctionId, string>)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.Type>)
    : Result<MIR.Function list * MIR.VariantRegistry * MIR.RecordRegistry, string> =
    toMIRFunctionsOnlyInternal
        phaseRecorder
        projectedRegistries
        program
        typeMap
        typeReg
        variantLookup
        typeRegForRecords
        enableCoverage
        functionNames
        externalReturnTypes
