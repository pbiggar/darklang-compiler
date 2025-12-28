// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
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

/// Build VariantRegistry from VariantLookup
/// VariantLookup: variantName -> (typeName, typeParams, tagIndex, payloadType)
/// VariantRegistry: typeName -> list of (variantName, tagIndex, payloadType)
let buildVariantRegistry (variantLookup: AST_to_ANF.VariantLookup) : MIR.VariantRegistry =
    variantLookup
    |> Map.toList
    |> List.map (fun (variantName, (typeName, _typeParams, tagIndex, payloadType)) ->
        (typeName, (variantName, tagIndex, payloadType)))
    |> List.groupBy fst
    |> List.map (fun (typeName, entries) ->
        let variants = entries |> List.map snd |> List.sortBy (fun (_, tag, _) -> tag)
        (typeName, variants))
    |> Map.ofList

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

/// Sequence a list of Results into a Result of list
let sequenceResults (results: Result<'a, string> list) : Result<'a list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | r :: rest ->
            match r with
            | Ok v -> loop (v :: acc) rest
            | Error e -> Error e
    loop [] results

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Find the maximum TempId in an atom (returns -1 if no TempId)
let maxTempIdInAtom (atom: ANF.Atom) : int =
    match atom with
    | ANF.Var (ANF.TempId id) -> id
    | _ -> -1

/// Find the maximum TempId in a CExpr
let maxTempIdInCExpr (cexpr: ANF.CExpr) : int =
    match cexpr with
    | ANF.Atom atom -> maxTempIdInAtom atom
    | ANF.Prim (_, left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.UnaryPrim (_, atom) -> maxTempIdInAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAtom thenVal) (maxTempIdInAtom elseVal))
    | ANF.Call (_, args) ->
        args |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.IndirectCall (func, args) ->
        max (maxTempIdInAtom func) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.TupleAlloc atoms ->
        atoms |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.TupleGet (tuple, _) -> maxTempIdInAtom tuple
    | ANF.StringConcat (left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.RefCountInc (atom, _) -> maxTempIdInAtom atom
    | ANF.RefCountDec (atom, _) -> maxTempIdInAtom atom
    | ANF.Print (atom, _) -> maxTempIdInAtom atom
    | ANF.ClosureAlloc (_, captures) ->
        captures |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.ClosureCall (closure, args) ->
        max (maxTempIdInAtom closure) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.FileReadText path -> maxTempIdInAtom path
    | ANF.FileExists path -> maxTempIdInAtom path
    | ANF.FileWriteText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileAppendText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileDelete path -> maxTempIdInAtom path
    | ANF.FileSetExecutable path -> maxTempIdInAtom path
    | ANF.RawAlloc numBytes -> maxTempIdInAtom numBytes
    | ANF.RawFree ptr -> maxTempIdInAtom ptr
    | ANF.RawGet (ptr, offset) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
    | ANF.RawSet (ptr, offset, value) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.FloatSqrt atom -> maxTempIdInAtom atom
    | ANF.FloatAbs atom -> maxTempIdInAtom atom
    | ANF.FloatNeg atom -> maxTempIdInAtom atom
    | ANF.IntToFloat atom -> maxTempIdInAtom atom
    | ANF.FloatToInt atom -> maxTempIdInAtom atom
    | ANF.StringHash str -> maxTempIdInAtom str
    | ANF.StringEq (left, right) -> max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.RefCountIncString str -> maxTempIdInAtom str
    | ANF.RefCountDecString str -> maxTempIdInAtom str

/// Find the maximum TempId in an AExpr
let rec maxTempIdInAExpr (expr: ANF.AExpr) : int =
    match expr with
    | ANF.Let (ANF.TempId id, cexpr, body) ->
        max id (max (maxTempIdInCExpr cexpr) (maxTempIdInAExpr body))
    | ANF.Return atom -> maxTempIdInAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAExpr thenBranch) (maxTempIdInAExpr elseBranch))

/// Find the maximum TempId in a function
let maxTempIdInFunction (func: ANF.Function) : int =
    let paramMax =
        func.Params
        |> List.map (fun (ANF.TempId id) -> id)
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

/// Collect all string literals from an ANF atom
let collectStringsFromAtom (atom: ANF.Atom) : string list =
    match atom with
    | ANF.StringLiteral s -> [s]
    | _ -> []

/// Collect all float literals from an ANF atom
let collectFloatsFromAtom (atom: ANF.Atom) : float list =
    match atom with
    | ANF.FloatLiteral f -> [f]
    | _ -> []

/// Collect all string literals from a CExpr
let collectStringsFromCExpr (cexpr: ANF.CExpr) : string list =
    match cexpr with
    | ANF.Atom atom -> collectStringsFromAtom atom
    | ANF.Prim (_, left, right) ->
        collectStringsFromAtom left @ collectStringsFromAtom right
    | ANF.UnaryPrim (_, atom) -> collectStringsFromAtom atom
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        collectStringsFromAtom cond @
        collectStringsFromAtom thenAtom @
        collectStringsFromAtom elseAtom
    | ANF.Call (_, args) ->
        args |> List.collect collectStringsFromAtom
    | ANF.IndirectCall (func, args) ->
        collectStringsFromAtom func @ (args |> List.collect collectStringsFromAtom)
    | ANF.TupleAlloc elems ->
        elems |> List.collect collectStringsFromAtom
    | ANF.TupleGet (tupleAtom, _) ->
        collectStringsFromAtom tupleAtom
    | ANF.StringConcat (left, right) ->
        collectStringsFromAtom left @ collectStringsFromAtom right
    | ANF.RefCountInc (atom, _) -> collectStringsFromAtom atom
    | ANF.RefCountDec (atom, _) -> collectStringsFromAtom atom
    | ANF.Print (atom, _) -> collectStringsFromAtom atom
    | ANF.ClosureAlloc (_, captures) -> captures |> List.collect collectStringsFromAtom
    | ANF.ClosureCall (closure, args) ->
        collectStringsFromAtom closure @ (args |> List.collect collectStringsFromAtom)
    | ANF.FileReadText path -> collectStringsFromAtom path
    | ANF.FileExists path -> collectStringsFromAtom path
    | ANF.FileWriteText (path, content) -> collectStringsFromAtom path @ collectStringsFromAtom content
    | ANF.FileAppendText (path, content) -> collectStringsFromAtom path @ collectStringsFromAtom content
    | ANF.FileDelete path -> collectStringsFromAtom path
    | ANF.FileSetExecutable path -> collectStringsFromAtom path
    | ANF.RawAlloc numBytes -> collectStringsFromAtom numBytes
    | ANF.RawFree ptr -> collectStringsFromAtom ptr
    | ANF.RawGet (ptr, offset) -> collectStringsFromAtom ptr @ collectStringsFromAtom offset
    | ANF.RawSet (ptr, offset, value) -> collectStringsFromAtom ptr @ collectStringsFromAtom offset @ collectStringsFromAtom value
    | ANF.FloatSqrt atom -> collectStringsFromAtom atom
    | ANF.FloatAbs atom -> collectStringsFromAtom atom
    | ANF.FloatNeg atom -> collectStringsFromAtom atom
    | ANF.IntToFloat atom -> collectStringsFromAtom atom
    | ANF.FloatToInt atom -> collectStringsFromAtom atom
    | ANF.StringHash str -> collectStringsFromAtom str
    | ANF.StringEq (left, right) -> collectStringsFromAtom left @ collectStringsFromAtom right
    | ANF.RefCountIncString str -> collectStringsFromAtom str
    | ANF.RefCountDecString str -> collectStringsFromAtom str

/// Collect all float literals from a CExpr
let collectFloatsFromCExpr (cexpr: ANF.CExpr) : float list =
    match cexpr with
    | ANF.Atom atom -> collectFloatsFromAtom atom
    | ANF.Prim (_, left, right) ->
        collectFloatsFromAtom left @ collectFloatsFromAtom right
    | ANF.UnaryPrim (_, atom) -> collectFloatsFromAtom atom
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        collectFloatsFromAtom cond @
        collectFloatsFromAtom thenAtom @
        collectFloatsFromAtom elseAtom
    | ANF.Call (_, args) ->
        args |> List.collect collectFloatsFromAtom
    | ANF.IndirectCall (func, args) ->
        collectFloatsFromAtom func @ (args |> List.collect collectFloatsFromAtom)
    | ANF.TupleAlloc elems ->
        elems |> List.collect collectFloatsFromAtom
    | ANF.TupleGet (tupleAtom, _) ->
        collectFloatsFromAtom tupleAtom
    | ANF.StringConcat (left, right) ->
        collectFloatsFromAtom left @ collectFloatsFromAtom right
    | ANF.RefCountInc (atom, _) -> collectFloatsFromAtom atom
    | ANF.RefCountDec (atom, _) -> collectFloatsFromAtom atom
    | ANF.Print (atom, _) -> collectFloatsFromAtom atom
    | ANF.ClosureAlloc (_, captures) -> captures |> List.collect collectFloatsFromAtom
    | ANF.ClosureCall (closure, args) ->
        collectFloatsFromAtom closure @ (args |> List.collect collectFloatsFromAtom)
    | ANF.FileReadText path -> collectFloatsFromAtom path
    | ANF.FileExists path -> collectFloatsFromAtom path
    | ANF.FileWriteText (path, content) -> collectFloatsFromAtom path @ collectFloatsFromAtom content
    | ANF.FileAppendText (path, content) -> collectFloatsFromAtom path @ collectFloatsFromAtom content
    | ANF.FileDelete path -> collectFloatsFromAtom path
    | ANF.FileSetExecutable path -> collectFloatsFromAtom path
    | ANF.RawAlloc numBytes -> collectFloatsFromAtom numBytes
    | ANF.RawFree ptr -> collectFloatsFromAtom ptr
    | ANF.RawGet (ptr, offset) -> collectFloatsFromAtom ptr @ collectFloatsFromAtom offset
    | ANF.RawSet (ptr, offset, value) -> collectFloatsFromAtom ptr @ collectFloatsFromAtom offset @ collectFloatsFromAtom value
    | ANF.FloatSqrt atom -> collectFloatsFromAtom atom
    | ANF.FloatAbs atom -> collectFloatsFromAtom atom
    | ANF.FloatNeg atom -> collectFloatsFromAtom atom
    | ANF.IntToFloat atom -> collectFloatsFromAtom atom
    | ANF.FloatToInt atom -> collectFloatsFromAtom atom
    | ANF.StringHash str -> collectFloatsFromAtom str
    | ANF.StringEq (left, right) -> collectFloatsFromAtom left @ collectFloatsFromAtom right
    | ANF.RefCountIncString str -> collectFloatsFromAtom str
    | ANF.RefCountDecString str -> collectFloatsFromAtom str

/// Collect all string literals from an ANF expression
let rec collectStringsFromExpr (expr: ANF.AExpr) : string list =
    match expr with
    | ANF.Return atom -> collectStringsFromAtom atom
    | ANF.Let (_, cexpr, rest) ->
        collectStringsFromCExpr cexpr @ collectStringsFromExpr rest
    | ANF.If (cond, thenBranch, elseBranch) ->
        collectStringsFromAtom cond @
        collectStringsFromExpr thenBranch @
        collectStringsFromExpr elseBranch

/// Collect all float literals from an ANF expression
let rec collectFloatsFromExpr (expr: ANF.AExpr) : float list =
    match expr with
    | ANF.Return atom -> collectFloatsFromAtom atom
    | ANF.Let (_, cexpr, rest) ->
        collectFloatsFromCExpr cexpr @ collectFloatsFromExpr rest
    | ANF.If (cond, thenBranch, elseBranch) ->
        collectFloatsFromAtom cond @
        collectFloatsFromExpr thenBranch @
        collectFloatsFromExpr elseBranch

/// Collect all string literals from an ANF function
let collectStringsFromFunction (func: ANF.Function) : string list =
    collectStringsFromExpr func.Body

/// Collect all float literals from an ANF function
let collectFloatsFromFunction (func: ANF.Function) : float list =
    collectFloatsFromExpr func.Body

/// Collect all string literals from an ANF program
let collectStringsFromProgram (program: ANF.Program) : string list =
    let (ANF.Program (functions, mainExpr)) = program
    let funcStrings = functions |> List.collect collectStringsFromFunction
    let mainStrings = collectStringsFromExpr mainExpr
    funcStrings @ mainStrings

/// Collect all float literals from an ANF program
let collectFloatsFromProgram (program: ANF.Program) : float list =
    let (ANF.Program (functions, mainExpr)) = program
    let funcFloats = functions |> List.collect collectFloatsFromFunction
    let mainFloats = collectFloatsFromExpr mainExpr
    funcFloats @ mainFloats

/// Build a string pool from a list of strings (deduplicates)
let buildStringPool (strings: string list) : MIR.StringPool =
    strings
    |> List.fold (fun pool s ->
        let (_, pool') = MIR.addString pool s
        pool') MIR.emptyStringPool

/// Build a float pool from a list of floats (deduplicates)
let buildFloatPool (floats: float list) : MIR.FloatPool =
    floats
    |> List.fold (fun pool f ->
        let (_, pool') = MIR.addFloat pool f
        pool') MIR.emptyFloatPool

/// Build a lookup map from string content to pool index
let buildStringLookup (pool: MIR.StringPool) : Map<string, int> =
    pool.Strings
    |> Map.fold (fun lookup idx (s, _) -> Map.add s idx lookup) Map.empty

/// Build a lookup map from float value to pool index
let buildFloatLookup (pool: MIR.FloatPool) : Map<float, int> =
    pool.Floats
    |> Map.fold (fun lookup idx f -> Map.add f idx lookup) Map.empty

/// Helper to check if an atom is a float value
let isFloatAtom (floatRegs: Set<int>) (atom: ANF.Atom) : bool =
    match atom with
    | ANF.FloatLiteral _ -> true
    | ANF.Var (ANF.TempId id) -> Set.contains id floatRegs
    | _ -> false

/// Helper to check if a CExpr produces a float value
/// returnTypeReg: map from function name to return type (for checking Call results)
let cexprProducesFloat (floatRegs: Set<int>) (returnTypeReg: Map<string, AST.Type>) (cexpr: ANF.CExpr) : bool =
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
    | ANF.FloatSqrt _ | ANF.FloatAbs _ | ANF.FloatNeg _ | ANF.IntToFloat _ -> true
    | ANF.Atom atom -> isFloatAtom floatRegs atom
    | ANF.IfValue (_, thenAtom, _) ->
        // IfValue produces a float if either branch produces a float
        // (then and else should have the same type, so we check then)
        isFloatAtom floatRegs thenAtom
    | ANF.Call (funcName, _) ->
        // Check if the called function returns a float
        match Map.tryFind funcName returnTypeReg with
        | Some AST.TFloat64 -> true
        | _ -> false
    | _ -> false

/// Analyze return statements in an ANF expression, tracking float temps
/// Returns the type of the expression's result
/// returnTypeReg: map from function name to return type (for checking Call results)
let rec getExprReturnType (floatRegs: Set<int>) (typeMap: ANF.TypeMap) (returnTypeReg: Map<string, AST.Type>) (expr: ANF.AExpr) : AST.Type =
    match expr with
    | ANF.Return atom ->
        match atom with
        | ANF.FloatLiteral _ -> AST.TFloat64
        | ANF.IntLiteral _ -> AST.TInt64
        | ANF.BoolLiteral _ -> AST.TBool
        | ANF.StringLiteral _ -> AST.TString
        | ANF.UnitLiteral -> AST.TUnit
        | ANF.Var (ANF.TempId id) ->
            if Set.contains id floatRegs then AST.TFloat64
            else
                match Map.tryFind (ANF.TempId id) typeMap with
                | Some t -> t
                | None -> AST.TInt64
        | ANF.FuncRef _ -> AST.TInt64
    | ANF.Let (ANF.TempId destId, cexpr, rest) ->
        // Update floatRegs if this binding produces a float
        let floatRegs' =
            if cexprProducesFloat floatRegs returnTypeReg cexpr then
                Set.add destId floatRegs
            else
                floatRegs
        getExprReturnType floatRegs' typeMap returnTypeReg rest
    | ANF.If (_, thenBranch, _) -> getExprReturnType floatRegs typeMap returnTypeReg thenBranch

/// Compute return type for an ANF function by analyzing return statements
/// Uses typeReg to determine which parameters are floats
/// Note: This is used during buildReturnTypeReg, so we pass an empty return type map
/// (we don't yet know return types of other functions)
let computeReturnType (anfFunc: ANF.Function) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) : AST.Type =
    // Get float parameter IDs for this function
    let funcParamTypes =
        match Map.tryFind anfFunc.Name typeReg with
        | Some paramInfos -> paramInfos |> List.map snd
        | None -> []
    let floatParamIds =
        if List.length funcParamTypes = List.length anfFunc.Params then
            List.zip anfFunc.Params funcParamTypes
            |> List.filter (fun (_, typ) -> typ = AST.TFloat64)
            |> List.map (fun (ANF.TempId id, _) -> id)
            |> Set.ofList
        else
            Set.empty
    // Pass empty return type map since we're building it
    getExprReturnType floatParamIds typeMap Map.empty anfFunc.Body

/// Build a map from function name to return type for all functions
let buildReturnTypeReg (functions: ANF.Function list) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) : Map<string, AST.Type> =
    functions
    |> List.map (fun f -> (f.Name, computeReturnType f typeMap typeReg))
    |> Map.ofList

/// CFG builder state - includes lookups to avoid mutable module-level state
/// which would cause race conditions in parallel test execution
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
    StringLookup: Map<string, int>
    FloatLookup: Map<float, int>
    TypeMap: ANF.TypeMap
    TypeReg: Map<string, (string * AST.Type) list>
    ReturnTypeReg: Map<string, AST.Type>  // Function name -> return type
    FuncName: string  // For generating unique labels per function
    FloatRegs: Set<int>  // VReg IDs that hold float values
}

/// Get payload size for an atom used in reference counting
/// Returns Error if type lookup fails
let getPayloadSizeForAtom (builder: CFGBuilder) (atom: ANF.Atom) : Result<int, string> =
    match atom with
    | ANF.Var tid ->
        match Map.tryFind tid builder.TypeMap with
        | Some typ -> Ok (ANF.payloadSize typ builder.TypeReg)
        | None -> Error $"Internal error: type not found for {tid} in TypeMap"
    | _ -> Error "Internal error: RefCount operation on non-variable atom"

/// Convert ANF Atom to MIR Operand using lookups from builder
/// Returns Error if float/string lookup fails (internal invariant violation)
let atomToOperand (builder: CFGBuilder) (atom: ANF.Atom) : Result<MIR.Operand, string> =
    match atom with
    | ANF.UnitLiteral -> Ok (MIR.IntConst 0L)  // Unit is represented as 0
    | ANF.IntLiteral n -> Ok (MIR.IntConst n)
    | ANF.BoolLiteral b -> Ok (MIR.BoolConst b)
    | ANF.FloatLiteral f ->
        match Map.tryFind f builder.FloatLookup with
        | Some idx -> Ok (MIR.FloatRef idx)
        | None -> Error $"Internal error: float literal {f} not found in pool"
    | ANF.StringLiteral s ->
        match Map.tryFind s builder.StringLookup with
        | Some idx -> Ok (MIR.StringRef idx)
        | None -> Error $"Internal error: string literal not found in pool"
    | ANF.Var tempId -> Ok (MIR.Register (tempToVReg tempId))
    | ANF.FuncRef funcName -> Ok (MIR.FuncAddr funcName)

/// Get the type of an ANF Atom (for generating type-specific instructions)
let atomType (builder: CFGBuilder) (atom: ANF.Atom) : AST.Type =
    match atom with
    | ANF.UnitLiteral -> AST.TUnit
    | ANF.IntLiteral _ -> AST.TInt64
    | ANF.BoolLiteral _ -> AST.TBool
    | ANF.StringLiteral _ -> AST.TString
    | ANF.FloatLiteral _ -> AST.TFloat64
    | ANF.Var (ANF.TempId id) ->
        // Check if this VReg is known to hold a float
        if Set.contains id builder.FloatRegs then AST.TFloat64
        else
            match Map.tryFind (ANF.TempId id) builder.TypeMap with
            | Some t -> t
            | None -> AST.TInt64  // Fallback
    | ANF.FuncRef _ -> AST.TInt64  // Function addresses are pointer-sized

/// Get the operand type for a binary operation (checks both operands)
/// If either operand is float, the operation is float
let binOpType (builder: CFGBuilder) (leftAtom: ANF.Atom) (rightAtom: ANF.Atom) : AST.Type =
    let leftType = atomType builder leftAtom
    let rightType = atomType builder rightAtom
    match leftType, rightType with
    | AST.TFloat64, _ | _, AST.TFloat64 -> AST.TFloat64
    | _ -> leftType

/// Get the type of an MIR operand (for generating type-specific instructions)
let operandType (builder: CFGBuilder) (operand: MIR.Operand) : AST.Type =
    match operand with
    | MIR.IntConst _ -> AST.TInt64
    | MIR.BoolConst _ -> AST.TBool
    | MIR.FloatRef _ -> AST.TFloat64
    | MIR.StringRef _ -> AST.TString
    | MIR.FuncAddr _ -> AST.TInt64  // Function addresses are pointer-sized
    | MIR.Register (MIR.VReg id) ->
        // Check if this VReg is known to hold a float
        if Set.contains id builder.FloatRegs then AST.TFloat64
        else AST.TInt64  // Default to integer

/// Convert ANF expression to CFG
/// Returns: Result of (final value operand, CFG builder with all blocks)
let rec convertExpr
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            let block = {
                MIR.Label = currentLabel
                MIR.Instrs = currentInstrs
                MIR.Terminator = MIR.Ret operand
            }
            let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
            Ok (operand, builder'))

    | ANF.Let (tempId, cexpr, rest) ->
        // Let binding: handle based on cexpr type
        let destReg = tempToVReg tempId

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow blocks
            // 1. End current block with branch on condition
            // 2. Create then-block (assigns thenAtom to destReg, jumps to join)
            // 3. Create else-block (assigns elseAtom to destReg, jumps to join)
            // 4. Create join-block (continues with rest)

            atomToOperand builder condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builder thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builder elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

                        // Current block ends with branch
                        let currentBlock = {
                            MIR.Label = currentLabel
                            MIR.Instrs = currentInstrs
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Determine the type of the if result (then/else should have same type)
                        let resultType = atomType builder thenAtom

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        let builder' = {
                            builder with
                                Blocks = builder.Blocks
                                         |> Map.add currentLabel currentBlock
                                         |> Map.add thenLabel thenBlock
                                         |> Map.add elseLabel elseBlock
                                LabelGen = labelGen3
                        }

                        // Continue with rest in join block (no instructions yet)
                        convertExpr rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: add instruction(s) to current block, continue
            // Track if dest is float type for later builder update
            let destType = ref AST.TInt64
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    let aType = atomType builder atom
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    let opType = binOpType builder leftAtom rightAtom
                    destType := opType
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp, opType)]))
                | ANF.UnaryPrim (op, atom) ->
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        [MIR.UnaryOp (destReg, convertUnaryOp op, operand)])
                | ANF.Call (funcName, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType = Map.tryFind funcName builder.ReturnTypeReg |> Option.defaultValue AST.TInt64
                    destType := returnType  // Track call result type for FloatRegs update
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectCall (func, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    // For indirect calls, we don't know the return type - default to TInt64
                    let returnType = AST.TInt64
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
                    // Store function pointer at offset 0
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName)
                    // Store captured values at offsets 8, 16, ...
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))
                | ANF.FileDelete pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileDelete (destReg, pathOp)])
                | ANF.FileSetExecutable pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileSetExecutable (destReg, pathOp)])
                | ANF.RawAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.RawAlloc (destReg, numBytesOp)])
                | ANF.RawFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawFree ptrOp])
                | ANF.RawGet (ptrAtom, offsetAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp)]))
                | ANF.RawSet (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSet (ptrOp, offsetOp, valueOp)])))
                | ANF.FloatSqrt atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatSqrt (destReg, op)])
                | ANF.FloatAbs atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatAbs (destReg, op)])
                | ANF.FloatNeg atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatNeg (destReg, op)])
                | ANF.IntToFloat atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.IntToFloat (destReg, op)])
                | ANF.FloatToInt atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToInt (destReg, op)])
                | ANF.StringHash strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.StringHash (destReg, strOp)])
                | ANF.StringEq (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringEq (destReg, leftOp, rightOp)]))
                | ANF.RefCountIncString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountIncString strOp])
                | ANF.RefCountDecString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountDecString strOp])

            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                let newInstrs = currentInstrs @ instrs
                // Update FloatRegs if this dest is a float
                let (MIR.VReg destId) = destReg
                let builder' =
                    if !destType = AST.TFloat64 then
                        { builder with FloatRegs = Set.add destId builder.FloatRegs }
                    else
                        builder
                convertExpr rest currentLabel newInstrs builder'

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression:
        // 1. End current block with Branch terminator
        // 2. Create then-block and else-block
        // 3. Create join-block where both branches meet
        // 4. Both branches put result in same register and jump to join

        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->

        // Generate labels for then, else, and join blocks
        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

        // Create a register to hold the result from both branches
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        // End current block with conditional branch
        let currentBlock = {
            MIR.Label = currentLabel
            MIR.Instrs = currentInstrs
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            Blocks = Map.add currentLabel currentBlock builder.Blocks
            LabelGen = labelGen3
            RegGen = regGen1
            StringLookup = builder.StringLookup
            FloatLookup = builder.FloatLookup
            TypeMap = builder.TypeMap
            TypeReg = builder.TypeReg
            ReturnTypeReg = builder.ReturnTypeReg
            FuncName = builder.FuncName
            FloatRegs = builder.FloatRegs
        }

        // Convert then-branch: result goes into resultReg, then jump to join
        match convertExprToOperand thenBranch thenLabel [] builder1 with
        | Error err -> Error err
        | Ok (thenResult, thenJoinOpt, builder2) ->

        // If then-branch created blocks (nested if), patch its join block
        // Otherwise, create a simple block that moves result and jumps
        let thenResultType = operandType builder2 thenResult
        let builder3 =
            match thenJoinOpt with
            | Some nestedJoinLabel ->
                // Patch the nested join block to jump to our join instead of returning
                match Map.tryFind nestedJoinLabel builder2.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                | None -> builder2  // Should not happen
            | None ->
                // Simple expression - create block that moves result and jumps
                let thenBlock = {
                    MIR.Label = thenLabel
                    MIR.Instrs = [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        // Convert else-branch: result goes into resultReg, then jump to join
        match convertExprToOperand elseBranch elseLabel [] builder3 with
        | Error err -> Error err
        | Ok (elseResult, elseJoinOpt, builder4) ->

        // Same logic for else-branch
        let elseResultType = operandType builder4 elseResult
        let builder5 =
            match elseJoinOpt with
            | Some nestedJoinLabel ->
                match Map.tryFind nestedJoinLabel builder4.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                | None -> builder4  // Should not happen
            | None ->
                let elseBlock = {
                    MIR.Label = elseLabel
                    MIR.Instrs = [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        // Create join block that returns the result
        let joinBlock = {
            MIR.Label = joinLabel
            MIR.Instrs = []
            MIR.Terminator = MIR.Ret (MIR.Register resultReg)
        }
        let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

        // Return the result operand
        let resultOp = MIR.Register resultReg
        Ok (resultOp, builder6))

/// Helper: convert expression and extract final operand
/// Returns: Result of (operand, optional join label if blocks were created, builder)
/// - If join label is Some(label), the expression created blocks ending at that join block
/// - If join label is None, no blocks were created (simple expression)
and convertExprToOperand
    (expr: ANF.AExpr)
    (startLabel: MIR.Label)
    (startInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * MIR.Label option * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // If we have accumulated instructions from Let bindings, create a block
        // Otherwise just return the operand
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            if List.isEmpty startInstrs then
                Ok (operand, None, builder)
            else
                // Create a block with accumulated instructions
                // Use temporary Ret terminator - caller will patch if needed
                let block = {
                    MIR.Label = startLabel
                    MIR.Instrs = startInstrs
                    MIR.Terminator = MIR.Ret operand
                }
                let builder' = { builder with Blocks = Map.add startLabel block builder.Blocks }
                Ok (operand, Some startLabel, builder'))

    | ANF.Let (tempId, cexpr, rest) ->
        let destReg = tempToVReg tempId

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow - similar to convertExpr version
            atomToOperand builder condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builder thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builder elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

                        // Current block ends with branch
                        let startBlock = {
                            MIR.Label = startLabel
                            MIR.Instrs = startInstrs
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Determine the type of the if result (then/else should have same type)
                        let resultType = atomType builder thenAtom

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        let builder' = {
                            builder with
                                Blocks = builder.Blocks
                                         |> Map.add startLabel startBlock
                                         |> Map.add thenLabel thenBlock
                                         |> Map.add elseLabel elseBlock
                                LabelGen = labelGen3
                        }

                        // Continue with rest in join block (no instructions yet)
                        convertExprToOperand rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: create instruction(s) and accumulate
            // Track if dest is float type for later builder update
            let destType = ref AST.TInt64
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    let aType = atomType builder atom
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    let opType = binOpType builder leftAtom rightAtom
                    destType := opType
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp, opType)]))
                | ANF.UnaryPrim (op, atom) ->
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        [MIR.UnaryOp (destReg, convertUnaryOp op, operand)])
                | ANF.Call (funcName, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType = Map.tryFind funcName builder.ReturnTypeReg |> Option.defaultValue AST.TInt64
                    destType := returnType  // Track call result type for FloatRegs update
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectCall (func, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    // For indirect calls, we don't know the return type - default to TInt64
                    let returnType = AST.TInt64
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
                    // Store function pointer at offset 0
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName)
                    // Store captured values at offsets 8, 16, ...
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))
                | ANF.FileDelete pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileDelete (destReg, pathOp)])
                | ANF.FileSetExecutable pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileSetExecutable (destReg, pathOp)])
                | ANF.RawAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.RawAlloc (destReg, numBytesOp)])
                | ANF.RawFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawFree ptrOp])
                | ANF.RawGet (ptrAtom, offsetAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp)]))
                | ANF.RawSet (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSet (ptrOp, offsetOp, valueOp)])))
                | ANF.FloatSqrt atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatSqrt (destReg, op)])
                | ANF.FloatAbs atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatAbs (destReg, op)])
                | ANF.FloatNeg atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatNeg (destReg, op)])
                | ANF.IntToFloat atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.IntToFloat (destReg, op)])
                | ANF.FloatToInt atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToInt (destReg, op)])
                | ANF.StringHash strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.StringHash (destReg, strOp)])
                | ANF.StringEq (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringEq (destReg, leftOp, rightOp)]))
                | ANF.RefCountIncString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountIncString strOp])
                | ANF.RefCountDecString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountDecString strOp])

            // Let bindings accumulate instructions, pass through join label
            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                // Update FloatRegs if this dest is a float
                let (MIR.VReg destId) = destReg
                let builder' =
                    if !destType = AST.TFloat64 then
                        { builder with FloatRegs = Set.add destId builder.FloatRegs }
                    else
                        builder
                convertExprToOperand rest startLabel (startInstrs @ instrs) builder'

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression: creates blocks with branch/jump/join structure
        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->
            let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
            let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
            let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2
            let (resultReg, regGen1) = MIR.freshReg builder.RegGen

            let startBlock = {
                MIR.Label = startLabel
                MIR.Instrs = startInstrs
                MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
            }

            let builder1 = {
                Blocks = Map.add startLabel startBlock builder.Blocks
                LabelGen = labelGen3
                RegGen = regGen1
                StringLookup = builder.StringLookup
                FloatLookup = builder.FloatLookup
                TypeMap = builder.TypeMap
                TypeReg = builder.TypeReg
                ReturnTypeReg = builder.ReturnTypeReg
                FuncName = builder.FuncName
                FloatRegs = builder.FloatRegs
            }

            // Convert then-branch
            match convertExprToOperand thenBranch thenLabel [] builder1 with
            | Error err -> Error err
            | Ok (thenResult, thenJoinOpt, builder2) ->

            // If then-branch created blocks (nested if), patch its join block
            // Otherwise, create a simple block that moves result and jumps
            let thenResultType = operandType builder2 thenResult
            let builder3 =
                match thenJoinOpt with
                | Some nestedJoinLabel ->
                    // Patch the nested join block to jump to our join instead of returning
                    match Map.tryFind nestedJoinLabel builder2.Blocks with
                    | Some nestedJoinBlock ->
                        let patchedBlock = {
                            nestedJoinBlock with
                                Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                                Terminator = MIR.Jump joinLabel
                        }
                        { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                    | None -> builder2  // Should not happen
                | None ->
                    // Simple expression - create block that moves result and jumps
                    let thenBlock = {
                        MIR.Label = thenLabel
                        MIR.Instrs = [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                        MIR.Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

            // Convert else-branch
            match convertExprToOperand elseBranch elseLabel [] builder3 with
            | Error err -> Error err
            | Ok (elseResult, elseJoinOpt, builder4) ->

            // Same logic for else-branch
            let elseResultType = operandType builder4 elseResult
            let builder5 =
                match elseJoinOpt with
                | Some nestedJoinLabel ->
                    match Map.tryFind nestedJoinLabel builder4.Blocks with
                    | Some nestedJoinBlock ->
                        let patchedBlock = {
                            nestedJoinBlock with
                                Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                                Terminator = MIR.Jump joinLabel
                        }
                        { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                    | None -> builder4  // Should not happen
                | None ->
                    let elseBlock = {
                        MIR.Label = elseLabel
                        MIR.Instrs = [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                        MIR.Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

            // Create join block
            let joinBlock = {
                MIR.Label = joinLabel
                MIR.Instrs = []
                MIR.Terminator = MIR.Ret (MIR.Register resultReg)
            }
            let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

            // Return result register and our join label for potential patching by caller
            Ok (MIR.Register resultReg, Some joinLabel, builder6))

/// Convert an ANF function to a MIR function
let convertANFFunction (anfFunc: ANF.Function) (regGen: MIR.RegGen) (strLookup: Map<string, int>) (fltLookup: Map<float, int>) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) (returnTypeReg: Map<string, AST.Type>) : Result<MIR.Function * MIR.RegGen, string> =
    // Initialize FloatRegs with float parameter IDs
    // This is critical so that the function body knows which VRegs hold floats
    // Use typeReg to look up function's parameter types by name, since typeMap is empty
    let funcParamTypes =
        match Map.tryFind anfFunc.Name typeReg with
        | Some paramInfos -> paramInfos |> List.map snd
        | None -> []  // Fallback: no type info

    // Zip params with types and find the float ones
    // Guard against length mismatch (can happen if typeReg is incomplete)
    let floatParamIds =
        if List.length funcParamTypes = List.length anfFunc.Params then
            List.zip anfFunc.Params funcParamTypes
            |> List.filter (fun (_, typ) -> typ = AST.TFloat64)
            |> List.map (fun (ANF.TempId id, _) -> id)
            |> Set.ofList
        else
            Set.empty

    // Create initial builder with lookups
    let initialBuilder = {
        RegGen = regGen
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        StringLookup = strLookup
        FloatLookup = fltLookup
        TypeMap = typeMap
        TypeReg = typeReg
        ReturnTypeReg = returnTypeReg
        FuncName = anfFunc.Name
        FloatRegs = floatParamIds
    }

    // Create entry label for CFG (internal to function body)
    let entryLabel = MIR.Label $"{anfFunc.Name}_body"

    // Convert ANF parameter TempIds to MIR VRegs
    // Must use tempToVReg to preserve the TempId values, not fresh VRegs,
    // because the body uses Var (TempId n) which converts to VReg n
    let paramVRegs = anfFunc.Params |> List.map tempToVReg

    // Get parameter types from funcParamTypes (already extracted from typeReg)
    let paramTypes =
        if List.length funcParamTypes = List.length anfFunc.Params then
            funcParamTypes
        else
            // Fallback: use Int64 for all params if type info unavailable
            anfFunc.Params |> List.map (fun _ -> AST.TInt64)

    // Helper to get return type by finding return atoms in the body
    // Uses returnTypeReg to check if function calls return floats
    let rec getReturnType (floatRegs: Set<int>) (expr: ANF.AExpr) : AST.Type =
        match expr with
        | ANF.Return atom ->
            match atom with
            | ANF.FloatLiteral _ -> AST.TFloat64
            | ANF.IntLiteral _ -> AST.TInt64
            | ANF.BoolLiteral _ -> AST.TBool
            | ANF.StringLiteral _ -> AST.TString
            | ANF.UnitLiteral -> AST.TUnit
            | ANF.Var (ANF.TempId id) ->
                if Set.contains id floatRegs then AST.TFloat64
                else
                    match Map.tryFind (ANF.TempId id) typeMap with
                    | Some t -> t
                    | None -> AST.TInt64
            | ANF.FuncRef _ -> AST.TInt64
        | ANF.Let (ANF.TempId destId, cexpr, rest) ->
            // Update floatRegs if this binding produces a float
            let floatRegs' =
                if cexprProducesFloat floatRegs returnTypeReg cexpr then
                    Set.add destId floatRegs
                else
                    floatRegs
            getReturnType floatRegs' rest
        | ANF.If (_, thenBranch, _) -> getReturnType floatRegs thenBranch  // Assume both branches have same type

    let returnType = getReturnType floatParamIds anfFunc.Body

    // Convert function body to CFG
    match convertExpr anfFunc.Body entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->

    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }

    let mirFunc = {
        MIR.Name = anfFunc.Name
        MIR.Params = paramVRegs
        MIR.ParamTypes = paramTypes
        MIR.ReturnType = returnType
        MIR.CFG = cfg
    }

    Ok (mirFunc, finalBuilder.RegGen)

/// Convert ANF program to MIR program
/// mainExprType: the type of the main expression (used for _start's return type)
/// variantLookup: mapping from variant names to type info (for enum printing)
/// recordRegistry: mapping from record type names to field info (for record printing)
let toMIR (program: ANF.Program) (_regGen: MIR.RegGen) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) (mainExprType: AST.Type) (variantLookup: AST_to_ANF.VariantLookup) (recordRegistry: MIR.RecordRegistry) : Result<MIR.Program * MIR.RegGen, string> =
    let (ANF.Program (functions, mainExpr)) = program

    // Critical: freshReg must generate VRegs that don't conflict with TempId-derived VRegs.
    // tempToVReg (TempId n) → VReg n, so freshReg must start past the max TempId used.
    let maxId = maxTempIdInProgram program
    let regGen = MIR.RegGen (maxId + 1)

    // Phase 1: Collect all strings and floats, build pools and lookups
    // Lookups are local (not mutable module-level) to avoid race conditions in parallel tests
    let allStrings = collectStringsFromProgram program
    let stringPool = buildStringPool allStrings
    let strLookup = buildStringLookup stringPool

    let allFloats = collectFloatsFromProgram program
    let floatPool = buildFloatPool allFloats
    let fltLookup = buildFloatLookup floatPool

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeReg = buildReturnTypeReg functions typeMap typeReg

    // Phase 2: Convert all functions to MIR
    let rec convertFunctions funcs rg remaining =
        match remaining with
        | [] -> Ok (funcs, rg)
        | anfFunc :: rest ->
            match convertANFFunction anfFunc rg strLookup fltLookup typeMap typeReg returnTypeReg with
            | Error err -> Error err
            | Ok (mirFunc, rg') -> convertFunctions (funcs @ [mirFunc]) rg' rest

    match convertFunctions [] regGen functions with
    | Error err -> Error err
    | Ok (mirFuncs, regGen1) ->

    // Convert main expression to a synthetic "_start" function
    let entryLabel = MIR.Label "_start_body"
    let initialBuilder = {
        RegGen = regGen1
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        StringLookup = strLookup
        FloatLookup = fltLookup
        TypeMap = typeMap
        TypeReg = typeReg
        ReturnTypeReg = returnTypeReg
        FuncName = "_start"
        FloatRegs = Set.empty
    }
    match convertExpr mainExpr entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->
    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }
    // Use the passed mainExprType for _start's return type
    // This is needed for proper float handling in the Ret terminator
    let startFunc = {
        MIR.Name = "_start"
        MIR.Params = []
        MIR.ParamTypes = []
        MIR.ReturnType = mainExprType
        MIR.CFG = cfg
    }
    let allFuncs = mirFuncs @ [startFunc]
    let variantRegistry = buildVariantRegistry variantLookup
    // recordRegistry is passed in from ConversionResult.TypeReg
    Ok (MIR.Program (allFuncs, stringPool, floatPool, variantRegistry, recordRegistry), finalBuilder.RegGen)

/// Convert ANF program to MIR (functions only, no _start)
/// Use for stdlib where there's no real main expression to convert.
/// Returns just the function list, pools, variant registry, and record registry without wrapping in MIR.Program.
let toMIRFunctionsOnly (program: ANF.Program) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) (variantLookup: AST_to_ANF.VariantLookup) (recordRegistry: MIR.RecordRegistry) : Result<MIR.Function list * MIR.StringPool * MIR.FloatPool * MIR.VariantRegistry * MIR.RecordRegistry, string> =
    let (ANF.Program (functions, _mainExpr)) = program

    // Same regGen calculation as toMIR
    let maxId = maxTempIdInProgram program
    let regGen = MIR.RegGen (maxId + 1)

    // Phase 1: Collect all strings and floats, build pools and lookups
    let allStrings = collectStringsFromProgram program
    let stringPool = buildStringPool allStrings
    let strLookup = buildStringLookup stringPool

    let allFloats = collectFloatsFromProgram program
    let floatPool = buildFloatPool allFloats
    let fltLookup = buildFloatLookup floatPool

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeReg = buildReturnTypeReg functions typeMap typeReg

    // Phase 2: Convert all functions to MIR (skip main/_start)
    let rec convertFunctions funcs rg remaining =
        match remaining with
        | [] -> Ok (funcs, rg)
        | anfFunc :: rest ->
            match convertANFFunction anfFunc rg strLookup fltLookup typeMap typeReg returnTypeReg with
            | Error err -> Error err
            | Ok (mirFunc, rg') -> convertFunctions (funcs @ [mirFunc]) rg' rest

    match convertFunctions [] regGen functions with
    | Error err -> Error err
    | Ok (mirFuncs, _) ->
        let variantRegistry = buildVariantRegistry variantLookup
        Ok (mirFuncs, stringPool, floatPool, variantRegistry, recordRegistry)

// ============================================================================
// MIR Program Merging (for stdlib MIR caching optimization)
// ============================================================================

/// Offset pool references in an operand
let private offsetOperand (strOffset: int) (fltOffset: int) (op: MIR.Operand) : MIR.Operand =
    match op with
    | MIR.StringRef idx -> MIR.StringRef (idx + strOffset)
    | MIR.FloatRef idx -> MIR.FloatRef (idx + fltOffset)
    | other -> other

/// Offset pool references in a list of operands
let private offsetOperands strOffset fltOffset ops =
    List.map (offsetOperand strOffset fltOffset) ops

/// Offset pool references in a phi source
let private offsetPhiSource strOffset fltOffset (op, label) =
    (offsetOperand strOffset fltOffset op, label)

/// Offset pool references in an instruction
let private offsetInstr (strOffset: int) (fltOffset: int) (instr: MIR.Instr) : MIR.Instr =
    match instr with
    | MIR.Mov (dest, src, vt) -> MIR.Mov (dest, offsetOperand strOffset fltOffset src, vt)
    | MIR.BinOp (dest, op, left, right, t) -> MIR.BinOp (dest, op, offsetOperand strOffset fltOffset left, offsetOperand strOffset fltOffset right, t)
    | MIR.UnaryOp (dest, op, src) -> MIR.UnaryOp (dest, op, offsetOperand strOffset fltOffset src)
    | MIR.Call (dest, name, args, argTypes, retType) -> MIR.Call (dest, name, offsetOperands strOffset fltOffset args, argTypes, retType)
    | MIR.IndirectCall (dest, func, args, argTypes, retType) -> MIR.IndirectCall (dest, offsetOperand strOffset fltOffset func, offsetOperands strOffset fltOffset args, argTypes, retType)
    | MIR.ClosureAlloc (dest, name, caps) -> MIR.ClosureAlloc (dest, name, offsetOperands strOffset fltOffset caps)
    | MIR.ClosureCall (dest, closure, args) -> MIR.ClosureCall (dest, offsetOperand strOffset fltOffset closure, offsetOperands strOffset fltOffset args)
    | MIR.HeapAlloc (dest, size) -> MIR.HeapAlloc (dest, size)
    | MIR.HeapStore (addr, offset, src) -> MIR.HeapStore (addr, offset, offsetOperand strOffset fltOffset src)
    | MIR.HeapLoad (dest, addr, offset) -> MIR.HeapLoad (dest, addr, offset)
    | MIR.StringConcat (dest, left, right) -> MIR.StringConcat (dest, offsetOperand strOffset fltOffset left, offsetOperand strOffset fltOffset right)
    | MIR.RefCountInc (addr, size) -> MIR.RefCountInc (addr, size)
    | MIR.RefCountDec (addr, size) -> MIR.RefCountDec (addr, size)
    | MIR.Print (src, vt) -> MIR.Print (offsetOperand strOffset fltOffset src, vt)
    | MIR.FileReadText (dest, path) -> MIR.FileReadText (dest, offsetOperand strOffset fltOffset path)
    | MIR.FileExists (dest, path) -> MIR.FileExists (dest, offsetOperand strOffset fltOffset path)
    | MIR.FileWriteText (dest, path, content) -> MIR.FileWriteText (dest, offsetOperand strOffset fltOffset path, offsetOperand strOffset fltOffset content)
    | MIR.FileAppendText (dest, path, content) -> MIR.FileAppendText (dest, offsetOperand strOffset fltOffset path, offsetOperand strOffset fltOffset content)
    | MIR.FileDelete (dest, path) -> MIR.FileDelete (dest, offsetOperand strOffset fltOffset path)
    | MIR.FileSetExecutable (dest, path) -> MIR.FileSetExecutable (dest, offsetOperand strOffset fltOffset path)
    | MIR.FloatSqrt (dest, src) -> MIR.FloatSqrt (dest, offsetOperand strOffset fltOffset src)
    | MIR.FloatAbs (dest, src) -> MIR.FloatAbs (dest, offsetOperand strOffset fltOffset src)
    | MIR.FloatNeg (dest, src) -> MIR.FloatNeg (dest, offsetOperand strOffset fltOffset src)
    | MIR.IntToFloat (dest, src) -> MIR.IntToFloat (dest, offsetOperand strOffset fltOffset src)
    | MIR.FloatToInt (dest, src) -> MIR.FloatToInt (dest, offsetOperand strOffset fltOffset src)
    | MIR.RawAlloc (dest, numBytes) -> MIR.RawAlloc (dest, offsetOperand strOffset fltOffset numBytes)
    | MIR.RawFree ptr -> MIR.RawFree (offsetOperand strOffset fltOffset ptr)
    | MIR.RawGet (dest, ptr, offset) -> MIR.RawGet (dest, offsetOperand strOffset fltOffset ptr, offsetOperand strOffset fltOffset offset)
    | MIR.RawSet (ptr, offset, value) -> MIR.RawSet (offsetOperand strOffset fltOffset ptr, offsetOperand strOffset fltOffset offset, offsetOperand strOffset fltOffset value)
    | MIR.StringHash (dest, str) -> MIR.StringHash (dest, offsetOperand strOffset fltOffset str)
    | MIR.StringEq (dest, left, right) -> MIR.StringEq (dest, offsetOperand strOffset fltOffset left, offsetOperand strOffset fltOffset right)
    | MIR.RefCountIncString str -> MIR.RefCountIncString (offsetOperand strOffset fltOffset str)
    | MIR.RefCountDecString str -> MIR.RefCountDecString (offsetOperand strOffset fltOffset str)
    | MIR.Phi (dest, sources) -> MIR.Phi (dest, List.map (offsetPhiSource strOffset fltOffset) sources)

/// Offset pool references in a terminator
let private offsetTerminator (strOffset: int) (fltOffset: int) (term: MIR.Terminator) : MIR.Terminator =
    match term with
    | MIR.Ret op -> MIR.Ret (offsetOperand strOffset fltOffset op)
    | MIR.Branch (cond, trueL, falseL) -> MIR.Branch (offsetOperand strOffset fltOffset cond, trueL, falseL)
    | MIR.Jump label -> MIR.Jump label

/// Offset pool references in a basic block
let private offsetBlock (strOffset: int) (fltOffset: int) (block: MIR.BasicBlock) : MIR.BasicBlock =
    { Label = block.Label
      Instrs = List.map (offsetInstr strOffset fltOffset) block.Instrs
      Terminator = offsetTerminator strOffset fltOffset block.Terminator }

/// Offset pool references in a function
let private offsetFunction (strOffset: int) (fltOffset: int) (func: MIR.Function) : MIR.Function =
    let offsetBlocks =
        func.CFG.Blocks
        |> Map.map (fun _ block -> offsetBlock strOffset fltOffset block)
    { Name = func.Name
      Params = func.Params
      ParamTypes = func.ParamTypes
      ReturnType = func.ReturnType
      CFG = { Entry = func.CFG.Entry; Blocks = offsetBlocks } }

/// Append a user string pool to stdlib string pool with offset
let appendStringPools (stdlibPool: MIR.StringPool) (userPool: MIR.StringPool) : MIR.StringPool =
    let offset = stdlibPool.NextId
    let offsetUserStrings =
        userPool.Strings
        |> Map.toList
        |> List.map (fun (idx, value) -> (idx + offset, value))
        |> Map.ofList
    let mergedStrings = Map.fold (fun acc k v -> Map.add k v acc) stdlibPool.Strings offsetUserStrings
    // Build reverse index from merged strings
    let mergedStringToId =
        mergedStrings
        |> Map.toSeq
        |> Seq.map (fun (idx, (s, _)) -> (s, idx))
        |> Map.ofSeq
    { Strings = mergedStrings
      StringToId = mergedStringToId
      NextId = stdlibPool.NextId + userPool.NextId }

/// Append a user float pool to stdlib float pool with offset
let appendFloatPools (stdlibPool: MIR.FloatPool) (userPool: MIR.FloatPool) : MIR.FloatPool =
    let offset = stdlibPool.NextId
    let offsetUserFloats =
        userPool.Floats
        |> Map.toList
        |> List.map (fun (idx, value) -> (idx + offset, value))
        |> Map.ofList
    let mergedFloats = Map.fold (fun acc k v -> Map.add k v acc) stdlibPool.Floats offsetUserFloats
    // Build reverse index from merged floats
    let mergedFloatToId =
        mergedFloats
        |> Map.toSeq
        |> Seq.map (fun (idx, f) -> (f, idx))
        |> Map.ofSeq
    { Floats = mergedFloats
      FloatToId = mergedFloatToId
      NextId = stdlibPool.NextId + userPool.NextId }

/// Merge user MIR with cached stdlib MIR.
/// Offsets user's StringRef/FloatRef indices to account for stdlib pools.
/// Excludes stdlib's _start function (user's _start is the entry point).
let mergeMIRPrograms (stdlibMIR: MIR.Program) (userMIR: MIR.Program) : MIR.Program =
    let (MIR.Program (stdlibFuncs, stdlibStrings, stdlibFloats, stdlibVariants, stdlibRecords)) = stdlibMIR
    let (MIR.Program (userFuncs, userStrings, userFloats, userVariants, userRecords)) = userMIR

    let stringOffset = stdlibStrings.NextId
    let floatOffset = stdlibFloats.NextId

    // Exclude stdlib's _start function (user's _start is the real entry point)
    let stdlibFuncsNoStart = stdlibFuncs |> List.filter (fun f -> f.Name <> "_start")

    // Offset user function pool references
    let offsetUserFuncs = userFuncs |> List.map (offsetFunction stringOffset floatOffset)

    // Merge pools (stdlib first, user appended with offset)
    let mergedStrings = appendStringPools stdlibStrings userStrings
    let mergedFloats = appendFloatPools stdlibFloats userFloats

    // Merge variant registries (user overrides stdlib for same type names)
    let mergedVariants = Map.fold (fun acc k v -> Map.add k v acc) stdlibVariants userVariants

    // Merge record registries (user overrides stdlib for same type names)
    let mergedRecords = Map.fold (fun acc k v -> Map.add k v acc) stdlibRecords userRecords

    MIR.Program (stdlibFuncsNoStart @ offsetUserFuncs, mergedStrings, mergedFloats, mergedVariants, mergedRecords)
