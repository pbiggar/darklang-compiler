// TypeFacts.fs - Track ANF value types and immutable context projections for RC insertion.

module RcTypeFacts

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open ClosureAnalysis
open LiftExpressions
open LiftFunctions
open LoweringExpressions
open AST_to_ANF
/// Immutable registry projections and memoized type plans shared by every
/// function processed in one RC insertion pass. TypeContext itself is copied
/// as local TempId information changes, so keep the reusable planning state in
/// a reference object carried by those copies.
type RcTypePlanningContext = {
    mutable RecordRegistries:
        (Map<string, (string * AST.Type) list> * Map<string, string list>) option
    Shapes: System.Collections.Generic.Dictionary<AST.Type, RcShape>
    Metadata: System.Collections.Generic.Dictionary<AST.Type, RcMetadata>
}

let createRcTypePlanningContext () : RcTypePlanningContext =
    {
        RecordRegistries = None
        Shapes = System.Collections.Generic.Dictionary<AST.Type, RcShape>()
        Metadata = System.Collections.Generic.Dictionary<AST.Type, RcMetadata>()
    }

/// Type context for inferring types during RC insertion
type TypeContext = {
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    SumShapeReg: RcSumShapeRegistry
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    /// Maps TempId -> Type for values we've seen
    TempTypes: Map<TempId, AST.Type>
    /// Maps TempId -> function name for closures (to resolve closure call return types)
    ClosureFuncs: Map<TempId, string>
    /// Registry projections and canonical ownership plans shared across local
    /// TypeContext copies for this pass.
    TypePlanning: RcTypePlanningContext
}

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    let (Program (functions, _)) = result.Program
    let funcReg =
        functions
        |> List.fold
            (fun registry func ->
                Map.add
                    func.Name
                    (AST.TFunction (
                        func.TypedParams |> List.map (fun param -> param.Type),
                        func.ReturnType
                    ))
                    registry)
            result.FuncReg
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      SumShapeReg = result.RcSumShapeReg
      FuncReg = funcReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty
      ClosureFuncs = Map.empty
      TypePlanning = createRcTypePlanningContext () }

let internal withTempTypes (ctx: TypeContext) (types: Map<TempId, AST.Type>) : TypeContext =
    { ctx with TempTypes = types }

/// Add a closure TempId -> function name mapping to context
let addClosureFunc (ctx: TypeContext) (tempId: TempId) (funcName: string) : TypeContext =
    { ctx with ClosureFuncs = Map.add tempId funcName ctx.ClosureFuncs }

/// Try to get the function name of a closure from its TempId
let tryGetClosureFunc (ctx: TypeContext) (atom: Atom) : string option =
    match atom with
    | Var tid -> Map.tryFind tid ctx.ClosureFuncs
    | _ -> None

/// Try to get the type of a TempId
let tryGetType (ctx: TypeContext) (tempId: TempId) : AST.Type option =
    Map.tryFind tempId ctx.TempTypes

/// Try to get a function's return type from the function registry
let tryGetFuncReturnTypeFromReg (ctx: TypeContext) (funcName: string) : AST.Type option =
    match Map.tryFind funcName ctx.FuncReg with
    | Some (AST.TFunction (_, retType)) -> Some retType
    | Some otherType -> Some otherType
    | None -> None

/// Infer the type of an atom (best-effort)
let inferAtomType (ctx: TypeContext) (atom: Atom) : AST.Type option =
    match atom with
    | UnitLiteral -> Some AST.TUnit
    | IntLiteral n -> Some (ANF.sizedIntToType n)
    | BoolLiteral _ -> Some AST.TBool
    | StringLiteral _ -> Some AST.TString
    | FloatLiteral _ -> Some AST.TFloat64
    | Var tid -> tryGetType ctx tid
    | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg

let private isIntegerType (typ: AST.Type) : bool =
    match typ with
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true
    | _ -> false

let private inferArithmeticType (leftType: AST.Type option) (rightType: AST.Type option) : AST.Type option =
    match leftType, rightType with
    | Some AST.TFloat64, _
    | _, Some AST.TFloat64 ->
        Some AST.TFloat64
    | Some left, Some right when left = right && isIntegerType left ->
        Some left
    | Some left, None when isIntegerType left ->
        Some left
    | None, Some right when isIntegerType right ->
        Some right
    | _ ->
        None

let private isHeapLikeForBitwiseTagging (typ: AST.Type) : bool =
    match typ with
    | AST.TTuple _
    | AST.TRecord _
    | AST.TSum _
    | AST.TList _
    | AST.TDict _ ->
        true
    | _ ->
        false

/// Return types for monomorphized intrinsics that are not always present in FuncReg
let private tryGetMonomorphizedIntrinsicReturnType (ctx: TypeContext) (funcName: string) : AST.Type option =
    let tryParseMangled (mangled: string) : AST.Type option =
        match tryParseMangledType ctx.VariantLookup mangled with
        | Ok typ -> Some typ
        | Error _ -> None

    if funcName.StartsWith("__raw_get_") then
        funcName.Substring("__raw_get_".Length)
        |> tryParseMangled
    elif funcName.StartsWith("__raw_take_") then
        funcName.Substring("__raw_take_".Length)
        |> tryParseMangled
    elif funcName.StartsWith("__raw_slot_init_") then Some AST.TUnit
    elif funcName.StartsWith("__hash_") then Some AST.TInt64
    elif funcName.StartsWith("__key_eq_") then Some AST.TBool
    elif funcName.StartsWith("__empty_dict_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__dict_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_to_rawptr_") then Some AST.TRawPtr
    elif funcName.StartsWith("__rawptr_to_dict_") then
        funcName.Substring("__rawptr_to_dict_".Length)
        |> fun suffix -> tryParseMangled $"dict_{suffix}"
    elif funcName.StartsWith("__list_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__list_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__list_to_rawptr_") then Some AST.TRawPtr
    elif funcName.StartsWith("__rawptr_to_list_") then
        funcName.Substring("__rawptr_to_list_".Length)
        |> tryParseMangled
        |> Option.map AST.TList
    else None

/// Infer the type of a CExpr in the given context
let inferCExprType (ctx: TypeContext) (cexpr: CExpr) : AST.Type option =
    match cexpr with
    | Atom atom -> inferAtomType ctx atom
    | TypedAtom (_, typ) -> Some typ  // Use the explicit type annotation
    | Prim (op, left, right) ->
        // Binary ops return int or bool depending on op
        match op with
        | Add | Sub | Mul | Div ->
            let leftType = inferAtomType ctx left
            let rightType = inferAtomType ctx right
            inferArithmeticType leftType rightType
        | Mod | Shl | Shr ->
            let leftType = inferAtomType ctx left
            let rightType = inferAtomType ctx right
            match leftType, rightType with
            | Some l, Some r when l = r && isIntegerType l -> Some l
            | Some l, None when isIntegerType l -> Some l
            | None, Some r when isIntegerType r -> Some r
            | Some l, Some _ -> Some l
            | Some l, None -> Some l
            | None, Some r -> Some r
            | None, None -> None
        | BitAnd | BitOr | BitXor ->
            let leftType = inferAtomType ctx left
            let rightType = inferAtomType ctx right
            match leftType, rightType with
            // Pointer-tagging lowerings use bitwise ops over tagged heap values and masks.
            // The result is a scalar tag/masked pointer value, not a heap object ownership value.
            | Some l, _ when isHeapLikeForBitwiseTagging l -> Some AST.TInt64
            | _, Some r when isHeapLikeForBitwiseTagging r -> Some AST.TInt64
            | Some l, Some r when l = r && isIntegerType l -> Some l
            | Some l, None when isIntegerType l -> Some l
            | None, Some r when isIntegerType r -> Some r
            | Some l, Some _ -> Some l
            | Some l, None -> Some l
            | None, Some r -> Some r
            | None, None -> None
        | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Some AST.TBool
    | CanonicalBufferEq _ -> Some AST.TBool
    | UnaryPrim (op, atom) ->
        match op with
        | Neg ->
            match inferAtomType ctx atom with
            | Some AST.TFloat64 -> Some AST.TFloat64
            | Some _ -> Some AST.TInt64
            | None -> None
        | Not -> Some AST.TBool
        | BitNot ->
            // Preserve the operand type instead of assuming Int64.
            // This keeps sized integer semantics (e.g. UInt8) intact.
            inferAtomType ctx atom
    // Float intrinsics
    | FloatSqrt _ -> Some AST.TFloat64
    | FloatAbs _ -> Some AST.TFloat64
    | FloatNeg _ -> Some AST.TFloat64
    | Int64ToFloat _ -> Some AST.TFloat64
    | FloatToInt64 _ -> Some AST.TInt64
    | FloatToBits _ -> Some AST.TUInt64
    | FloatToString _ -> Some AST.TString
    | RandomInt64 -> Some AST.TInt64
    | DateTimeNow -> Some AST.TDateTime
    | Sleep _ -> Some AST.TUnit
    | StdoutWrite _ -> Some AST.TUnit
    | StdinReadLine -> Some AST.TString
    | CliNative (operation, _) ->
        match operation with
        | Execute | ProcessIO | TerminateProcess -> Some (AST.TRecord ("Stdlib.Cli.NativeOutput", []))
        | RunProcess -> Some (AST.TRecord ("Stdlib.Cli.NativeProcessOutput", []))
        | GetEnv | GetArgv -> Some (AST.TSum ("Stdlib.Option.Option", [AST.TString]))
        | Kill -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TRecord ("Stdlib.Cli.NativePosixError", [])]))
        | Hostname -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TString; AST.TRecord ("Stdlib.Cli.NativePosixError", [])]))
        | HostOS | HostArchitecture | GetPid | GetUid | CpuCount | SpawnProcess -> Some AST.TInt64
    | IfValue (_, thenAtom, _) -> inferAtomType ctx thenAtom
    | Call (funcName, args)
    | BorrowedCall (funcName, args) ->
        // Return type from function registry (with special-case inference for stdlib list/tuple helpers)
        match funcName, args with
        | name, [listAtom; _] when name.StartsWith("Stdlib.List.getAt") || name.StartsWith("Stdlib.List.__getAt") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.head") || name.StartsWith("Stdlib.List.__head") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.tail") || name.StartsWith("Stdlib.List.__tail") ->
            match inferAtomType ctx listAtom with
            | Some (AST.TList elemType) when name.StartsWith("Stdlib.List.tail") ->
                Some (AST.TSum ("Stdlib.Option.Option", [AST.TList elemType]))
            | Some (AST.TList elemType) ->
                Some (AST.TList elemType)
            | _ -> tryGetFuncReturnTypeFromReg ctx funcName
        | name, [tupleAtom] when name.StartsWith("Stdlib.Tuple2.first") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx tupleAtom with
                | Some (AST.TTuple (firstType :: _)) -> Some firstType
                | _ -> None
        | name, [tupleAtom] when name.StartsWith("Stdlib.Tuple2.second") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx tupleAtom with
                | Some (AST.TTuple (_ :: secondType :: _)) -> Some secondType
                | _ -> None
        | _ ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some t -> Some t
            | None -> tryGetMonomorphizedIntrinsicReturnType ctx funcName
    | TailCall (funcName, _) ->
        // Tail calls have same return type as regular calls
        Map.tryFind funcName ctx.FuncReg
    | IndirectCall (funcAtom, _) ->
        // Look up the function's type to get its return type
        match funcAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TFunction (_, retType)) -> Some retType
            // Raw code pointers are pointer-sized integers after projection
            // from the internal function-closure layout.
            | Some AST.TRawPtr | Some AST.TInt64 -> Some AST.TBool
            | _ -> None
        | _ -> None
    | IndirectTailCall (funcAtom, _) ->
        // Same as IndirectCall
        match funcAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TFunction (_, retType)) -> Some retType
            | Some AST.TRawPtr | Some AST.TInt64 -> Some AST.TBool
            | _ -> None
        | _ -> None
    | ClosureAlloc (funcName, _) ->
        // Closure payload size is resolved by the backend from the function
        // pointer, so ownership insertion must preserve the source function type
        // instead of treating the closure as an ordinary fixed block.
        match Map.tryFind funcName ctx.FuncReg with
        | Some (AST.TFunction _ as funcType) -> Some funcType
        | Some otherType ->
            Crash.crash $"RefCountInsertion: ClosureAlloc target '{funcName}' has non-function type {otherType}"
        | None ->
            Crash.crash $"RefCountInsertion: ClosureAlloc target '{funcName}' not found in function registry"
    | ClosureCall (closureAtom, _) ->
        // Prefer the concrete closure target when available; otherwise use
        // the closure value's function type carried by the ANF type registry.
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> tryGetFuncReturnTypeFromReg ctx funcName
        | None ->
            match closureAtom with
            | Var tid ->
                match tryGetType ctx tid with
                | Some (AST.TFunction (_, retType)) -> Some retType
                | _ -> None
            | _ -> None
    | ClosureTailCall (closureAtom, _) ->
        // Same as ClosureCall
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> tryGetFuncReturnTypeFromReg ctx funcName
        | None ->
            match closureAtom with
            | Var tid ->
                match tryGetType ctx tid with
                | Some (AST.TFunction (_, retType)) -> Some retType
                | _ -> None
            | _ -> None
    | TupleAlloc elems ->
        // Infer element types and create TTuple
        let elemTypes =
            elems
            |> List.map (function
                | UnitLiteral -> AST.TUnit
                | IntLiteral n -> ANF.sizedIntToType n
                | BoolLiteral _ -> AST.TBool
                | StringLiteral _ -> AST.TString
                | FloatLiteral _ -> AST.TFloat64
                | Var tid ->
                    match tryGetType ctx tid with
                    | Some t -> t
                    | None -> Crash.crash $"RefCountInsertion: Type not found for temp {tid} in TupleAlloc"
                | FuncRef funcName ->
                    match Map.tryFind funcName ctx.FuncReg with
                    | Some t -> t
                    | None -> Crash.crash $"RefCountInsertion: Type not found for function {funcName} in TupleAlloc")
        Some (AST.TTuple elemTypes)
    | RecordAlloc (descriptor, _) ->
        Some (AST.TRecord (descriptor.RuntimeTypeName, descriptor.TypeArgs))
    | RecordClone (descriptor, _, _) ->
        Some (AST.TRecord (descriptor.RuntimeTypeName, descriptor.TypeArgs))
    | RecordReuse (descriptor, _, _) ->
        Some (AST.TRecord (descriptor.RuntimeTypeName, descriptor.TypeArgs))
    | RecordGet (descriptor, _, index) ->
        descriptor.Fields
        |> List.tryItem index
        |> Option.map snd
    | TupleGet (tupleAtom, index) ->
        // Get element type from tuple type
        match tupleAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                Some (List.item index elemTypes)
            | Some (AST.TRecord (typeName, _)) ->
                // Record fields - look up field type
                match Map.tryFind typeName ctx.TypeReg with
                | Some recordInfo when index < List.length recordInfo.Fields ->
                    Some (snd (List.item index recordInfo.Fields))
                | _ -> None
            | Some (AST.TList elemType) ->
                // List Cons cells are (tag, head, tail) - index 1 is head, index 2 is tail
                match index with
                | 0 -> Some AST.TInt64  // tag
                | 1 -> Some elemType    // head element
                | 2 -> Some (AST.TList elemType)  // tail is same list type
                | _ -> None
            | Some (AST.TSum (_typeName, typeArgs)) ->
                // Sum type layout: [tag:8][payload:8]
                // index 0 = tag (Int64), index 1 = payload
                match index with
                | 0 -> Some AST.TInt64  // tag
                | 1 ->
                    // Payload type depends on variant, but for simple cases like Option<T>,
                    // the payload type is the first type argument
                    match typeArgs with
                    | [singleType] -> Some singleType
                    | _ -> None
                | _ -> None
            | Some (AST.TFunction _) ->
                // Closures are typed as TFunction but laid out as tuples:
                // [func_ptr:8][cap1:8][cap2:8]...
                // Index 0 is the function pointer (Int64), rest are captures
                Some AST.TInt64  // All closure slots are pointer-sized
            | _ -> None
        | _ -> None
    | StringConcat _ -> Some AST.TString  // String concatenation returns a string
    | RefCountInc (_, _, _, _) -> Some AST.TUnit
    | RefCountDec (_, _, _, _) -> Some AST.TUnit
    | Print _ -> Some AST.TUnit
    | FileReadText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TString; AST.TString]))  // Result<String, String>
    | FileExists _ -> Some AST.TBool  // Bool
    | FileWriteText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileAppendText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileDelete _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileSetExecutable _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileWriteFromPtr _ -> Some AST.TBool  // Returns Bool (success/failure)
    // Raw memory intrinsics (no ref counting - manually managed)
    | RawAlloc _ -> Some AST.TRawPtr  // Returns raw pointer
    | MappedAlloc _ -> Some AST.TRawPtr  // Returns raw pointer
    | RawFree _ -> Some AST.TUnit  // Returns unit
    | MappedFree _ -> Some AST.TUnit  // Returns unit
    | RawGet (_, _, valueType) -> valueType
    | RawTake (_, _, valueType) -> valueType
    | RawGetByte _ -> Some AST.TInt64  // Returns 1-byte value (zero-extended)
    | RawWriteWord _ -> Some AST.TUnit  // Returns unit
    | RawWriteByte _ -> Some AST.TUnit  // Returns unit
    | RawSlotInit _ -> Some AST.TUnit  // Returns unit
    | StringToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToString _ -> Some AST.TString
    | BlobToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToBlob _ -> Some AST.TBlob
    | RawPtrToInt128 _ -> Some AST.TInt128
    | RawPtrToUInt128 _ -> Some AST.TUInt128
    | DictToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToDict (_, _, dictType) -> Some dictType
    | ListToRawPtr _ -> Some AST.TRawPtr
    | FixedBlockToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToList (_, _, listType) -> Some listType
    // Dynamic buffer refcount intrinsics
    | RefCountIncString _ -> Some AST.TUnit  // Returns unit
    | RefCountDecString _ -> Some AST.TUnit  // Returns unit
    | RefCountIncBlob _ -> Some AST.TUnit   // Returns unit
    | RefCountDecBlob _ -> Some AST.TUnit   // Returns unit
    | RuntimeError _ -> Some AST.TUnit
    | RuntimeErrorString _ -> Some AST.TUnit

/// Return analysis annotation for AExpr nodes
