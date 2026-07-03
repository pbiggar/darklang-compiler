// 2.5_RefCountInsertion.fs - Reference Count Insertion Pass
//
// Inserts RefCountInc and RefCountDec operations into ANF.
//
// Design decisions:
// - Borrowed calling convention: callers retain ownership, callees borrow
// - Decrement when heap values go out of scope (end of Let body)
// - Don't decrement returned values (ownership transfers to caller)
// - Increment when extracting heap values from tuples (they become shared)
//
// The pass uses type inference from the conversion result to determine
// which TempIds hold heap-allocated values.
//
// Heap types (reference counted): Tuples, Records, Sum types, Lists, Dicts, Strings
// Stack types (NOT RC'd): Integers, Booleans, Float64, RawPtr
//
// See docs/features/reference-counting.md for detailed documentation.

module RefCountInsertion

open ANF
open AST_to_ANF

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
}

/// Cached CExpr type inference results
type CExprTypeCache = Map<CExpr, AST.Type option>

let emptyCExprTypeCache : CExprTypeCache = Map.empty

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      SumShapeReg = rcSumShapeRegistryFromVariantLookup result.VariantLookup
      FuncReg = result.FuncReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty
      ClosureFuncs = Map.empty }

let private withTempTypes (ctx: TypeContext) (types: Map<TempId, AST.Type>) : TypeContext =
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
        |> Option.defaultValue AST.TInt64
        |> Some
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
    | Atom (UnitLiteral) -> Some AST.TUnit
    | Atom (IntLiteral n) -> Some (ANF.sizedIntToType n)
    | Atom (BoolLiteral _) -> Some AST.TBool
    | Atom (StringLiteral _) -> Some AST.TString
    | Atom (FloatLiteral _) -> Some AST.TFloat64
    | Atom (Var tid) -> tryGetType ctx tid
    | Atom (FuncRef funcName) -> Map.tryFind funcName ctx.FuncReg
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
    | DateNow -> Some AST.TInt64
    | IfValue (_, thenAtom, _) ->
        // Type is the type of the branches (should be the same)
        match thenAtom with
        | Var tid -> tryGetType ctx tid
        | UnitLiteral -> Some AST.TUnit
        | IntLiteral n -> Some (ANF.sizedIntToType n)
        | BoolLiteral _ -> Some AST.TBool
        | StringLiteral _ -> Some AST.TString
        | FloatLiteral _ -> Some AST.TFloat64
        | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg
    | Call (funcName, args)
    | BorrowedCall (funcName, args) ->
        // Return type from function registry (with special-case inference for stdlib list/tuple helpers)
        match funcName, args with
        | name, [listAtom; _] when name.StartsWith("Stdlib.List.getAt") || name.StartsWith("Stdlib.__FingerTree.getAt") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.head") || name.StartsWith("Stdlib.__FingerTree.head") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.tail") || name.StartsWith("Stdlib.__FingerTree.tail") ->
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
            | _ -> None
        | _ -> None
    | IndirectTailCall (funcAtom, _) ->
        // Same as IndirectCall
        match funcAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TFunction (_, retType)) -> Some retType
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
                | Some fields when index < List.length fields ->
                    Some (snd (List.item index fields))
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
    | StringConcat (_, _) -> Some AST.TString  // String concatenation returns a string
    | RefCountInc (_, _, _, _) -> Some AST.TUnit
    | RefCountDec (_, _, _, _) -> Some AST.TUnit
    | Print (_, valueType) -> Some valueType  // Print returns the type it prints
    | FileReadText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TString; AST.TString]))  // Result<String, String>
    | FileExists _ -> Some AST.TBool  // Bool
    | FileWriteText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileAppendText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileDelete _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileSetExecutable _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileWriteFromPtr _ -> Some AST.TBool  // Returns Bool (success/failure)
    // Raw memory intrinsics (no ref counting - manually managed)
    | RawAlloc _ -> Some AST.TRawPtr  // Returns raw pointer
    | RawFree _ -> Some AST.TUnit  // Returns unit
    | RawGet (_, _, valueType) -> valueType
    | RawGetByte _ -> Some AST.TInt64  // Returns 1-byte value (zero-extended)
    | RawWriteWord _ -> Some AST.TUnit  // Returns unit
    | RawWriteByte _ -> Some AST.TUnit  // Returns unit
    | RawSlotInit _ -> Some AST.TUnit  // Returns unit
    | StringToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToString _ -> Some AST.TString
    | BytesToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToBytes _ -> Some AST.TBytes
    | DictToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToDict (_, _, dictType) -> Some dictType
    | ListToRawPtr _ -> Some AST.TRawPtr
    | RawPtrToList (_, _, listType) -> Some listType
    // Dynamic buffer refcount intrinsics
    | RefCountIncString _ -> Some AST.TUnit  // Returns unit
    | RefCountDecString _ -> Some AST.TUnit  // Returns unit
    | RefCountIncBytes _ -> Some AST.TUnit   // Returns unit
    | RefCountDecBytes _ -> Some AST.TUnit   // Returns unit
    | RuntimeError _ -> Some AST.TUnit

/// Return analysis annotation for AExpr nodes
type ReturnAnnotatedExpr =
    | RReturn of Atom * Set<TempId>
    | RLet of TempId * CExpr * ReturnAnnotatedExpr * Set<TempId>
    | RIf of Atom * ReturnAnnotatedExpr * ReturnAnnotatedExpr * Set<TempId>

/// Get the set of returned TempIds for a return-annotated expression
let returnedSet (expr: ReturnAnnotatedExpr) : Set<TempId> =
    match expr with
    | RReturn (_, returned) -> returned
    | RLet (_, _, _, returned) -> returned
    | RIf (_, _, _, returned) -> returned

/// Collect the alias chain for a TempId (includes the TempId itself)
let rec collectAliasChain (aliases: Map<TempId, TempId>) (tempId: TempId) : Set<TempId> =
    match Map.tryFind tempId aliases with
    | Some nextId -> Set.add tempId (collectAliasChain aliases nextId)
    | None -> Set.singleton tempId

/// Analyze return values and track alias chains in a single pass
let rec analyzeReturns
    (aliases: Map<TempId, TempId>)
    (expr: AExpr)
    : ReturnAnnotatedExpr =
    match expr with
    | Return atom ->
        let returned =
            match atom with
            | Var tid -> collectAliasChain aliases tid
            | _ -> Set.empty
        RReturn (atom, returned)
    | Let (tempId, cexpr, body) ->
        let aliases' =
            match cexpr with
            | Atom (Var sourceId) -> Map.add tempId sourceId aliases
            | _ -> aliases
        let bodyInfo = analyzeReturns aliases' body
        RLet (tempId, cexpr, bodyInfo, returnedSet bodyInfo)
    | If (cond, thenBranch, elseBranch) ->
        let thenInfo = analyzeReturns aliases thenBranch
        let elseInfo = analyzeReturns aliases elseBranch
        let returned = Set.union (returnedSet thenInfo) (returnedSet elseInfo)
        RIf (cond, thenInfo, elseInfo, returned)

/// Check if a CExpr is a borrowing/aliasing operation
/// Borrowed/aliased values should NOT get their own RefCountDec - the original value owns the memory
let isBorrowingExpr (cexpr: CExpr) : bool =
    match cexpr with
    | IfValue _ -> true            // Selects one of two existing values; no ownership transfer
    | TupleGet _ -> true           // Extracts pointer from tuple/list - borrowed from parent
    | RawGet _ -> true             // RawGet reads existing memory; it does not transfer ownership
    | StringToRawPtr _ -> true     // RawPtr view is borrowed from the dynamic buffer
    | BytesToRawPtr _ -> true      // RawPtr view is borrowed from the dynamic buffer
    | DictToRawPtr _ -> true       // RawPtr view is borrowed from the tagged container
    | ListToRawPtr _ -> true       // RawPtr view is borrowed from the tagged container
    | Atom (Var _) -> true         // Alias/copy of existing variable - don't double-dec
    | TypedAtom (Var _, _) -> true // TypedAtom wrapping a variable - also borrowed
    | _ -> false

let private canonicalRcTypeForShape (ctx: TypeContext) (typ: AST.Type) : AST.Type =
    let sumTypeNames =
        ctx.VariantLookup
        |> Map.toList
        |> List.map (fun (_, (typeName, _, _, _)) -> typeName)
        |> Set.ofList

    let canonicalBareSum name =
        AST.TSum (name, [])

    let rec canonicalize typ =
        match typ with
        | AST.TRecord (name, []) when Set.contains name sumTypeNames ->
            canonicalBareSum name
        | AST.TSum (name, []) when Set.contains name sumTypeNames ->
            canonicalBareSum name
        | AST.TRecord (name, typeArgs) ->
            AST.TRecord (name, List.map canonicalize typeArgs)
        | AST.TSum (name, typeArgs) ->
            AST.TSum (name, List.map canonicalize typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | AST.TTuple elemTypes ->
            AST.TTuple (List.map canonicalize elemTypes)
        | AST.TList elemType ->
            AST.TList (canonicalize elemType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalize keyType, canonicalize valueType)
        | AST.TVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
        | AST.TBool | AST.TFloat64 | AST.TString | AST.TBytes | AST.TChar
        | AST.TUnit | AST.TRawPtr | AST.TRuntimeError ->
            typ

    canonicalize typ

let private canonicalRcSourceType (ctx: TypeContext) (typ: AST.Type) : AST.Type =
    let sumTypeNames =
        ctx.VariantLookup
        |> Map.toList
        |> List.map (fun (_, (typeName, _, _, _)) -> typeName)
        |> Set.ofList

    let rec canonicalize typ =
        match typ with
        | AST.TRecord (name, []) when Set.contains name sumTypeNames ->
            AST.TSum (name, [])
        | AST.TRecord (name, typeArgs) ->
            AST.TRecord (name, List.map canonicalize typeArgs)
        | AST.TSum (name, typeArgs) ->
            AST.TSum (name, List.map canonicalize typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | AST.TTuple elemTypes ->
            AST.TTuple (List.map canonicalize elemTypes)
        | AST.TList elemType ->
            AST.TList (canonicalize elemType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalize keyType, canonicalize valueType)
        | AST.TVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
        | AST.TBool | AST.TFloat64 | AST.TString | AST.TBytes | AST.TChar
        | AST.TUnit | AST.TRawPtr | AST.TRuntimeError ->
            typ

    canonicalize typ

let private rcShapeForType (ctx: TypeContext) (typ: AST.Type) : RcShape =
    typ
    |> canonicalRcTypeForShape ctx
    |> rcShapeOfTypeWithSums ctx.TypeReg ctx.SumShapeReg

let private rcMetadataForType (ctx: TypeContext) (typ: AST.Type) : RcMetadata =
    let canonicalType = canonicalRcSourceType ctx typ
    let shape = rcShapeForType ctx typ
    {
        ReleasePlan = Some (rcShapeReleasePlan shape)
        SourceType = Some canonicalType
    }

let private shapeNeedsManagedAliasRootPreservation (ctx: TypeContext) (typ: AST.Type) : bool =
    typ |> rcShapeForType ctx |> rcShapeNeedsManagedAliasRootPreservation

let private shapeNeedsAutomaticBindingDec (ctx: TypeContext) (typ: AST.Type) : bool =
    typ |> rcShapeForType ctx |> rcShapeNeedsAutomaticBindingDec

let private bindingNeedsShapeAutomaticDec (ctx: TypeContext) (cexpr: CExpr) (typ: AST.Type) : bool =
    shapeNeedsAutomaticBindingDec ctx typ
    || match typ, cexpr with
       | AST.TFunction _, ClosureAlloc _ -> true
       | AST.TFunction _, Call (funcName, _) when not (funcName.StartsWith("Stdlib.")) -> true
       | AST.TFunction _, ClosureCall _ -> true
       | _ -> false

type private ReturnDec = TempId * AST.Type * RcKind option

let private retainExprForType (ctx: TypeContext) (tempId: TempId) (typ: AST.Type) : CExpr =
    let shape = rcShapeForType ctx typ
    match rcShapeRetainOperation shape with
    | Some DynamicStringBuffer ->
        RefCountIncString (Var tempId)
    | Some DynamicBytesBuffer ->
        RefCountIncBytes (Var tempId)
    | Some (FixedSizeRoot (size, kind)) ->
        RefCountInc (Var tempId, size, kind, Some (rcMetadataForType ctx typ))
    | None ->
        Crash.crash $"retainExprForType: type '{typ}' does not have an RC retain operation"

let private releaseExprForType
    (ctx: TypeContext)
    (tempId: TempId)
    (typ: AST.Type)
    (kindOverride: RcKind option)
    : CExpr =
    let shape = rcShapeForType ctx typ
    match rcShapeReleaseOperation shape with
    | Some DynamicStringBuffer ->
        RefCountDecString (Var tempId)
    | Some DynamicBytesBuffer ->
        RefCountDecBytes (Var tempId)
    | Some (FixedSizeRoot (size, defaultKind)) ->
        let kind = kindOverride |> Option.defaultValue defaultKind
        RefCountDec (Var tempId, size, kind, Some (rcMetadataForType ctx typ))
    | None ->
        Crash.crash $"releaseExprForType: type '{typ}' does not have an RC release operation"

let private shapeNeedsBorrowedRetain (ctx: TypeContext) (typ: AST.Type) : bool =
    typ |> rcShapeForType ctx |> rcShapeNeedsBorrowedRetain

let private functionParamReturnTransfersOwnedAccumulator
    (ctx: TypeContext)
    (funcName: string)
    (paramIndex: int)
    (paramType: AST.Type)
    : bool =
    let isMapHelper =
        funcName = "Stdlib.List.__mapHelper"
        || funcName.StartsWith("Stdlib.List.__mapHelper_")
    let returnsClosureList =
        match tryGetFuncReturnTypeFromReg ctx funcName with
        | Some (AST.TList (AST.TFunction _)) -> true
        | _ -> false

    match isMapHelper, paramIndex, paramType with
    | true, 0, AST.TList _ when returnsClosureList -> true
    | true, 2, AST.TList _ -> true
    | _ -> false

/// Insert RefCountInc for returned parameters at a Return node
let insertParamIncsAtReturn
    (ctx: TypeContext)
    (paramIncs: (TempId * AST.Type) list)
    (returned: Set<TempId>)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let active =
        paramIncs
        |> List.filter (fun (tempId, _) -> Set.contains tempId returned)
    List.foldBack
        (fun (tempId, typ) (accExpr, accVarGen, accTypes) ->
            let (dummyId, varGen') = freshVar accVarGen
            let incExpr = retainExprForType ctx tempId typ
            let accExpr' = Let (dummyId, incExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        active
        (expr, varGen, types)

/// Insert RefCountDec operations before a Return using the current dec stack
let insertReturnDecs
    (ctx: TypeContext)
    (returnDecs: ReturnDec list)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let decsInOrder = List.rev returnDecs
    List.fold
        (fun (accExpr, accVarGen, accTypes) (tempId, typ, kindOverride) ->
            let (dummyId, varGen') = freshVar accVarGen
            let decExpr = releaseExprForType ctx tempId typ kindOverride
            let accExpr' = Let (dummyId, decExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        (expr, varGen, types)
        decsInOrder

/// Stored state for rebuilding a Let while unwinding an expression spine
type LetFrame = {
    TempId: TempId
    CExpr: CExpr
    TupleIncTargets: (TempId * AST.Type) list
    ReturnInc: AST.Type option
    BranchDec: ReturnDec option
}

/// Apply a single Let frame around an expression (uses current varGen/types)
let applyLetFrame
    (ctx: TypeContext)
    (frame: LetFrame)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let (incBindingsRev, varGen1) =
        frame.TupleIncTargets
        |> List.fold (fun (acc, vg) (tid, typ) ->
            let (dummyId, vg') = freshVar vg
            ((dummyId, retainExprForType ctx tid typ) :: acc, vg')) ([], varGen)
    let incBindings = List.rev incBindingsRev

    let typesWithIncs =
        incBindings
        |> List.fold (fun m (tid, _) -> Map.add tid AST.TUnit m) types

    let (returnIncBinding, varGen2, typesWithReturnInc) =
        match frame.ReturnInc with
        | Some typ ->
            let (incId, vg) = freshVar varGen1
            let incExpr = retainExprForType ctx frame.TempId typ
            ([(incId, incExpr)], vg, Map.add incId AST.TUnit typesWithIncs)
        | None ->
            ([], varGen1, typesWithIncs)

    let bodyWithReturnInc = wrapBindings returnIncBinding expr
    let letExpr = Let (frame.TempId, frame.CExpr, bodyWithReturnInc)
    let exprWithIncs = wrapBindings incBindings letExpr
    (exprWithIncs, varGen2, typesWithReturnInc)

/// Apply a stack of Let frames (innermost-first)
let applyLetFrames
    (ctx: TypeContext)
    (frames: LetFrame list)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let folder
        ((accExpr, accVarGen, accTypes): AExpr * VarGen * Map<TempId, AST.Type>)
        (frame: LetFrame)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        applyLetFrame ctx frame (accExpr, accVarGen, accTypes)
    List.fold folder (expr, varGen, types) frames

let private tailCallArgTempIds (cexpr: CExpr) : Set<TempId> =
    let fromAtom (atom: Atom) : Set<TempId> =
        match atom with
        | Var tid -> Set.singleton tid
        | _ -> Set.empty
    match cexpr with
    | TailCall (_, args) ->
        args |> List.fold (fun acc atom -> Set.union acc (fromAtom atom)) Set.empty
    | IndirectTailCall (func, args) ->
        (fromAtom func, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | ClosureTailCall (closure, args) ->
        (fromAtom closure, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | _ ->
        Set.empty

let private isSelfTailCallTarget (currentFuncName: string) (targetFunc: string) : bool =
    targetFunc = currentFuncName
    || targetFunc.StartsWith($"{currentFuncName}_")

let rec private isTempUsedAsSelfTailCallArg
    (currentFuncName: string)
    (targetTemp: TempId)
    (expr: ReturnAnnotatedExpr)
    : bool =
    let rec loop (aliases: Set<TempId>) (expr: ReturnAnnotatedExpr) : bool =
        let argsContainAlias (args: Atom list) : bool =
            args
            |> List.exists (function
                | Var tempId -> Set.contains tempId aliases
                | _ -> false)

        match expr with
        | RReturn _ ->
            false
        | RLet (_, Call (targetFunc, args), _, _)
            when isSelfTailCallTarget currentFuncName targetFunc && argsContainAlias args ->
            true
        | RLet (_, TailCall (targetFunc, args), _, _)
            when isSelfTailCallTarget currentFuncName targetFunc && argsContainAlias args ->
            true
        | RLet (aliasTemp, Atom (Var sourceTemp), body, _)
        | RLet (aliasTemp, TypedAtom (Var sourceTemp, _), body, _)
            when Set.contains sourceTemp aliases ->
            loop (Set.add aliasTemp aliases) body
        | RLet (_, _, body, _) ->
            loop aliases body
        | RIf (_, thenBranch, elseBranch, _) ->
            loop aliases thenBranch || loop aliases elseBranch

    loop (Set.singleton targetTemp) expr

let rec private collectMovableTailDecPrefix
    (tailArgTemps: Set<TempId>)
    (expr: AExpr)
    : (TempId * CExpr) list * AExpr =
    match expr with
    | Let (tmpId, RefCountDec (Var tid, size, kind, sourceType), rest) when not (Set.contains tid tailArgTemps) ->
        let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
        ((tmpId, RefCountDec (Var tid, size, kind, sourceType)) :: bindings, remaining)
    | Let (tmpId, RefCountDecString atom, rest) ->
        let overlaps =
            match atom with
            | Var tid -> Set.contains tid tailArgTemps
            | _ -> false
        if overlaps then
            ([], expr)
        else
            let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
            ((tmpId, RefCountDecString atom) :: bindings, remaining)
    | Let (tmpId, RefCountDecBytes atom, rest) ->
        let overlaps =
            match atom with
            | Var tid -> Set.contains tid tailArgTemps
            | _ -> false
        if overlaps then
            ([], expr)
        else
            let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
            ((tmpId, RefCountDecBytes atom) :: bindings, remaining)
    | _ ->
        ([], expr)

let rec private moveDecsBeforeNonSelfTailCalls (currentFuncName: string) (expr: AExpr) : AExpr =
    match expr with
    | Return _ ->
        expr
    | If (cond, thenBranch, elseBranch) ->
        If (
            cond,
            moveDecsBeforeNonSelfTailCalls currentFuncName thenBranch,
            moveDecsBeforeNonSelfTailCalls currentFuncName elseBranch
        )
    | Let (tempId, cexpr, body) ->
        let body' = moveDecsBeforeNonSelfTailCalls currentFuncName body
        match cexpr with
        | TailCall (targetFunc, _) when targetFunc <> currentFuncName ->
            let tailArgTemps = tailCallArgTempIds cexpr
            let (movableDecs, remainingBody) = collectMovableTailDecPrefix tailArgTemps body'
            let tailLet = Let (tempId, cexpr, remainingBody)
            wrapBindings movableDecs tailLet
        | _ ->
            Let (tempId, cexpr, body')

let rec private insertOwnedAccumulatorDecsBeforeSelfTailCalls
    (ctx: TypeContext)
    (currentFuncName: string)
    (ownedParamDecs: ReturnDec list)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let decsForSelfTailCall (args: Atom list) : ReturnDec list =
        let argTemps =
            args
            |> List.fold (fun acc atom ->
                match atom with
                | Var tempId -> Set.add tempId acc
                | _ -> acc) Set.empty

        ownedParamDecs
        |> List.filter (fun (tempId, _, _) -> not (Set.contains tempId argTemps))

    let wrapOwnedAccumulatorDecs
        (decs: ReturnDec list)
        (tailExpr: AExpr)
        (varGen: VarGen)
        (types: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        decs
        |> List.fold
            (fun (accExpr, accVarGen, accTypes) (tempId, typ, kindOverride) ->
                let (dummyId, varGen') = freshVar accVarGen
                let decExpr = releaseExprForType ctx tempId typ kindOverride
                (Let (dummyId, decExpr, accExpr), varGen', Map.add dummyId AST.TUnit accTypes))
            (tailExpr, varGen, types)

    match expr with
    | Return _ ->
        (expr, varGen, types)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs thenBranch varGen types
        let (elseBranch', varGen2, types2) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs elseBranch varGen1 types1
        (If (cond, thenBranch', elseBranch'), varGen2, types2)
    | Let (tempId, Call (targetFunc, args), body) when isSelfTailCallTarget currentFuncName targetFunc ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        let callExpr = Let (tempId, Call (targetFunc, args), body')
        wrapOwnedAccumulatorDecs (decsForSelfTailCall args) callExpr varGen1 types1
    | Let (tempId, TailCall (targetFunc, args), body) when isSelfTailCallTarget currentFuncName targetFunc ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        let tailExpr = Let (tempId, TailCall (targetFunc, args), body')
        wrapOwnedAccumulatorDecs (decsForSelfTailCall args) tailExpr varGen1 types1
    | Let (tempId, cexpr, body) ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        (Let (tempId, cexpr, body'), varGen1, types1)

let rec private insertClosureMapSourceRetainsBeforeHelperCalls
    (ctx: TypeContext)
    (currentFuncName: string)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let currentIsMapHelper =
        currentFuncName = "Stdlib.List.__mapHelper"
        || currentFuncName.StartsWith("Stdlib.List.__mapHelper_")

    let isMapHelperTarget (targetFunc: string) : bool =
        targetFunc = "Stdlib.List.__mapHelper"
        || targetFunc.StartsWith("Stdlib.List.__mapHelper_")

    let targetReturnsClosureList (targetFunc: string) : bool =
        match tryGetFuncReturnTypeFromReg ctx targetFunc with
        | Some (AST.TList (AST.TFunction _)) -> true
        | _ -> false

    let wrapSourceRetain
        (targetFunc: string)
        (args: Atom list)
        (callExpr: AExpr)
        (varGen: VarGen)
        (types: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        match currentIsMapHelper, targetReturnsClosureList targetFunc, args with
        | false, true, Var sourceTemp :: _ ->
            match tryGetType (withTempTypes ctx types) sourceTemp with
            | Some sourceType when shapeNeedsBorrowedRetain ctx sourceType ->
                let (dummyId, varGen') = freshVar varGen
                let incExpr = retainExprForType ctx sourceTemp sourceType
                (Let (dummyId, incExpr, callExpr), varGen', Map.add dummyId AST.TUnit types)
            | _ ->
                (callExpr, varGen, types)
        | _ ->
            (callExpr, varGen, types)

    match expr with
    | Return _ ->
        (expr, varGen, types)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName thenBranch varGen types
        let (elseBranch', varGen2, types2) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName elseBranch varGen1 types1
        (If (cond, thenBranch', elseBranch'), varGen2, types2)
    | Let (tempId, Call (targetFunc, args), body) when isMapHelperTarget targetFunc ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        let callExpr = Let (tempId, Call (targetFunc, args), body')
        wrapSourceRetain targetFunc args callExpr varGen1 types1
    | Let (tempId, TailCall (targetFunc, args), body) when isMapHelperTarget targetFunc ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        let callExpr = Let (tempId, TailCall (targetFunc, args), body')
        wrapSourceRetain targetFunc args callExpr varGen1 types1
    | Let (tempId, cexpr, body) ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        (Let (tempId, cexpr, body'), varGen1, types1)

/// Insert reference counting operations using return analysis and a dec stack
/// Returns (transformed expr, varGen, types defined in this subtree)
let rec insertRCWithAnalysis
    (ctx: TypeContext)
    (currentFuncName: string option)
    (expr: ReturnAnnotatedExpr)
    (varGen: VarGen)
    (returnDecs: ReturnDec list)
    (paramIncs: (TempId * AST.Type) list)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache =
    let ctxWithTypes = withTempTypes ctx types
    let rec descend
        (ctx: TypeContext)
        (expr: ReturnAnnotatedExpr)
        (varGen: VarGen)
        (returnDecs: ReturnDec list)
        (frames: LetFrame list)
        (types: Map<TempId, AST.Type>)
        (typeCache: CExprTypeCache)
        : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache =
        match expr with
        | RReturn (atom, returned) ->
            let baseExpr = Return atom
            let (withParamIncs, varGen1, types1) =
                insertParamIncsAtReturn ctx paramIncs returned baseExpr varGen types
            let (withDecs, varGen2, types2) = insertReturnDecs ctx returnDecs withParamIncs varGen1 types1
            let (finalExpr, finalVarGen, finalTypes) = applyLetFrames ctx frames (withDecs, varGen2, types2)
            (finalExpr, finalVarGen, finalTypes, typeCache)

        | RIf (cond, thenBranch, elseBranch, _) ->
            let returnDecTemps =
                returnDecs
                |> List.map (fun (tempId, _, _) -> tempId)
                |> Set.ofList
            let branchLocalDecs (branchReturned: Set<TempId>) : ReturnDec list =
                let frameDecs =
                    frames
                    |> List.choose (fun frame ->
                        match frame.BranchDec with
                        | Some (tempId, _, _ as dec)
                            when not (Set.contains tempId branchReturned)
                                 && not (Set.contains tempId returnDecTemps) ->
                            Some dec
                        | _ ->
                            None)
                frameDecs
            let (thenBranch', varGen1, types1, typeCache1) =
                insertRCWithAnalysis
                    ctx
                    currentFuncName
                    thenBranch
                    varGen
                    (branchLocalDecs (returnedSet thenBranch) @ returnDecs)
                    paramIncs
                    types
                    typeCache
            let (elseBranch', varGen2, types2, typeCache2) =
                insertRCWithAnalysis
                    ctx
                    currentFuncName
                    elseBranch
                    varGen1
                    (branchLocalDecs (returnedSet elseBranch) @ returnDecs)
                    paramIncs
                    types1
                    typeCache1
            let (finalExpr, finalVarGen, finalTypes) =
                applyLetFrames ctx frames (If (cond, thenBranch', elseBranch'), varGen2, types2)
            (finalExpr, finalVarGen, finalTypes, typeCache2)

        | RLet (tempId, cexpr, bodyInfo, _) ->
            let (TempId tempIdInt) = tempId

            // First, infer the type of this binding and add to context
            let (maybeType, typeCache1) =
                match Map.tryFind cexpr typeCache with
                | Some cached -> (cached, typeCache)
                | None ->
                    let inferred = inferCExprType ctx cexpr
                    (inferred, Map.add cexpr inferred typeCache)

            // When a temp is aliased through one or more let-bound vars, infer its type
            // from the first concrete use-site (typically a call argument position).
            let rec inferAliasedVarTypeFromUse (aliasedTemp: TempId) (nextBody: ReturnAnnotatedExpr) : AST.Type option =
                let inferFromCall (funcName: string) (args: Atom list) : AST.Type option =
                    match Map.tryFind funcName ctx.FuncReg with
                    | Some (AST.TFunction (paramTypes, _)) ->
                        args
                        |> List.mapi (fun idx atom -> (idx, atom))
                        |> List.tryPick (fun (idx, atom) ->
                            match atom with
                            | Var tid when tid = aliasedTemp && idx < List.length paramTypes ->
                                Some (List.item idx paramTypes)
                            | _ ->
                                None)
                    | _ ->
                        None

                match nextBody with
                | RLet (_, RawSlotInit (_, _, Var valueTemp, valueType), _, _) when valueTemp = aliasedTemp ->
                    Some valueType
                | RLet (_, Call (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (_, BorrowedCall (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (_, TailCall (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (nextAliasTemp, Atom (Var sourceId), nextNextBody, _) when sourceId = aliasedTemp ->
                    inferAliasedVarTypeFromUse nextAliasTemp nextNextBody
                | RLet (nextAliasTemp, TypedAtom (Var sourceId, aliasType), nextNextBody, _) when sourceId = aliasedTemp ->
                    if shapeNeedsManagedAliasRootPreservation ctx aliasType then
                        Some aliasType
                    else
                        inferAliasedVarTypeFromUse nextAliasTemp nextNextBody
                | RIf (Var tid, _, _, _) when tid = aliasedTemp ->
                    Some AST.TBool
                | _ ->
                    None

            // Use a TypedAtom alias in the body to preserve the intended payload type
            // when TupleGet cannot infer it from a multi-parameter sum.
            let inferredType =
                match maybeType with
                | Some t ->
                    let aliasTypeFromBody =
                        match bodyInfo with
                        | RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                            Some aliasType
                        | RLet (aliasTemp, Atom (Var sourceId), nextBody, _) when sourceId = tempId ->
                            inferAliasedVarTypeFromUse aliasTemp nextBody
                        | _ ->
                            None

                    match aliasTypeFromBody with
                    | Some inferredAliasType when shapeNeedsManagedAliasRootPreservation ctx inferredAliasType && not (shapeNeedsManagedAliasRootPreservation ctx t) ->
                        inferredAliasType
                    | _ ->
                        t
                | None ->
                    match cexpr, bodyInfo with
                    | TupleGet _, RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                        aliasType
                    | RawGet (_, _, None), RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                        // RawGet without an explicit type is often immediately re-typed via TypedAtom.
                        // Preserve that alias type instead of guessing Int64.
                        aliasType
                    | RawGet (_, _, None), RLet (aliasTemp, Atom (Var sourceId), nextBody, _) when sourceId = tempId ->
                        match inferAliasedVarTypeFromUse aliasTemp nextBody with
                        | Some inferredAliasType ->
                            inferredAliasType
                        | None ->
                            // Keep unresolved rather than guessing Int64.
                            AST.TVar $"raw_get_{tempIdInt}"
                    | RawGet (_, _, None), _ ->
                        // Unknown RawGet payload type: preserve as unresolved type variable.
                        AST.TVar $"raw_get_{tempIdInt}"
                    | _ ->
                        // Preserve unresolved type information instead of defaulting to Int64.
                        AST.TVar $"inferred_{tempIdInt}"

            let typesWithBinding =
                match cexpr with
                | TypedAtom (Var sourceId, aliasType) ->
                    types |> Map.add tempId inferredType |> Map.add sourceId aliasType
                | _ ->
                    Map.add tempId inferredType types

            let ctxWithTypes = withTempTypes ctx typesWithBinding

            // Track closure function names for later ClosureCall type resolution
            let ctx'' =
                match cexpr with
                | ClosureAlloc (funcName, _) -> addClosureFunc ctxWithTypes tempId funcName
                | _ -> ctxWithTypes

            let bodyReturned = returnedSet bodyInfo
            let consumedByImmediateI64Push =
                let isI64Push (funcName: string) : bool =
                    funcName = "Stdlib.__FingerTree.push_i64"
                    || funcName = "Stdlib.__FingerTree.pushBack_i64"
                let consumesSecondArg (args: Atom list) : bool =
                    match args with
                    | _listAtom :: Var valueTemp :: _ -> valueTemp = tempId
                    | _ -> false
                match bodyInfo with
                | RLet (_, Call (funcName, args), _, _)
                | RLet (_, TailCall (funcName, args), _, _) ->
                    isI64Push funcName && consumesSecondArg args
                | _ ->
                    false
            let secondParamNeedsOwnershipTransfer (funcName: string) : bool =
                let isOwnershipTransferredParamType (typ: AST.Type) : bool =
                    typ |> rcShapeForType ctx |> rcShapeIsOwnershipTransferRoot
                match Map.tryFind funcName ctx.FuncReg with
                | Some (AST.TFunction (paramTypes, _)) ->
                    match paramTypes with
                    | _ :: secondParamType :: _ -> isOwnershipTransferredParamType secondParamType
                    | _ -> false
                | _ ->
                    false
            let skipReturnDecForMapHelperLists =
                match currentFuncName with
                | Some funcName ->
                    let isMapHelper =
                        funcName = "Stdlib.List.__mapHelper"
                        || funcName.StartsWith("Stdlib.List.__mapHelper_")
                    isMapHelper
                    && secondParamNeedsOwnershipTransfer funcName
                    && match inferredType with
                       | AST.TList _ -> true
                       | _ -> false
                | None ->
                    false
            let closureOwnershipTransferredToRawStorage = false
            let returnDecs' =
                let functionReturnsNestedRecordListDict =
                    let isSingleListDictRecord (name: string) : bool =
                        ctx.TypeReg
                        |> Map.tryFind name
                        |> Option.map (fun fields ->
                            match fields |> List.map snd with
                            | [ AST.TList (AST.TDict _) ] -> true
                            | _ -> false)
                        |> Option.defaultValue false

                    match currentFuncName |> Option.bind (tryGetFuncReturnTypeFromReg ctx) with
                    | Some (AST.TList (AST.TRecord (name, _))) ->
                        isSingleListDictRecord name
                    | _ ->
                        false
                let needsNestedRecordListDictDictDec =
                    functionReturnsNestedRecordListDict
                    && match inferredType with
                       | AST.TDict (AST.TInt64, AST.TInt64) -> true
                       | _ -> false

                if bindingNeedsShapeAutomaticDec ctx cexpr inferredType
                   && not (Set.contains tempId bodyReturned)
                   && not (isBorrowingExpr cexpr)
                   && not skipReturnDecForMapHelperLists
                   && not consumedByImmediateI64Push
                   && not closureOwnershipTransferredToRawStorage then
                    let kindOverride =
                        match inferredType with
                        | AST.TList (AST.TFunction _) -> Some TaggedList
                        | _ -> None
                    let dec = (tempId, inferredType, kindOverride)
                    if needsNestedRecordListDictDictDec then
                        // List<Record { List<Dict<Int64, Int64>> }> construction retains the
                        // dict once for the inner list payload and once for the returned graph.
                        // The current shape-specific ARM64 helpers release that graph, but the
                        // local dict temp still needs both ownership edges balanced.
                        dec :: dec :: returnDecs
                    else
                        dec :: returnDecs
                else
                    returnDecs

            let tupleIncTargets =
                match cexpr with
                | TupleAlloc elems ->
                    elems
                    |> List.fold (fun acc atom ->
                        match atom with
                        | Var tid ->
                            match tryGetType ctx tid with
                            | Some t when shapeNeedsBorrowedRetain ctx t ->
                                (tid, t) :: acc
                            | _ -> acc
                        | _ -> acc
                    ) []
                    |> List.rev
                | ClosureAlloc (_, captures) ->
                    captures
                    |> List.fold (fun acc atom ->
                        match atom with
                        | Var tid ->
                            match tryGetType ctx tid with
                            | Some t when shapeNeedsBorrowedRetain ctx t ->
                                (tid, t) :: acc
                            | _ -> acc
                        | _ -> acc
                    ) []
                    |> List.rev
                | _ -> []

            let returnInc =
                let retainedTypeFromAtom (atom: Atom) : AST.Type option =
                    match atom with
                    | Var tid ->
                        match tryGetType ctx tid with
                        | Some t when shapeNeedsBorrowedRetain ctx t -> Some t
                        | _ -> None
                    | _ -> None

                let borrowedProjectionFeedsSelfTailCall =
                    let sourceParentIsOwnedLocal (sourceId: TempId) : bool =
                        let rec loop (visited: Set<TempId>) (candidateId: TempId) : bool =
                            if Set.contains candidateId visited then
                                false
                            else
                                frames
                                |> List.tryFind (fun frame -> frame.TempId = candidateId)
                                |> Option.exists (fun frame ->
                                    match frame.CExpr with
                                    | Atom (Var aliasedId)
                                    | TypedAtom (Var aliasedId, _) ->
                                        loop (Set.add candidateId visited) aliasedId
                                    | _ ->
                                        not (isBorrowingExpr frame.CExpr))

                        loop Set.empty sourceId

                    match currentFuncName, cexpr with
                    | Some funcName, TupleGet (Var sourceId, _) ->
                        sourceParentIsOwnedLocal sourceId
                        && isTempUsedAsSelfTailCallArg funcName tempId bodyInfo
                    | _ ->
                        false

                match cexpr with
                | IfValue (_, thenAtom, elseAtom) ->
                    // IfValue selects one of two existing heap values.
                    // Materialize ownership on the selected temp before source temps are decref'd.
                    match retainedTypeFromAtom thenAtom, retainedTypeFromAtom elseAtom with
                    | Some info, _ -> Some info
                    | None, Some info -> Some info
                    | None, None -> None
                | _ when borrowedProjectionFeedsSelfTailCall
                         && shapeNeedsBorrowedRetain ctx inferredType ->
                    Some inferredType
                | Atom (Var sourceId)
                | TypedAtom (Var sourceId, _) ->
                    // Returning a pure alias of an already-returned owned value should not inc again.
                    if shapeNeedsBorrowedRetain ctx inferredType
                       && Set.contains tempId bodyReturned
                       && isBorrowingExpr cexpr then
                        if Set.contains sourceId bodyReturned then
                            None
                        else
                            Some inferredType
                    else
                        None
                | _ ->
                    if shapeNeedsBorrowedRetain ctx inferredType
                       && Set.contains tempId bodyReturned
                       && isBorrowingExpr cexpr then
                        Some inferredType
                    else
                        None

            let frame = {
                TempId = tempId
                CExpr = cexpr
                TupleIncTargets = tupleIncTargets
                ReturnInc = returnInc
                BranchDec =
                    if bindingNeedsShapeAutomaticDec ctx cexpr inferredType
                       && not (isBorrowingExpr cexpr)
                       && not skipReturnDecForMapHelperLists
                       && not consumedByImmediateI64Push
                       && not closureOwnershipTransferredToRawStorage then
                        let kindOverride =
                            match inferredType with
                            | AST.TList (AST.TFunction _) -> Some TaggedList
                            | _ -> None
                        Some (tempId, inferredType, kindOverride)
                    else
                        None
            }

            // Process the body iteratively, then rebuild on the way back out
            descend ctx'' bodyInfo varGen returnDecs' (frame :: frames) typesWithBinding typeCache1

    descend ctxWithTypes expr varGen returnDecs [] types typeCache

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let private insertRCInternal
    (ctx: TypeContext)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache =
    let ctxWithTypes = withTempTypes ctx types
    let analyzed = analyzeReturns Map.empty expr
    let (expr', varGen', types', typeCache') =
        insertRCWithAnalysis ctxWithTypes None analyzed varGen [] [] types typeCache
    (expr', varGen', types', typeCache')

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen * Map<TempId, AST.Type> =
    let (expr', varGen', types', _typeCache) =
        insertRCInternal ctx expr varGen Map.empty emptyCExprTypeCache
    (expr', varGen', types')

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let private insertRCInFunctionInternal
    (ctx: TypeContext)
    (func: Function)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : Function * VarGen * Map<TempId, AST.Type> * CExprTypeCache =
    let typesWithParams =
        func.TypedParams
        |> List.fold (fun m tp -> Map.add tp.Id tp.Type m) types
    let ctxWithParams = withTempTypes ctx typesWithParams

    let bodyInfo = analyzeReturns Map.empty func.Body
    let paramIncsRev =
        func.TypedParams
        |> List.mapi (fun index param -> (index, param))
        |> List.fold (fun acc (index, param) ->
            if shapeNeedsBorrowedRetain ctxWithParams param.Type
               && not (functionParamReturnTransfersOwnedAccumulator ctxWithParams func.Name index param.Type) then
                (param.Id, param.Type) :: acc
            else
                acc
        ) []
    let paramIncs = List.rev paramIncsRev
    let ownedParamDecs =
        func.TypedParams
        |> List.mapi (fun index param -> (index, param))
        |> List.choose (fun (index, param) ->
            if functionParamReturnTransfersOwnedAccumulator ctxWithParams func.Name index param.Type then
                Some (param.Id, param.Type, None)
            else
                None)

    // Process function body with return analysis
    let (bodyWithRC, varGen', accTypes, typeCache') =
        insertRCWithAnalysis ctxWithParams (Some func.Name) bodyInfo varGen [] paramIncs typesWithParams typeCache
    let (bodyWithOwnedAccumulatorDecs, varGen'', accTypes') =
        insertOwnedAccumulatorDecsBeforeSelfTailCalls ctxWithParams func.Name ownedParamDecs bodyWithRC varGen' accTypes
    let (bodyWithClosureMapSourceRetains, varGen''', accTypes'') =
        insertClosureMapSourceRetainsBeforeHelperCalls
            ctxWithParams
            func.Name
            bodyWithOwnedAccumulatorDecs
            varGen''
            accTypes'
    let body' = moveDecsBeforeNonSelfTailCalls func.Name bodyWithClosureMapSourceRetains
    ({ func with Body = body' }, varGen''', accTypes'', typeCache')

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen * Map<TempId, AST.Type> =
    let (func', varGen', types', _typeCache) =
        insertRCInFunctionInternal ctx func varGen Map.empty emptyCExprTypeCache
    (func', varGen', types')

// ============================================================================
// TypeMap Completeness Verification
// ============================================================================

let private isTempMissing (typeMap: ANF.TypeMap) (tempId: TempId) : bool =
    not (Map.containsKey tempId typeMap)

let rec collectMissingTempIdsInExpr
    (typeMap: ANF.TypeMap)
    (expr: AExpr)
    (acc: TempId list)
    : TempId list =
    match expr with
    | Return _ -> acc
    | Let (tempId, _, body) ->
        let acc' = if isTempMissing typeMap tempId then tempId :: acc else acc
        collectMissingTempIdsInExpr typeMap body acc'
    | If (_, thenBranch, elseBranch) ->
        let acc' = collectMissingTempIdsInExpr typeMap thenBranch acc
        collectMissingTempIdsInExpr typeMap elseBranch acc'

let collectMissingTempIdsInFunction
    (typeMap: ANF.TypeMap)
    (func: Function)
    (acc: TempId list)
    : TempId list =
    let acc' =
        func.TypedParams
        |> List.fold (fun acc tp -> if isTempMissing typeMap tp.Id then tp.Id :: acc else acc) acc
    collectMissingTempIdsInExpr typeMap func.Body acc'

/// Verify that all defined TempIds have types in the TypeMap
/// Returns a list of TempIds that are missing from the TypeMap
let verifyTypeMapCompleteness (program: ANF.Program) (typeMap: ANF.TypeMap) : TempId list =
    let (ANF.Program (functions, mainExpr)) = program
    let missing =
        functions
        |> List.fold (fun acc func -> collectMissingTempIdsInFunction typeMap func acc) []
        |> collectMissingTempIdsInExpr typeMap mainExpr
    List.rev missing

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let private insertRCInProgramInternal
    (result: ConversionResult)
    : Result<ANF.Program * ANF.TypeMap, string> =
    let ctx = createContext result
    let (ANF.Program (functions, mainExpr)) = result.Program
    let varGen = VarGen 1000  // Start high to avoid conflicts

    // Process all functions, accumulating types
    let rec processFuncs
        (funcs: Function list)
        (vg: VarGen)
        (accFuncs: Function list)
        (accTypes: Map<TempId, AST.Type>)
        : Function list * VarGen * Map<TempId, AST.Type> =
        match funcs with
        | [] -> (List.rev accFuncs, vg, accTypes)
        | f :: rest ->
            // Cache keys use TempIds from the current function body, so sharing
            // across functions can reuse stale types for unrelated TempIds.
            let (f', vg', types, _typeCache) =
                insertRCInFunctionInternal ctx f vg Map.empty emptyCExprTypeCache
            let accTypes' =
                Map.fold (fun acc tempId typ -> Map.add tempId typ acc) accTypes types
            processFuncs rest vg' (f' :: accFuncs) accTypes'

    let (functions', varGen1, typesFromFuncs) =
        processFuncs functions varGen [] Map.empty

    // Process main expression
    let (mainExpr', _, finalTypeMap, _typeCache) =
        insertRCInternal ctx mainExpr varGen1 typesFromFuncs emptyCExprTypeCache

    // Verify TypeMap completeness - all defined TempIds should have types
    let program' = ANF.Program (functions', mainExpr')
    let missingTypes = verifyTypeMapCompleteness program' finalTypeMap
    if not (List.isEmpty missingTypes) then
        let missingStr = missingTypes |> List.map (fun (TempId n) -> $"t{n}") |> String.concat ", "
        Crash.crash $"RefCountInsertion: TypeMap incomplete - missing types for: {missingStr}"

    Ok (program', finalTypeMap)

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap, string> =
    insertRCInProgramInternal result
