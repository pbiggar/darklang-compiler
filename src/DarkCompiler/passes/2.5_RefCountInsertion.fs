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
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    /// Maps TempId -> Type for values we've seen
    TempTypes: Map<TempId, AST.Type>
    /// Maps TempId -> function name for closures (to resolve closure call return types)
    ClosureFuncs: Map<TempId, string>
}

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      FuncReg = result.FuncReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty
      ClosureFuncs = Map.empty }

/// Add a TempId -> Type mapping to context
let addType (ctx: TypeContext) (tempId: TempId) (typ: AST.Type) : TypeContext =
    { ctx with TempTypes = Map.add tempId typ ctx.TempTypes }

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

/// Infer the type of a CExpr in the given context
let inferCExprType (ctx: TypeContext) (cexpr: CExpr) : AST.Type option =
    match cexpr with
    | Atom (UnitLiteral) -> Some AST.TUnit
    | Atom (IntLiteral _) -> Some AST.TInt64
    | Atom (BoolLiteral _) -> Some AST.TBool
    | Atom (StringLiteral _) -> Some AST.TString
    | Atom (FloatLiteral _) -> Some AST.TFloat64
    | Atom (Var tid) -> tryGetType ctx tid
    | Atom (FuncRef funcName) -> Map.tryFind funcName ctx.FuncReg
    | Prim (op, _, _) ->
        // Binary ops return int or bool depending on op
        match op with
        | Add | Sub | Mul | Div | Mod | Shl | Shr | BitAnd | BitOr | BitXor -> Some AST.TInt64
        | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Some AST.TBool
    | UnaryPrim (op, _) ->
        match op with
        | Neg -> Some AST.TInt64
        | Not -> Some AST.TBool
        | BitNot -> Some AST.TInt64
    | IfValue (_, thenAtom, _) ->
        // Type is the type of the branches (should be the same)
        match thenAtom with
        | Var tid -> tryGetType ctx tid
        | UnitLiteral -> Some AST.TUnit
        | IntLiteral _ -> Some AST.TInt64
        | BoolLiteral _ -> Some AST.TBool
        | StringLiteral _ -> Some AST.TString
        | FloatLiteral _ -> Some AST.TFloat64
        | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg
    | Call (funcName, _) ->
        // Return type from function registry
        Map.tryFind funcName ctx.FuncReg
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
    | ClosureAlloc (_, captures) ->
        // Closure is a tuple-like structure: (func_ptr, cap1, cap2, ...)
        // Return a tuple type for ref counting purposes
        Some (AST.TTuple (AST.TInt64 :: List.replicate (List.length captures) AST.TInt64))
    | ClosureCall (closureAtom, _) ->
        // Try to find the closure's function name and look up return type
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> Map.tryFind funcName ctx.FuncReg
        | None -> None
    | ClosureTailCall (closureAtom, _) ->
        // Same as ClosureCall
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> Map.tryFind funcName ctx.FuncReg
        | None -> None
    | TupleAlloc elems ->
        // Infer element types and create TTuple
        let elemTypes =
            elems
            |> List.map (function
                | UnitLiteral -> AST.TUnit
                | IntLiteral _ -> AST.TInt64
                | BoolLiteral _ -> AST.TBool
                | StringLiteral _ -> AST.TString
                | FloatLiteral _ -> AST.TFloat64
                | Var tid ->
                    match tryGetType ctx tid with
                    | Some t -> t
                    | None -> failwith $"RefCountInsertion: Type not found for temp {tid} in TupleAlloc"
                | FuncRef funcName ->
                    match Map.tryFind funcName ctx.FuncReg with
                    | Some t -> t
                    | None -> failwith $"RefCountInsertion: Type not found for function {funcName} in TupleAlloc")
        Some (AST.TTuple elemTypes)
    | TupleGet (tupleAtom, index) ->
        // Get element type from tuple type
        match tupleAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                Some (List.item index elemTypes)
            | Some (AST.TRecord typeName) ->
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
                    | _ -> None  // Multiple type args - can't determine without variant info
                | _ -> None
            | _ -> None
        | _ -> None
    | StringConcat (_, _) -> Some AST.TString  // String concatenation returns a string
    | RefCountInc (_, _) -> Some AST.TUnit
    | RefCountDec (_, _) -> Some AST.TUnit
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
    | RawGet _ -> Some AST.TInt64  // Returns 8-byte value
    | RawGetByte _ -> Some AST.TInt64  // Returns 1-byte value (zero-extended)
    | RawSet _ -> Some AST.TUnit  // Returns unit
    | RawSetByte _ -> Some AST.TUnit  // Returns unit
    | RandomInt64 -> Some AST.TInt64  // Returns random Int64
    // Float intrinsics
    | FloatSqrt _ -> Some AST.TFloat64
    | FloatAbs _ -> Some AST.TFloat64
    | FloatNeg _ -> Some AST.TFloat64
    | IntToFloat _ -> Some AST.TFloat64
    | FloatToInt _ -> Some AST.TInt64
    | FloatToString _ -> Some AST.TString  // Returns heap String
    // String intrinsics
    | StringHash _ -> Some AST.TInt64  // Returns hash as Int64
    | StringEq _ -> Some AST.TBool  // Returns Bool
    // String refcount intrinsics
    | RefCountIncString _ -> Some AST.TUnit  // Returns unit
    | RefCountDecString _ -> Some AST.TUnit  // Returns unit

/// Check if sourceId is transitively an alias of targetId
/// The aliases map is: aliases[x] = y means "x is an alias of y" (from Let x = Atom (Var y))
/// We follow the chain from sourceId to see if it reaches targetId
let rec isTransitiveAliasOf (aliases: Map<TempId, TempId>) (sourceId: TempId) (targetId: TempId) : bool =
    if sourceId = targetId then true
    else
        match Map.tryFind sourceId aliases with
        | Some nextId -> isTransitiveAliasOf aliases nextId targetId
        | None -> false

/// Check if an atom is returned in the expression, following aliases
/// An alias is a binding like "let x = Atom (Var y)" which means x is an alias of y
let rec isAtomReturnedWithAliases (aliases: Map<TempId, TempId>) (atom: Atom) (expr: AExpr) : bool =
    match expr with
    | Return retAtom ->
        // Check if the returned atom is our target or a transitive alias of it
        match atom, retAtom with
        | Var targetId, Var retId ->
            // Check if retId is the same as targetId or transitively aliases to it
            isTransitiveAliasOf aliases retId targetId
        | _, _ -> retAtom = atom
    | Let (boundId, cexpr, body) ->
        // Check if this is an alias binding (let x = Atom (Var y))
        let aliases' =
            match cexpr with
            | Atom (Var sourceId) -> Map.add boundId sourceId aliases
            | _ -> aliases
        isAtomReturnedWithAliases aliases' atom body
    | If (_, thenBranch, elseBranch) ->
        isAtomReturnedWithAliases aliases atom thenBranch ||
        isAtomReturnedWithAliases aliases atom elseBranch

/// Check if an atom is returned in the expression
let isAtomReturned (atom: Atom) (expr: AExpr) : bool =
    isAtomReturnedWithAliases Map.empty atom expr

/// Check if a TempId is returned in the expression
let isTempReturned (tempId: TempId) (expr: AExpr) : bool =
    isAtomReturned (Var tempId) expr

/// Insert RefCountDec before all Return points in an expression
/// This ensures Dec happens AFTER the body is evaluated but BEFORE returning
let rec insertDecBeforeReturn (tempId: TempId) (size: int) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen =
    match expr with
    | Return atom ->
        // Insert Dec right before Return
        let (dummyId, varGen') = freshVar varGen
        (Let (dummyId, RefCountDec (Var tempId, size), Return atom), varGen')
    | Let (x, c, body) ->
        // Recurse into body
        let (body', varGen') = insertDecBeforeReturn tempId size body varGen
        (Let (x, c, body'), varGen')
    | If (cond, thenBr, elseBr) ->
        // Insert in both branches
        let (thenBr', varGen1) = insertDecBeforeReturn tempId size thenBr varGen
        let (elseBr', varGen2) = insertDecBeforeReturn tempId size elseBr varGen1
        (If (cond, thenBr', elseBr'), varGen2)

/// Insert RefCountDec for a TempId at all return points
let wrapWithDec (tempId: TempId) (typ: AST.Type) (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen =
    let size = payloadSize typ ctx.TypeReg
    insertDecBeforeReturn tempId size expr varGen

/// Check if a CExpr is a borrowing/aliasing operation
/// Borrowed/aliased values should NOT get their own RefCountDec - the original value owns the memory
let isBorrowingExpr (cexpr: CExpr) : bool =
    match cexpr with
    | TupleGet _ -> true           // Extracts pointer from tuple/list - borrowed from parent
    | Atom (Var _) -> true         // Alias/copy of existing variable - don't double-dec
    | _ -> false

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let rec insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen * Map<TempId, AST.Type> =
    match expr with
    | Return _ ->
        // Nothing to do at return - return current accumulated types
        (expr, varGen, ctx.TempTypes)

    | If (cond, thenBranch, elseBranch) ->
        // Process both branches
        let (thenBranch', varGen1, types1) = insertRC ctx thenBranch varGen
        let ctx1 = { ctx with TempTypes = types1 }
        let (elseBranch', varGen2, types2) = insertRC ctx1 elseBranch varGen1
        (If (cond, thenBranch', elseBranch'), varGen2, types2)

    | Let (tempId, cexpr, body) ->
        // First, infer the type of this binding and add to context
        let maybeType = inferCExprType ctx cexpr
        // Always add the type to context - use TInt64 as fallback when inference fails
        // This ensures all TempIds are tracked even if we can't determine exact type
        let inferredType = Option.defaultValue AST.TInt64 maybeType
        let ctx' = addType ctx tempId inferredType

        // Track closure function names for later ClosureCall type resolution
        let ctx'' =
            match cexpr with
            | ClosureAlloc (funcName, _) -> addClosureFunc ctx' tempId funcName
            | _ -> ctx'

        // Process the body recursively
        let (body', varGen1, accTypes) = insertRC ctx'' body varGen

        // For TupleAlloc, insert RefCountInc for heap-typed elements.
        // This is necessary because the new tuple stores a reference to the element,
        // so the element's refcount should reflect this additional reference.
        // Without this, if the original owner of the element decrements it, the
        // element could be freed while the tuple still holds a pointer to it.
        // Note: RefCountInc in codegen safely handles null pointers (e.g., empty list = 0).
        let (incBindings, varGen2) =
            match cexpr with
            | TupleAlloc elems ->
                let rec collectIncs (atoms: Atom list) (vg: VarGen) (acc: (TempId * CExpr) list) =
                    match atoms with
                    | [] -> (List.rev acc, vg)
                    | atom :: rest ->
                        match atom with
                        | Var tid ->
                            // Check if this variable is heap-typed
                            match tryGetType ctx tid with
                            | Some t when isHeapType t ->
                                // Insert RefCountInc for this heap element
                                let size = payloadSize t ctx.TypeReg
                                let (dummyId, vg') = freshVar vg
                                collectIncs rest vg' ((dummyId, RefCountInc (Var tid, size)) :: acc)
                            | _ ->
                                collectIncs rest vg acc
                        | _ ->
                            // Literals don't need refcount operations
                            collectIncs rest vg acc
                collectIncs elems varGen1 []
            | _ -> ([], varGen1)

        // Check if this TempId is heap-typed, not returned, and not borrowed
        // Borrowed values don't own their memory - the parent does
        match maybeType with
        | Some t when isHeapType t && not (isTempReturned tempId body') && not (isBorrowingExpr cexpr) ->
            // Insert Dec after body completes but value isn't returned
            let (bodyWithDec, varGen3) = wrapWithDec tempId t ctx' body' varGen2
            // Wrap with Inc bindings, then the Let, then Dec
            let letExpr = Let (tempId, cexpr, bodyWithDec)
            let exprWithIncs = wrapBindings incBindings letExpr
            (exprWithIncs, varGen3, accTypes)
        | _ ->
            // Wrap with Inc bindings, then the Let
            let letExpr = Let (tempId, cexpr, body')
            let exprWithIncs = wrapBindings incBindings letExpr
            (exprWithIncs, varGen2, accTypes)

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen * Map<TempId, AST.Type> =
    // Add parameter types to context
    let paramTypes =
        match Map.tryFind func.Name ctx.FuncParams with
        | Some paramList -> paramList |> List.map snd
        | None -> []

    let ctx' =
        List.zip func.Params paramTypes
        |> List.fold (fun c (tid, t) -> addType c tid t) ctx

    // Process function body
    let (body', varGen', accTypes) = insertRC ctx' func.Body varGen
    ({ func with Body = body' }, varGen', accTypes)

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap, string> =
    let ctx = createContext result
    let (ANF.Program (functions, mainExpr)) = result.Program
    let varGen = VarGen 1000  // Start high to avoid conflicts

    // Process all functions, accumulating types
    let rec processFuncs (funcs: Function list) (vg: VarGen) (accFuncs: Function list) (accTypes: Map<TempId, AST.Type>) : Function list * VarGen * Map<TempId, AST.Type> =
        match funcs with
        | [] -> (List.rev accFuncs, vg, accTypes)
        | f :: rest ->
            let (f', vg', types) = insertRCInFunction { ctx with TempTypes = accTypes } f vg
            // Merge accumulated types
            let mergedTypes = Map.fold (fun m k v -> Map.add k v m) accTypes types
            processFuncs rest vg' (f' :: accFuncs) mergedTypes

    let (functions', varGen1, typesFromFuncs) = processFuncs functions varGen [] Map.empty

    // Process main expression
    let (mainExpr', _, typesFromMain) = insertRC { ctx with TempTypes = typesFromFuncs } mainExpr varGen1

    // Final merged TypeMap
    let finalTypeMap = Map.fold (fun m k v -> Map.add k v m) typesFromFuncs typesFromMain

    Ok (ANF.Program (functions', mainExpr'), finalTypeMap)
