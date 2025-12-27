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
}

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      FuncReg = result.FuncReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty }

/// Add a TempId -> Type mapping to context
let addType (ctx: TypeContext) (tempId: TempId) (typ: AST.Type) : TypeContext =
    { ctx with TempTypes = Map.add tempId typ ctx.TempTypes }

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
    | IndirectCall (_, _) ->
        // For indirect calls, we would need to track the function type
        // For now, return None (no ref counting for indirect call results)
        None
    | ClosureAlloc (_, captures) ->
        // Closure is a tuple-like structure: (func_ptr, cap1, cap2, ...)
        // Return a tuple type for ref counting purposes
        Some (AST.TTuple (AST.TInt64 :: List.replicate (List.length captures) AST.TInt64))
    | ClosureCall (_, _) ->
        // Closure calls return unknown type for now
        None
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
                | Var tid -> tryGetType ctx tid |> Option.defaultValue AST.TInt64
                | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg |> Option.defaultValue AST.TInt64)
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

/// Check if an atom is returned in the expression
let rec isAtomReturned (atom: Atom) (expr: AExpr) : bool =
    match expr with
    | Return retAtom -> retAtom = atom
    | Let (_, _, body) -> isAtomReturned atom body
    | If (_, thenBranch, elseBranch) ->
        isAtomReturned atom thenBranch || isAtomReturned atom elseBranch

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
let rec insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen =
    match expr with
    | Return _ ->
        // Nothing to do at return
        (expr, varGen)

    | If (cond, thenBranch, elseBranch) ->
        // Process both branches
        let (thenBranch', varGen1) = insertRC ctx thenBranch varGen
        let (elseBranch', varGen2) = insertRC ctx elseBranch varGen1
        (If (cond, thenBranch', elseBranch'), varGen2)

    | Let (tempId, cexpr, body) ->
        // First, infer the type of this binding and add to context
        let maybeType = inferCExprType ctx cexpr
        let ctx' =
            match maybeType with
            | Some t -> addType ctx tempId t
            | None -> ctx

        // Process the body recursively
        let (body', varGen1) = insertRC ctx' body varGen

        // Check if this TempId is heap-typed, not returned, and not borrowed
        // Borrowed values don't own their memory - the parent does
        match maybeType with
        | Some t when isHeapType t && not (isTempReturned tempId body') && not (isBorrowingExpr cexpr) ->
            // Insert Dec after body completes but value isn't returned
            let (bodyWithDec, varGen2) = wrapWithDec tempId t ctx' body' varGen1
            (Let (tempId, cexpr, bodyWithDec), varGen2)
        | _ ->
            (Let (tempId, cexpr, body'), varGen1)

/// Insert RC operations into a function
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen =
    // Add parameter types to context
    let paramTypes =
        match Map.tryFind func.Name ctx.FuncParams with
        | Some paramList -> paramList |> List.map snd
        | None -> []

    let ctx' =
        List.zip func.Params paramTypes
        |> List.fold (fun c (tid, t) -> addType c tid t) ctx

    // Process function body
    let (body', varGen') = insertRC ctx' func.Body varGen
    ({ func with Body = body' }, varGen')

/// Insert RC operations into a program
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program, string> =
    let ctx = createContext result
    let (ANF.Program (functions, mainExpr)) = result.Program
    let varGen = VarGen 1000  // Start high to avoid conflicts

    // Process all functions
    let rec processFuncs (funcs: Function list) (vg: VarGen) (acc: Function list) : Function list * VarGen =
        match funcs with
        | [] -> (List.rev acc, vg)
        | f :: rest ->
            let (f', vg') = insertRCInFunction ctx f vg
            processFuncs rest vg' (f' :: acc)

    let (functions', varGen1) = processFuncs functions varGen []

    // Process main expression
    let (mainExpr', _) = insertRC ctx mainExpr varGen1

    Ok (ANF.Program (functions', mainExpr'))
