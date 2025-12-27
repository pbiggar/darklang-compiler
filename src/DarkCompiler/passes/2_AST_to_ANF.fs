// 2_AST_to_ANF.fs - ANF Transformation (Pass 2)
//
// Transforms AST into A-Normal Form (ANF).
//
// Algorithm:
// - Recursively processes nested expressions
// - Converts complex operands to atoms (literals or variables)
// - Introduces let-bindings for intermediate computations
// - Uses VarGen for generating fresh temporary variable names
//
// Example:
//   BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))
//   →
//   let tmp0 = 3; let tmp1 = 4; let tmp2 = tmp0 * tmp1;
//   let tmp3 = 2; let tmp4 = tmp3 + tmp2; return tmp4

module AST_to_ANF

open ANF

/// Try to convert a function call to a file I/O intrinsic CExpr
/// Returns Some CExpr if it's a file intrinsic, None otherwise
let tryFileIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.File.readText", [pathAtom] ->
        Some (ANF.FileReadText pathAtom)
    | "Stdlib.File.exists", [pathAtom] ->
        Some (ANF.FileExists pathAtom)
    | "Stdlib.File.writeText", [pathAtom; contentAtom] ->
        Some (ANF.FileWriteText (pathAtom, contentAtom))
    | "Stdlib.File.appendText", [pathAtom; contentAtom] ->
        Some (ANF.FileAppendText (pathAtom, contentAtom))
    | _ -> None

/// Try to convert a function call to a raw memory intrinsic CExpr
/// These are internal-only functions for implementing HAMT data structures
/// Returns Some CExpr if it's a raw memory intrinsic, None otherwise
let tryRawMemoryIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "__raw_alloc", [numBytesAtom] ->
        Some (ANF.RawAlloc numBytesAtom)
    | "__raw_free", [ptrAtom] ->
        Some (ANF.RawFree ptrAtom)
    | "__raw_get", [ptrAtom; offsetAtom] ->
        Some (ANF.RawGet (ptrAtom, offsetAtom))
    | "__raw_set", [ptrAtom; offsetAtom; valueAtom] ->
        Some (ANF.RawSet (ptrAtom, offsetAtom, valueAtom))
    // Cast operations are no-ops at runtime - just pass through the value
    | "__rawptr_to_int64", [ptrAtom] ->
        Some (ANF.Atom ptrAtom)
    | "__int64_to_rawptr", [intAtom] ->
        Some (ANF.Atom intAtom)
    | _ -> None

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * AST.Type) list>

/// Variant lookup - maps variant names to (type name, type params, tag index, payload type)
type VariantLookup = Map<string, (string * string list * int * AST.Type option)>

/// Function registry - maps function names to their FULL function types (TFunction)
type FunctionRegistry = Map<string, AST.Type>

/// Variable environment - maps variable names to their TempIds and types
/// The type information is used for type-directed field lookup in record access
type VarEnv = Map<string, ANF.TempId * AST.Type>

/// Extract just the type environment from VarEnv for use with inferType
let typeEnvFromVarEnv (varEnv: VarEnv) : Map<string, AST.Type> =
    varEnv |> Map.map (fun _ (_, t) -> t)

// ============================================================================
// Monomorphization Support for Generic Functions
// ============================================================================

/// Generic function registry - maps generic function names to their definitions
type GenericFuncDefs = Map<string, AST.FunctionDef>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.Type list  // (funcName, typeArgs)

/// Specialization registry - tracks which specializations are needed
/// Maps (funcName, typeArgs) -> specialized name
type SpecRegistry = Map<SpecKey, string>

/// Convert a type to a string for name mangling
let rec typeToMangledName (t: AST.Type) : string =
    match t with
    | AST.TInt8 -> "i8"
    | AST.TInt16 -> "i16"
    | AST.TInt32 -> "i32"
    | AST.TInt64 -> "i64"
    | AST.TUInt8 -> "u8"
    | AST.TUInt16 -> "u16"
    | AST.TUInt32 -> "u32"
    | AST.TUInt64 -> "u64"
    | AST.TBool -> "bool"
    | AST.TFloat64 -> "f64"
    | AST.TString -> "str"
    | AST.TUnit -> "unit"
    | AST.TFunction (paramTypes, retType) ->
        let paramStr = paramTypes |> List.map typeToMangledName |> String.concat "_"
        let retStr = typeToMangledName retType
        $"fn_{paramStr}_to_{retStr}"
    | AST.TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToMangledName |> String.concat "_"
        $"tup_{elemsStr}"
    | AST.TRecord name -> name
    | AST.TSum (name, []) -> name
    | AST.TSum (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{name}_{argsStr}"
    | AST.TList elemType -> $"list_{typeToMangledName elemType}"
    | AST.TDict (keyType, valueType) -> $"dict_{typeToMangledName keyType}_{typeToMangledName valueType}"
    | AST.TVar name -> name  // Should not appear after monomorphization
    | AST.TRawPtr -> "rawptr"  // Internal raw pointer type

/// Generate a specialized function name
let specName (funcName: string) (typeArgs: AST.Type list) : string =
    if List.isEmpty typeArgs then
        funcName
    else
        let typeStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{funcName}_{typeStr}"

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, AST.Type>

/// Apply a substitution to a type, replacing type variables with concrete types
let rec applySubstToType (subst: Substitution) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TVar name ->
        match Map.tryFind name subst with
        | Some concreteType -> concreteType
        | None -> typ  // Unbound type variable remains as-is
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (List.map (applySubstToType subst) paramTypes, applySubstToType subst returnType)
    | AST.TTuple elemTypes ->
        AST.TTuple (List.map (applySubstToType subst) elemTypes)
    | AST.TList elemType ->
        AST.TList (applySubstToType subst elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (applySubstToType subst keyType, applySubstToType subst valueType)
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TUnit | AST.TRecord _ | AST.TSum _ | AST.TRawPtr ->
        typ  // Concrete types are unchanged

/// Apply a substitution to an expression, replacing type variables in type annotations
let rec applySubstToExpr (subst: Substitution) (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        expr  // No types to substitute in literals, variables, function references, and closures
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, applySubstToExpr subst left, applySubstToExpr subst right)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, applySubstToExpr subst inner)
    | AST.Let (name, value, body) ->
        AST.Let (name, applySubstToExpr subst value, applySubstToExpr subst body)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (applySubstToExpr subst cond, applySubstToExpr subst thenBranch, applySubstToExpr subst elseBranch)
    | AST.Call (funcName, args) ->
        AST.Call (funcName, List.map (applySubstToExpr subst) args)
    | AST.TypeApp (funcName, typeArgs, args) ->
        // Substitute in type arguments and value arguments
        AST.TypeApp (funcName, List.map (applySubstToType subst) typeArgs, List.map (applySubstToExpr subst) args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map (applySubstToExpr subst) elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (applySubstToExpr subst tuple, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) fields)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (applySubstToExpr subst record, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map (applySubstToExpr subst) payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (applySubstToExpr subst scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (applySubstToExpr subst); Body = applySubstToExpr subst mc.Body }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map (applySubstToExpr subst) elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map (applySubstToExpr subst) headElements, applySubstToExpr subst tail)
    | AST.Lambda (parameters, body) ->
        // Substitute types in parameter annotations and body
        let substParams = parameters |> List.map (fun (name, ty) -> (name, applySubstToType subst ty))
        AST.Lambda (substParams, applySubstToExpr subst body)
    | AST.Apply (func, args) ->
        AST.Apply (applySubstToExpr subst func, List.map (applySubstToExpr subst) args)
    | AST.InterpolatedString parts ->
        let substPart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (applySubstToExpr subst e)
        AST.InterpolatedString (List.map substPart parts)

/// Specialize a generic function definition with specific type arguments
let specializeFunction (funcDef: AST.FunctionDef) (typeArgs: AST.Type list) : AST.FunctionDef =
    // Build substitution from type parameters to type args
    let subst = List.zip funcDef.TypeParams typeArgs |> Map.ofList
    // Generate specialized name
    let specializedName = specName funcDef.Name typeArgs
    // Apply substitution to parameters, return type, and body
    let specializedParams = funcDef.Params |> List.map (fun (name, ty) -> (name, applySubstToType subst ty))
    let specializedReturnType = applySubstToType subst funcDef.ReturnType
    let specializedBody = applySubstToExpr subst funcDef.Body
    { Name = specializedName
      TypeParams = []  // Specialized function has no type parameters
      Params = specializedParams
      ReturnType = specializedReturnType
      Body = specializedBody }

/// Collect all TypeApp call sites from an expression
let rec collectTypeApps (expr: AST.Expr) : Set<SpecKey> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        Set.empty
    | AST.BinOp (_, left, right) ->
        Set.union (collectTypeApps left) (collectTypeApps right)
    | AST.UnaryOp (_, inner) ->
        collectTypeApps inner
    | AST.Let (_, value, body) ->
        Set.union (collectTypeApps value) (collectTypeApps body)
    | AST.If (cond, thenBranch, elseBranch) ->
        Set.union (collectTypeApps cond) (Set.union (collectTypeApps thenBranch) (collectTypeApps elseBranch))
    | AST.Call (_, args) ->
        args |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.TypeApp (funcName, typeArgs, args) ->
        // This is a generic call - collect this specialization plus any in args
        let argSpecs = args |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.add (funcName, typeArgs) argSpecs
    | AST.TupleLiteral elements ->
        elements |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.TupleAccess (tuple, _) ->
        collectTypeApps tuple
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (snd >> collectTypeApps) |> List.fold Set.union Set.empty
    | AST.RecordAccess (record, _) ->
        collectTypeApps record
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map collectTypeApps |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
        let scrutineeSpecs = collectTypeApps scrutinee
        let caseSpecs = cases |> List.map (fun mc ->
            let guardSpecs = mc.Guard |> Option.map collectTypeApps |> Option.defaultValue Set.empty
            Set.union guardSpecs (collectTypeApps mc.Body)) |> List.fold Set.union Set.empty
        Set.union scrutineeSpecs caseSpecs
    | AST.ListLiteral elements ->
        elements |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.ListCons (headElements, tail) ->
        let headsSpecs = headElements |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.union headsSpecs (collectTypeApps tail)
    | AST.Lambda (_, body) ->
        collectTypeApps body
    | AST.Apply (func, args) ->
        let funcSpecs = collectTypeApps func
        let argsSpecs = args |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.union funcSpecs argsSpecs
    | AST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | AST.StringText _ -> None
            | AST.StringExpr e -> Some (collectTypeApps e))
        |> List.fold Set.union Set.empty

/// Collect TypeApps from a function definition
let collectTypeAppsFromFunc (funcDef: AST.FunctionDef) : Set<SpecKey> =
    collectTypeApps funcDef.Body

/// Replace TypeApp with Call using specialized name in an expression
let rec replaceTypeApps (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        expr
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, replaceTypeApps left, replaceTypeApps right)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, replaceTypeApps inner)
    | AST.Let (name, value, body) ->
        AST.Let (name, replaceTypeApps value, replaceTypeApps body)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (replaceTypeApps cond, replaceTypeApps thenBranch, replaceTypeApps elseBranch)
    | AST.Call (funcName, args) ->
        AST.Call (funcName, List.map replaceTypeApps args)
    | AST.TypeApp (funcName, typeArgs, args) ->
        // Replace with a regular Call to the specialized name
        let specializedName = specName funcName typeArgs
        AST.Call (specializedName, List.map replaceTypeApps args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map replaceTypeApps elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (replaceTypeApps tuple, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, replaceTypeApps e)) fields)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (replaceTypeApps record, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map replaceTypeApps payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (replaceTypeApps scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map replaceTypeApps; Body = replaceTypeApps mc.Body }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map replaceTypeApps elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map replaceTypeApps headElements, replaceTypeApps tail)
    | AST.Lambda (parameters, body) ->
        AST.Lambda (parameters, replaceTypeApps body)
    | AST.Apply (func, args) ->
        AST.Apply (replaceTypeApps func, List.map replaceTypeApps args)
    | AST.InterpolatedString parts ->
        let replacePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (replaceTypeApps e)
        AST.InterpolatedString (List.map replacePart parts)

/// Replace TypeApp with Call in a function definition
let replaceTypeAppsInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = replaceTypeApps funcDef.Body }

// =============================================================================
// Lambda Inlining
// =============================================================================
// For first-class function support, we inline lambdas at their call sites.
// This transforms:
//   let f = (x: int) => x + 1 in f(5)
// Into:
//   let f = (x: int) => x + 1 in ((x: int) => x + 1)(5)
// Which is then handled by immediate application desugaring.

/// Environment mapping variable names to their lambda definitions
type LambdaEnv = Map<string, AST.Expr>

/// Check if a variable occurs in an expression (for dead code elimination)
let rec varOccursInExpr (name: string) (expr: AST.Expr) : bool =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ -> false
    | AST.Var n -> n = name
    | AST.BinOp (_, left, right) -> varOccursInExpr name left || varOccursInExpr name right
    | AST.UnaryOp (_, inner) -> varOccursInExpr name inner
    | AST.Let (n, value, body) ->
        varOccursInExpr name value || (n <> name && varOccursInExpr name body)
    | AST.If (cond, thenBranch, elseBranch) ->
        varOccursInExpr name cond || varOccursInExpr name thenBranch || varOccursInExpr name elseBranch
    | AST.Call (funcName, args) ->
        // funcName could be a lambda variable reference (parser can't distinguish)
        funcName = name || List.exists (varOccursInExpr name) args
    | AST.TypeApp (_, _, args) -> List.exists (varOccursInExpr name) args
    | AST.TupleLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.TupleAccess (tuple, _) -> varOccursInExpr name tuple
    | AST.RecordLiteral (_, fields) -> List.exists (fun (_, e) -> varOccursInExpr name e) fields
    | AST.RecordAccess (record, _) -> varOccursInExpr name record
    | AST.Constructor (_, _, payload) -> Option.exists (varOccursInExpr name) payload
    | AST.Match (scrutinee, cases) ->
        varOccursInExpr name scrutinee ||
        List.exists (fun (mc: AST.MatchCase) ->
            (mc.Guard |> Option.map (varOccursInExpr name) |> Option.defaultValue false) ||
            varOccursInExpr name mc.Body) cases
    | AST.ListLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.ListCons (headElements, tail) ->
        List.exists (varOccursInExpr name) headElements || varOccursInExpr name tail
    | AST.Lambda (parameters, body) ->
        // If name is shadowed by a parameter, it doesn't occur
        let paramNames = parameters |> List.map fst |> Set.ofList
        if Set.contains name paramNames then false
        else varOccursInExpr name body
    | AST.Apply (func, args) ->
        varOccursInExpr name func || List.exists (varOccursInExpr name) args
    | AST.FuncRef _ ->
        false  // Function references don't contain variable references
    | AST.Closure (_, captures) ->
        // Check if name occurs in captured expressions
        List.exists (varOccursInExpr name) captures
    | AST.InterpolatedString parts ->
        parts |> List.exists (fun part ->
            match part with
            | AST.StringText _ -> false
            | AST.StringExpr e -> varOccursInExpr name e)

/// Inline lambdas at Apply sites
/// lambdaEnv: maps variable names to their lambda expressions
let rec inlineLambdas (expr: AST.Expr) (lambdaEnv: LambdaEnv) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ ->
        expr
    | AST.Var _ -> expr  // Variable references stay as-is (not at call position)
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, inlineLambdas left lambdaEnv, inlineLambdas right lambdaEnv)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, inlineLambdas inner lambdaEnv)
    | AST.Let (name, value, body) ->
        let value' = inlineLambdas value lambdaEnv
        // If the value is a lambda, add it to the environment for the body
        let lambdaEnv' =
            match value' with
            | AST.Lambda _ -> Map.add name value' lambdaEnv
            | _ -> lambdaEnv
        let body' = inlineLambdas body lambdaEnv'
        // Dead lambda elimination: if the value was a lambda and the variable
        // is no longer used in the body (all uses were inlined), drop the binding
        match value' with
        | AST.Lambda _ when not (varOccursInExpr name body') -> body'
        | _ -> AST.Let (name, value', body')
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (inlineLambdas cond lambdaEnv, inlineLambdas thenBranch lambdaEnv, inlineLambdas elseBranch lambdaEnv)
    | AST.Call (funcName, args) ->
        let args' = List.map (fun a -> inlineLambdas a lambdaEnv) args
        // Check if funcName is actually a lambda variable (parser can't distinguish)
        match Map.tryFind funcName lambdaEnv with
        | Some lambdaExpr -> AST.Apply (lambdaExpr, args')
        | None -> AST.Call (funcName, args')
    | AST.TypeApp (funcName, typeArgs, args) ->
        AST.TypeApp (funcName, typeArgs, List.map (fun a -> inlineLambdas a lambdaEnv) args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (inlineLambdas tuple lambdaEnv, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) fields)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (inlineLambdas record lambdaEnv, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map (fun e -> inlineLambdas e lambdaEnv) payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (inlineLambdas scrutinee lambdaEnv,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (fun g -> inlineLambdas g lambdaEnv); Body = inlineLambdas mc.Body lambdaEnv }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map (fun e -> inlineLambdas e lambdaEnv) headElements, inlineLambdas tail lambdaEnv)
    | AST.Lambda (parameters, body) ->
        // Lambdas can reference outer lambdas, so inline in body
        AST.Lambda (parameters, inlineLambdas body lambdaEnv)
    | AST.Apply (func, args) ->
        let args' = List.map (fun a -> inlineLambdas a lambdaEnv) args
        match func with
        | AST.Var name ->
            // Check if this variable is a known lambda
            match Map.tryFind name lambdaEnv with
            | Some lambdaExpr ->
                // Substitute the lambda at the call site
                AST.Apply (lambdaExpr, args')
            | None ->
                // Unknown function variable - keep as-is (will error later if not valid)
                AST.Apply (AST.Var name, args')
        | _ ->
            // Non-variable function (could be lambda or other expr)
            AST.Apply (inlineLambdas func lambdaEnv, args')
    | AST.FuncRef _ ->
        // Function references don't need lambda inlining
        expr
    | AST.Closure (funcName, captures) ->
        // Inline lambdas in captured expressions
        AST.Closure (funcName, List.map (fun c -> inlineLambdas c lambdaEnv) captures)
    | AST.InterpolatedString parts ->
        let inlinePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (inlineLambdas e lambdaEnv)
        AST.InterpolatedString (List.map inlinePart parts)

/// Inline lambdas in a function definition
let inlineLambdasInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = inlineLambdas funcDef.Body Map.empty }

/// Inline lambdas in a program
let inlineLambdasInProgram (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program
    let topLevels' =
        topLevels
        |> List.map (function
            | AST.FunctionDef f -> AST.FunctionDef (inlineLambdasInFunc f)
            | AST.Expression e -> AST.Expression (inlineLambdas e Map.empty)
            | AST.TypeDef t -> AST.TypeDef t)
    AST.Program topLevels'

// ============================================================================
// Lambda Lifting: Convert non-capturing lambdas to top-level functions
// ============================================================================

/// State for lambda lifting - tracks generated functions and counter
type LiftState = {
    Counter: int
    LiftedFunctions: AST.FunctionDef list
}

/// Collect free variables in an expression (variables not bound by let or lambda parameters)
let rec freeVars (expr: AST.Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ -> Set.empty
    | AST.Var name -> if Set.contains name bound then Set.empty else Set.singleton name
    | AST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | AST.UnaryOp (_, inner) -> freeVars inner bound
    | AST.Let (name, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars = freeVars body (Set.add name bound)
        Set.union valueVars bodyVars
    | AST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | AST.Call (_, args) | AST.TypeApp (_, _, args) ->
        args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
    | AST.TupleLiteral elems | AST.ListLiteral elems ->
        elems |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.ListCons (headElements, tail) ->
        let headsFree = headElements |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union headsFree (freeVars tail bound)
    | AST.TupleAccess (tuple, _) -> freeVars tuple bound
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.RecordAccess (record, _) -> freeVars record bound
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> freeVars e bound) |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
        let scrutineeVars = freeVars scrutinee bound
        let caseVars = cases |> List.map (fun mc ->
            let guardVars = mc.Guard |> Option.map (fun g -> freeVars g bound) |> Option.defaultValue Set.empty
            Set.union guardVars (freeVars mc.Body bound)) |> List.fold Set.union Set.empty
        Set.union scrutineeVars caseVars
    | AST.Lambda (parameters, body) ->
        let paramNames = parameters |> List.map fst |> Set.ofList
        freeVars body (Set.union bound paramNames)
    | AST.Apply (func, args) ->
        let funcVars = freeVars func bound
        let argVars = args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcVars argVars
    | AST.FuncRef _ -> Set.empty
    | AST.Closure (_, captures) ->
        // Closure captures may contain free variables
        captures |> List.map (fun c -> freeVars c bound) |> List.fold Set.union Set.empty
    | AST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | AST.StringText _ -> None
            | AST.StringExpr e -> Some (freeVars e bound))
        |> List.fold Set.union Set.empty

/// Lift lambdas in an expression, returning (transformed expr, new state)
let rec liftLambdasInExpr (expr: AST.Expr) (state: LiftState) : Result<AST.Expr * LiftState, string> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        Ok (expr, state)
    | AST.BinOp (op, left, right) ->
        liftLambdasInExpr left state
        |> Result.bind (fun (left', state1) ->
            liftLambdasInExpr right state1
            |> Result.map (fun (right', state2) -> (AST.BinOp (op, left', right'), state2)))
    | AST.UnaryOp (op, inner) ->
        liftLambdasInExpr inner state
        |> Result.map (fun (inner', state') -> (AST.UnaryOp (op, inner'), state'))
    | AST.Let (name, value, body) ->
        liftLambdasInExpr value state
        |> Result.bind (fun (value', state1) ->
            liftLambdasInExpr body state1
            |> Result.map (fun (body', state2) -> (AST.Let (name, value', body'), state2)))
    | AST.If (cond, thenBr, elseBr) ->
        liftLambdasInExpr cond state
        |> Result.bind (fun (cond', state1) ->
            liftLambdasInExpr thenBr state1
            |> Result.bind (fun (thenBr', state2) ->
                liftLambdasInExpr elseBr state2
                |> Result.map (fun (elseBr', state3) -> (AST.If (cond', thenBr', elseBr'), state3))))
    | AST.Call (funcName, args) ->
        // Process args, lifting any lambdas
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.Call (funcName, args'), state'))
    | AST.TypeApp (funcName, typeArgs, args) ->
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.TypeApp (funcName, typeArgs, args'), state'))
    | AST.TupleLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.TupleLiteral elems', state'))
    | AST.ListLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.ListLiteral elems', state'))
    | AST.ListCons (headElements, tail) ->
        liftLambdasInList headElements state
        |> Result.bind (fun (heads', state') ->
            liftLambdasInExpr tail state'
            |> Result.map (fun (tail', state'') -> (AST.ListCons (heads', tail'), state'')))
    | AST.TupleAccess (tuple, index) ->
        liftLambdasInExpr tuple state
        |> Result.map (fun (tuple', state') -> (AST.TupleAccess (tuple', index), state'))
    | AST.RecordLiteral (typeName, fields) ->
        liftLambdasInFields fields state
        |> Result.map (fun (fields', state') -> (AST.RecordLiteral (typeName, fields'), state'))
    | AST.RecordAccess (record, fieldName) ->
        liftLambdasInExpr record state
        |> Result.map (fun (record', state') -> (AST.RecordAccess (record', fieldName), state'))
    | AST.Constructor (typeName, variantName, payload) ->
        match payload with
        | None -> Ok (expr, state)
        | Some p ->
            liftLambdasInExpr p state
            |> Result.map (fun (p', state') -> (AST.Constructor (typeName, variantName, Some p'), state'))
    | AST.Match (scrutinee, cases) ->
        liftLambdasInExpr scrutinee state
        |> Result.bind (fun (scrutinee', state1) ->
            liftLambdasInCases cases state1
            |> Result.map (fun (cases', state2) -> (AST.Match (scrutinee', cases'), state2)))
    | AST.Lambda (parameters, body) ->
        // Lambda not in argument position - just recurse into body
        liftLambdasInExpr body state
        |> Result.map (fun (body', state') -> (AST.Lambda (parameters, body'), state'))
    | AST.Apply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (AST.Apply (func', args'), state2)))
    | AST.InterpolatedString parts ->
        let rec liftParts (ps: AST.StringPart list) (st: LiftState) (acc: AST.StringPart list) : Result<AST.StringPart list * LiftState, string> =
            match ps with
            | [] -> Ok (List.rev acc, st)
            | AST.StringText s :: rest ->
                liftParts rest st (AST.StringText s :: acc)
            | AST.StringExpr e :: rest ->
                liftLambdasInExpr e st
                |> Result.bind (fun (e', st') ->
                    liftParts rest st' (AST.StringExpr e' :: acc))
        liftParts parts state []
        |> Result.map (fun (parts', state') -> (AST.InterpolatedString parts', state'))

/// Lift lambdas in function arguments, converting all lambdas to Closures
/// (even non-capturing lambdas become trivial closures for uniform calling convention)
/// Also wraps FuncRef in closures for uniform calling convention
and liftLambdasInArgs (args: AST.Expr list) (state: LiftState) : Result<AST.Expr list * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | arg :: rest ->
            match arg with
            | AST.Lambda (parameters, body) ->
                // Check for free variables (captures)
                let paramNames = parameters |> List.map fst |> Set.ofList
                let freeVarsInBody = freeVars body paramNames
                let captures = Set.toList freeVarsInBody

                // All lambdas become closures (even non-capturing ones) for uniform calling convention
                // The lifted function takes closure as first param, then original params
                let funcName = $"__closure_{state.Counter}"
                let closureParam = ("__closure", AST.TTuple (List.replicate (List.length captures + 1) AST.TInt64))

                // Build body that extracts captures from closure tuple:
                // let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
                let bodyWithExtractions =
                    if List.isEmpty captures then
                        body  // No captures to extract
                    else
                        captures
                        |> List.mapi (fun i capName ->
                            // Capture at index i+1 (index 0 is the function pointer)
                            (capName, AST.TupleAccess (AST.Var "__closure", i + 1)))
                        |> List.foldBack (fun (capName, accessor) acc ->
                            AST.Let (capName, accessor, acc)) <| body

                let funcDef : AST.FunctionDef = {
                    Name = funcName
                    TypeParams = []
                    Params = closureParam :: parameters  // Closure is always first param
                    ReturnType = AST.TInt64
                    Body = bodyWithExtractions
                }
                let state' = {
                    Counter = state.Counter + 1
                    LiftedFunctions = funcDef :: state.LiftedFunctions
                }
                // Replace lambda with Closure (captures may be empty for non-capturing lambdas)
                let captureExprs = captures |> List.map AST.Var
                loop rest state' (AST.Closure (funcName, captureExprs) :: acc)

            | AST.FuncRef origFuncName ->
                // Named function used as value - wrap in a closure for uniform calling convention
                // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
                // For now, assume single int param and int return (will be generalized later)
                let wrapperName = $"__funcref_wrapper_{state.Counter}"
                let closureParam = ("__closure", AST.TTuple [AST.TInt64])
                let argParam = ("__arg", AST.TInt64)
                let wrapperBody = AST.Call (origFuncName, [AST.Var "__arg"])
                let wrapperDef : AST.FunctionDef = {
                    Name = wrapperName
                    TypeParams = []
                    Params = [closureParam; argParam]
                    ReturnType = AST.TInt64
                    Body = wrapperBody
                }
                let state' = {
                    Counter = state.Counter + 1
                    LiftedFunctions = wrapperDef :: state.LiftedFunctions
                }
                // Create trivial closure with no captures
                loop rest state' (AST.Closure (wrapperName, []) :: acc)

            | AST.Var varName ->
                // Check if this is a function being passed as value
                // For now, treat as potential function ref - will be handled at ANF level
                liftLambdasInExpr arg state
                |> Result.bind (fun (arg', state') -> loop rest state' (arg' :: acc))

            | other ->
                liftLambdasInExpr other state
                |> Result.bind (fun (other', state') -> loop rest state' (other' :: acc))
    loop args state []

/// Helper to lift lambdas in a list of expressions
and liftLambdasInList (exprs: AST.Expr list) (state: LiftState) : Result<AST.Expr list * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | e :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' (e' :: acc))
    loop exprs state []

/// Helper to lift lambdas in record fields
and liftLambdasInFields (fields: (string * AST.Expr) list) (state: LiftState) : Result<(string * AST.Expr) list * LiftState, string> =
    let rec loop (remaining: (string * AST.Expr) list) (state: LiftState) (acc: (string * AST.Expr) list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | (name, e) :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' ((name, e') :: acc))
    loop fields state []

/// Helper to lift lambdas in match cases
and liftLambdasInCases (cases: AST.MatchCase list) (state: LiftState) : Result<AST.MatchCase list * LiftState, string> =
    let rec loop (remaining: AST.MatchCase list) (state: LiftState) (acc: AST.MatchCase list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | mc :: rest ->
            // Lift lambdas in guard if present
            let guardResult =
                match mc.Guard with
                | None -> Ok (None, state)
                | Some g ->
                    liftLambdasInExpr g state
                    |> Result.map (fun (g', s) -> (Some g', s))
            guardResult
            |> Result.bind (fun (guard', state1) ->
                liftLambdasInExpr mc.Body state1
                |> Result.bind (fun (body', state2) ->
                    let newCase = { mc with Guard = guard'; Body = body' }
                    loop rest state2 (newCase :: acc)))
    loop cases state []

/// Lift lambdas in a function definition
let liftLambdasInFunc (funcDef: AST.FunctionDef) (state: LiftState) : Result<AST.FunctionDef * LiftState, string> =
    liftLambdasInExpr funcDef.Body state
    |> Result.map (fun (body', state') -> ({ funcDef with Body = body' }, state'))

/// State extended to include known function names and their parameters
type LiftStateWithFuncs = {
    State: LiftState
    FuncParams: Map<string, (string * AST.Type) list>  // function name -> params (for generating wrappers)
    GeneratedWrappers: Map<string, string>  // original func name -> wrapper name
}

/// Generate a wrapper for a named function used as a value
let generateFuncWrapper (origFuncName: string) (funcParams: Map<string, (string * AST.Type) list>) (stateWithFuncs: LiftStateWithFuncs) : Result<(AST.FunctionDef * LiftStateWithFuncs), string> =
    match Map.tryFind origFuncName funcParams with
    | Some parameters ->
        // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
        let wrapperName = $"__funcref_wrapper_{stateWithFuncs.State.Counter}"
        let closureParam = ("__closure", AST.TTuple [AST.TInt64])
        let wrapperBody = AST.Call (origFuncName, parameters |> List.map (fun (name, _) -> AST.Var name))
        let wrapperDef : AST.FunctionDef = {
            Name = wrapperName
            TypeParams = []
            Params = closureParam :: parameters
            ReturnType = AST.TInt64  // Simplified
            Body = wrapperBody
        }
        let newState = {
            stateWithFuncs with
                State = { stateWithFuncs.State with Counter = stateWithFuncs.State.Counter + 1 }
                GeneratedWrappers = Map.add origFuncName wrapperName stateWithFuncs.GeneratedWrappers
        }
        Ok (wrapperDef, newState)
    | None ->
        Error $"Cannot find parameters for function '{origFuncName}'"

/// Lift lambdas in a program, generating new top-level functions
let rec liftLambdasInProgram (program: AST.Program) : Result<AST.Program, string> =
    let (AST.Program topLevels) = program
    let initialState = { Counter = 0; LiftedFunctions = [] }

    // First pass: collect all function definitions and their parameters
    let userFuncParams : Map<string, (string * AST.Type) list> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f -> Some (f.Name, f.Params)
            | _ -> None)
        |> Map.ofList

    // Add module function parameters from Stdlib
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            // Create parameter names like "arg0", "arg1" for each parameter type
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let funcParams = Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams

    let rec processTopLevels (remaining: AST.TopLevel list) (state: LiftState) (acc: AST.TopLevel list) : Result<AST.TopLevel list * LiftState, string> =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f ->
                liftLambdasInFunc f state
                |> Result.bind (fun (f', state') ->
                    processTopLevels rest state' (AST.FunctionDef f' :: acc))
            | AST.Expression e ->
                liftLambdasInExpr e state
                |> Result.bind (fun (e', state') ->
                    processTopLevels rest state' (AST.Expression e' :: acc))
            | AST.TypeDef t ->
                processTopLevels rest state (AST.TypeDef t :: acc)

    processTopLevels topLevels initialState []
    |> Result.bind (fun (topLevels', state') ->
        // Second pass: find all functions used as values and generate wrappers
        // Look for Var references to known functions in Call arguments
        let funcNamesUsedAsValues =
            topLevels'
            |> List.collect (function
                | AST.FunctionDef f -> collectFuncRefsInExpr f.Body funcParams
                | AST.Expression e -> collectFuncRefsInExpr e funcParams
                | _ -> [])
            |> List.distinct

        // Generate wrappers for functions used as values
        let stateWithFuncs = { State = state'; FuncParams = funcParams; GeneratedWrappers = Map.empty }
        let rec generateWrappers (funcNames: string list) (st: LiftStateWithFuncs) (wrapperAcc: AST.FunctionDef list) =
            match funcNames with
            | [] -> Ok (wrapperAcc, st)
            | name :: rest ->
                generateFuncWrapper name funcParams st
                |> Result.bind (fun (wrapperDef, st') ->
                    generateWrappers rest st' (wrapperDef :: wrapperAcc))

        generateWrappers funcNamesUsedAsValues stateWithFuncs []
        |> Result.map (fun (wrappers, finalStateWithFuncs) ->
            // Replace function references with wrapper references in the program
            let topLevels'' = topLevels' |> List.map (replaceFuncRefsWithWrappers finalStateWithFuncs.GeneratedWrappers)
            // Add wrappers and lifted functions to the program
            let liftedFuncDefs = (wrappers @ finalStateWithFuncs.State.LiftedFunctions) |> List.rev |> List.map AST.FunctionDef
            AST.Program (liftedFuncDefs @ topLevels'')))

/// Collect function names that are used as values (not in Call position)
and collectFuncRefsInExpr (expr: AST.Expr) (knownFuncs: Map<string, (string * AST.Type) list>) : string list =
    match expr with
    | AST.Call (_, args) ->
        // Check if any arg is a reference to a known function
        args
        |> List.collect (fun arg ->
            match arg with
            | AST.Var name when Map.containsKey name knownFuncs -> [name]
            | _ -> collectFuncRefsInExpr arg knownFuncs)
    | AST.Let (_, value, body) ->
        // Also check if value is a function reference being bound
        let valueRefs =
            match value with
            | AST.Var name when Map.containsKey name knownFuncs -> [name]
            | _ -> collectFuncRefsInExpr value knownFuncs
        valueRefs @ collectFuncRefsInExpr body knownFuncs
    | AST.If (c, t, e) ->
        collectFuncRefsInExpr c knownFuncs @ collectFuncRefsInExpr t knownFuncs @ collectFuncRefsInExpr e knownFuncs
    | AST.BinOp (_, l, r) ->
        collectFuncRefsInExpr l knownFuncs @ collectFuncRefsInExpr r knownFuncs
    | AST.UnaryOp (_, e) -> collectFuncRefsInExpr e knownFuncs
    | AST.TupleLiteral es | AST.ListLiteral es ->
        es |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | AST.ListCons (headElements, tail) ->
        (headElements |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)) @
        collectFuncRefsInExpr tail knownFuncs
    | AST.TupleAccess (e, _) -> collectFuncRefsInExpr e knownFuncs
    | AST.RecordLiteral (_, fields) ->
        fields |> List.collect (fun (_, e) -> collectFuncRefsInExpr e knownFuncs)
    | AST.RecordAccess (e, _) -> collectFuncRefsInExpr e knownFuncs
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> collectFuncRefsInExpr e knownFuncs) |> Option.defaultValue []
    | AST.Match (scrut, cases) ->
        collectFuncRefsInExpr scrut knownFuncs @ (cases |> List.collect (fun mc ->
            (mc.Guard |> Option.map (fun g -> collectFuncRefsInExpr g knownFuncs) |> Option.defaultValue []) @
            collectFuncRefsInExpr mc.Body knownFuncs))
    | AST.Lambda (_, body) -> collectFuncRefsInExpr body knownFuncs
    | AST.Apply (f, args) ->
        collectFuncRefsInExpr f knownFuncs @ (args |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs))
    | AST.Closure (_, caps) ->
        caps |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | AST.TypeApp (_, _, args) ->
        args |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | _ -> []

/// Replace function references with wrapper references in a TopLevel
and replaceFuncRefsWithWrappers (wrapperMap: Map<string, string>) (topLevel: AST.TopLevel) : AST.TopLevel =
    match topLevel with
    | AST.FunctionDef f ->
        AST.FunctionDef { f with Body = replaceInExpr wrapperMap f.Body }
    | AST.Expression e ->
        AST.Expression (replaceInExpr wrapperMap e)
    | AST.TypeDef t -> AST.TypeDef t

/// Replace function references with wrapper references in an expression
and replaceInExpr (wrapperMap: Map<string, string>) (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.Var name when Map.containsKey name wrapperMap ->
        // This is a function reference used as a value - replace with closure to wrapper
        AST.Closure (Map.find name wrapperMap, [])
    | AST.Closure (funcName, caps) ->
        // If this closure references a known function, use the wrapper instead
        let newFuncName = Map.tryFind funcName wrapperMap |> Option.defaultValue funcName
        AST.Closure (newFuncName, caps |> List.map (replaceInExpr wrapperMap))
    | AST.Call (name, args) ->
        AST.Call (name, args |> List.map (replaceInExpr wrapperMap))
    | AST.Let (n, v, b) ->
        AST.Let (n, replaceInExpr wrapperMap v, replaceInExpr wrapperMap b)
    | AST.If (c, t, e) ->
        AST.If (replaceInExpr wrapperMap c, replaceInExpr wrapperMap t, replaceInExpr wrapperMap e)
    | AST.BinOp (op, l, r) ->
        AST.BinOp (op, replaceInExpr wrapperMap l, replaceInExpr wrapperMap r)
    | AST.UnaryOp (op, e) ->
        AST.UnaryOp (op, replaceInExpr wrapperMap e)
    | AST.TupleLiteral es ->
        AST.TupleLiteral (es |> List.map (replaceInExpr wrapperMap))
    | AST.TupleAccess (e, i) ->
        AST.TupleAccess (replaceInExpr wrapperMap e, i)
    | AST.RecordLiteral (t, fields) ->
        AST.RecordLiteral (t, fields |> List.map (fun (n, e) -> (n, replaceInExpr wrapperMap e)))
    | AST.RecordAccess (e, f) ->
        AST.RecordAccess (replaceInExpr wrapperMap e, f)
    | AST.Constructor (t, v, payload) ->
        AST.Constructor (t, v, payload |> Option.map (replaceInExpr wrapperMap))
    | AST.Match (scrut, cases) ->
        AST.Match (replaceInExpr wrapperMap scrut,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (replaceInExpr wrapperMap); Body = replaceInExpr wrapperMap mc.Body }))
    | AST.ListLiteral es ->
        AST.ListLiteral (es |> List.map (replaceInExpr wrapperMap))
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (headElements |> List.map (replaceInExpr wrapperMap), replaceInExpr wrapperMap tail)
    | AST.Lambda (ps, body) ->
        AST.Lambda (ps, replaceInExpr wrapperMap body)
    | AST.Apply (f, args) ->
        AST.Apply (replaceInExpr wrapperMap f, args |> List.map (replaceInExpr wrapperMap))
    | AST.TypeApp (n, ts, args) ->
        AST.TypeApp (n, ts, args |> List.map (replaceInExpr wrapperMap))
    | _ -> expr

/// Monomorphize a program: collect all specializations, generate specialized functions, replace TypeApps
let monomorphize (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program

    // Collect generic function definitions
    let genericFuncDefs : GenericFuncDefs =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
            | _ -> None)
        |> Map.ofList

    // Collect all specialization sites from all functions and expressions
    let allSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | AST.TypeDef _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Generate specialized function definitions
    let specializedFuncs : AST.FunctionDef list =
        allSpecs
        |> Set.toList
        |> List.choose (fun (funcName, typeArgs) ->
            match Map.tryFind funcName genericFuncDefs with
            | Some funcDef ->
                let specialized = specializeFunction funcDef typeArgs
                // Also replace any TypeApps in the specialized body
                Some (replaceTypeAppsInFunc specialized)
            | None -> None)

    // Replace TypeApps with Calls in all original top-levels (except generic function defs)
    let transformedTopLevels =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                // Skip generic function definitions (they're replaced by specializations)
                None
            | AST.FunctionDef f ->
                Some (AST.FunctionDef (replaceTypeAppsInFunc f))
            | AST.Expression e ->
                Some (AST.Expression (replaceTypeApps e))
            | AST.TypeDef td ->
                Some (AST.TypeDef td))

    // Add specialized functions to the program
    let specializationTopLevels =
        specializedFuncs |> List.map AST.FunctionDef

    AST.Program (specializationTopLevels @ transformedTopLevels)

/// Convert AST.BinOp to ANF.BinOp
/// Note: StringConcat is handled separately as ANF.StringConcat CExpr
let convertBinOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div
    | AST.Mod -> ANF.Mod
    | AST.Shl -> ANF.Shl
    | AST.Shr -> ANF.Shr
    | AST.BitAnd -> ANF.BitAnd
    | AST.BitOr -> ANF.BitOr
    | AST.BitXor -> ANF.BitXor
    | AST.Eq -> ANF.Eq
    | AST.Neq -> ANF.Neq
    | AST.Lt -> ANF.Lt
    | AST.Gt -> ANF.Gt
    | AST.Lte -> ANF.Lte
    | AST.Gte -> ANF.Gte
    | AST.And -> ANF.And
    | AST.Or -> ANF.Or
    | AST.StringConcat -> ANF.Add  // Never reached - StringConcat handled as CExpr

/// Convert AST.UnaryOp to ANF.UnaryOp
let convertUnaryOp (op: AST.UnaryOp) : ANF.UnaryOp =
    match op with
    | AST.Neg -> ANF.Neg
    | AST.Not -> ANF.Not

/// Infer the type of an expression using type environment and registries
/// Used for type-directed field lookup in record access
let rec inferType (expr: AST.Expr) (typeEnv: Map<string, AST.Type>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<AST.Type, string> =
    match expr with
    | AST.UnitLiteral -> Ok AST.TUnit
    | AST.IntLiteral _ -> Ok AST.TInt64
    | AST.Int8Literal _ -> Ok AST.TInt8
    | AST.Int16Literal _ -> Ok AST.TInt16
    | AST.Int32Literal _ -> Ok AST.TInt32
    | AST.UInt8Literal _ -> Ok AST.TUInt8
    | AST.UInt16Literal _ -> Ok AST.TUInt16
    | AST.UInt32Literal _ -> Ok AST.TUInt32
    | AST.UInt64Literal _ -> Ok AST.TUInt64
    | AST.BoolLiteral _ -> Ok AST.TBool
    | AST.StringLiteral _ -> Ok AST.TString
    | AST.FloatLiteral _ -> Ok AST.TFloat64
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some t -> Ok t
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some moduleFunc -> Ok (Stdlib.getFunctionType moduleFunc)
            | None -> Error $"Cannot infer type: undefined variable '{name}'"
    | AST.RecordLiteral (typeName, fields) ->
        if typeName = "" then
            // Anonymous record literal - try to find matching type by field names
            let literalFieldNames = fields |> List.map fst |> Set.ofList
            let matchingTypes =
                typeReg
                |> Map.toList
                |> List.filter (fun (_, typeFields) ->
                    let typeFieldNames = typeFields |> List.map fst |> Set.ofList
                    typeFieldNames = literalFieldNames)
                |> List.map fst
            match matchingTypes with
            | [singleMatch] -> Ok (AST.TRecord singleMatch)
            | [] -> Error "Cannot infer type: no record type matches the field names"
            | matches ->
                let names = String.concat ", " matches
                Error $"Ambiguous record literal: matches multiple types: {names}"
        else
            Ok (AST.TRecord typeName)
    | AST.RecordAccess (recordExpr, fieldName) ->
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFind (fun (name, _) -> name = fieldName) fields with
                    | Some (_, fieldType) -> Ok fieldType
                    | None -> Error $"Record type {typeName} has no field '{fieldName}'"
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error $"Cannot access field on non-record type")
    | AST.TupleLiteral elems ->
        elems
        |> List.map (fun e -> inferType e typeEnv typeReg variantLookup funcReg)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok types, Ok t -> Ok (types @ [t])
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map AST.TTuple
    | AST.TupleAccess (tupleExpr, index) ->
        inferType tupleExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun tupleType ->
            match tupleType with
            | AST.TTuple elemTypes when index >= 0 && index < List.length elemTypes ->
                Ok (List.item index elemTypes)
            | AST.TTuple _ -> Error $"Tuple index {index} out of bounds"
            | _ -> Error "Cannot access index on non-tuple type")
    | AST.Constructor (_, variantName, _) ->
        match Map.tryFind variantName variantLookup with
        | Some (typeName, _, _, _) -> Ok (AST.TSum (typeName, []))  // Type args inferred during type checking
        | None -> Error $"Unknown constructor: {variantName}"
    | AST.ListLiteral elements ->
        match elements with
        | [] -> Ok (AST.TList AST.TInt64)  // Default empty list to List<int>
        | first :: _ ->
            inferType first typeEnv typeReg variantLookup funcReg
            |> Result.map (fun elemType -> AST.TList elemType)
    | AST.ListCons (_, tail) ->
        // List cons has same type as tail
        inferType tail typeEnv typeReg variantLookup funcReg
    | AST.Let (name, value, body) ->
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            let typeEnv' = Map.add name valueType typeEnv
            inferType body typeEnv' typeReg variantLookup funcReg)
    | AST.If (_, thenExpr, _) ->
        // Both branches should have same type, just infer from then branch
        inferType thenExpr typeEnv typeReg variantLookup funcReg
    | AST.BinOp (op, _, _) ->
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> Ok AST.TInt64
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Ok AST.TBool
        | AST.StringConcat -> Ok AST.TString
    | AST.UnaryOp (op, _) ->
        match op with
        | AST.Neg -> Ok AST.TInt64
        | AST.Not -> Ok AST.TBool
    | AST.Match (_, cases) ->
        // Infer from first case body
        match cases with
        | mc :: _ -> inferType mc.Body typeEnv typeReg variantLookup funcReg
        | [] -> Error "Empty match expression"
    | AST.Call (funcName, _) ->
        // Look up function return type from the function registry
        match Map.tryFind funcName funcReg with
        | Some returnType -> Ok returnType
        | None ->
            // Check if it's a module function (e.g., Stdlib.File.exists)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some moduleFunc -> Ok moduleFunc.ReturnType
            | None -> Error $"Unknown function: '{funcName}'"
    | AST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"
    | AST.Lambda (parameters, body) ->
        // Lambda has function type (paramTypes) -> returnType
        let paramTypes = parameters |> List.map snd
        let typeEnv' = parameters |> List.fold (fun env (name, ty) -> Map.add name ty env) typeEnv
        inferType body typeEnv' typeReg variantLookup funcReg
        |> Result.map (fun returnType -> AST.TFunction (paramTypes, returnType))
    | AST.Apply (func, _args) ->
        // Apply result is the return type of the function
        inferType func typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun funcType ->
            match funcType with
            | AST.TFunction (_, returnType) -> Ok returnType
            | _ -> Error "Apply requires a function type")
    | AST.FuncRef name ->
        // Function reference has the function's type
        match Map.tryFind name funcReg with
        | Some returnType -> Ok returnType
        | None -> Error $"Cannot infer type: undefined function '{name}'"
    | AST.Closure (funcName, _) ->
        // Closure has function type (without the closure param)
        match Map.tryFind funcName funcReg with
        | Some (AST.TFunction (_ :: restParams, returnType)) ->
            Ok (AST.TFunction (restParams, returnType))
        | Some funcType -> Ok funcType
        | None -> Error $"Cannot infer type: undefined closure function '{funcName}'"
    | AST.InterpolatedString _ ->
        // Interpolated strings are always String type
        Ok AST.TString

/// Convert AST expression to ANF
/// env maps user variable names to ANF TempIds and their types
/// typeReg maps record type names to field definitions
/// variantLookup maps variant names to (type name, tag index)
/// funcReg maps function names to their return types
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        // Unit literal becomes return of unit value (represented as 0)
        Ok (ANF.Return (ANF.UnitLiteral), varGen)

    | AST.IntLiteral n ->
        // Integer literal becomes return
        Ok (ANF.Return (ANF.IntLiteral n), varGen)

    | AST.Int8Literal n ->
        // Convert to int64 for ANF (type info is tracked separately)
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.Int16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.Int32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.UInt64Literal n ->
        // Note: uint64 to int64 conversion may lose data for values > Int64.MaxValue
        // but for now we treat all integers as 64-bit signed
        Ok (ANF.Return (ANF.IntLiteral (int64 n)), varGen)

    | AST.BoolLiteral b ->
        // Boolean literal becomes return
        Ok (ANF.Return (ANF.BoolLiteral b), varGen)

    | AST.StringLiteral s ->
        // String literal becomes return
        Ok (ANF.Return (ANF.StringLiteral s), varGen)

    | AST.FloatLiteral f ->
        // Float literal becomes return
        Ok (ANF.Return (ANF.FloatLiteral f), varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some (tempId, _) -> Ok (ANF.Return (ANF.Var tempId), varGen)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some _ ->
                // Module function reference - wrap in closure for uniform calling convention
                let (closureId, varGen') = ANF.freshVar varGen
                let closureAlloc = ANF.ClosureAlloc (name, [])
                Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')
            | None ->
                // Check if it's a function reference (function name used as value)
                if Map.containsKey name funcReg then
                    // Wrap in closure for uniform calling convention
                    let (closureId, varGen') = ANF.freshVar varGen
                    let closureAlloc = ANF.ClosureAlloc (name, [])
                    Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')
                else
                    Error $"Undefined variable: {name}"

    | AST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')

    | AST.Closure (funcName, captures) ->
        // Closure: allocate closure tuple with function address and captured values
        // Convert each capture expression to an atom
        let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | cap :: rest ->
                toAtom cap vg env typeReg variantLookup funcReg
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Generate ClosureAlloc: allocate closure tuple
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            let finalExpr = ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId))
            let exprWithBindings = wrapBindings allBindings finalExpr
            (exprWithBindings, varGen2))

    | AST.Let (name, value, body) ->
        // Let binding: convert value to atom, allocate fresh temp, convert body with extended env
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toANF body varGen2 env' typeReg variantLookup funcReg |> Result.map (fun (bodyExpr, varGen3) ->
                    // Build: valueBindings + let tempId = valueAtom + body
                    let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
                    let exprWithBindings = wrapBindings valueBindings finalExpr
                    (exprWithBindings, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.Return (ANF.IntLiteral System.Int64.MinValue), varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.Return (ANF.FloatLiteral (-f)), varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom
        toAtom innerExpr varGen env typeReg variantLookup funcReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        toAtom left varGen env typeReg variantLookup funcReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create binop and bind to fresh variable
                let (tempVar, varGen3) = ANF.freshVar varGen2
                // StringConcat is a separate CExpr, not a Prim
                let cexpr =
                    match op with
                    | AST.StringConcat -> ANF.StringConcat (leftAtom, rightAtom)
                    | _ -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)

                // Build the expression: leftBindings + rightBindings + let tempVar = op
                let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                let exprWithRight = wrapBindings rightBindings finalExpr
                let exprWithLeft = wrapBindings leftBindings exprWithRight

                (exprWithLeft, varGen3)))

    | AST.If (cond, thenBranch, elseBranch) ->
        // If expression: convert condition to atom, both branches to ANF
        toAtom cond varGen env typeReg variantLookup funcReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg variantLookup funcReg |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg variantLookup funcReg |> Result.map (fun (elseExpr, varGen3) ->
                    // Build the expression: condBindings + if condAtom then thenExpr else elseExpr
                    let finalExpr = ANF.If (condAtom, thenExpr, elseExpr)
                    let exprWithBindings = wrapBindings condBindings finalExpr

                    (exprWithBindings, varGen3))))

    | AST.Call (funcName, args) ->
        // Function call: convert all arguments to atoms
        // If an argument is a function reference, wrap it in a trivial closure for uniform calling convention
        let wrapFuncRefInClosure (atom: ANF.Atom) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
            match atom with
            | ANF.FuncRef fnName ->
                // Function reference needs to be wrapped in a closure
                // Create a ClosureAlloc with no captures
                let (closureId, vg') = ANF.freshVar vg
                let closureAlloc = ANF.ClosureAlloc (fnName, [])
                (ANF.Var closureId, bindings @ [(closureId, closureAlloc)], vg')
            | _ -> (atom, bindings, vg)

        let rec convertArgs (argExprs: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match argExprs with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | arg :: rest ->
                toAtom arg vg env typeReg variantLookup funcReg
                |> Result.bind (fun (argAtom, argBindings, vg') ->
                    // Wrap function references in closures for uniform calling convention
                    let (wrappedAtom, allBindings, vg'') = wrapFuncRefInClosure argAtom (accBindings @ argBindings) vg'
                    convertArgs rest vg'' (wrappedAtom :: accAtoms) allBindings)

        // Regular function call (including module functions like Stdlib.Int64.add)
        convertArgs args varGen [] []
        |> Result.bind (fun (argAtoms, argBindings, varGen1) ->
            // Bind call result to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            // Check if funcName is a variable (indirect call) or a defined function (direct call)
            match Map.tryFind funcName env with
            | Some (tempId, AST.TFunction (_, _)) ->
                // Variable with function type - use closure call
                // All function values are now closures (even non-capturing ones)
                let callExpr = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                let exprWithBindings = wrapBindings argBindings finalExpr
                Ok (exprWithBindings, varGen2)
            | Some (_, varType) ->
                // Variable exists but is not a function type
                Error $"Cannot call '{funcName}' - it has type {varType}, not a function type"
            | None ->
                // Not a variable - check if it's a file intrinsic first
                match tryFileIntrinsic funcName argAtoms with
                | Some intrinsicExpr ->
                    // File I/O intrinsic call
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings argBindings finalExpr
                    Ok (exprWithBindings, varGen2)
                | None ->
                    // Check if it's a raw memory intrinsic
                    match tryRawMemoryIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Raw memory intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        let exprWithBindings = wrapBindings argBindings finalExpr
                        Ok (exprWithBindings, varGen2)
                    | None ->
                    // Check if it's a defined function
                    match Map.tryFind funcName funcReg with
                    | Some _ ->
                        // Direct call to defined function
                        let callExpr = ANF.Call (funcName, argAtoms)
                        let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                        let exprWithBindings = wrapBindings argBindings finalExpr
                        Ok (exprWithBindings, varGen2)
                    | None ->
                        // Unknown function - could be error or forward reference
                        // For now, assume it's a valid function (will fail at link time if not)
                        let callExpr = ANF.Call (funcName, argAtoms)
                        let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                        let exprWithBindings = wrapBindings argBindings finalExpr
                        Ok (exprWithBindings, varGen2))

    | AST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg variantLookup funcReg
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create TupleAlloc and bind to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let tupleExpr = ANF.TupleAlloc elemAtoms
            let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings elemBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg variantLookup funcReg
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let getExpr = ANF.TupleGet (tupleAtom, index)
            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings tupleBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples - allocate heap space and store fields
        // Get field order from type registry (or use order from literal if anonymous)
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst  // Use literal order for anonymous records
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> fields |> List.map fst  // Fallback to literal order

        // Reorder field values according to type definition order
        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Convert to TupleLiteral and reuse tuple handling
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg
                        |> Result.map (fun (recordAtom, recordBindings, varGen1) ->
                            let (resultVar, varGen2) = ANF.freshVar varGen1
                            let getExpr = ANF.TupleGet (recordAtom, index)
                            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                            let exprWithBindings = wrapBindings recordBindings finalExpr
                            (exprWithBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, _, tag, _) ->
            // Check if ANY variant in this type has a payload
            // If so, all variants must be heap-allocated for consistency
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type (no payloads anywhere): return tag as an integer
                Ok (ANF.Return (ANF.IntLiteral (int64 tag)), varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag] for consistent representation
                let tagAtom = ANF.IntLiteral (int64 tag)
                let (resultVar, varGen1) = ANF.freshVar varGen
                let tupleExpr = ANF.TupleAlloc [tagAtom]
                let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                Ok (finalExpr, varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (resultVar, varGen2) = ANF.freshVar varGen1
                    let tupleExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings payloadBindings finalExpr
                    (exprWithBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as linked list: Nil = 0, Cons = [tag=1, head, tail]
        // Build right-to-left: start with Nil, then Cons(last, Nil), etc.
        let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (tailAtom: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (tailAtom, allBindings, vg)
            | elem :: rest ->
                // First build the rest of the list
                buildList rest vg tailAtom allBindings
                |> Result.bind (fun (restAtom, restBindings, vg1) ->
                    // Now add this element: Cons(elem, rest)
                    toAtom elem vg1 env typeReg variantLookup funcReg
                    |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                        let (consVar, vg3) = ANF.freshVar vg2
                        // Cons = [tag=1, head, tail]
                        let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                        let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                        (ANF.Var consVar, newBindings, vg3)))

        if List.isEmpty elements then
            // Empty list is Nil (represented as 0)
            Ok (ANF.Return (ANF.IntLiteral 0L), varGen)
        else
            // Build the list starting with Nil
            buildList elements varGen (ANF.IntLiteral 0L) []
            |> Result.map (fun (listAtom, listBindings, varGen1) ->
                let finalExpr = ANF.Return listAtom
                let exprWithBindings = wrapBindings listBindings finalExpr
                (exprWithBindings, varGen1))

    | AST.ListCons (headElements, tail) ->
        // Compile list cons: [a, b, ...tail] prepends elements to tail
        // First convert tail to atom
        toAtom tail varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            // Use same buildList helper but start with tail instead of Nil
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentTail: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentTail, allBindings, vg)
                | elem :: rest ->
                    // First build the rest of the list
                    buildList rest vg currentTail allBindings
                    |> Result.bind (fun (restAtom, restBindings, vg1) ->
                        // Now add this element: Cons(elem, rest)
                        toAtom elem vg1 env typeReg variantLookup funcReg
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (consVar, vg3) = ANF.freshVar vg2
                            // Cons = [tag=1, head, tail]
                            let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                            let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                            (ANF.Var consVar, newBindings, vg3)))

            if List.isEmpty headElements then
                // No head elements, just return tail
                let finalExpr = ANF.Return tailAtom
                let exprWithBindings = wrapBindings tailBindings finalExpr
                Ok (exprWithBindings, varGen1)
            else
                // Build the list starting with tail
                buildList headElements varGen1 tailAtom tailBindings
                |> Result.map (fun (listAtom, listBindings, varGen2) ->
                    let finalExpr = ANF.Return listAtom
                    let exprWithBindings = wrapBindings listBindings finalExpr
                    (exprWithBindings, varGen2)))

    | AST.Match (scrutinee, cases) ->
        // Compile match to if-else chain
        // First convert scrutinee to atom
        toAtom scrutinee varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (scrutineeAtom, scrutineeBindings, varGen1) ->
            // Check if any pattern needs to access list structure
            // If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
            let hasNonEmptyListPattern =
                cases |> List.exists (fun mc ->
                    mc.Patterns |> List.exists (fun pat ->
                        match pat with
                        | AST.PList (_ :: _) -> true
                        | AST.PListCons (_ :: _, _) -> true  // [h, ...t] also needs list access
                        | _ -> false))

            // If there are non-empty list patterns, bind the scrutinee to a variable
            let (scrutineeAtom', scrutineeBindings', varGen1') =
                match scrutineeAtom with
                | ANF.Var _ -> (scrutineeAtom, scrutineeBindings, varGen1)
                | _ when hasNonEmptyListPattern ->
                    let (tempVar, vg) = ANF.freshVar varGen1
                    (ANF.Var tempVar, scrutineeBindings @ [(tempVar, ANF.Atom scrutineeAtom)], vg)
                | _ -> (scrutineeAtom, scrutineeBindings, varGen1)

            // Check if the TYPE that a variant belongs to has any variant with a payload
            // This determines if values are heap-allocated or simple integers
            let typeHasAnyPayload (variantName: string) : bool =
                match Map.tryFind variantName variantLookup with
                | Some (typeName, _, _, _) ->
                    variantLookup
                    |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)
                | None -> false

            // Check if pattern always matches (wildcard or variable)
            let rec patternAlwaysMatches (pattern: AST.Pattern) : bool =
                match pattern with
                | AST.PUnit -> true
                | AST.PWildcard -> true
                | AST.PVar _ -> true
                | _ -> false

            // Extract pattern bindings and compile body with extended environment
            // Note: Pattern-bound variables use TInt64 as placeholder type since full type inference
            // for pattern variables requires scrutinee type analysis (future improvement)
            let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (currentEnv: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match pattern with
                | AST.PUnit -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PWildcard -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PLiteral _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PBool _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PString _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PFloat _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PVar name ->
                    // Bind scrutinee to variable name
                    let (tempId, vg1) = ANF.freshVar vg
                    // Use TInt64 as placeholder - pattern variable type tracking is future work
                    let env' = Map.add name (tempId, AST.TInt64) currentEnv
                    toANF body vg1 env' typeReg variantLookup funcReg
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                        (expr, vg2))
                | AST.PConstructor (_, payloadPattern) ->
                    match payloadPattern with
                    | None -> toANF body vg currentEnv typeReg variantLookup funcReg
                    | Some innerPattern ->
                        // Extract payload from heap-allocated variant
                        // Variant layout: [tag:8][payload:8], so payload is at index 1
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let payloadExpr = ANF.TupleGet (scrutAtom, 1)
                        // Now compile the inner pattern with the payload
                        extractAndCompileBody innerPattern body (ANF.Var payloadVar) currentEnv vg1
                        |> Result.map (fun (innerExpr, vg2) ->
                            let expr = ANF.Let (payloadVar, payloadExpr, innerExpr)
                            (expr, vg2))
                | AST.PTuple patterns ->
                    // Recursively collect all variable bindings from a pattern
                    // Returns: updated env, list of bindings, updated vargen
                    let rec collectPatternBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pat with
                        | AST.PUnit | AST.PWildcard | AST.PLiteral _ | AST.PBool _ | AST.PString _ | AST.PFloat _ ->
                            // No variable bindings
                            Ok (env, bindings, vg)
                        | AST.PVar name ->
                            // Bind the source to a variable
                            let (tempId, vg1) = ANF.freshVar vg
                            let binding = (tempId, ANF.Atom sourceAtom)
                            let newEnv = Map.add name (tempId, AST.TInt64) env
                            Ok (newEnv, binding :: bindings, vg1)
                        | AST.PTuple innerPatterns ->
                            // Extract each element and recursively collect bindings
                            let rec collectFromTuple (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] -> Ok (env, bindings, vg)
                                | p :: rest ->
                                    let (elemVar, vg1) = ANF.freshVar vg
                                    let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                    let elemBinding = (elemVar, elemExpr)
                                    // Recursively collect bindings from this element's pattern
                                    collectPatternBindings p (ANF.Var elemVar) env (elemBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromTuple rest (idx + 1) env' bindings' vg')
                            collectFromTuple innerPatterns 0 env bindings vg
                        | AST.PConstructor (_, payloadPattern) ->
                            match payloadPattern with
                            | None -> Ok (env, bindings, vg)
                            | Some innerPat ->
                                // Extract payload (at index 1) and recursively collect
                                let (payloadVar, vg1) = ANF.freshVar vg
                                let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                                let payloadBinding = (payloadVar, payloadExpr)
                                collectPatternBindings innerPat (ANF.Var payloadVar) env (payloadBinding :: bindings) vg1
                        | AST.PRecord (_, fieldPatterns) ->
                            // Extract each field and recursively collect bindings
                            let rec collectFromRecord (fields: (string * AST.Pattern) list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match fields with
                                | [] -> Ok (env, bindings, vg)
                                | (_, p) :: rest ->
                                    let (fieldVar, vg1) = ANF.freshVar vg
                                    let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                    let fieldBinding = (fieldVar, fieldExpr)
                                    collectPatternBindings p (ANF.Var fieldVar) env (fieldBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromRecord rest (idx + 1) env' bindings' vg')
                            collectFromRecord fieldPatterns 0 env bindings vg
                        | AST.PList innerPatterns ->
                            // For list patterns, extract head elements
                            let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] -> Ok (env, bindings, vg)
                                | p :: rest ->
                                    // Head is at index 0, tail at index 1
                                    let (headVar, vg1) = ANF.freshVar vg
                                    let headExpr = ANF.TupleGet (currentList, 0)
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) env (headBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        if List.isEmpty rest then
                                            Ok (env', bindings', vg')
                                        else
                                            // Get tail for next iteration
                                            let (tailVar, vg2) = ANF.freshVar vg'
                                            let tailExpr = ANF.TupleGet (currentList, 1)
                                            let tailBinding = (tailVar, tailExpr)
                                            collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                            collectFromList innerPatterns sourceAtom env bindings vg
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // Extract head elements then bind tail
                            let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] ->
                                    // Bind the remaining list to tail pattern
                                    collectPatternBindings tailPattern currentList env bindings vg
                                | p :: rest ->
                                    let (headVar, vg1) = ANF.freshVar vg
                                    let headExpr = ANF.TupleGet (currentList, 0)
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) env (headBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        let (tailVar, vg2) = ANF.freshVar vg'
                                        let tailExpr = ANF.TupleGet (currentList, 1)
                                        let tailBinding = (tailVar, tailExpr)
                                        collectHeads rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                            collectHeads headPatterns sourceAtom env bindings vg

                    // Collect all bindings from the tuple pattern, then compile body
                    collectPatternBindings (AST.PTuple patterns) scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings (List.rev bindings) bodyExpr
                            (finalExpr, vg2)))
                | AST.PRecord (_, fieldPatterns) ->
                    // Extract each field and bind pattern variables
                    let rec collectRecordBindings (fields: (string * AST.Pattern) list) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) (fieldIdx: int) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match fields with
                        | [] -> Ok (env, List.rev bindings, vg)
                        | (_, pat) :: rest ->
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use TInt64 as placeholder type
                                let newEnv = Map.add name (fieldVar, AST.TInt64) env
                                collectRecordBindings rest newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest env bindings vg1 (fieldIdx + 1)
                            | _ ->
                                collectRecordBindings rest env (binding :: bindings) vg1 (fieldIdx + 1)
                    collectRecordBindings fieldPatterns currentEnv [] vg 0
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PList patterns ->
                    // Extract list elements: walk through Cons cells
                    // List layout: Nil = 0, Cons = [tag=1, head, tail]
                    let rec collectListBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, vg)
                        | pat :: rest ->
                            // Extract head from current Cons cell (index 1)
                            let (headVar, vg1) = ANF.freshVar vg
                            let headExpr = ANF.TupleGet (listAtom, 1)
                            let headBinding = (headVar, headExpr)
                            // Extract tail from current Cons cell (index 2)
                            let (tailVar, vg2) = ANF.freshVar vg1
                            let tailExpr = ANF.TupleGet (listAtom, 2)
                            let tailBinding = (tailVar, tailExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use TInt64 as placeholder type
                                let newEnv = Map.add name (headVar, AST.TInt64) env
                                collectListBindings rest (ANF.Var tailVar) newEnv (tailBinding :: headBinding :: bindings) vg2
                            | AST.PWildcard ->
                                collectListBindings rest (ANF.Var tailVar) env (tailBinding :: bindings) vg2
                            | _ ->
                                collectListBindings rest (ANF.Var tailVar) env (tailBinding :: headBinding :: bindings) vg2
                    collectListBindings patterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PListCons (headPatterns, tailPattern) ->
                    // Extract head elements and bind tail
                    // List layout: Nil = 0, Cons = [tag=1, head, tail]
                    let rec collectListConsBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, listAtom, vg)
                        | pat :: rest ->
                            // Extract head from current Cons cell (index 1)
                            let (headVar, vg1) = ANF.freshVar vg
                            let headExpr = ANF.TupleGet (listAtom, 1)
                            let headBinding = (headVar, headExpr)
                            // Extract tail from current Cons cell (index 2)
                            let (tailVar, vg2) = ANF.freshVar vg1
                            let tailExpr = ANF.TupleGet (listAtom, 2)
                            let tailBinding = (tailVar, tailExpr)
                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (headVar, AST.TInt64) env
                                collectListConsBindings rest (ANF.Var tailVar) newEnv (tailBinding :: headBinding :: bindings) vg2
                            | AST.PWildcard ->
                                collectListConsBindings rest (ANF.Var tailVar) env (tailBinding :: bindings) vg2
                            | _ ->
                                collectListConsBindings rest (ANF.Var tailVar) env (tailBinding :: headBinding :: bindings) vg2
                    collectListConsBindings headPatterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, tailAtom, vg1) ->
                        // Bind tail pattern
                        match tailPattern with
                        | AST.PVar name ->
                            let (tailVar, vg2) = ANF.freshVar vg1
                            // Tail is a list, use TList TInt64 as placeholder
                            let newEnv' = Map.add name (tailVar, AST.TList AST.TInt64) newEnv
                            toANF body vg2 newEnv' typeReg variantLookup funcReg
                            |> Result.map (fun (bodyExpr, vg3) ->
                                let tailBinding = (tailVar, ANF.Atom tailAtom)
                                let allBindings = bindings @ [tailBinding]
                                let finalExpr = wrapBindings allBindings bodyExpr
                                (finalExpr, vg3))
                        | AST.PWildcard ->
                            toANF body vg1 newEnv typeReg variantLookup funcReg
                            |> Result.map (fun (bodyExpr, vg2) ->
                                let finalExpr = wrapBindings bindings bodyExpr
                                (finalExpr, vg2))
                        | _ -> Error "Tail pattern in list cons must be variable or wildcard")

            // Extract pattern bindings, check guard, and compile body
            // Returns: if guard is true, execute body; otherwise execute elseExpr
            and extractAndCompileBodyWithGuard (pattern: AST.Pattern) (guardExpr: AST.Expr) (body: AST.Expr) (scrutAtom: ANF.Atom) (currentEnv: VarEnv) (vg: ANF.VarGen) (elseExpr: ANF.AExpr) : Result<ANF.AExpr * ANF.VarGen, string> =
                // First, we need to extract bindings from the pattern
                // Then compile the guard with those bindings in scope
                // Then compile the body with those bindings in scope
                // Finally, generate: let <bindings> in if <guard> then <body> else <elseExpr>

                // Helper to collect pattern variable bindings (simplified version for common patterns)
                let rec collectBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
                    match pat with
                    | AST.PUnit | AST.PWildcard | AST.PLiteral _ | AST.PBool _ | AST.PString _ | AST.PFloat _ ->
                        (env, bindings, vg)
                    | AST.PVar name ->
                        let (tempId, vg1) = ANF.freshVar vg
                        let binding = (tempId, ANF.Atom sourceAtom)
                        let newEnv = Map.add name (tempId, AST.TInt64) env
                        (newEnv, binding :: bindings, vg1)
                    | AST.PTuple innerPatterns ->
                        let rec collectFromTuple pats idx env bindings vg =
                            match pats with
                            | [] -> (env, bindings, vg)
                            | p :: rest ->
                                let (elemVar, vg1) = ANF.freshVar vg
                                let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var elemVar) env ((elemVar, elemExpr) :: bindings) vg1
                                collectFromTuple rest (idx + 1) env' bindings' vg'
                        collectFromTuple innerPatterns 0 env bindings vg
                    | AST.PConstructor (_, Some innerPat) ->
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                        collectBindings innerPat (ANF.Var payloadVar) env ((payloadVar, payloadExpr) :: bindings) vg1
                    | AST.PConstructor (_, None) ->
                        (env, bindings, vg)
                    | AST.PRecord (_, fieldPatterns) ->
                        let rec collectFromRecord fields idx env bindings vg =
                            match fields with
                            | [] -> (env, bindings, vg)
                            | (_, p) :: rest ->
                                let (fieldVar, vg1) = ANF.freshVar vg
                                let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var fieldVar) env ((fieldVar, fieldExpr) :: bindings) vg1
                                collectFromRecord rest (idx + 1) env' bindings' vg'
                        collectFromRecord fieldPatterns 0 env bindings vg
                    | AST.PList _ | AST.PListCons _ ->
                        // List patterns with guards - simplified handling
                        (env, bindings, vg)

                let (newEnv, bindings, vg1) = collectBindings pattern scrutAtom currentEnv [] vg

                // Compile guard expression in the extended environment
                toAtom guardExpr vg1 newEnv typeReg variantLookup funcReg
                |> Result.bind (fun (guardAtom, guardBindings, vg2) ->
                    // Compile body expression in the extended environment
                    toANF body vg2 newEnv typeReg variantLookup funcReg
                    |> Result.map (fun (bodyExpr, vg3) ->
                        // Build: if guard then body else elseExpr
                        let ifExpr = ANF.If (guardAtom, bodyExpr, elseExpr)
                        // Wrap guard bindings
                        let withGuardBindings = wrapBindings guardBindings ifExpr
                        // Wrap pattern bindings (in reverse order since we accumulated in reverse)
                        let finalExpr = wrapBindings (List.rev bindings) withGuardBindings
                        (finalExpr, vg3)))

            // Build comparison expression for a pattern
            let rec buildPatternComparison (pattern: AST.Pattern) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                match pattern with
                | AST.PUnit -> Ok None  // Unit pattern always matches unit type
                | AST.PWildcard -> Ok None
                | AST.PVar _ -> Ok None
                | AST.PLiteral n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral n)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PBool b ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.BoolLiteral b)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PString s ->
                    // String comparison - for now just compare as atoms
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.StringLiteral s)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PFloat f ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.FloatLiteral f)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PConstructor (variantName, _) ->
                    match Map.tryFind variantName variantLookup with
                    | Some (_, _, tag, _) ->
                        if typeHasAnyPayload variantName then
                            // Load tag from heap (index 0), then compare
                            let (tagVar, vg1) = ANF.freshVar vg
                            let tagLoadExpr = ANF.TupleGet (scrutAtom, 0)
                            let (cmpVar, vg2) = ANF.freshVar vg1
                            let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (int64 tag))
                            Ok (Some (ANF.Var cmpVar, [(tagVar, tagLoadExpr); (cmpVar, cmpExpr)], vg2))
                        else
                            // Simple enum - scrutinee IS the tag
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (int64 tag))
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    | None -> Error $"Unknown constructor in pattern: {variantName}"
                | AST.PTuple _ -> Ok None  // Tuple patterns just bind, don't compare
                | AST.PRecord _ -> Ok None  // Record patterns just bind, don't compare
                | AST.PList patterns ->
                    // List pattern comparison: check length matches
                    // [] matches Nil (0), [a, b, ...] needs scrutinee to be non-nil first
                    // IMPORTANT: Must check non-nil BEFORE loading tag to avoid null deref
                    if List.isEmpty patterns then
                        // Empty list pattern: check scrutinee == 0
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral 0L)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    else
                        // Non-empty list pattern: check scrutinee != 0 (not Nil)
                        // We only check that it's a Cons (non-nil). Length matching
                        // is not fully validated here - mismatched lengths may crash
                        // when extracting elements. Full validation would need nested ifs.
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Neq, scrutAtom, ANF.IntLiteral 0L)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PListCons (headPatterns, _) ->
                    // List cons pattern: [...t] matches any list, [h, ...t] needs at least one element
                    if List.isEmpty headPatterns then
                        // [...t] matches any list including empty
                        Ok None
                    else
                        // [h, ...t] or [a, b, ...t] - needs at least one element (non-nil)
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Neq, scrutAtom, ANF.IntLiteral 0L)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))

            // Compile a list pattern with proper length validation.
            // Generates nested if-expressions that check each element exists before extracting.
            // This fixes the bug where [a, b] pattern would crash when matching against [1].
            let rec compileListPatternWithChecks
                (patterns: AST.Pattern list)
                (listAtom: ANF.Atom)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                match patterns with
                | [] ->
                    // All elements extracted - check tail is nil for exact length matching
                    // listAtom should be nil (0) if pattern matched exactly
                    let (tailCheckVar, vg1) = ANF.freshVar vg
                    let tailCheckExpr = ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral 0L)
                    toANF body vg1 currentEnv typeReg variantLookup funcReg
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let ifExpr = ANF.If (ANF.Var tailCheckVar, bodyExpr, elseExpr)
                        (ANF.Let (tailCheckVar, tailCheckExpr, ifExpr), vg2))

                | pat :: restPatterns ->
                    // Check current position is non-nil (list has at least one more element)
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral 0L)

                    // Extract head (index 1) and tail (index 2) from Cons cell
                    let (headVar, vg2) = ANF.freshVar vg1
                    let headExpr = ANF.TupleGet (listAtom, 1)
                    let (tailVar, vg3) = ANF.freshVar vg2
                    let tailExpr = ANF.TupleGet (listAtom, 2)

                    // Update environment based on pattern type
                    let newEnv =
                        match pat with
                        | AST.PVar name -> Map.add name (headVar, AST.TInt64) currentEnv
                        | _ -> currentEnv

                    // Recursively compile rest of list pattern
                    compileListPatternWithChecks restPatterns (ANF.Var tailVar) newEnv body elseExpr vg3
                    |> Result.map (fun (innerExpr, vg4) ->
                        // Build nested structure:
                        // let checkVar = (listAtom != 0) in
                        //   if checkVar then
                        //     let headVar = TupleGet(listAtom, 1) in
                        //     let tailVar = TupleGet(listAtom, 2) in
                        //     <innerExpr>
                        //   else
                        //     <elseExpr>
                        let withTail = ANF.Let (tailVar, tailExpr, innerExpr)
                        let withHead = ANF.Let (headVar, headExpr, withTail)
                        let ifExpr = ANF.If (ANF.Var checkVar, withHead, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg4))

            // Compile a list cons pattern [h, ...t] with proper checks
            let rec compileListConsPatternWithChecks
                (headPatterns: AST.Pattern list)
                (tailPattern: AST.Pattern)
                (listAtom: ANF.Atom)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                match headPatterns with
                | [] ->
                    // All head elements extracted - bind tail and compile body
                    match tailPattern with
                    | AST.PVar name ->
                        let (tailVar, vg1) = ANF.freshVar vg
                        let newEnv = Map.add name (tailVar, AST.TList AST.TInt64) currentEnv
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let withTail = ANF.Let (tailVar, ANF.Atom listAtom, bodyExpr)
                            (withTail, vg2))
                    | AST.PWildcard ->
                        toANF body vg currentEnv typeReg variantLookup funcReg
                    | _ -> Error "Tail pattern in list cons must be variable or wildcard"

                | pat :: restPatterns ->
                    // Check current position is non-nil
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral 0L)

                    // Extract head (index 1) and tail (index 2)
                    let (headVar, vg2) = ANF.freshVar vg1
                    let headExpr = ANF.TupleGet (listAtom, 1)
                    let (tailVar, vg3) = ANF.freshVar vg2
                    let tailExpr = ANF.TupleGet (listAtom, 2)

                    // Update env based on head pattern
                    let newEnv =
                        match pat with
                        | AST.PVar name -> Map.add name (headVar, AST.TInt64) currentEnv
                        | _ -> currentEnv

                    // Recursively compile rest
                    compileListConsPatternWithChecks restPatterns tailPattern (ANF.Var tailVar) newEnv body elseExpr vg3
                    |> Result.map (fun (innerExpr, vg4) ->
                        let withTail = ANF.Let (tailVar, tailExpr, innerExpr)
                        let withHead = ANF.Let (headVar, headExpr, withTail)
                        let ifExpr = ANF.If (ANF.Var checkVar, withHead, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg4))

            // Build OR of multiple pattern conditions for pattern grouping
            // Returns: combined condition atom, all bindings, updated vargen
            let buildPatternGroupComparison (patterns: AST.Pattern list) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                match patterns with
                | [] -> Ok None
                | [single] -> buildPatternComparison single scrutAtom vg
                | multiple ->
                    // Build comparison for each pattern, then OR them together
                    let rec buildOr (pats: AST.Pattern list) (accCondOpt: ANF.Atom option) (accBindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                        match pats with
                        | [] ->
                            match accCondOpt with
                            | None -> Ok None
                            | Some cond -> Ok (Some (cond, accBindings, vg))
                        | pat :: rest ->
                            buildPatternComparison pat scrutAtom vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches (wildcard/var) - the whole group always matches
                                    Ok None
                                | Some (condAtom, bindings, vg1) ->
                                    let (newCondOpt, newBindings, vg2) =
                                        match accCondOpt with
                                        | None ->
                                            // First condition
                                            (Some condAtom, bindings @ accBindings, vg1)
                                        | Some accCond ->
                                            // OR with previous conditions
                                            // Put bindings in dependency order: comparison bindings first, OR at end
                                            // (foldBack makes first binding outermost, so dependencies must come first)
                                            let (orVar, vg') = ANF.freshVar vg1
                                            let orExpr = ANF.Prim (ANF.Or, accCond, condAtom)
                                            (Some (ANF.Var orVar), accBindings @ bindings @ [(orVar, orExpr)], vg')
                                    buildOr rest newCondOpt newBindings vg2)
                    buildOr multiple None [] vg

            // Build the if-else chain from cases
            let rec buildChain (remaining: AST.MatchCase list) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match remaining with
                | [] ->
                    // No cases left - shouldn't happen if we have wildcard/var
                    Error "Non-exhaustive pattern match"
                | [mc] ->
                    // Last case - for most patterns just compile the body
                    // For now, use first pattern (pattern grouping not yet fully supported)
                    let pattern = List.head mc.Patterns
                    let body = mc.Body
                    // For non-empty list patterns, we still need proper length checking
                    match pattern with
                    | AST.PList (_ :: _ as listPatterns) ->
                        // For list patterns as last case, generate proper checks
                        // If the pattern doesn't match, this is a non-exhaustive match
                        // We generate an expression that returns 0 as a fallback (TODO: proper error)
                        let fallbackExpr = ANF.Return (ANF.IntLiteral 0L)
                        compileListPatternWithChecks listPatterns scrutineeAtom' env body fallbackExpr vg
                    | AST.PListCons (headPatterns, tailPattern) ->
                        // List cons pattern as last case
                        let fallbackExpr = ANF.Return (ANF.IntLiteral 0L)
                        compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' env body fallbackExpr vg
                    | _ ->
                        // Other patterns - original behavior
                        // Handle guard if present
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody pattern body scrutineeAtom' env vg
                        | Some guardExpr ->
                            // With guard: compile pattern match with guard check
                            let fallbackExpr = ANF.Return (ANF.IntLiteral 0L)
                            extractAndCompileBodyWithGuard pattern guardExpr body scrutineeAtom' env vg fallbackExpr
                | mc :: rest ->
                    // For pattern grouping, use first pattern for bindings but OR all patterns for comparison
                    let firstPattern = List.head mc.Patterns
                    let body = mc.Body
                    if patternAlwaysMatches firstPattern then
                        // Wildcard or var - matches everything, but may still need guard
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody firstPattern body scrutineeAtom' env vg
                        | Some guardExpr ->
                            // Wildcard with guard - still need to check guard, fall through if false
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' env vg1 elseExpr)
                    else
                        // Non-empty list patterns need special handling with interleaved checks
                        match firstPattern with
                        | AST.PList (_ :: _ as listPatterns) ->
                            // Build the else branch first (rest of cases)
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                // Use the new interleaved check-and-extract function
                                compileListPatternWithChecks listPatterns scrutineeAtom' env body elseExpr vg1)
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // List cons pattern - needs interleaved checks
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' env body elseExpr vg1)
                        | _ ->
                            // Use pattern grouping: OR all patterns in the group
                            buildPatternGroupComparison mc.Patterns scrutineeAtom' vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' env vg
                                    | Some guardExpr ->
                                        buildChain rest vg
                                        |> Result.bind (fun (elseExpr, vg1) ->
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' env vg1 elseExpr)
                                | Some (condAtom, bindings, vg1) ->
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' env vg1
                                        |> Result.bind (fun (thenExpr, vg2) ->
                                            buildChain rest vg2
                                            |> Result.map (fun (elseExpr, vg3) ->
                                                let ifExpr = ANF.If (condAtom, thenExpr, elseExpr)
                                                let finalExpr = wrapBindings bindings ifExpr
                                                (finalExpr, vg3)))
                                    | Some guardExpr ->
                                        // Pattern match + guard: if pattern matches, bind, check guard
                                        buildChain rest vg1
                                        |> Result.bind (fun (elseExpr, vg2) ->
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' env vg2 elseExpr
                                            |> Result.map (fun (guardedBody, vg3) ->
                                                let ifExpr = ANF.If (condAtom, guardedBody, elseExpr)
                                                let finalExpr = wrapBindings bindings ifExpr
                                                (finalExpr, vg3))))

            buildChain cases varGen1'
            |> Result.map (fun (chainExpr, varGen2) ->
                let exprWithBindings = wrapBindings scrutineeBindings' chainExpr
                (exprWithBindings, varGen2)))

    | AST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        // $"Hello {name}!" → "Hello " ++ name ++ "!"
        let partToExpr (part: AST.StringPart) : AST.Expr =
            match part with
            | AST.StringText s -> AST.StringLiteral s
            | AST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.Return (ANF.StringLiteral ""), varGen)
        | [single] ->
            // Single part → convert directly
            toANF (partToExpr single) varGen env typeReg variantLookup funcReg
        | first :: rest ->
            // Multiple parts → fold with StringConcat
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toANF desugared varGen env typeReg variantLookup funcReg

    | AST.Lambda (_parameters, _body) ->
        // Lambda in expression position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | AST.Apply (func, args) ->
        // Apply a function expression to arguments
        // For now, only support immediate application of lambdas
        match func with
        | AST.Lambda (parameters, body) ->
            // Immediate application: ((x: int) => x + 1)(5) becomes let x = 5 in x + 1
            if List.length args <> List.length parameters then
                Error $"Lambda expects {List.length parameters} arguments, got {List.length args}"
            else
                // Build nested let bindings: let p1 = arg1 in let p2 = arg2 in ... body
                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | (pName, _) :: restPs, argExpr :: restAs ->
                        AST.Let (pName, argExpr, buildLets restPs restAs)
                    | _ -> body  // Should not happen due to length check
                let desugared = buildLets parameters args
                toANF desugared varGen env typeReg variantLookup funcReg
        | AST.Var name ->
            // Calling a variable that holds a function - not yet supported
            Error $"Cannot apply variable '{name}' as function - closures not yet fully implemented"
        | _ ->
            // Complex function expression - not yet supported
            Error "Complex function application not yet fully implemented"

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        Ok (ANF.UnitLiteral, [], varGen)

    | AST.IntLiteral n ->
        Ok (ANF.IntLiteral n, [], varGen)

    | AST.Int8Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.Int16Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.Int32Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.UInt64Literal n ->
        Ok (ANF.IntLiteral (int64 n), [], varGen)

    | AST.BoolLiteral b ->
        Ok (ANF.BoolLiteral b, [], varGen)

    | AST.StringLiteral s ->
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.FloatLiteral f ->
        Ok (ANF.FloatLiteral f, [], varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some (tempId, _) -> Ok (ANF.Var tempId, [], varGen)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some _ ->
                // Module function reference - wrap in closure for uniform calling convention
                let (closureId, varGen') = ANF.freshVar varGen
                let closureAlloc = ANF.ClosureAlloc (name, [])
                Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
            | None ->
                // Check if it's a function reference (function name used as value)
                if Map.containsKey name funcReg then
                    // Wrap in closure for uniform calling convention
                    let (closureId, varGen') = ANF.freshVar varGen
                    let closureAlloc = ANF.ClosureAlloc (name, [])
                    Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
                else
                    Error $"Undefined variable: {name}"

    | AST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')

    | AST.Closure (funcName, captures) ->
        // Closure in atom position: convert captures and create ClosureAlloc binding
        let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | cap :: rest ->
                toAtom cap vg env typeReg variantLookup funcReg
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Create binding for ClosureAlloc
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            (ANF.Var closureId, allBindings @ [(closureId, closureAlloc)], varGen2))

    | AST.Let (name, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toAtom body varGen2 env' typeReg variantLookup funcReg |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                    // All bindings: valueBindings + binding tempId to value + bodyBindings
                    let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
                    (bodyAtom, allBindings, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.IntLiteral System.Int64.MinValue, [], varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.FloatLiteral (-f), [], varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg variantLookup funcReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create the operation
                let (tempVar, varGen3) = ANF.freshVar varGen2
                // StringConcat is a separate CExpr, not a Prim
                let cexpr =
                    match op with
                    | AST.StringConcat -> ANF.StringConcat (leftAtom, rightAtom)
                    | _ -> ANF.Prim (convertBinOp op, leftAtom, rightAtom)

                // Return the temp variable as atom, plus all bindings
                let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg variantLookup funcReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg variantLookup funcReg |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg variantLookup funcReg |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
                    // Create a temporary for the result
                    let (tempVar, varGen4) = ANF.freshVar varGen3
                    // Create an IfValue CExpr
                    let ifCExpr = ANF.IfValue (condAtom, thenAtom, elseAtom)
                    // Return temp as atom with all bindings
                    let allBindings = condBindings @ thenBindings @ elseBindings @ [(tempVar, ifCExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen4))))

    | AST.Call (funcName, args) ->
        // Function call in atom position: convert all arguments to atoms
        let rec convertArgs (argExprs: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match argExprs with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | arg :: rest ->
                toAtom arg vg env typeReg variantLookup funcReg
                |> Result.bind (fun (argAtom, argBindings, vg') ->
                    convertArgs rest vg' (argAtom :: accAtoms) (accBindings @ argBindings))

        convertArgs args varGen [] []
        |> Result.bind (fun (argAtoms, argBindings, varGen1) ->
            // Create a temporary for the call result
            let (tempVar, varGen2) = ANF.freshVar varGen1
            // Check if funcName is a variable (indirect call) or a defined function (direct call)
            match Map.tryFind funcName env with
            | Some (tempId, AST.TFunction (_, _)) ->
                // Variable with function type - use closure call
                // All function values are now closures (even non-capturing ones)
                let callCExpr = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                let allBindings = argBindings @ [(tempVar, callCExpr)]
                Ok (ANF.Var tempVar, allBindings, varGen2)
            | Some (_, varType) ->
                // Variable exists but is not a function type
                Error $"Cannot call '{funcName}' - it has type {varType}, not a function type"
            | None ->
                // Not a variable - check if it's a file intrinsic first
                match tryFileIntrinsic funcName argAtoms with
                | Some intrinsicExpr ->
                    // File I/O intrinsic call
                    let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen2)
                | None ->
                    // Check if it's a raw memory intrinsic
                    match tryRawMemoryIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Raw memory intrinsic call
                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2)
                    | None ->
                        // Assume it's a defined function (direct call)
                        let callCExpr = ANF.Call (funcName, argAtoms)
                        let allBindings = argBindings @ [(tempVar, callCExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2))

    | AST.TypeApp (_, _, _) ->
        // Placeholder: Generic instantiation not yet implemented
        Error "TypeApp (generic instantiation) not yet implemented in toAtom"

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg variantLookup funcReg
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create a temporary for the tuple
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let tupleCExpr = ANF.TupleAlloc elemAtoms
            // Return temp as atom with all bindings
            let allBindings = elemBindings @ [(tempVar, tupleCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg variantLookup funcReg
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let getCExpr = ANF.TupleGet (tupleAtom, index)
            // Return temp as atom with all bindings
            let allBindings = tupleBindings @ [(tempVar, getCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> fields |> List.map fst

        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Reuse tuple handling
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg
                        |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                            let (tempVar, varGen2) = ANF.freshVar varGen1
                            let getCExpr = ANF.TupleGet (recordAtom, index)
                            let allBindings = recordBindings @ [(tempVar, getCExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, _, tag, _) ->
            // Check if ANY variant in this type has a payload
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type: return tag as an integer (no bindings needed)
                Ok (ANF.IntLiteral (int64 tag), [], varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag] for consistent representation
                let tagAtom = ANF.IntLiteral (int64 tag)
                let (tempVar, varGen1) = ANF.freshVar varGen
                let tupleCExpr = ANF.TupleAlloc [tagAtom]
                Ok (ANF.Var tempVar, [(tempVar, tupleCExpr)], varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as linked list in atom position
        let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (tailAtom: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (tailAtom, allBindings, vg)
            | elem :: rest ->
                // First build the rest of the list
                buildList rest vg tailAtom allBindings
                |> Result.bind (fun (restAtom, restBindings, vg1) ->
                    // Now add this element: Cons(elem, rest)
                    toAtom elem vg1 env typeReg variantLookup funcReg
                    |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                        let (consVar, vg3) = ANF.freshVar vg2
                        // Cons = [tag=1, head, tail]
                        let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                        let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                        (ANF.Var consVar, newBindings, vg3)))

        if List.isEmpty elements then
            // Empty list is Nil (represented as 0)
            Ok (ANF.IntLiteral 0L, [], varGen)
        else
            // Build the list starting with Nil
            buildList elements varGen (ANF.IntLiteral 0L) []

    | AST.ListCons (headElements, tail) ->
        // Compile list cons in atom position: [a, b, ...tail] prepends elements to tail
        toAtom tail varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentTail: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentTail, allBindings, vg)
                | elem :: rest ->
                    buildList rest vg currentTail allBindings
                    |> Result.bind (fun (restAtom, restBindings, vg1) ->
                        toAtom elem vg1 env typeReg variantLookup funcReg
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (consVar, vg3) = ANF.freshVar vg2
                            let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                            let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                            (ANF.Var consVar, newBindings, vg3)))

            if List.isEmpty headElements then
                Ok (tailAtom, tailBindings, varGen1)
            else
                buildList headElements varGen1 tailAtom tailBindings)

    | AST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        let partToExpr (part: AST.StringPart) : AST.Expr =
            match part with
            | AST.StringText s -> AST.StringLiteral s
            | AST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.StringLiteral "", [], varGen)
        | [single] ->
            // Single part → convert directly
            toAtom (partToExpr single) varGen env typeReg variantLookup funcReg
        | first :: rest ->
            // Multiple parts → desugar to StringConcat and convert
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toAtom desugared varGen env typeReg variantLookup funcReg

    | AST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANF (AST.Match (scrutinee, cases)) varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (matchExpr, varGen1) ->
            // The match compiles to an if-else chain that returns a value
            // We need to extract that value into a temp variable
            // For now, just return an error - complex match in atom position needs more work
            Error "Match expressions in atom position not yet supported (use let binding)")

    | AST.Lambda (_parameters, _body) ->
        // Lambda in atom position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | AST.Apply (func, args) ->
        // Apply in atom position - convert via toANF and extract result
        match func with
        | AST.Lambda (parameters, body) ->
            // Immediate application: desugar to let bindings
            if List.length args <> List.length parameters then
                Error $"Lambda expects {List.length parameters} arguments, got {List.length args}"
            else
                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | (pName, _) :: restPs, argExpr :: restAs ->
                        AST.Let (pName, argExpr, buildLets restPs restAs)
                    | _ -> body
                let desugared = buildLets parameters args
                toAtom desugared varGen env typeReg variantLookup funcReg
        | _ ->
            Error "Complex function application in atom position not yet supported"

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.Function * ANF.VarGen, string> =
    // Allocate TempIds for parameters
    let (paramIds, varGen1) =
        funcDef.Params
        |> List.fold (fun (ids, vg) (_, _) ->
            let (tempId, vg') = ANF.freshVar vg
            (ids @ [tempId], vg')) ([], varGen)

    // Build environment mapping param names to (TempId, Type)
    let paramEnv : VarEnv =
        List.zip funcDef.Params paramIds
        |> List.map (fun ((name, typ), tempId) -> (name, (tempId, typ)))
        |> Map.ofList

    // Convert body
    toANF funcDef.Body varGen1 paramEnv typeReg variantLookup funcReg
    |> Result.map (fun (body, varGen2) ->
        ({ Name = funcDef.Name; Params = paramIds; Body = body }, varGen2))

/// Result type that includes registries needed for later passes
type ConversionResult = {
    Program: ANF.Program
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> param list with types
}

/// Convert a program to ANF with type information for reference counting
let convertProgramWithTypes (program: AST.Program) : Result<ConversionResult, string> =
    // First, monomorphize the program (specialize generic functions, replace TypeApp with Call)
    let monomorphizedProgram = monomorphize program
    // Then, inline lambdas at their call sites for first-class function support
    let inlinedProgram = inlineLambdasInProgram monomorphizedProgram
    // Then, lift non-capturing lambdas to top-level functions
    liftLambdasInProgram inlinedProgram
    |> Result.bind (fun liftedProgram ->
    let (AST.Program topLevels) = liftedProgram
    let varGen = ANF.VarGen 0

    // Build type registry from type definitions
    // Note: typeParams are stored but not used for ANF conversion (monomorphized at use sites)
    let typeReg : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    // Build variant lookup from sum type definitions
    // Note: typeParams are stored but not used here (monomorphization happens elsewhere)
    let variantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Separate functions and expressions
    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    // Build function registry - maps function names to their FULL function types
    let funcReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    // Build function parameters map (includes user-defined and module functions)
    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f -> (f.Name, f.Params))
        |> Map.ofList

    // Add module function parameters from Stdlib
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            // Create parameter names like "arg0", "arg1" for each parameter type
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let funcParams = Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams

    // Convert all functions
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup funcReg
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    // Stdlib functions are now loaded from stdlib.dark and included as regular functions
    convertFunctions functions varGen []
    |> Result.bind (fun (anfFuncs, varGen1) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv typeReg variantLookup funcReg
            |> Result.map (fun (anfExpr, _) ->
                { Program = ANF.Program (anfFuncs, anfExpr)
                  TypeReg = typeReg
                  VariantLookup = variantLookup
                  FuncReg = funcReg
                  FuncParams = funcParams })
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))

/// Convert a program to ANF
let convertProgram (program: AST.Program) : Result<ANF.Program, string> =
    // First, monomorphize the program (specialize generic functions, replace TypeApp with Call)
    let monomorphizedProgram = monomorphize program
    // Then, inline lambdas at their call sites for first-class function support
    let inlinedProgram = inlineLambdasInProgram monomorphizedProgram
    // Then, lift non-capturing lambdas to top-level functions
    liftLambdasInProgram inlinedProgram
    |> Result.bind (fun liftedProgram ->
    let (AST.Program topLevels) = liftedProgram
    let varGen = ANF.VarGen 0

    // Build type registry from type definitions
    let typeReg : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    // Build variant lookup from sum type definitions
    // Maps variant name -> (type name, tag index, payload type)
    let variantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Separate functions and expressions
    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    // Build function registry - maps function names to their FULL function types
    let funcReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    // Convert all functions
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup funcReg
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    // Stdlib functions are now loaded from stdlib.dark and included as regular functions
    convertFunctions functions varGen []
    |> Result.bind (fun (anfFuncs, varGen1) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv typeReg variantLookup funcReg
            |> Result.map (fun (anfExpr, _) ->
                ANF.Program (anfFuncs, anfExpr))
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))
