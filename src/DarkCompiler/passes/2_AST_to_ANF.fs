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

open System.Collections.Concurrent
open ANF
open Output

/// Thread-safe cache for specialized (monomorphized) functions
/// Key: (funcName, typeArgs as strings), Value: specialized AST.FunctionDef
/// Using string list for type args to ensure proper equality comparison
type SpecializationCache = ConcurrentDictionary<string * string list, AST.FunctionDef>

/// Thread-safe cache for ANF user functions - avoids re-converting same function across tests
/// Key: (filename, line_number, function_name)
/// Value: (ANF.Function, ending VarGen) - we need the VarGen to avoid TempId collisions
type ANFFunctionCache = ConcurrentDictionary<string * int * string, ANF.Function * ANF.VarGen>

/// Convert AST.Type to a string for cache key
let rec typeToString (ty: AST.Type) : string =
    match ty with
    | AST.TInt64 -> "i64"
    | AST.TInt32 -> "i32"
    | AST.TInt16 -> "i16"
    | AST.TInt8 -> "i8"
    | AST.TUInt64 -> "u64"
    | AST.TUInt32 -> "u32"
    | AST.TUInt16 -> "u16"
    | AST.TUInt8 -> "u8"
    | AST.TBool -> "bool"
    | AST.TString -> "str"
    | AST.TBytes -> "bytes"
    | AST.TChar -> "char"
    | AST.TFloat64 -> "f64"
    | AST.TUnit -> "unit"
    | AST.TRawPtr -> "ptr"
    | AST.TVar name -> name
    | AST.TRecord name -> name
    | AST.TSum (name, args) -> name + "<" + (args |> List.map typeToString |> String.concat ",") + ">"
    | AST.TList elemType -> "List<" + typeToString elemType + ">"
    | AST.TDict (keyType, valueType) -> "Dict<" + typeToString keyType + "," + typeToString valueType + ">"
    | AST.TFunction (paramTypes, retType) ->
        "(" + (paramTypes |> List.map typeToString |> String.concat ",") + ")->" + typeToString retType
    | AST.TTuple types -> "(" + (types |> List.map typeToString |> String.concat "*") + ")"

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
    | "Stdlib.File.delete", [pathAtom] ->
        Some (ANF.FileDelete pathAtom)
    | "Stdlib.File.setExecutable", [pathAtom] ->
        Some (ANF.FileSetExecutable pathAtom)
    | "Stdlib.File.writeFromPtr", [pathAtom; ptrAtom; lengthAtom] ->
        Some (ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom))
    | _ -> None

/// Try to convert a function call to a Float intrinsic CExpr
/// Returns Some CExpr if it's a Float intrinsic, None otherwise
let tryFloatIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Float.sqrt", [xAtom] ->
        Some (ANF.FloatSqrt xAtom)
    | "Stdlib.Float.abs", [xAtom] ->
        Some (ANF.FloatAbs xAtom)
    | "Stdlib.Float.negate", [xAtom] ->
        Some (ANF.FloatNeg xAtom)
    | "Stdlib.Float.toInt", [xAtom] ->
        Some (ANF.FloatToInt xAtom)
    | "Stdlib.Int64.toFloat", [xAtom] ->
        Some (ANF.IntToFloat xAtom)
    | "Stdlib.Float64.toString", [xAtom] ->
        Some (ANF.FloatToString xAtom)
    | _ -> None

/// Try to constant-fold platform/path intrinsics at compile time
/// Returns Some CExpr if it's a constant-foldable intrinsic, None otherwise
let tryConstantFoldIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Platform.isMacOS", [] ->
        // Constant-fold based on target platform using .NET runtime detection
        let isMac = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.OSX)
        Some (ANF.Atom (ANF.BoolLiteral isMac))
    | "Stdlib.Platform.isLinux", [] ->
        let isLinux = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.Linux)
        Some (ANF.Atom (ANF.BoolLiteral isLinux))
    | "Stdlib.Path.tempDir", [] ->
        // Both macOS and Linux use /tmp
        Some (ANF.Atom (ANF.StringLiteral "/tmp"))
    | _ -> None

/// Try to convert a function call to a raw memory intrinsic CExpr
/// These are internal-only functions for implementing HAMT data structures
/// Returns Some CExpr if it's a raw memory intrinsic, None otherwise
/// Note: __raw_get and __raw_set are generic and become monomorphized names like
/// __raw_get_i64, __raw_get_str, __raw_set_i64, etc.
let tryRawMemoryIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "__raw_alloc", [numBytesAtom] ->
        Some (ANF.RawAlloc numBytesAtom)
    | "__raw_free", [ptrAtom] ->
        Some (ANF.RawFree ptrAtom)
    | "__raw_get_byte", [ptrAtom; offsetAtom] ->
        // Read single byte at offset, returns Int64 (zero-extended)
        // IMPORTANT: Must come before the generic __raw_get_* pattern
        Some (ANF.RawGetByte (ptrAtom, offsetAtom))
    | name, [ptrAtom; offsetAtom] when name = "__raw_get" || name.StartsWith("__raw_get_") ->
        // Generic __raw_get<v> monomorphizes to __raw_get_i64, __raw_get_str, __raw_get_f64, etc.
        // Track Float type so we can convert GP bits to FP register
        // IMPORTANT: Only detect _f64 when it's the ENTIRE suffix (not list_f64 or other containers)
        let valueType = if name = "__raw_get_f64" then Some AST.TFloat64 else None
        Some (ANF.RawGet (ptrAtom, offsetAtom, valueType))
    | "__raw_set_byte", [ptrAtom; offsetAtom; valueAtom] ->
        // Write single byte at offset
        // IMPORTANT: Must come before the generic __raw_set_* pattern
        Some (ANF.RawSetByte (ptrAtom, offsetAtom, valueAtom))
    | name, [ptrAtom; offsetAtom; valueAtom] when name = "__raw_set" || name.StartsWith("__raw_set_") ->
        // Generic __raw_set<v> monomorphizes to __raw_set_i64, __raw_set_str, __raw_set_f64, etc.
        // Track Float type so we can convert FP register to GP bits for storage
        // IMPORTANT: Only detect _f64 when it's the ENTIRE suffix (not list_f64 or other containers)
        let valueType = if name = "__raw_set_f64" then Some AST.TFloat64 else None
        Some (ANF.RawSet (ptrAtom, offsetAtom, valueAtom, valueType))
    // Cast operations are no-ops at runtime - just pass through the value
    | "__rawptr_to_int64", [ptrAtom] ->
        Some (ANF.Atom ptrAtom)
    | "__int64_to_rawptr", [intAtom] ->
        Some (ANF.Atom intAtom)
    // String intrinsics for Dict with string keys
    | "__string_hash", [strAtom] ->
        Some (ANF.StringHash strAtom)
    | "__string_eq", [leftAtom; rightAtom] ->
        Some (ANF.StringEq (leftAtom, rightAtom))
    // String refcount intrinsics (for Dict with string keys)
    | "__refcount_inc_string", [strAtom] ->
        Some (ANF.RefCountIncString strAtom)
    | "__refcount_dec_string", [strAtom] ->
        Some (ANF.RefCountDecString strAtom)
    // String pointer cast operations are no-ops at runtime - just pass through the value
    | "__string_to_int64", [strAtom] ->
        Some (ANF.Atom strAtom)
    | "__int64_to_string", [intAtom] ->
        Some (ANF.Atom intAtom)

    // Bytes pointer cast operations are no-ops at runtime - just pass through the value
    | "__bytes_to_int64", [bytesAtom] ->
        Some (ANF.Atom bytesAtom)
    | "__int64_to_bytes", [intAtom] ->
        Some (ANF.Atom intAtom)

    // Dict intrinsics - for type-safe Dict<k, v> operations
    // __empty_dict<k, v> returns 0 (null pointer)
    | name, [] when name = "__empty_dict" || name.StartsWith("__empty_dict_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_is_null<k, v> checks if pointer is 0
    | name, [dictAtom] when name = "__dict_is_null" || name.StartsWith("__dict_is_null_") ->
        Some (ANF.Prim (ANF.Eq, dictAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_get_tag<k, v> extracts low 2 bits (dict & 3)
    | name, [dictAtom] when name = "__dict_get_tag" || name.StartsWith("__dict_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, dictAtom, ANF.IntLiteral (ANF.Int64 3L)))
    // __dict_to_rawptr<k, v> clears tag bits (dict & -4)
    | name, [dictAtom] when name = "__dict_to_rawptr" || name.StartsWith("__dict_to_rawptr_") ->
        Some (ANF.Prim (ANF.BitAnd, dictAtom, ANF.IntLiteral (ANF.Int64 -4L)))
    // __rawptr_to_dict<k, v> combines pointer + tag (ptr | tag)
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_dict" || name.StartsWith("__rawptr_to_dict_") ->
        Some (ANF.Prim (ANF.BitOr, ptrAtom, tagAtom))

    // List intrinsics - for Finger Tree implementation
    // __list_is_null<a> checks if list pointer is 0 (empty)
    | name, [listAtom] when name = "__list_is_null" || name.StartsWith("__list_is_null_") ->
        Some (ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __list_get_tag<a> extracts low 3 bits (list & 7) for Finger Tree tags (0-4)
    | name, [listAtom] when name = "__list_get_tag" || name.StartsWith("__list_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L)))
    // __list_to_rawptr<a> clears tag bits (list & -8) to get raw pointer
    | name, [listAtom] when name = "__list_to_rawptr" || name.StartsWith("__list_to_rawptr_") ->
        Some (ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 -8L)))
    // __rawptr_to_list<a> combines pointer + tag (ptr | tag) to create tagged list
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_list" || name.StartsWith("__rawptr_to_list_") ->
        Some (ANF.Prim (ANF.BitOr, ptrAtom, tagAtom))
    // __list_empty<a> returns 0 (null pointer = empty finger tree)
    | name, [] when name = "__list_empty" || name.StartsWith("__list_empty_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))

    // Key intrinsics - dispatch based on monomorphized type
    // __hash<Int64> is identity (Int64 is its own hash)
    | "__hash_i64", [keyAtom] ->
        Some (ANF.Atom keyAtom)
    // __hash<String> uses string hash
    | "__hash_str", [keyAtom] ->
        Some (ANF.StringHash keyAtom)
    // __hash<Bool> - Bool is 0 or 1, valid hash value
    | "__hash_bool", [keyAtom] ->
        Some (ANF.Atom keyAtom)
    // __key_eq<Int64> uses integer equality
    | "__key_eq_i64", [leftAtom; rightAtom] ->
        Some (ANF.Prim (ANF.Eq, leftAtom, rightAtom))
    // __key_eq<String> uses string equality
    | "__key_eq_str", [leftAtom; rightAtom] ->
        Some (ANF.StringEq (leftAtom, rightAtom))
    // __key_eq<Bool> uses integer equality (0 or 1)
    | "__key_eq_bool", [leftAtom; rightAtom] ->
        Some (ANF.Prim (ANF.Eq, leftAtom, rightAtom))

    | _ -> None

/// Try to convert a function call to a random intrinsic CExpr
/// Returns Some CExpr if it's a random intrinsic, None otherwise
let tryRandomIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Random.int64", [] ->
        Some ANF.RandomInt64
    | _ -> None

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * AST.Type) list>

/// Variant lookup - maps variant names to (type name, type params, tag index, payload type)
type VariantLookup = Map<string, (string * string list * int * AST.Type option)>

/// Function registry - maps function names to their FULL function types (TFunction)
type FunctionRegistry = Map<string, AST.Type>

/// Alias registry - maps type alias names to their target types
/// For simple record aliases: "Vec" -> TRecord "Point"
type AliasRegistry = Map<string, AST.Type>

/// Resolve a type name through the alias registry
/// If the name is an alias for a record type, returns the resolved record name
/// Otherwise returns the original name
let rec resolveRecordTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    | Some (AST.TRecord targetName) -> resolveRecordTypeName aliasReg targetName
    | Some (AST.TSum (targetName, _)) -> resolveRecordTypeName aliasReg targetName
    | _ -> typeName

/// Expand a type registry to include alias entries
/// If "Vec" aliases to "Point" and "Point" has fields [x, y], then "Vec" also gets [x, y]
let expandTypeRegWithAliases (typeReg: TypeRegistry) (aliasReg: AliasRegistry) : TypeRegistry =
    aliasReg
    |> Map.fold (fun accReg aliasName targetType ->
        match targetType with
        | AST.TRecord targetName ->
            let resolvedName = resolveRecordTypeName aliasReg targetName
            match Map.tryFind resolvedName typeReg with
            | Some fields -> Map.add aliasName fields accReg
            | None -> accReg  // Target not found, skip
        | _ -> accReg  // Not a record alias, skip
    ) typeReg

/// Variable environment - maps variable names to their TempIds and types
/// The type information is used for type-directed field lookup in record access
type VarEnv = Map<string, ANF.TempId * AST.Type>

/// Extract just the type environment from VarEnv for use with inferType
let typeEnvFromVarEnv (varEnv: VarEnv) : Map<string, AST.Type> =
    varEnv |> Map.map (fun _ (_, t) -> t)

// ============================================================================
// Monomorphization Support for Generic Functions
// ============================================================================
//
// The Dark compiler uses monomorphization to handle generics - each generic
// function instantiation becomes a separate specialized function with a
// mangled name (e.g., identity<Int64> → identity_i64).
//
// Algorithm:
// 1. Collect all generic function definitions (functions with TypeParams)
// 2. Scan for TypeApp expressions (calls to generic functions with type args)
// 3. For each unique (funcName, [typeArgs]) pair:
//    - Substitute type parameters with concrete types in the function body
//    - Generate a specialized function with mangled name
// 4. Replace all TypeApp calls with regular Calls to mangled names
// 5. Iterate until fixed-point (new specializations may contain more TypeApps)
//
// Key design decisions:
// - No runtime type info: all types resolved at compile time
// - Name mangling encodes types: identity_i64, swap_str_bool
// - Iterative: handles nested generics like List<Option<T>>
//
// See docs/features/generics.md for detailed documentation.
// ============================================================================

/// Generic function registry - maps generic function names to their definitions
type GenericFuncDefs = Map<string, AST.FunctionDef>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.Type list  // (funcName, typeArgs)

/// Specialization registry - tracks which specializations are needed
/// Maps (funcName, typeArgs) -> specialized name
type SpecRegistry = Map<SpecKey, string>

/// Extract generic function definitions (functions with type parameters)
/// from a program. Used for on-demand monomorphization of stdlib generics.
let extractGenericFuncDefs (program: AST.Program) : GenericFuncDefs =
    let (AST.Program topLevels) = program
    topLevels
    |> List.choose (function
        | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
        | _ -> None)
    |> Map.ofList

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
    | AST.TBytes -> "bytes"
    | AST.TChar -> "char"
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

/// Check if a type is concrete (no type variables)
let rec isConcrete (ty: AST.Type) : bool =
    match ty with
    | AST.TVar _ -> false
    | AST.TFunction (paramTypes, retType) ->
        List.forall isConcrete paramTypes && isConcrete retType
    | AST.TTuple elemTypes -> List.forall isConcrete elemTypes
    | AST.TSum (_, typeArgs) -> List.forall isConcrete typeArgs
    | AST.TList elemType -> isConcrete elemType
    | AST.TDict (keyType, valueType) -> isConcrete keyType && isConcrete valueType
    | _ -> true

/// Default unresolved type variables to Int64
let rec defaultTypeVars (ty: AST.Type) : AST.Type =
    match ty with
    | AST.TVar _ -> AST.TInt64  // Default to Int64
    | AST.TFunction (paramTypes, retType) ->
        AST.TFunction (List.map defaultTypeVars paramTypes, defaultTypeVars retType)
    | AST.TTuple elemTypes -> AST.TTuple (List.map defaultTypeVars elemTypes)
    | AST.TSum (name, typeArgs) -> AST.TSum (name, List.map defaultTypeVars typeArgs)
    | AST.TList elemType -> AST.TList (defaultTypeVars elemType)
    | AST.TDict (keyType, valueType) -> AST.TDict (defaultTypeVars keyType, defaultTypeVars valueType)
    | _ -> ty

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
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TBytes | AST.TChar | AST.TUnit | AST.TRecord _ | AST.TSum _ | AST.TRawPtr ->
        typ  // Concrete types are unchanged

/// Apply a substitution to an expression, replacing type variables in type annotations
let rec applySubstToExpr (subst: Substitution) (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
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
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (applySubstToExpr subst record, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) updates)
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

/// Specialize a generic function with caching to avoid redundant work across tests
let specializeFunctionCached (cache: SpecializationCache) (funcDef: AST.FunctionDef) (typeArgs: AST.Type list) : AST.FunctionDef =
    let key = (funcDef.Name, typeArgs |> List.map typeToString)
    cache.GetOrAdd(key, fun _ -> specializeFunction funcDef typeArgs)

/// Collect all TypeApp call sites from an expression
let rec collectTypeApps (expr: AST.Expr) : Set<SpecKey> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
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
        // Default any unresolved type variables to Int64 so we can specialize
        let concreteTypeArgs = typeArgs |> List.map defaultTypeVars
        let argSpecs = args |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.add (funcName, concreteTypeArgs) argSpecs
    | AST.TupleLiteral elements ->
        elements |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.TupleAccess (tuple, _) ->
        collectTypeApps tuple
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (snd >> collectTypeApps) |> List.fold Set.union Set.empty
    | AST.RecordUpdate (record, updates) ->
        let recordSpecs = collectTypeApps record
        let updatesSpecs = updates |> List.map (snd >> collectTypeApps) |> List.fold Set.union Set.empty
        Set.union recordSpecs updatesSpecs
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
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
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
        // Default any unresolved type variables to Int64
        let concreteTypeArgs = typeArgs |> List.map defaultTypeVars
        let specializedName = specName funcName concreteTypeArgs
        AST.Call (specializedName, List.map replaceTypeApps args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map replaceTypeApps elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (replaceTypeApps tuple, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, replaceTypeApps e)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (replaceTypeApps record, List.map (fun (n, e) -> (n, replaceTypeApps e)) updates)
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
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ -> false
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
    | AST.RecordUpdate (record, updates) ->
        varOccursInExpr name record || List.exists (fun (_, e) -> varOccursInExpr name e) updates
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
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ ->
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
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (inlineLambdas record lambdaEnv, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) updates)
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
// Lambda Lifting: Convert Lambdas to Top-Level Functions with Closures
// ============================================================================
//
// Lambda lifting transforms nested lambda expressions into top-level functions.
// The process handles both capturing and non-capturing lambdas uniformly.
//
// Algorithm:
// 1. Identify lambdas in argument positions (function calls, let bindings)
// 2. Collect free variables (captures) from each lambda body
// 3. Generate a lifted function with signature: (closure_tuple, original_params...) -> result
// 4. Replace the lambda with a ClosureAlloc expression containing the function and captures
//
// Closure representation at runtime:
//   [func_ptr, cap1, cap2, ...]  -- heap-allocated tuple
//
// The lifted function extracts captures from the closure tuple:
//   let __closure_N(__closure, x, y) =
//       let cap1 = __closure.1
//       let cap2 = __closure.2
//       in <original body with captures replaced>
//
// All function values use closures for uniform calling convention, even non-capturing
// lambdas and function references. This simplifies higher-order function support.
//
// See docs/features/closures.md for detailed documentation.
// ============================================================================

/// State for lambda lifting - tracks generated functions and counter
type LiftState = {
    Counter: int
    LiftedFunctions: AST.FunctionDef list
    TypeEnv: Map<string, AST.Type>  // Variable name -> Type (for tracking types of captured variables)
}

/// Collect free variables in an expression (variables not bound by let or lambda parameters)
let rec freeVars (expr: AST.Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ -> Set.empty
    | AST.Var name -> if Set.contains name bound then Set.empty else Set.singleton name
    | AST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | AST.UnaryOp (_, inner) -> freeVars inner bound
    | AST.Let (name, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars = freeVars body (Set.add name bound)
        Set.union valueVars bodyVars
    | AST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | AST.Call (funcName, args) ->
        // Check if funcName is a local variable (not in bound) - if so, it's a free variable
        // Top-level function names will be filtered out later since they won't be in TypeEnv
        let funcFree = if Set.contains funcName bound then Set.empty else Set.singleton funcName
        let argsFree = args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | AST.TypeApp (_, _, args) ->
        args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
    | AST.TupleLiteral elems | AST.ListLiteral elems ->
        elems |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.ListCons (headElements, tail) ->
        let headsFree = headElements |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union headsFree (freeVars tail bound)
    | AST.TupleAccess (tuple, _) -> freeVars tuple bound
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.RecordUpdate (record, updates) ->
        let recordVars = freeVars record bound
        let updateVars = updates |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordVars updateVars
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

/// Simple type inference for lambda lifting - infers types of simple expressions
/// This allows let-bound variables to be captured in nested lambdas
let rec simpleInferType (expr: AST.Expr) (typeEnv: Map<string, AST.Type>) : AST.Type option =
    match expr with
    | AST.IntLiteral _ -> Some AST.TInt64
    | AST.Int8Literal _ -> Some AST.TInt8
    | AST.Int16Literal _ -> Some AST.TInt16
    | AST.Int32Literal _ -> Some AST.TInt32
    | AST.UInt8Literal _ -> Some AST.TUInt8
    | AST.UInt16Literal _ -> Some AST.TUInt16
    | AST.UInt32Literal _ -> Some AST.TUInt32
    | AST.UInt64Literal _ -> Some AST.TUInt64
    | AST.BoolLiteral _ -> Some AST.TBool
    | AST.StringLiteral _ -> Some AST.TString
    | AST.CharLiteral _ -> Some AST.TChar
    | AST.FloatLiteral _ -> Some AST.TFloat64
    | AST.UnitLiteral -> Some AST.TUnit
    | AST.Var name -> Map.tryFind name typeEnv
    | AST.TupleLiteral elements ->
        // Recursively infer types of tuple elements
        let elemTypes = elements |> List.map (fun e -> simpleInferType e typeEnv)
        if List.forall Option.isSome elemTypes then
            Some (AST.TTuple (elemTypes |> List.map Option.get))
        else
            None
    | AST.RecordLiteral (typeName, _) ->
        // Record literal has the record's type
        Some (AST.TRecord typeName)
    | AST.Constructor (typeName, _, _) ->
        // Sum type constructor has the sum type
        Some (AST.TSum (typeName, []))
    | AST.BinOp (op, _, _) ->
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> Some AST.TInt64
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Some AST.TBool
        | AST.StringConcat -> Some AST.TString
    | _ -> None  // Complex expressions require full type inference

/// Lift lambdas in an expression, returning (transformed expr, new state)
let rec liftLambdasInExpr (expr: AST.Expr) (state: LiftState) : Result<AST.Expr * LiftState, string> =
    match expr with
    | AST.UnitLiteral | AST.IntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
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
            // Try to infer the type of the value for capturing in nested lambdas
            let valueType = simpleInferType value' state1.TypeEnv
            let state1' = match valueType with
                          | Some t -> { state1 with TypeEnv = Map.add name t state1.TypeEnv }
                          | None -> state1
            liftLambdasInExpr body state1'
            |> Result.map (fun (body', state2) ->
                // Restore TypeEnv (remove the let binding)
                let state2' = { state2 with TypeEnv = Map.remove name state2.TypeEnv }
                (AST.Let (name, value', body'), state2')))
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
    | AST.RecordUpdate (record, updates) ->
        liftLambdasInExpr record state
        |> Result.bind (fun (record', state1) ->
            liftLambdasInFields updates state1
            |> Result.map (fun (updates', state2) -> (AST.RecordUpdate (record', updates'), state2)))
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
        // Lambda in expression position - lift it to a closure
        // Add lambda parameters to type environment before processing body
        let lambdaParamTypes = parameters |> Map.ofList
        let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
        // First, lift any lambdas within the body
        liftLambdasInExpr body stateWithLambdaParams
        |> Result.bind (fun (body', state1) ->
            // Now lift this lambda itself to a closure
            let paramNames = parameters |> List.map fst |> Set.ofList
            let freeVarsInBody = freeVars body' paramNames
            // Filter to only include variables actually in TypeEnv (excludes top-level function names)
            let captures = freeVarsInBody |> Set.filter (fun name -> Map.containsKey name state.TypeEnv) |> Set.toList

            // Get actual types of captured variables from type environment
            let captureTypes = captures |> List.map (fun name ->
                Map.tryFind name state.TypeEnv |> Option.defaultValue AST.TInt64)

            // Create lifted function
            let funcName = $"__closure_{state1.Counter}"
            // First element is function pointer (Int64), rest are captures with their actual types
            let closureTupleTypes = AST.TInt64 :: captureTypes
            let closureParam = ("__closure", AST.TTuple closureTupleTypes)

            // Build body that extracts captures from closure tuple
            let bodyWithExtractions =
                if List.isEmpty captures then
                    body'
                else
                    captures
                    |> List.mapi (fun i capName ->
                        (capName, AST.TupleAccess (AST.Var "__closure", i + 1)))
                    |> List.foldBack (fun (capName, accessor) acc ->
                        AST.Let (capName, accessor, acc)) <| body'

            // Infer return type from body expression
            let returnType = simpleInferType body' stateWithLambdaParams.TypeEnv |> Option.defaultValue AST.TInt64

            let funcDef : AST.FunctionDef = {
                Name = funcName
                TypeParams = []
                Params = closureParam :: parameters
                ReturnType = returnType
                Body = bodyWithExtractions
            }
            let state' = {
                Counter = state1.Counter + 1
                LiftedFunctions = funcDef :: state1.LiftedFunctions
                TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
            }
            // Replace lambda with Closure
            let captureExprs = captures |> List.map AST.Var
            Ok (AST.Closure (funcName, captureExprs), state'))
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
                // Add lambda parameters to type environment before processing body
                let lambdaParamTypes = parameters |> Map.ofList
                let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
                // First, recursively lift any nested lambdas in the body
                liftLambdasInExpr body stateWithLambdaParams
                |> Result.bind (fun (body', state1) ->
                    // Check for free variables (captures)
                    let paramNames = parameters |> List.map fst |> Set.ofList
                    let freeVarsInBody = freeVars body' paramNames
                    // Filter to only include variables actually in TypeEnv (excludes top-level function names)
                    let captures = freeVarsInBody |> Set.filter (fun name -> Map.containsKey name state.TypeEnv) |> Set.toList

                    // Get actual types of captured variables from type environment
                    let captureTypes = captures |> List.map (fun name ->
                        Map.tryFind name state.TypeEnv |> Option.defaultValue AST.TInt64)

                    // All lambdas become closures (even non-capturing ones) for uniform calling convention
                    // The lifted function takes closure as first param, then original params
                    let funcName = $"__closure_{state1.Counter}"
                    // First element is function pointer (Int64), rest are captures with their actual types
                    let closureTupleTypes = AST.TInt64 :: captureTypes
                    let closureParam = ("__closure", AST.TTuple closureTupleTypes)

                    // Build body that extracts captures from closure tuple:
                    // let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
                    let bodyWithExtractions =
                        if List.isEmpty captures then
                            body'  // No captures to extract
                        else
                            captures
                            |> List.mapi (fun i capName ->
                                // Capture at index i+1 (index 0 is the function pointer)
                                (capName, AST.TupleAccess (AST.Var "__closure", i + 1)))
                            |> List.foldBack (fun (capName, accessor) acc ->
                                AST.Let (capName, accessor, acc)) <| body'

                    // Infer return type from body expression
                    let returnType = simpleInferType body' stateWithLambdaParams.TypeEnv |> Option.defaultValue AST.TInt64

                    let funcDef : AST.FunctionDef = {
                        Name = funcName
                        TypeParams = []
                        Params = closureParam :: parameters  // Closure is always first param
                        ReturnType = returnType
                        Body = bodyWithExtractions
                    }
                    let state' = {
                        Counter = state1.Counter + 1
                        LiftedFunctions = funcDef :: state1.LiftedFunctions
                        TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
                    }
                    // Replace lambda with Closure (captures may be empty for non-capturing lambdas)
                    let captureExprs = captures |> List.map AST.Var
                    loop rest state' (AST.Closure (funcName, captureExprs) :: acc))

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
                    TypeEnv = state.TypeEnv
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
    // Add function parameters to the type environment
    let paramTypes = funcDef.Params |> List.map (fun (name, typ) -> (name, typ)) |> Map.ofList
    let stateWithParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv paramTypes }
    liftLambdasInExpr funcDef.Body stateWithParams
    |> Result.map (fun (body', state') ->
        // Restore original TypeEnv (remove parameters) after processing the function
        ({ funcDef with Body = body' }, { state' with TypeEnv = state.TypeEnv }))

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
    let initialState = { Counter = 0; LiftedFunctions = []; TypeEnv = Map.empty }

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
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
let monomorphize (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program

    // Collect generic function definitions
    let genericFuncDefs : GenericFuncDefs =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
            | _ -> None)
        |> Map.ofList

    // Collect initial specialization sites from non-generic functions and expressions
    let initialSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Iterate: specialize, collect new TypeApps from specialized bodies, repeat
    let rec iterate (pendingSpecs: Set<SpecKey>) (processedSpecs: Set<SpecKey>) (accFuncs: AST.FunctionDef list) =
        // Filter to only specs not yet processed
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            // No new specs, we're done
            accFuncs
        else
            // Generate specialized functions for new specs
            let (newFuncs, newPendingSpecs) =
                newSpecs
                |> Set.toList
                |> List.fold (fun (funcs, pending) (funcName, typeArgs) ->
                    match Map.tryFind funcName genericFuncDefs with
                    | Some funcDef ->
                        let specialized = specializeFunction funcDef typeArgs
                        // Collect TypeApps from the specialized body (these may be new specs)
                        let bodySpecs = collectTypeAppsFromFunc specialized
                        (specialized :: funcs, Set.union pending bodySpecs)
                    | None ->
                        (funcs, pending)) ([], Set.empty)

            // Continue with new pending specs
            iterate newPendingSpecs (Set.union processedSpecs newSpecs) (newFuncs @ accFuncs)

    // Run iterative specialization
    let specializedFuncs = iterate initialSpecs Set.empty []

    // Now replace all TypeApps with Calls in all specialized functions
    let specializedFuncsReplaced = specializedFuncs |> List.map replaceTypeAppsInFunc

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
        specializedFuncsReplaced |> List.map AST.FunctionDef

    AST.Program (specializationTopLevels @ transformedTopLevels)

/// Monomorphize a program with access to external generic function definitions.
/// Used when user code needs to specialize stdlib generics - the stdlib generic
/// function bodies are passed in as externalGenericDefs so they can be specialized
/// without merging the full stdlib AST with user code.
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
let monomorphizeWithExternalDefs (externalGenericDefs: GenericFuncDefs) (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program

    // Collect generic function definitions from this program
    let localGenericDefs = extractGenericFuncDefs program

    // Merge external defs with local defs (local takes precedence)
    let genericFuncDefs =
        Map.fold (fun acc k v -> Map.add k v acc) externalGenericDefs localGenericDefs

    // Collect initial specialization sites from non-generic functions and expressions
    let initialSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Iterate: specialize, collect new TypeApps from specialized bodies, repeat
    let rec iterate (pendingSpecs: Set<SpecKey>) (processedSpecs: Set<SpecKey>) (accFuncs: AST.FunctionDef list) =
        // Filter to only specs not yet processed
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            // No new specs, we're done
            accFuncs
        else
            // Generate specialized functions for new specs
            let (newFuncs, newPendingSpecs) =
                newSpecs
                |> Set.toList
                |> List.fold (fun (funcs, pending) (funcName, typeArgs) ->
                    match Map.tryFind funcName genericFuncDefs with
                    | Some funcDef ->
                        let specialized = specializeFunction funcDef typeArgs
                        // Collect TypeApps from the specialized body (these may be new specs)
                        let bodySpecs = collectTypeAppsFromFunc specialized
                        (specialized :: funcs, Set.union pending bodySpecs)
                    | None ->
                        (funcs, pending)) ([], Set.empty)

            // Continue with new pending specs
            iterate newPendingSpecs (Set.union processedSpecs newSpecs) (newFuncs @ accFuncs)

    // Run iterative specialization
    let specializedFuncs = iterate initialSpecs Set.empty []

    // Now replace all TypeApps with Calls in all specialized functions
    let specializedFuncsReplaced = specializedFuncs |> List.map replaceTypeAppsInFunc

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
        specializedFuncsReplaced |> List.map AST.FunctionDef

    AST.Program (specializationTopLevels @ transformedTopLevels)

/// Monomorphize with caching - reuses specialized functions from previous compilations
/// This dramatically speeds up repeated compilations that use the same type combinations
/// Returns the monomorphized program AND the set of specialized function names
let monomorphizeWithExternalDefsCached (cache: SpecializationCache) (externalGenericDefs: GenericFuncDefs) (program: AST.Program) : AST.Program * Set<string> =
    let (AST.Program topLevels) = program

    // Collect generic function definitions from this program
    let localGenericDefs = extractGenericFuncDefs program

    // Merge external defs with local defs (local takes precedence)
    let genericFuncDefs =
        Map.fold (fun acc k v -> Map.add k v acc) externalGenericDefs localGenericDefs

    // Collect initial specialization sites from non-generic functions and expressions
    let initialSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Iterate: specialize, collect new TypeApps from specialized bodies, repeat
    let rec iterate (pendingSpecs: Set<SpecKey>) (processedSpecs: Set<SpecKey>) (accFuncs: AST.FunctionDef list) =
        // Filter to only specs not yet processed
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            // No new specs, we're done
            accFuncs
        else
            // Generate specialized functions for new specs (WITH CACHING)
            let (newFuncs, newPendingSpecs) =
                newSpecs
                |> Set.toList
                |> List.fold (fun (funcs, pending) (funcName, typeArgs) ->
                    match Map.tryFind funcName genericFuncDefs with
                    | Some funcDef ->
                        // USE CACHED SPECIALIZATION
                        let specialized = specializeFunctionCached cache funcDef typeArgs
                        // Collect TypeApps from the specialized body (these may be new specs)
                        let bodySpecs = collectTypeAppsFromFunc specialized
                        (specialized :: funcs, Set.union pending bodySpecs)
                    | None ->
                        (funcs, pending)) ([], Set.empty)

            // Continue with new pending specs
            iterate newPendingSpecs (Set.union processedSpecs newSpecs) (newFuncs @ accFuncs)

    // Run iterative specialization
    let specializedFuncs = iterate initialSpecs Set.empty []

    // Collect the names of all specialized functions
    let specializedFuncNames = specializedFuncs |> List.map (fun f -> f.Name) |> Set.ofList

    // Now replace all TypeApps with Calls in all specialized functions
    let specializedFuncsReplaced = specializedFuncs |> List.map replaceTypeAppsInFunc

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
        specializedFuncsReplaced |> List.map AST.FunctionDef

    (AST.Program (specializationTopLevels @ transformedTopLevels), specializedFuncNames)

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
    | AST.BitNot -> ANF.BitNot

/// Check if a type requires structural equality (compound types)
let isCompoundType (typ: AST.Type) : bool =
    match typ with
    | AST.TTuple _ -> true
    | AST.TRecord _ -> true
    | AST.TSum _ -> true
    | _ -> false

/// Generate structural equality comparison for compound types.
/// Returns a list of bindings and the final result atom that holds the comparison result.
let rec generateStructuralEquality
    (leftAtom: ANF.Atom)
    (rightAtom: ANF.Atom)
    (typ: AST.Type)
    (varGen: ANF.VarGen)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =

    match typ with
    | AST.TTuple elemTypes ->
        // Compare each tuple element and AND the results
        let rec compareElements index types accBindings accResults vg =
            match types with
            | [] ->
                // AND all results together
                match accResults with
                | [] ->
                    // Empty tuple - always equal
                    let (trueVar, vg') = ANF.freshVar vg
                    (accBindings @ [(trueVar, ANF.Atom (ANF.BoolLiteral true))], ANF.Var trueVar, vg')
                | [single] ->
                    (accBindings, single, vg)
                | first :: rest ->
                    // Chain ANDs: result = r0 && r1 && r2 ...
                    let rec chainAnds results accBindings vg =
                        match results with
                        | [] -> crash "empty results in chainAnds"
                        | [last] -> (accBindings, last, vg)
                        | a :: b :: rest ->
                            let (andVar, vg') = ANF.freshVar vg
                            let andExpr = ANF.Prim (ANF.And, a, b)
                            chainAnds (ANF.Var andVar :: rest) (accBindings @ [(andVar, andExpr)]) vg'
                    chainAnds (first :: rest) accBindings vg

            | elemType :: restTypes ->
                // Get left[index] and right[index]
                let (leftElemVar, vg1) = ANF.freshVar vg
                let leftGet = ANF.TupleGet (leftAtom, index)
                let (rightElemVar, vg2) = ANF.freshVar vg1
                let rightGet = ANF.TupleGet (rightAtom, index)

                let baseBindings = accBindings @ [(leftElemVar, leftGet); (rightElemVar, rightGet)]

                // Check if element type is also compound
                if isCompoundType elemType then
                    // Recursive structural comparison
                    let (nestedBindings, resultAtom, vg3) =
                        generateStructuralEquality (ANF.Var leftElemVar) (ANF.Var rightElemVar) elemType vg2 typeReg variantLookup
                    compareElements (index + 1) restTypes (baseBindings @ nestedBindings) (accResults @ [resultAtom]) vg3
                else
                    // Primitive comparison
                    let (cmpVar, vg3) = ANF.freshVar vg2
                    let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var leftElemVar, ANF.Var rightElemVar)
                    compareElements (index + 1) restTypes (baseBindings @ [(cmpVar, cmpExpr)]) (accResults @ [ANF.Var cmpVar]) vg3

        compareElements 0 elemTypes [] [] varGen

    | AST.TRecord typeName ->
        // Compare each record field and AND the results
        match Map.tryFind typeName typeReg with
        | None ->
            // Unknown record type - fall back to pointer comparison
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        | Some fields ->
            let rec compareFields index fieldList accBindings accResults vg =
                match fieldList with
                | [] ->
                    match accResults with
                    | [] ->
                        let (trueVar, vg') = ANF.freshVar vg
                        (accBindings @ [(trueVar, ANF.Atom (ANF.BoolLiteral true))], ANF.Var trueVar, vg')
                    | [single] ->
                        (accBindings, single, vg)
                    | first :: rest ->
                        let rec chainAnds results accBindings vg =
                            match results with
                            | [] -> crash "empty results in chainAnds"
                            | [last] -> (accBindings, last, vg)
                            | a :: b :: rest ->
                                let (andVar, vg') = ANF.freshVar vg
                                let andExpr = ANF.Prim (ANF.And, a, b)
                                chainAnds (ANF.Var andVar :: rest) (accBindings @ [(andVar, andExpr)]) vg'
                        chainAnds (first :: rest) accBindings vg

                | (_, fieldType) :: restFields ->
                    // Get left.field and right.field (using TupleGet with field index)
                    let (leftFieldVar, vg1) = ANF.freshVar vg
                    let leftGet = ANF.TupleGet (leftAtom, index)
                    let (rightFieldVar, vg2) = ANF.freshVar vg1
                    let rightGet = ANF.TupleGet (rightAtom, index)

                    let baseBindings = accBindings @ [(leftFieldVar, leftGet); (rightFieldVar, rightGet)]

                    if isCompoundType fieldType then
                        let (nestedBindings, resultAtom, vg3) =
                            generateStructuralEquality (ANF.Var leftFieldVar) (ANF.Var rightFieldVar) fieldType vg2 typeReg variantLookup
                        compareFields (index + 1) restFields (baseBindings @ nestedBindings) (accResults @ [resultAtom]) vg3
                    else
                        let (cmpVar, vg3) = ANF.freshVar vg2
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var leftFieldVar, ANF.Var rightFieldVar)
                        compareFields (index + 1) restFields (baseBindings @ [(cmpVar, cmpExpr)]) (accResults @ [ANF.Var cmpVar]) vg3

            compareFields 0 fields [] [] varGen

    | AST.TSum (typeName, _) ->
        // Check if ANY variant in this type has a payload
        let typeVariants =
            variantLookup
            |> Map.filter (fun _ (tName, _, _, _) -> tName = typeName)
        let hasAnyPayload =
            typeVariants |> Map.exists (fun _ (_, _, _, pType) -> pType.IsSome)

        if not hasAnyPayload then
            // Pure enum - tags are integers, direct comparison works
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        else
            // Mixed enum - uniform [tag, payload] representation
            // Compare tag (index 0) AND payload (index 1)
            let (leftTagVar, vg1) = ANF.freshVar varGen
            let (rightTagVar, vg2) = ANF.freshVar vg1
            let (tagEqVar, vg3) = ANF.freshVar vg2
            let (leftPayloadVar, vg4) = ANF.freshVar vg3
            let (rightPayloadVar, vg5) = ANF.freshVar vg4
            let (payloadEqVar, vg6) = ANF.freshVar vg5
            let (resultVar, vg7) = ANF.freshVar vg6

            let bindings = [
                (leftTagVar, ANF.TupleGet (leftAtom, 0))
                (rightTagVar, ANF.TupleGet (rightAtom, 0))
                (tagEqVar, ANF.Prim (ANF.Eq, ANF.Var leftTagVar, ANF.Var rightTagVar))
                (leftPayloadVar, ANF.TupleGet (leftAtom, 1))
                (rightPayloadVar, ANF.TupleGet (rightAtom, 1))
                (payloadEqVar, ANF.Prim (ANF.Eq, ANF.Var leftPayloadVar, ANF.Var rightPayloadVar))
                (resultVar, ANF.Prim (ANF.And, ANF.Var tagEqVar, ANF.Var payloadEqVar))
            ]
            (bindings, ANF.Var resultVar, vg7)

    | _ ->
        // Primitive types - simple comparison
        let (cmpVar, vg') = ANF.freshVar varGen
        ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')

/// Infer the type of an expression using type environment and registries
/// Used for type-directed field lookup in record access
let rec inferType (expr: AST.Expr) (typeEnv: Map<string, AST.Type>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<AST.Type, string> =
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
    | AST.CharLiteral _ -> Ok AST.TChar
    | AST.FloatLiteral _ -> Ok AST.TFloat64
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some t -> Ok t
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (moduleFunc, _) -> Ok (Stdlib.getFunctionType moduleFunc)
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
    | AST.RecordUpdate (recordExpr, _) ->
        // Record update returns the same type as the record being updated
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
    | AST.RecordAccess (recordExpr, fieldName) ->
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
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
        |> List.map (fun e -> inferType e typeEnv typeReg variantLookup funcReg moduleRegistry)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok types, Ok t -> Ok (types @ [t])
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map AST.TTuple
    | AST.TupleAccess (tupleExpr, index) ->
        inferType tupleExpr typeEnv typeReg variantLookup funcReg moduleRegistry
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
            inferType first typeEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.map (fun elemType -> AST.TList elemType)
    | AST.ListCons (_, tail) ->
        // List cons has same type as tail
        inferType tail typeEnv typeReg variantLookup funcReg moduleRegistry
    | AST.Let (name, value, body) ->
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            let typeEnv' = Map.add name valueType typeEnv
            inferType body typeEnv' typeReg variantLookup funcReg moduleRegistry)
    | AST.If (_, thenExpr, _) ->
        // Both branches should have same type, just infer from then branch
        inferType thenExpr typeEnv typeReg variantLookup funcReg moduleRegistry
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
        | AST.BitNot -> Ok AST.TInt64
    | AST.Match (scrutinee, cases) ->
        // Infer from first case body, but first extend environment with pattern variables
        // Infer scrutinee type to help with pattern variable typing
        let scrutineeTypeResult = inferType scrutinee typeEnv typeReg variantLookup funcReg moduleRegistry

        // Helper to extract pattern variable names and infer their types
        let rec extractPatternBindings (pattern: AST.Pattern) (scrutType: AST.Type) : Map<string, AST.Type> =
            match pattern with
            | AST.PVar name -> Map.ofList [(name, scrutType)]
            | AST.PWildcard -> Map.empty
            | AST.PUnit | AST.PLiteral _ | AST.PBool _ | AST.PString _ | AST.PFloat _ -> Map.empty
            | AST.PTuple innerPats ->
                match scrutType with
                | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                    List.zip innerPats elemTypes
                    |> List.fold (fun acc (pat, typ) -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat typ)) Map.empty
                | _ -> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat AST.TInt64)) Map.empty innerPats
            | AST.PRecord (_, fieldPats) ->
                fieldPats |> List.fold (fun acc (_, pat) -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat AST.TInt64)) Map.empty
            | AST.PConstructor (_, payloadPat) ->
                payloadPat |> Option.map (fun p -> extractPatternBindings p AST.TInt64) |> Option.defaultValue Map.empty
            | AST.PList innerPats ->
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | _ -> failwith $"PList pattern expects TList scrutinee, got {scrutType}"
                innerPats |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
            | AST.PListCons (headPats, tailPat) ->
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | _ -> failwith $"PListCons pattern expects TList scrutinee, got {scrutType}"
                let headBindings = headPats |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
                let tailBindings = extractPatternBindings tailPat scrutType
                Map.fold (fun m k v -> Map.add k v m) headBindings tailBindings

        match cases with
        | mc :: _ ->
            let patternType =
                match scrutineeTypeResult with
                | Ok t -> t
                | Error msg -> failwith $"Pattern match: Could not determine scrutinee type: {msg}"
            // Get bindings from first pattern (cases can have multiple patterns, use first)
            let patBindings =
                mc.Patterns
                |> AST.NonEmptyList.toList
                |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat patternType)) Map.empty
            let typeEnv' = Map.fold (fun m k v -> Map.add k v m) typeEnv patBindings
            inferType mc.Body typeEnv' typeReg variantLookup funcReg moduleRegistry
        | [] -> Error "Empty match expression"
    | AST.Call (funcName, _) ->
        // Look up function return type from the function registry
        match Map.tryFind funcName funcReg with
        | Some (AST.TFunction (_, returnType)) -> Ok returnType
        | Some _ -> Error $"Expected function type for {funcName} in funcReg"
        | None ->
            // Check if it's a function parameter (variable with function type)
            match Map.tryFind funcName typeEnv with
            | Some (AST.TFunction (_, returnType)) -> Ok returnType
            | _ ->
            // Check if it's a module function (e.g., Stdlib.File.exists)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry funcName with
            | Some (moduleFunc, _) -> Ok moduleFunc.ReturnType
            | None ->
                // Check if it's a monomorphized intrinsic (e.g., __raw_get_i64)
                // These are raw memory operations that work with 8-byte values
                if funcName.StartsWith("__raw_get_") then
                    // __raw_get<T> returns T, but at ANF level it's just Int64 (8 bytes)
                    Ok AST.TInt64
                elif funcName.StartsWith("__raw_set_") then
                    // __raw_set<T> returns Unit
                    Ok AST.TUnit
                // Key intrinsics for Dict - monomorphized versions
                elif funcName.StartsWith("__hash_") then
                    // __hash<k> returns Int64 (hash value)
                    Ok AST.TInt64
                elif funcName.StartsWith("__key_eq_") then
                    // __key_eq<k> returns Bool (equality check)
                    Ok AST.TBool
                // Dict intrinsics - monomorphized versions
                elif funcName.StartsWith("__empty_dict_") then
                    // __empty_dict<k, v> returns Dict<k, v> - but at ANF level it's Int64 (null ptr)
                    Ok AST.TInt64
                elif funcName.StartsWith("__dict_is_null_") then
                    // __dict_is_null<k, v> returns Bool
                    Ok AST.TBool
                elif funcName.StartsWith("__dict_get_tag_") then
                    // __dict_get_tag<k, v> returns Int64 (tag bits)
                    Ok AST.TInt64
                elif funcName.StartsWith("__dict_to_rawptr_") then
                    // __dict_to_rawptr<k, v> returns RawPtr (as Int64)
                    Ok AST.TInt64
                elif funcName.StartsWith("__rawptr_to_dict_") then
                    // __rawptr_to_dict<k, v> returns Dict<k, v> (as Int64)
                    Ok AST.TInt64
                // List intrinsics - monomorphized versions for Finger Tree
                elif funcName.StartsWith("__list_is_null_") then
                    // __list_is_null<a> returns Bool
                    Ok AST.TBool
                elif funcName.StartsWith("__list_get_tag_") then
                    // __list_get_tag<a> returns Int64 (tag bits)
                    Ok AST.TInt64
                elif funcName.StartsWith("__list_to_rawptr_") then
                    // __list_to_rawptr<a> returns RawPtr (as Int64)
                    Ok AST.TInt64
                elif funcName.StartsWith("__rawptr_to_list_") then
                    // __rawptr_to_list<a> returns List<a> (as Int64)
                    Ok AST.TInt64
                elif funcName.StartsWith("__list_empty_") then
                    // __list_empty<a> returns List<a> - but at ANF level it's Int64 (null ptr)
                    Ok AST.TInt64
                else
                    Error $"Unknown function: '{funcName}'"
    | AST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"
    | AST.Lambda (parameters, body) ->
        // Lambda has function type (paramTypes) -> returnType
        let paramTypes = parameters |> List.map snd
        let typeEnv' = parameters |> List.fold (fun env (name, ty) -> Map.add name ty env) typeEnv
        inferType body typeEnv' typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun returnType -> AST.TFunction (paramTypes, returnType))
    | AST.Apply (func, _args) ->
        // Apply result is the return type of the function
        inferType func typeEnv typeReg variantLookup funcReg moduleRegistry
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
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        // Unit literal becomes return of unit value (represented as 0)
        Ok (ANF.Return (ANF.UnitLiteral), varGen)

    | AST.IntLiteral n ->
        // Integer literal (default Int64)
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 n)), varGen)

    | AST.Int8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int8 n)), varGen)

    | AST.Int16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int16 n)), varGen)

    | AST.Int32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int32 n)), varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt8 n)), varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt16 n)), varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt32 n)), varGen)

    | AST.UInt64Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt64 n)), varGen)

    | AST.BoolLiteral b ->
        // Boolean literal becomes return
        Ok (ANF.Return (ANF.BoolLiteral b), varGen)

    | AST.StringLiteral s ->
        // String literal becomes return
        Ok (ANF.Return (ANF.StringLiteral s), varGen)

    | AST.CharLiteral s ->
        // Char literal becomes return (stored as string, same runtime representation)
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
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (_, _) ->
                // Module function reference - wrap in closure for uniform calling convention
                // Note: name should already be resolved by the type checker
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
                toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
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
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            // Try toAtom first; if it fails for complex expressions like Match, use toANF
            match toAtom value varGen env typeReg variantLookup funcReg moduleRegistry with
            | Ok (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toANF body varGen2 env' typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (bodyExpr, varGen3) ->
                    // Build: valueBindings + let tempId = valueAtom + body
                    let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
                    let exprWithBindings = wrapBindings valueBindings finalExpr
                    (exprWithBindings, varGen3))
            | Error _ ->
                // Complex expression (like Match) - compile with toANF and transform returns
                let (tempId, varGen1) = ANF.freshVar varGen
                let env' = Map.add name (tempId, valueType) env
                toANF value varGen1 env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (valueExpr, varGen2) ->
                    toANF body varGen2 env' typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, varGen3) ->
                        // Transform: replace all Returns in valueExpr with Let bindings to tempId + bodyExpr
                        let rec transformReturns expr =
                            match expr with
                            | ANF.Return atom -> ANF.Let (tempId, ANF.Atom atom, bodyExpr)
                            | ANF.Let (id, cexpr, rest) -> ANF.Let (id, cexpr, transformReturns rest)
                            | ANF.If (cond, thenBr, elseBr) ->
                                ANF.If (cond, transformReturns thenBr, transformReturns elseBr)
                        (transformReturns valueExpr, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue)), varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.Return (ANF.FloatLiteral (-f)), varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom and apply Not
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.UnaryOp (AST.BitNot, innerExpr) ->
        // Bitwise NOT: convert operand to atom and apply BitNot
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.BitNot, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        toAtom left varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (rightAtom, rightBindings, varGen2) ->
                // Check if this is an equality comparison on compound types
                let typeEnv = typeEnvFromVarEnv env
                match op with
                | AST.Eq | AST.Neq ->
                    // Infer type of left operand to check if structural comparison is needed
                    match inferType left typeEnv typeReg variantLookup funcReg moduleRegistry with
                    | Ok operandType when isCompoundType operandType ->
                        // Generate structural equality
                        let (eqBindings, eqResultAtom, varGen3) =
                            generateStructuralEquality leftAtom rightAtom operandType varGen2 typeReg variantLookup
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                            else
                                (eqResultAtom, eqBindings, varGen3)
                        // Build expression with all bindings
                        let finalExpr = ANF.Return finalAtom
                        let exprWithEq = wrapBindings finalBindings finalExpr
                        let exprWithRight = wrapBindings rightBindings exprWithEq
                        let exprWithLeft = wrapBindings leftBindings exprWithRight
                        Ok (exprWithLeft, varGen4)
                    | Ok AST.TString ->
                        // String equality - use StringEq
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.StringEq (leftAtom, rightAtom)
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                            else
                                (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                        let finalExpr = ANF.Return finalAtom
                        let exprWithBindings = wrapBindings finalBindings finalExpr
                        let exprWithRight = wrapBindings rightBindings exprWithBindings
                        let exprWithLeft = wrapBindings leftBindings exprWithRight
                        Ok (exprWithLeft, varGen4)
                    | _ ->
                        // Primitive type or type inference failed - use simple comparison
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                        let exprWithRight = wrapBindings rightBindings finalExpr
                        let exprWithLeft = wrapBindings leftBindings exprWithRight
                        Ok (exprWithLeft, varGen3)
                | AST.StringConcat ->
                    // String concatenation
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.StringConcat (leftAtom, rightAtom)
                    let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                    let exprWithRight = wrapBindings rightBindings finalExpr
                    let exprWithLeft = wrapBindings leftBindings exprWithRight
                    Ok (exprWithLeft, varGen3)
                // Arithmetic, bitwise, and comparison operators - use simple primitive
                | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
                | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                | AST.And | AST.Or ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                    let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                    let exprWithRight = wrapBindings rightBindings finalExpr
                    let exprWithLeft = wrapBindings leftBindings exprWithRight
                    Ok (exprWithLeft, varGen3)))

    | AST.If (cond, thenBranch, elseBranch) ->
        // If expression: convert condition to atom, both branches to ANF
        // Try toAtom first; if it fails for complex expressions like Match, use toANF
        match toAtom cond varGen env typeReg variantLookup funcReg moduleRegistry with
        | Ok (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (elseExpr, varGen3) ->
                    // Build the expression: condBindings + if condAtom then thenExpr else elseExpr
                    let finalExpr = ANF.If (condAtom, thenExpr, elseExpr)
                    let exprWithBindings = wrapBindings condBindings finalExpr
                    (exprWithBindings, varGen3)))
        | Error _ ->
            // Complex condition (like Match) - compile with toANF and transform
            // Create: let condTemp = <cond> in if condTemp then <then> else <else>
            let (condTemp, varGen1) = ANF.freshVar varGen
            toANF cond varGen1 env typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (condExpr, varGen2) ->
                toANF thenBranch varGen2 env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (thenExpr, varGen3) ->
                    toANF elseBranch varGen3 env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (elseExpr, varGen4) ->
                        // Transform: replace Returns in condExpr with Let + If
                        let ifExpr = ANF.If (ANF.Var condTemp, thenExpr, elseExpr)
                        let rec transformReturns expr =
                            match expr with
                            | ANF.Return atom -> ANF.Let (condTemp, ANF.Atom atom, ifExpr)
                            | ANF.Let (id, cexpr, rest) -> ANF.Let (id, cexpr, transformReturns rest)
                            | ANF.If (c, t, e) -> ANF.If (c, transformReturns t, transformReturns e)
                        (transformReturns condExpr, varGen4))))

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
                toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
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
                    // Check if it's a Float intrinsic
                    match tryFloatIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Float intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        let exprWithBindings = wrapBindings argBindings finalExpr
                        Ok (exprWithBindings, varGen2)
                    | None ->
                    // Check if it's a constant-fold intrinsic (Platform, Path)
                    match tryConstantFoldIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Constant-folded intrinsic
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        let exprWithBindings = wrapBindings argBindings finalExpr
                        Ok (exprWithBindings, varGen2)
                    | None ->
                    // Check if it's a random intrinsic
                    match tryRandomIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Random intrinsic call
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
                toAtom elem vg env typeReg variantLookup funcReg moduleRegistry
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
        toAtom tupleExpr varGen env typeReg variantLookup funcReg moduleRegistry
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
                | None -> failwith $"Record type '{typeName}' not found in typeReg"

        // Reorder field values according to type definition order
        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Convert to TupleLiteral and reuse tuple handling
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.RecordUpdate (recordExpr, updates) ->
        // Record update: { record with field1 = val1, field2 = val2 }
        // Desugar to creating a new record with updated fields
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | Some typeFields ->
                    // Build a map of updates
                    let updateMap = Map.ofList updates
                    // For each field in the type, use update value or access from original record
                    let newFields =
                        typeFields
                        |> List.map (fun (fname, _) ->
                            match Map.tryFind fname updateMap with
                            | Some updateExpr -> (fname, updateExpr)
                            | None -> (fname, AST.RecordAccess (recordExpr, fname)))
                    // Create a new record literal with the combined fields
                    toANF (AST.RecordLiteral (typeName, newFields)) varGen env typeReg variantLookup funcReg moduleRegistry
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error "Cannot use record update syntax on non-record type")

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
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
                Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 (int64 tag))), varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag, 0] for uniform 2-element structure
                // This enables consistent structural equality comparison
                let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                let (resultVar, varGen1) = ANF.freshVar varGen
                let tupleExpr = ANF.TupleAlloc [tagAtom; dummyPayload]
                let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                Ok (finalExpr, varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (resultVar, varGen2) = ANF.freshVar varGen1
                    let tupleExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings payloadBindings finalExpr
                    (exprWithBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as FingerTree
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), elemAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 5L))  // tag 5 = LEAF
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a SINGLE node containing a TreeNode
        let allocSingle (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 1L))  // tag 1 = SINGLE
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a DEEP node
        let allocDeep (measure: int) (prefixNodes: ANF.Atom list) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let prefixCount = List.length prefixNodes
            let suffixCount = List.length suffixNodes
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 96L))  // 12 fields * 8 bytes

            // Build all the set operations
            let setAt offset value vg bindings =
                let (setVar, vg') = ANF.freshVar vg
                let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value, None)
                (vg', bindings @ [(setVar, setExpr)])

            let (vg2, bindings2) = setAt 0 (ANF.IntLiteral (ANF.Int64 (int64 measure))) vg1 (bindings @ [(ptrVar, allocExpr)])
            let (vg3, bindings3) = setAt 8 (ANF.IntLiteral (ANF.Int64 (int64 prefixCount))) vg2 bindings2

            // Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
            let rec setPrefix nodes offset vg bindings =
                match nodes with
                | [] -> (vg, bindings)
                | n :: rest ->
                    let (vg', bindings') = setAt offset n vg bindings
                    setPrefix rest (offset + 8) vg' bindings'
            let (vg4, bindings4) = setPrefix prefixNodes 16 vg3 bindings3

            // Set middle = EMPTY (0) at offset 48
            let (vg5, bindings5) = setAt 48 (ANF.IntLiteral (ANF.Int64 0L)) vg4 bindings4

            // Set suffix count at offset 56
            let (vg6, bindings6) = setAt 56 (ANF.IntLiteral (ANF.Int64 (int64 suffixCount))) vg5 bindings5

            // Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
            let (vg7, bindings7) = setPrefix suffixNodes 64 vg6 bindings6

            // Tag with DEEP (2)
            let (taggedVar, vg8) = ANF.freshVar vg7
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 2L))
            (ANF.Var taggedVar, bindings7 @ [(taggedVar, tagExpr)], vg8)

        if List.isEmpty elements then
            // Empty list is EMPTY (represented as 0)
            Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 0L)), varGen)
        else
            // Convert all elements to atoms first
            let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    toAtom e vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (atom, bindings, vg') ->
                        convertElements rest vg' ((atom, bindings) :: acc))

            convertElements elements varGen []
            |> Result.map (fun (atomsWithBindings, varGen1) ->
                let count = List.length atomsWithBindings

                // Flatten all element bindings
                let elemBindings = atomsWithBindings |> List.collect snd
                let elemAtoms = atomsWithBindings |> List.map fst

                // Create LEAF nodes for all elements
                let rec createLeaves (atoms: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) (acc: ANF.Atom list) =
                    match atoms with
                    | [] -> (List.rev acc, bindings, vg)
                    | a :: rest ->
                        let (leafAtom, bindings', vg') = allocLeaf a vg bindings
                        createLeaves rest vg' bindings' (leafAtom :: acc)

                let (leafAtoms, leafBindings, varGen2) = createLeaves elemAtoms varGen1 elemBindings []

                if count = 1 then
                    // Single element: SINGLE(LEAF(elem))
                    let (resultAtom, resultBindings, varGen3) = allocSingle (List.head leafAtoms) varGen2 leafBindings
                    let finalExpr = ANF.Return resultAtom
                    let exprWithBindings = wrapBindings resultBindings finalExpr
                    (exprWithBindings, varGen3)
                else
                    // Multiple elements: DEEP with prefix and suffix
                    // Split into prefix (first element) and suffix (rest, up to 4)
                    let prefixNodes = [List.head leafAtoms]
                    let suffixNodes = List.tail leafAtoms |> List.truncate 4
                    // For more than 5 elements, we'd need the middle spine, but for now support up to 5
                    let (resultAtom, resultBindings, varGen3) = allocDeep count prefixNodes suffixNodes varGen2 leafBindings
                    let finalExpr = ANF.Return resultAtom
                    let exprWithBindings = wrapBindings resultBindings finalExpr
                    (exprWithBindings, varGen3))

    | AST.ListCons (headElements, tail) ->
        // Compile list cons: [a, b, ...tail] prepends elements to tail
        // Use Stdlib.FingerTree.push to prepend each element
        toAtom tail varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            // Build list by prepending elements from right to left
            // [a, b, ...tail] means push(push(tail, b), a)
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentList: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentList, allBindings, vg)
                | elem :: rest ->
                    // First build the rest of the list, then prepend this element
                    buildList rest vg currentList allBindings
                    |> Result.bind (fun (restList, restBindings, vg1) ->
                        toAtom elem vg1 env typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (pushVar, vg3) = ANF.freshVar vg2
                            // Call Stdlib.FingerTree.push to prepend element
                            let pushExpr = ANF.Call ("Stdlib.FingerTree.push_i64", [restList; elemAtom])
                            let newBindings = restBindings @ elemBindings @ [(pushVar, pushExpr)]
                            (ANF.Var pushVar, newBindings, vg3)))

            if List.isEmpty headElements then
                // No head elements, just return tail
                let finalExpr = ANF.Return tailAtom
                let exprWithBindings = wrapBindings tailBindings finalExpr
                Ok (exprWithBindings, varGen1)
            else
                // Build the list by pushing elements
                buildList headElements varGen1 tailAtom tailBindings
                |> Result.map (fun (listAtom, listBindings, varGen2) ->
                    let finalExpr = ANF.Return listAtom
                    let exprWithBindings = wrapBindings listBindings finalExpr
                    (exprWithBindings, varGen2)))

    | AST.Match (scrutinee, cases) ->
        // Infer scrutinee type to pass to pattern extraction for correct typing
        let typeEnv = typeEnvFromVarEnv env
        let scrutType =
            match inferType scrutinee typeEnv typeReg variantLookup funcReg moduleRegistry with
            | Ok t -> t
            | Error _ -> AST.TInt64  // Fallback if type inference fails
        // Compile match to if-else chain
        // First convert scrutinee to atom
        toAtom scrutinee varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (scrutineeAtom, scrutineeBindings, varGen1) ->
            // Check if any pattern needs to access list structure
            // If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
            let hasNonEmptyListPattern =
                cases |> List.exists (fun mc ->
                    mc.Patterns |> AST.NonEmptyList.toList |> List.exists (fun pat ->
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
            // scrutType is the type of the scrutinee, used to determine correct types for pattern variables
            let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match pattern with
                | AST.PUnit -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PWildcard -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PLiteral _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PBool _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PString _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PFloat _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PVar name ->
                    // Bind scrutinee to variable name with the correct type
                    let (tempId, vg1) = ANF.freshVar vg
                    let env' = Map.add name (tempId, scrutType) currentEnv
                    toANF body vg1 env' typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                        (expr, vg2))
                | AST.PConstructor (constructorName, payloadPattern) ->
                    match payloadPattern with
                    | None -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    | Some innerPattern ->
                        // Extract payload from heap-allocated variant
                        // Variant layout: [tag:8][payload:8], so payload is at index 1
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let payloadExpr = ANF.TupleGet (scrutAtom, 1)
                        // Get payload type from variant lookup if available
                        let payloadType =
                            match Map.tryFind constructorName variantLookup with
                            | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                                // Apply type substitution if scrutType has type args
                                match scrutType with
                                | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                    let subst = List.zip typeParams typeArgs |> Map.ofList
                                    let rec substitute t =
                                        match t with
                                        | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue t
                                        | AST.TTuple elems -> AST.TTuple (List.map substitute elems)
                                        | AST.TList elem -> AST.TList (substitute elem)
                                        | AST.TDict (k, v) -> AST.TDict (substitute k, substitute v)
                                        | AST.TSum (name, args) -> AST.TSum (name, List.map substitute args)
                                        | AST.TFunction (args, ret) -> AST.TFunction (List.map substitute args, substitute ret)
                                        | _ -> t
                                    substitute payloadTypeTemplate
                                | _ -> payloadTypeTemplate
                            | _ -> AST.TInt64  // Fallback
                        // Now compile the inner pattern with the payload
                        extractAndCompileBody innerPattern body (ANF.Var payloadVar) payloadType currentEnv vg1
                        |> Result.map (fun (innerExpr, vg2) ->
                            let expr = ANF.Let (payloadVar, payloadExpr, innerExpr)
                            (expr, vg2))
                | AST.PTuple patterns ->
                    // Recursively collect all variable bindings from a pattern
                    // Returns: updated env, list of bindings, updated vargen
                    // sourceType is the type of the source being matched, used to get correct element types
                    let rec collectPatternBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pat with
                        | AST.PUnit | AST.PWildcard | AST.PLiteral _ | AST.PBool _ | AST.PString _ | AST.PFloat _ ->
                            // No variable bindings
                            Ok (env, bindings, vg)
                        | AST.PVar name ->
                            // Bind the source to a variable with the correct type
                            // Use TypedAtom to preserve the semantic type (e.g., tuple element type)
                            // even when the source comes from a function with generic return type
                            let (tempId, vg1) = ANF.freshVar vg
                            let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                            let newEnv = Map.add name (tempId, sourceType) env
                            Ok (newEnv, binding :: bindings, vg1)
                        | AST.PTuple innerPatterns ->
                            // Extract element types from the tuple type
                            let elemTypes =
                                match sourceType with
                                | AST.TTuple types -> types
                                | _ -> List.replicate (List.length innerPatterns) AST.TInt64
                            // Extract each element and recursively collect bindings
                            let rec collectFromTuple (pats: AST.Pattern list) (types: AST.Type list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats, types with
                                | [], _ -> Ok (env, bindings, vg)
                                | p :: rest, t :: restTypes ->
                                    let (elemVar, vg1) = ANF.freshVar vg
                                    let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                    let elemBinding = (elemVar, elemExpr)
                                    // Recursively collect bindings from this element's pattern with correct type
                                    collectPatternBindings p (ANF.Var elemVar) t env (elemBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromTuple rest restTypes (idx + 1) env' bindings' vg')
                                | p :: rest, [] ->
                                    // Fallback if types list is shorter (shouldn't happen)
                                    let (elemVar, vg1) = ANF.freshVar vg
                                    let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                    let elemBinding = (elemVar, elemExpr)
                                    collectPatternBindings p (ANF.Var elemVar) AST.TInt64 env (elemBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromTuple rest [] (idx + 1) env' bindings' vg')
                            collectFromTuple innerPatterns elemTypes 0 env bindings vg
                        | AST.PConstructor (_, payloadPattern) ->
                            match payloadPattern with
                            | None -> Ok (env, bindings, vg)
                            | Some innerPat ->
                                // Extract payload (at index 1) and recursively collect
                                // For constructors, fallback to TInt64 as payload type extraction is complex
                                let (payloadVar, vg1) = ANF.freshVar vg
                                let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                                let payloadBinding = (payloadVar, payloadExpr)
                                collectPatternBindings innerPat (ANF.Var payloadVar) AST.TInt64 env (payloadBinding :: bindings) vg1
                        | AST.PRecord (_, fieldPatterns) ->
                            // Extract field types from record type (look up in type registry)
                            let fieldTypes =
                                match sourceType with
                                | AST.TRecord recordName ->
                                    match Map.tryFind recordName typeReg with
                                    | Some fields -> fields |> List.map snd
                                    | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                                | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                            // Extract each field and recursively collect bindings
                            let rec collectFromRecord (fields: (string * AST.Pattern) list) (types: AST.Type list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match fields, types with
                                | [], _ -> Ok (env, bindings, vg)
                                | (_, p) :: rest, t :: restTypes ->
                                    let (fieldVar, vg1) = ANF.freshVar vg
                                    let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                    let fieldBinding = (fieldVar, fieldExpr)
                                    collectPatternBindings p (ANF.Var fieldVar) t env (fieldBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromRecord rest restTypes (idx + 1) env' bindings' vg')
                                | (_, p) :: rest, [] ->
                                    let (fieldVar, vg1) = ANF.freshVar vg
                                    let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                    let fieldBinding = (fieldVar, fieldExpr)
                                    collectPatternBindings p (ANF.Var fieldVar) AST.TInt64 env (fieldBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromRecord rest [] (idx + 1) env' bindings' vg')
                            collectFromRecord fieldPatterns fieldTypes 0 env bindings vg
                        | AST.PList innerPatterns ->
                            // Extract element type from list type
                            let elemType =
                                match sourceType with
                                | AST.TList t -> t
                                | _ -> AST.TInt64
                            // For list patterns, extract head elements using FingerTree operations
                            // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                            // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                            let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] -> Ok (env, bindings, vg)
                                | p :: rest ->
                                    // Lists are FingerTrees - use headUnsafe/tail to extract
                                    let (headVar, vg1) = ANF.freshVar vg
                                    let headExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [currentList])
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        if List.isEmpty rest then
                                            Ok (env', bindings', vg')
                                        else
                                            // Get tail for next iteration
                                            let (tailVar, vg2) = ANF.freshVar vg'
                                            let tailExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [currentList])
                                            let tailBinding = (tailVar, tailExpr)
                                            collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                            collectFromList innerPatterns sourceAtom env bindings vg
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // Extract element type from list type
                            let elemType =
                                match sourceType with
                                | AST.TList t -> t
                                | _ -> AST.TInt64
                            // Extract head elements then bind tail using FingerTree operations
                            // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                            // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                            let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] ->
                                    // Bind the remaining list to tail pattern (tail has same type as source)
                                    collectPatternBindings tailPattern currentList sourceType env bindings vg
                                | p :: rest ->
                                    // Lists are FingerTrees - use headUnsafe/tail to extract
                                    let (rawHeadVar, vg1) = ANF.freshVar vg
                                    let rawHeadExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [currentList])
                                    let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                                    // Wrap with TypedAtom to preserve correct element type in TypeMap
                                    let (headVar, vg1') = ANF.freshVar vg1
                                    let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) elemType env (rawHeadBinding :: headBinding :: bindings) vg1'
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        let (rawTailVar, vg2) = ANF.freshVar vg'
                                        let rawTailExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [currentList])
                                        let rawTailBinding = (rawTailVar, rawTailExpr)
                                        // Wrap tail with TypedAtom to preserve list type
                                        let (tailVar, vg2') = ANF.freshVar vg2
                                        let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, sourceType)
                                        let tailBinding = (tailVar, tailExpr)
                                        collectHeads rest (ANF.Var tailVar) env' (rawTailBinding :: tailBinding :: bindings') vg2')
                            collectHeads headPatterns sourceAtom env bindings vg

                    // Collect all bindings from the tuple pattern, then compile body
                    collectPatternBindings (AST.PTuple patterns) scrutAtom scrutType currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings (List.rev bindings) bodyExpr
                            (finalExpr, vg2)))
                | AST.PRecord (_, fieldPatterns) ->
                    // Extract field types from record type
                    let fieldTypes =
                        match scrutType with
                        | AST.TRecord recordName ->
                            match Map.tryFind recordName typeReg with
                            | Some fields -> fields |> List.map snd
                            | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                        | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                    // Extract each field and bind pattern variables
                    let rec collectRecordBindings (fields: (string * AST.Pattern) list) (types: AST.Type list) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) (fieldIdx: int) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match fields, types with
                        | [], _ -> Ok (env, List.rev bindings, vg)
                        | (_, pat) :: rest, t :: restTypes ->
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use the correct field type
                                let newEnv = Map.add name (fieldVar, t) env
                                collectRecordBindings rest restTypes newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest restTypes env bindings vg1 (fieldIdx + 1)
                            | AST.PUnit | AST.PConstructor _ | AST.PLiteral _ | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in record field not yet supported: {pat}"
                        | (_, pat) :: rest, [] ->
                            // Fallback
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (fieldVar, AST.TInt64) env
                                collectRecordBindings rest [] newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest [] env bindings vg1 (fieldIdx + 1)
                            | _ ->
                                Error $"Nested pattern in record field not yet supported: {pat}"
                    collectRecordBindings fieldPatterns fieldTypes currentEnv [] vg 0
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PList patterns ->
                    // Extract list elements from FingerTree structure
                    // FingerTree layout:
                    // SINGLE (tag 1): [node:8] where node is LEAF-tagged
                    // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
                    // LEAF (tag 5): [value:8]

                    // Get element type from list type
                    let elemType =
                        match scrutType with
                        | AST.TList t -> t
                        | _ -> AST.TInt64

                    // Helper to unwrap a LEAF node and get the value
                    let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                        let (leafPtrVar, vg1) = ANF.freshVar vg
                        let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        let (valueVar, vg2) = ANF.freshVar vg1
                        let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                        (ANF.Var valueVar, valueVar, newBindings, vg2)

                    // Helper to extract tuple elements from a value
                    // tupleType is the type of the tuple being destructured
                    let rec collectTupleBindings (tupPats: AST.Pattern list) (tupleAtom: ANF.Atom) (tupleType: AST.Type) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        let tupleElemTypes =
                            match tupleType with
                            | AST.TTuple types -> types
                            | _ -> List.replicate (List.length tupPats) AST.TInt64
                        match tupPats with
                        | [] -> Ok (env, bindings, vg)
                        | tupPat :: tupRest ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (tupleAtom, idx)
                            let elemBinding = (elemVar, elemExpr)
                            let elemT = if idx < List.length tupleElemTypes then List.item idx tupleElemTypes else AST.TInt64
                            match tupPat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (elemVar, elemT) env
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [elemBinding]) vg1
                            | AST.PWildcard ->
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) env bindings vg1
                            | AST.PUnit | AST.PConstructor _ | AST.PLiteral _ | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                    let patternLen = List.length patterns
                    if patternLen = 0 then
                        // Empty pattern - no bindings needed
                        toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    elif patternLen = 1 then
                        // SINGLE node: extract the single element
                        // Untag to get pointer to SINGLE structure
                        let (ptrVar, vg1) = ANF.freshVar vg
                        let ptrExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        // Get the LEAF-tagged node at offset 0
                        let (nodeVar, vg2) = ANF.freshVar vg1
                        let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        // Unwrap the LEAF to get the value
                        let (rawValueAtom, rawValueVar, rawBindings, vg3) = unwrapLeaf (ANF.Var nodeVar) vg2 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                        // Wrap with TypedAtom to preserve element type in TypeMap
                        let (typedValueVar, vg3') = ANF.freshVar vg3
                        let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                        let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                        let valueVar = typedValueVar
                        let valueAtom = ANF.Var typedValueVar
                        // Bind the pattern
                        match List.head patterns with
                        | AST.PVar name ->
                            let newEnv = Map.add name (valueVar, elemType) currentEnv
                            toANF body vg3' newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg4) ->
                                (wrapBindings bindings bodyExpr, vg4))
                        | AST.PWildcard ->
                            toANF body vg3' currentEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg4) ->
                                (wrapBindings bindings bodyExpr, vg4))
                        | AST.PTuple innerPatterns ->
                            // elemType is the list element type, use it as tuple type
                            collectTupleBindings innerPatterns valueAtom elemType 0 currentEnv bindings vg3'
                            |> Result.bind (fun (newEnv, newBindings, vg4) ->
                                toANF body vg4 newEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg5) ->
                                    (wrapBindings newBindings bodyExpr, vg5)))
                        | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                            Error "Nested pattern in list element not yet supported"
                        | _ ->
                            Error $"Unsupported pattern in single-element list: {List.head patterns}"
                    else
                        // DEEP node: extract elements from prefix and suffix
                        // Untag to get pointer to DEEP structure
                        let (ptrVar, vg1) = ANF.freshVar vg
                        let ptrExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        let initialBindings = [(ptrVar, ptrExpr)]

                        // Extract elements - first from prefix, then from suffix
                        // Prefix offsets: 16, 24, 32, 40 (p0-p3)
                        // Suffix offsets: 64, 72, 80, 88 (s0-s3)
                        let rec extractElements (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                            match pats with
                            | [] -> Ok (env, bindings, vg)
                            | pat :: rest ->
                                // Calculate offset based on position
                                // First element at idx 0 is in prefix at offset 16
                                // For DEEP nodes with elements in prefix/suffix:
                                // We place first element in prefix, rest in suffix
                                let offset =
                                    if idx = 0 then 16L  // p0
                                    else 64L + (int64 (idx - 1) * 8L)  // s0, s1, s2, s3 at 64, 72, 80, 88

                                // Get the LEAF-tagged node
                                let (nodeVar, vg1) = ANF.freshVar vg
                                let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 offset), None)
                                // Unwrap the LEAF to get the value
                                let (rawValueAtom, rawValueVar, rawBindings, vg2) = unwrapLeaf (ANF.Var nodeVar) vg1 (bindings @ [(nodeVar, nodeExpr)])
                                // Wrap with TypedAtom to preserve element type in TypeMap
                                let (typedValueVar, vg2') = ANF.freshVar vg2
                                let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                                let newBindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                                let valueVar = typedValueVar
                                let valueAtom = ANF.Var typedValueVar

                                match pat with
                                | AST.PVar name ->
                                    let newEnv = Map.add name (valueVar, elemType) env
                                    extractElements rest (idx + 1) newEnv newBindings vg2'
                                | AST.PWildcard ->
                                    extractElements rest (idx + 1) env newBindings vg2'
                                | AST.PTuple innerPatterns ->
                                    // elemType is the list element type, use it as tuple type
                                    collectTupleBindings innerPatterns valueAtom elemType 0 env newBindings vg2'
                                    |> Result.bind (fun (tupEnv, tupBindings, vg3) ->
                                        extractElements rest (idx + 1) tupEnv tupBindings vg3)
                                | _ ->
                                    Error $"Unsupported pattern in list element: {pat}"

                        extractElements patterns 0 currentEnv initialBindings vg1
                        |> Result.bind (fun (newEnv, bindings, vg2) ->
                            toANF body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg3) ->
                                (wrapBindings bindings bodyExpr, vg3)))
                | AST.PListCons (headPatterns, tailPattern) ->
                    // Get element type from list type
                    let elemType =
                        match scrutType with
                        | AST.TList t -> t
                        | _ -> AST.TInt64
                    // Extract head elements and bind tail using FingerTree operations
                    // Lists are FingerTrees, use headUnsafe_i64/tail_i64 for extraction
                    let rec collectListConsBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, listAtom, vg)
                        | pat :: rest ->
                            // Extract head using FingerTree.headUnsafe_i64
                            let (rawHeadVar, vg1) = ANF.freshVar vg
                            let rawHeadExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [listAtom])
                            let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (headVar, vg1') = ANF.freshVar vg1
                            let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                            let headBinding = (headVar, headExpr)
                            // Extract tail using FingerTree.tail_i64
                            let (rawTailVar, vg2) = ANF.freshVar vg1'
                            let rawTailExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [listAtom])
                            let rawTailBinding = (rawTailVar, rawTailExpr)
                            // Wrap with TypedAtom to preserve list type for tail
                            let listType = AST.TList elemType
                            let (tailVar, vg2') = ANF.freshVar vg2
                            let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)
                            let tailBinding = (tailVar, tailExpr)
                            // All bindings including raw extractions
                            let allBaseBindings = rawTailBinding :: tailBinding :: rawHeadBinding :: headBinding :: bindings
                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (headVar, elemType) env
                                collectListConsBindings rest (ANF.Var tailVar) newEnv allBaseBindings vg2'
                            | AST.PWildcard ->
                                collectListConsBindings rest (ANF.Var tailVar) env allBaseBindings vg2'
                            | AST.PTuple innerPatterns ->
                                // For tuple patterns inside list cons, extract each tuple element and bind variables
                                // elemType is the tuple type (since list elements are tuples)
                                let tupleElemTypes =
                                    match elemType with
                                    | AST.TTuple types -> types
                                    | _ -> List.replicate (List.length innerPatterns) AST.TInt64
                                let rec collectTupleBindings (tupPats: AST.Pattern list) (types: AST.Type list) (tupleAtom: ANF.Atom) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                                    match tupPats with
                                    | [] -> Ok (env, bindings, vg)
                                    | tupPat :: tupRest ->
                                        // Extract raw element with TupleGet
                                        let (rawElemVar, vg1) = ANF.freshVar vg
                                        let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                                        let rawElemBinding = (rawElemVar, rawElemExpr)
                                        let elemT = if idx < List.length types then List.item idx types else AST.TInt64
                                        // Wrap with TypedAtom to preserve correct element type
                                        let (elemVar, vg1') = ANF.freshVar vg1
                                        let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                                        let elemBinding = (elemVar, elemExpr)
                                        match tupPat with
                                        | AST.PVar name ->
                                            let newEnv = Map.add name (elemVar, elemT) env
                                            collectTupleBindings tupRest types tupleAtom (idx + 1) newEnv (elemBinding :: rawElemBinding :: bindings) vg1'
                                        | AST.PWildcard ->
                                            collectTupleBindings tupRest types tupleAtom (idx + 1) env (rawElemBinding :: bindings) vg1
                                        | AST.PUnit | AST.PConstructor _ | AST.PLiteral _ | AST.PBool _
                                        | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                                        | AST.PList _ | AST.PListCons _ ->
                                            Error $"Nested pattern in tuple element not yet supported: {tupPat}"
                                collectTupleBindings innerPatterns tupleElemTypes (ANF.Var headVar) 0 env allBaseBindings vg2'
                                |> Result.bind (fun (newEnv, newBindings, vg3) ->
                                    collectListConsBindings rest (ANF.Var tailVar) newEnv newBindings vg3)
                            | AST.PUnit | AST.PConstructor _ | AST.PLiteral _ | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in list cons element not yet supported: {pat}"
                    collectListConsBindings headPatterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, tailAtom, vg1) ->
                        // Bind tail pattern
                        match tailPattern with
                        | AST.PVar name ->
                            let (tailVar, vg2) = ANF.freshVar vg1
                            // Tail has the same list type as the scrutinee
                            let newEnv' = Map.add name (tailVar, scrutType) newEnv
                            toANF body vg2 newEnv' typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg3) ->
                                let tailBinding = (tailVar, ANF.Atom tailAtom)
                                let allBindings = bindings @ [tailBinding]
                                let finalExpr = wrapBindings allBindings bodyExpr
                                (finalExpr, vg3))
                        | AST.PWildcard ->
                            toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg2) ->
                                let finalExpr = wrapBindings bindings bodyExpr
                                (finalExpr, vg2))
                        | _ -> Error "Tail pattern in list cons must be variable or wildcard")

            // Extract pattern bindings, check guard, and compile body
            // Returns: if guard is true, execute body; otherwise execute elseExpr
            // scrutType is the type of the scrutinee for correct pattern variable typing
            and extractAndCompileBodyWithGuard (pattern: AST.Pattern) (guardExpr: AST.Expr) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) (elseExpr: ANF.AExpr) : Result<ANF.AExpr * ANF.VarGen, string> =
                // First, we need to extract bindings from the pattern
                // Then compile the guard with those bindings in scope
                // Then compile the body with those bindings in scope
                // Finally, generate: let <bindings> in if <guard> then <body> else <elseExpr>

                // Helper to collect pattern variable bindings (simplified version for common patterns)
                // sourceType is the type of the source being matched
                let rec collectBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
                    match pat with
                    | AST.PUnit | AST.PWildcard | AST.PLiteral _ | AST.PBool _ | AST.PString _ | AST.PFloat _ ->
                        (env, bindings, vg)
                    | AST.PVar name ->
                        let (tempId, vg1) = ANF.freshVar vg
                        // Use TypedAtom to preserve the correct type in TypeMap
                        let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                        let newEnv = Map.add name (tempId, sourceType) env
                        (newEnv, binding :: bindings, vg1)
                    | AST.PTuple innerPatterns ->
                        let elemTypes =
                            match sourceType with
                            | AST.TTuple types -> types
                            | _ -> List.replicate (List.length innerPatterns) AST.TInt64
                        let rec collectFromTuple pats types idx env bindings vg =
                            match pats, types with
                            | [], _ -> (env, bindings, vg)
                            | p :: rest, t :: restTypes ->
                                let (elemVar, vg1) = ANF.freshVar vg
                                let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var elemVar) t env ((elemVar, elemExpr) :: bindings) vg1
                                collectFromTuple rest restTypes (idx + 1) env' bindings' vg'
                            | p :: rest, [] ->
                                let (elemVar, vg1) = ANF.freshVar vg
                                let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var elemVar) AST.TInt64 env ((elemVar, elemExpr) :: bindings) vg1
                                collectFromTuple rest [] (idx + 1) env' bindings' vg'
                        collectFromTuple innerPatterns elemTypes 0 env bindings vg
                    | AST.PConstructor (_, Some innerPat) ->
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                        // Fallback to TInt64 for constructor payload (complex type extraction)
                        collectBindings innerPat (ANF.Var payloadVar) AST.TInt64 env ((payloadVar, payloadExpr) :: bindings) vg1
                    | AST.PConstructor (_, None) ->
                        (env, bindings, vg)
                    | AST.PRecord (_, fieldPatterns) ->
                        let fieldTypes =
                            match sourceType with
                            | AST.TRecord recordName ->
                                match Map.tryFind recordName typeReg with
                                | Some fields -> fields |> List.map snd
                                | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                            | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                        let rec collectFromRecord fields types idx env bindings vg =
                            match fields, types with
                            | [], _ -> (env, bindings, vg)
                            | (_, p) :: rest, t :: restTypes ->
                                let (fieldVar, vg1) = ANF.freshVar vg
                                let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var fieldVar) t env ((fieldVar, fieldExpr) :: bindings) vg1
                                collectFromRecord rest restTypes (idx + 1) env' bindings' vg'
                            | (_, p) :: rest, [] ->
                                let (fieldVar, vg1) = ANF.freshVar vg
                                let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                let (env', bindings', vg') = collectBindings p (ANF.Var fieldVar) AST.TInt64 env ((fieldVar, fieldExpr) :: bindings) vg1
                                collectFromRecord rest [] (idx + 1) env' bindings' vg'
                        collectFromRecord fieldPatterns fieldTypes 0 env bindings vg
                    | AST.PList innerPatterns ->
                        let elemType =
                            match sourceType with
                            | AST.TList t -> t
                            | _ -> AST.TInt64
                        // For list patterns, extract head elements using FingerTree operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                            match pats with
                            | [] -> (env, bindings, vg)
                            | p :: rest ->
                                // Lists are FingerTrees - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [currentList])
                                let headBinding = (headVar, headExpr)
                                let (env', bindings', vg') = collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                if List.isEmpty rest then
                                    (env', bindings', vg')
                                else
                                    // Get tail for next iteration
                                    let (tailVar, vg2) = ANF.freshVar vg'
                                    let tailExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [currentList])
                                    let tailBinding = (tailVar, tailExpr)
                                    collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2
                        collectFromList innerPatterns sourceAtom env bindings vg
                    | AST.PListCons (headPatterns, tailPattern) ->
                        let elemType =
                            match sourceType with
                            | AST.TList t -> t
                            | _ -> AST.TInt64
                        // Extract head elements then bind tail using FingerTree operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                            match pats with
                            | [] ->
                                // Bind the remaining list to tail pattern (tail has same type as source)
                                collectBindings tailPattern currentList sourceType env bindings vg
                            | p :: rest ->
                                // Lists are FingerTrees - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [currentList])
                                let headBinding = (headVar, headExpr)
                                let (env', bindings', vg') = collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                let (tailVar, vg2) = ANF.freshVar vg'
                                let tailExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [currentList])
                                let tailBinding = (tailVar, tailExpr)
                                collectHeads rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2
                        collectHeads headPatterns sourceAtom env bindings vg

                let (newEnv, bindings, vg1) = collectBindings pattern scrutAtom scrutType currentEnv [] vg

                // Compile guard expression in the extended environment
                toAtom guardExpr vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (guardAtom, guardBindings, vg2) ->
                    // Compile body expression in the extended environment
                    toANF body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
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
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 n))
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
                | AST.PConstructor (variantName, payloadPattern) ->
                    match Map.tryFind variantName variantLookup with
                    | Some (_, _, tag, _) ->
                        if typeHasAnyPayload variantName then
                            // Load tag from heap (index 0), then compare
                            let (tagVar, vg1) = ANF.freshVar vg
                            let tagLoadExpr = ANF.TupleGet (scrutAtom, 0)
                            let (tagCmpVar, vg2) = ANF.freshVar vg1
                            let tagCmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 tag)))

                            // Check if payload pattern needs comparison (e.g., Some(true) vs Some(false))
                            match payloadPattern with
                            | Some innerPattern ->
                                // Extract payload and check if inner pattern needs comparison
                                let (payloadVar, vg3) = ANF.freshVar vg2
                                let payloadLoadExpr = ANF.TupleGet (scrutAtom, 1)
                                buildPatternComparison innerPattern (ANF.Var payloadVar) vg3
                                |> Result.map (fun innerResult ->
                                    match innerResult with
                                    | None ->
                                        // Inner pattern is variable/wildcard, only need tag check
                                        Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg3)
                                    | Some (innerCond, innerBindings, vg4) ->
                                        // Need to AND tag check with payload check
                                        let (andVar, vg5) = ANF.freshVar vg4
                                        let andExpr = ANF.Prim (ANF.And, ANF.Var tagCmpVar, innerCond)
                                        let allBindings = [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr); (payloadVar, payloadLoadExpr)] @ innerBindings @ [(andVar, andExpr)]
                                        Some (ANF.Var andVar, allBindings, vg5))
                            | None ->
                                // No payload pattern, just check tag
                                Ok (Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg2))
                        else
                            // Simple enum - scrutinee IS the tag
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 (int64 tag)))
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    | None -> Error $"Unknown constructor in pattern: {variantName}"
                | AST.PTuple innerPatterns ->
                    // Tuple patterns with literals need to compare each element
                    let rec buildTupleComparisons (patterns: AST.Pattern list) (index: int) (vg: ANF.VarGen) (accBindings: (ANF.TempId * ANF.CExpr) list) (accConditions: ANF.Atom list) =
                        match patterns with
                        | [] ->
                            if List.isEmpty accConditions then
                                Ok None  // All variables/wildcards, no comparison needed
                            else
                                // AND together all conditions
                                let rec andAll (conds: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                                    match conds with
                                    | [] -> Error "Empty conditions list"
                                    | [single] -> Ok (single, bindings, vg)
                                    | first :: rest ->
                                        andAll rest vg bindings
                                        |> Result.map (fun (restResult, restBindings, vg1) ->
                                            let (andVar, vg2) = ANF.freshVar vg1
                                            let andExpr = ANF.Prim (ANF.And, first, restResult)
                                            (ANF.Var andVar, restBindings @ [(andVar, andExpr)], vg2))
                                andAll accConditions vg accBindings
                                |> Result.map (fun (result, bindings, vg') -> Some (result, bindings, vg'))
                        | p :: rest ->
                            // Extract element at index
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemLoad = ANF.TupleGet (scrutAtom, index)
                            let newBindings = accBindings @ [(elemVar, elemLoad)]
                            // Check if this pattern needs comparison
                            buildPatternComparison p (ANF.Var elemVar) vg1
                            |> Result.bind (fun compResult ->
                                match compResult with
                                | None ->
                                    // This element pattern doesn't need comparison (var/wildcard)
                                    buildTupleComparisons rest (index + 1) vg1 newBindings accConditions
                                | Some (cond, condBindings, vg2) ->
                                    // Add this comparison
                                    buildTupleComparisons rest (index + 1) vg2 (newBindings @ condBindings) (accConditions @ [cond]))
                    buildTupleComparisons innerPatterns 0 vg [] []
                | AST.PRecord (_, fieldPatterns) ->
                    // Record patterns with literals need to compare each field
                    let rec buildRecordComparisons (fields: (string * AST.Pattern) list) (vg: ANF.VarGen) (accBindings: (ANF.TempId * ANF.CExpr) list) (accConditions: ANF.Atom list) =
                        match fields with
                        | [] ->
                            if List.isEmpty accConditions then
                                Ok None
                            else
                                let rec andAll (conds: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                                    match conds with
                                    | [] -> Error "Empty conditions list"
                                    | [single] -> Ok (single, bindings, vg)
                                    | first :: rest ->
                                        andAll rest vg bindings
                                        |> Result.map (fun (restResult, restBindings, vg1) ->
                                            let (andVar, vg2) = ANF.freshVar vg1
                                            let andExpr = ANF.Prim (ANF.And, first, restResult)
                                            (ANF.Var andVar, restBindings @ [(andVar, andExpr)], vg2))
                                andAll accConditions vg accBindings
                                |> Result.map (fun (result, bindings, vg') -> Some (result, bindings, vg'))
                        | (fieldName, p) :: rest ->
                            // Find field index in the record type (would need type info)
                            // For now, use a simple approach - records are ordered by field definition
                            // This is a simplification; proper implementation would need type lookup
                            let fieldIndex = List.findIndex (fun (fn, _) -> fn = fieldName) fieldPatterns
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemLoad = ANF.TupleGet (scrutAtom, fieldIndex)
                            let newBindings = accBindings @ [(elemVar, elemLoad)]
                            buildPatternComparison p (ANF.Var elemVar) vg1
                            |> Result.bind (fun compResult ->
                                match compResult with
                                | None -> buildRecordComparisons rest vg1 newBindings accConditions
                                | Some (cond, condBindings, vg2) ->
                                    buildRecordComparisons rest vg2 (newBindings @ condBindings) (accConditions @ [cond]))
                    buildRecordComparisons fieldPatterns vg [] []
                | AST.PList patterns ->
                    // List pattern comparison: check length matches for FingerTree
                    // FingerTree tags: EMPTY=0, SINGLE=1, DEEP=2
                    // For [] pattern: check scrutinee == 0 (EMPTY)
                    // For [a] pattern: check tag == 1 (SINGLE)
                    // For [a, b, ...] pattern: check tag == 2 (DEEP) and measure == length
                    let patternLen = List.length patterns
                    if patternLen = 0 then
                        // Empty list pattern: check scrutinee == 0
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 0L))
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    elif patternLen = 1 then
                        // Single element pattern: check tag == 1 (SINGLE)
                        let (tagVar, vg1) = ANF.freshVar vg
                        let tagExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 7L))
                        let (cmpVar, vg2) = ANF.freshVar vg1
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))
                        Ok (Some (ANF.Var cmpVar, [(tagVar, tagExpr); (cmpVar, cmpExpr)], vg2))
                    else
                        // Multiple elements: check length == patternLen
                        // Use Stdlib.FingerTree.length which handles EMPTY/SINGLE/DEEP safely
                        let (lengthVar, vg1) = ANF.freshVar vg
                        let lengthExpr = ANF.Call ("Stdlib.FingerTree.length_i64", [scrutAtom])
                        let (cmpVar, vg2) = ANF.freshVar vg1
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                        Ok (Some (ANF.Var cmpVar, [(lengthVar, lengthExpr); (cmpVar, cmpExpr)], vg2))
                | AST.PListCons (headPatterns, _) ->
                    // List cons pattern: [...t] matches any list, [h, ...t] needs at least one element
                    if List.isEmpty headPatterns then
                        // [...t] matches any list including empty
                        Ok None
                    else
                        // [h, ...t] or [a, b, ...t] - needs at least one element (non-nil)
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Neq, scrutAtom, ANF.IntLiteral (ANF.Int64 0L))
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))

            // Compile a list pattern for FingerTree with proper length validation.
            // FingerTree layout:
            // SINGLE (tag 1): [node:8] where node is LEAF-tagged
            // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
            // LEAF (tag 5): [value:8]
            // listType is the list type (TList elemType) for correct pattern variable typing
            let compileListPatternWithChecks
                (patterns: AST.Pattern list)
                (listAtom: ANF.Atom)
                (listType: AST.Type)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                // Extract element type from list type
                let elemType =
                    match listType with
                    | AST.TList t -> t
                    | _ -> AST.TInt64  // Fallback

                let patternLen = List.length patterns

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements from a value
                // tupleType is the type of the tuple being matched (TTuple elemTypes)
                let rec extractTupleBindings
                    (tupPats: AST.Pattern list)
                    (tupleAtom: ANF.Atom)
                    (tupleType: AST.Type)
                    (idx: int)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    // Extract element types from tuple type
                    let tupleElemTypes =
                        match tupleType with
                        | AST.TTuple types -> types
                        | _ -> List.replicate (List.length tupPats) AST.TInt64
                    match tupPats with
                    | [] -> Ok (env, bindings, vg)
                    | tupPat :: tupRest ->
                        let (rawElemVar, vg1) = ANF.freshVar vg
                        let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                        let rawElemBinding = (rawElemVar, rawElemExpr)
                        let elemT = if idx < List.length tupleElemTypes then List.item idx tupleElemTypes else AST.TInt64
                        // Wrap with TypedAtom to preserve correct element type in TypeMap
                        let (elemVar, vg1') = ANF.freshVar vg1
                        let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                        let elemBinding = (elemVar, elemExpr)
                        match tupPat with
                        | AST.PVar name ->
                            let newEnv = Map.add name (elemVar, elemT) env  // Use correct element type
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                        | AST.PWildcard ->
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                        | AST.PUnit | AST.PConstructor _ | AST.PLiteral _ | AST.PBool _
                        | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                        | AST.PList _ | AST.PListCons _ ->
                            Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                if patternLen = 0 then
                    // Empty list: check scrutinee == 0 (EMPTY)
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L))
                    toANF body vg1 currentEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let ifExpr = ANF.If (ANF.Var checkVar, bodyExpr, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg2))
                elif patternLen = 1 then
                    // Single element: check tag == 1 (SINGLE), then extract
                    let (tagVar, vg1) = ANF.freshVar vg
                    let tagExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L))
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))

                    // Untag to get pointer to SINGLE structure
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    // Get the LEAF-tagged node at offset 0
                    let (nodeVar, vg4) = ANF.freshVar vg3
                    let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    // Unwrap the LEAF to get the value
                    let (rawValueAtom, rawValueVar, rawBindings, vg5) = unwrapLeaf (ANF.Var nodeVar) vg4 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                    // Wrap with TypedAtom to preserve element type in TypeMap
                    let (typedValueVar, vg5') = ANF.freshVar vg5
                    let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                    let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                    let valueVar = typedValueVar
                    let valueAtom = ANF.Var typedValueVar

                    // Bind the pattern
                    let pat = List.head patterns
                    match pat with
                    | AST.PVar name ->
                        let newEnv = Map.add name (valueVar, elemType) currentEnv  // Use element type
                        toANF body vg5' newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | AST.PWildcard ->
                        toANF body vg5' currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | AST.PTuple innerPatterns ->
                        extractTupleBindings innerPatterns valueAtom elemType 0 currentEnv bindings vg5'  // Pass tuple type
                        |> Result.bind (fun (newEnv, newBindings, vg6) ->
                            toANF body vg6 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg7) ->
                                let withBindings = wrapBindings newBindings bodyExpr
                                let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                                (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg7)))
                    | AST.PLiteral n ->
                        // Literal pattern: check tag==SINGLE, extract value, check value==literal
                        // Important: bindings must come BEFORE the literal check since they define valueVar
                        let (litCheckVar, vg6) = ANF.freshVar vg5'
                        let litCheckExpr = ANF.Prim (ANF.Eq, valueAtom, ANF.IntLiteral (ANF.Int64 n))
                        toANF body vg6 currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg7) ->
                            // Structure: check tag -> extract value (bindings) -> check literal -> if match then body else else
                            // Note: We use two nested Ifs because the tag check guards the memory access in bindings
                            let ifLitExpr = ANF.If (ANF.Var litCheckVar, bodyExpr, elseExpr)
                            let withLitBinding = ANF.Let (litCheckVar, litCheckExpr, ifLitExpr)
                            // bindings must be OUTSIDE the inner If to define valueVar before litCheckExpr uses it
                            let withBindings = wrapBindings bindings withLitBinding
                            let withTagCheck = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, withTagCheck)), vg7))
                    | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                        Error "Nested pattern in list element not yet supported"
                    | _ ->
                        Error $"Unsupported pattern in single-element list: {pat}"
                else
                    // Multiple elements: check length == patternLen (safe for all list types)
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthExpr = ANF.Call ("Stdlib.FingerTree.length_i64", [listAtom])
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                    // Untag to get pointer (only used in then-branch after length check passes)
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                    // Note: lengthExpr and checkExpr are safe (length handles EMPTY)
                    // ptrExpr just does bitwise and, doesn't dereference
                    let headerBindings = [(lengthVar, lengthExpr); (checkVar, checkExpr); (ptrVar, ptrExpr)]
                    let vg6 = vg3  // Keep consistent naming for the rest of the code

                    // Extract elements using getAt (handles varying prefix/suffix layouts)
                    // Returns: (env, bindings, literalChecks, vg) where literalChecks are (valueVar, expectedLit) pairs
                    let rec extractElements (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (litChecks: (ANF.TempId * int64) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * (ANF.TempId * int64) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, bindings, litChecks, vg)
                        | pat :: rest ->
                            // Use getAt to retrieve element at this index
                            // getAt returns Option, but we know length == patternLen so it's always Some
                            // Note: We call Stdlib.List.__getAtInt64 (a non-generic wrapper) rather than
                            // Stdlib.FingerTree.getAt_i64 because monomorphization happens at AST level,
                            // but pattern matching compilation happens at ANF level.
                            let (optVar, vg1) = ANF.freshVar vg
                            let getAtExpr = ANF.Call ("Stdlib.List.__getAtInt64", [listAtom; ANF.IntLiteral (ANF.Int64 (int64 idx))])
                            // Unwrap the Some - getAt returns tagged value with tag 1 for Some
                            let (rawValueVar, vg2) = ANF.freshVar vg1
                            let rawValueExpr = ANF.RawGet (ANF.Var optVar, ANF.IntLiteral (ANF.Int64 8L), None)  // Some payload at offset 8
                            // Wrap with TypedAtom to preserve element type in TypeMap
                            let (typedValueVar, vg2') = ANF.freshVar vg2
                            let typedValueExpr = ANF.TypedAtom (ANF.Var rawValueVar, elemType)
                            let newBindings = bindings @ [(optVar, getAtExpr); (rawValueVar, rawValueExpr); (typedValueVar, typedValueExpr)]
                            let valueVar = typedValueVar

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (valueVar, elemType) env  // Use element type
                                extractElements rest (idx + 1) newEnv newBindings litChecks vg2'
                            | AST.PWildcard ->
                                extractElements rest (idx + 1) env newBindings litChecks vg2'
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns (ANF.Var valueVar) elemType 0 env newBindings vg2'  // Pass tuple type
                                |> Result.bind (fun (tupEnv, tupBindings, vg3) ->
                                    extractElements rest (idx + 1) tupEnv tupBindings litChecks vg3)
                            | AST.PLiteral n ->
                                // Track this literal check for later
                                extractElements rest (idx + 1) env newBindings ((valueVar, n) :: litChecks) vg2'
                            | _ ->
                                Error $"Unsupported pattern in list element: {pat}"

                    extractElements patterns 0 currentEnv [] [] vg6
                    |> Result.bind (fun (newEnv, elemBindings, litChecks, vg7) ->
                        toANF body vg7 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg8) ->
                            // Build the inner expression based on whether we have literal checks
                            let (innerExpr, vg9) =
                                match litChecks with
                                | [] ->
                                    // No literals - just return body
                                    (bodyExpr, vg8)
                                | checks ->
                                    // Build literal checks - AND them all together
                                    // Note: We don't AND with checkVar since we already checked length
                                    let rec buildLitOnlyChecks (remaining: (ANF.TempId * int64) list) (accBindings: (ANF.TempId * ANF.CExpr) list) (prevCondVar: ANF.TempId option) (vg: ANF.VarGen) : (ANF.TempId * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) =
                                        match remaining with
                                        | [] ->
                                            (Option.get prevCondVar, accBindings, vg)
                                        | (valueVar, litVal) :: rest ->
                                            let (litCheckVar, vg1) = ANF.freshVar vg
                                            let litCheckExpr = ANF.Prim (ANF.Eq, ANF.Var valueVar, ANF.IntLiteral (ANF.Int64 litVal))
                                            match prevCondVar with
                                            | None ->
                                                // First check - use directly
                                                buildLitOnlyChecks rest (accBindings @ [(litCheckVar, litCheckExpr)]) (Some litCheckVar) vg1
                                            | Some prevVar ->
                                                // AND with previous check
                                                let (combinedVar, vg2) = ANF.freshVar vg1
                                                let combinedExpr = ANF.Prim (ANF.And, ANF.Var prevVar, ANF.Var litCheckVar)
                                                buildLitOnlyChecks rest (accBindings @ [(litCheckVar, litCheckExpr); (combinedVar, combinedExpr)]) (Some combinedVar) vg2
                                    let (litCondVar, litBindings, vg9') = buildLitOnlyChecks (List.rev checks) [] None vg8
                                    let litCheckedBody = ANF.If (ANF.Var litCondVar, bodyExpr, elseExpr)
                                    let withLitBindings = wrapBindings litBindings litCheckedBody
                                    (withLitBindings, vg9')
                            // Wrap with element bindings (inside length check)
                            let withElemBindings = wrapBindings elemBindings innerExpr
                            // Wrap with length check
                            let ifExpr = ANF.If (ANF.Var checkVar, withElemBindings, elseExpr)
                            let withHeader = wrapBindings headerBindings ifExpr
                            (withHeader, vg9)))

            // Compile a list cons pattern [h, ...t] for FingerTree
            // This pattern extracts head element(s) and binds the rest to tail
            // For FingerTree:
            // - SINGLE (tag 1): head is the element, tail is EMPTY
            // - DEEP (tag 2): head is prefix[0], tail requires calling FingerTree.tail
            // listType is the list type (TList elemType) for correct pattern variable typing
            let rec compileListConsPatternWithChecks
                (headPatterns: AST.Pattern list)
                (tailPattern: AST.Pattern)
                (listAtom: ANF.Atom)
                (listType: AST.Type)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                // Extract element type from list type
                let elemType =
                    match listType with
                    | AST.TList t -> t
                    | _ -> AST.TInt64  // Fallback

                // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                // The correct element type is tracked in the VarEnv/TypeMap, not in the function name

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements
                // tupleType is the type of the tuple being matched (TTuple elemTypes)
                let rec extractTupleBindings
                    (tupPats: AST.Pattern list)
                    (tupleAtom: ANF.Atom)
                    (tupleType: AST.Type)
                    (idx: int)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    // Extract element types from tuple type
                    let elemTypes =
                        match tupleType with
                        | AST.TTuple types -> types
                        | _ -> List.replicate (List.length tupPats) AST.TInt64
                    match tupPats with
                    | [] -> Ok (env, bindings, vg)
                    | tupPat :: tupRest ->
                        let (rawElemVar, vg1) = ANF.freshVar vg
                        let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                        let rawElemBinding = (rawElemVar, rawElemExpr)
                        let elemT = if idx < List.length elemTypes then List.item idx elemTypes else AST.TInt64
                        // Wrap with TypedAtom to preserve correct element type in TypeMap
                        let (elemVar, vg1') = ANF.freshVar vg1
                        let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                        let elemBinding = (elemVar, elemExpr)
                        match tupPat with
                        | AST.PVar name ->
                            let newEnv = Map.add name (elemVar, elemT) env
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                        | AST.PWildcard ->
                            // Even for wildcard, we need to extract the element (for proper tuple access)
                            // but don't bind it to a name. Just add the raw binding and continue.
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                        | _ ->
                            Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                match headPatterns with
                | [] ->
                    // All head elements extracted - bind tail and compile body
                    match tailPattern with
                    | AST.PVar name ->
                        let (tailVar, vg1) = ANF.freshVar vg
                        let newEnv = Map.add name (tailVar, listType) currentEnv  // Use actual list type
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let withTail = ANF.Let (tailVar, ANF.Atom listAtom, bodyExpr)
                            (withTail, vg2))
                    | AST.PWildcard ->
                        toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    | _ -> Error "Tail pattern in list cons must be variable or wildcard"

                | [singleHeadPattern] ->
                    // Single head pattern [h, ...t] - most common case
                    // Use branching based on tag to handle SINGLE vs DEEP nodes

                    // Check list is not empty
                    let (notEmptyVar, vg1) = ANF.freshVar vg
                    let notEmptyExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral (ANF.Int64 0L))

                    // Get tag
                    let (tagVar, vg2) = ANF.freshVar vg1
                    let tagExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L))

                    // Untag to get pointer
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                    // Check if SINGLE (tag 1)
                    let (isSingleVar, vg4) = ANF.freshVar vg3
                    let isSingleExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))

                    // notEmptyVar must be bound OUTSIDE the If since it's used as the condition
                    let condBindings = [(notEmptyVar, notEmptyExpr)]
                    let innerBindings = [(tagVar, tagExpr); (ptrVar, ptrExpr); (isSingleVar, isSingleExpr)]

                    // Compile the SINGLE branch: node at offset 0, tail = EMPTY
                    let compileSingleBranch vg =
                        let (singleNodeVar, vg1) = ANF.freshVar vg
                        let singleNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var singleNodeVar) vg1 [(singleNodeVar, singleNodeExpr)]
                        // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                        let (typedHeadVar, vg2') = ANF.freshVar vg2
                        let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                        let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                        let headBindingsWithType = headBindings @ [typedHeadBinding]
                        let typedHeadAtom = ANF.Var typedHeadVar
                        // Tail is empty list (0 = EMPTY sentinel) - wrap with TypedAtom to preserve list type
                        let (rawTailVar, vg3) = ANF.freshVar vg2'
                        let rawTailExpr = ANF.Atom (ANF.IntLiteral (ANF.Int64 0L))  // EMPTY
                        let (tailVar, vg3') = ANF.freshVar vg3
                        let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)

                        // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                        // guardOpt is Some(var, expr) for literal patterns that need comparison
                        let headEnvResult =
                            match singleHeadPattern with
                            | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                            | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns typedHeadAtom elemType 0 currentEnv [] vg3'  // Pass tuple type
                                |> Result.map (fun (env, bindings, vg') -> (env, bindings, vg', None))
                            | AST.PLiteral n ->
                                // Compare head value to literal - guard check
                                let (guardVar, vg4) = ANF.freshVar vg3'
                                let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral (ANF.Int64 n))
                                Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                            | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                        headEnvResult
                        |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                            let tailEnvResult =
                                match tailPattern with
                                | AST.PVar name -> Ok (Map.add name (tailVar, listType) envWithHead, vg4)  // Use actual list type
                                | AST.PWildcard -> Ok (envWithHead, vg4)
                                | _ -> Error "Tail pattern must be variable or wildcard"

                            tailEnvResult
                            |> Result.bind (fun (finalEnv, vg5) ->
                                toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg6) ->
                                    let withTupleBindings = wrapBindings tupleBindings bodyExpr
                                    let withTypedTail = ANF.Let (tailVar, tailExpr, withTupleBindings)
                                    let withTail = ANF.Let (rawTailVar, rawTailExpr, withTypedTail)
                                    // If there's a guard (literal pattern), add check AFTER head bindings
                                    // because guardExpr uses headVar which is defined in headBindings
                                    let withGuard =
                                        match guardOpt with
                                        | Some (guardVar, guardExpr) ->
                                            // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                            let ifGuard = ANF.If (ANF.Var guardVar, withTail, elseExpr)
                                            let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                            wrapBindings headBindingsWithType withGuardBinding
                                        | None -> wrapBindings headBindingsWithType withTail
                                    (withGuard, vg6))))

                    // Compile the DEEP branch: node at offset 16 (prefix[0])
                    // For tail, call Stdlib.FingerTree.tail to properly compute the tail
                    let compileDeepBranch vg =
                        let (deepNodeVar, vg1) = ANF.freshVar vg
                        let deepNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), None)
                        let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var deepNodeVar) vg1 [(deepNodeVar, deepNodeExpr)]
                        // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                        let (typedHeadVar, vg2') = ANF.freshVar vg2
                        let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                        let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                        let headBindingsWithType = headBindings @ [typedHeadBinding]
                        let typedHeadAtom = ANF.Var typedHeadVar

                        // Call Stdlib.FingerTree.tail to get the tail
                        let (tailResultVar, vg3) = ANF.freshVar vg2'
                        let tailCallExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [listAtom])
                        // Wrap with TypedAtom to preserve correct list type in TypeMap
                        let (typedTailVar, vg3') = ANF.freshVar vg3
                        let typedTailExpr = ANF.TypedAtom (ANF.Var tailResultVar, listType)

                        let tailBindings = [(tailResultVar, tailCallExpr); (typedTailVar, typedTailExpr)]

                        // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                        let headEnvResult =
                            match singleHeadPattern with
                            | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                            | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns typedHeadAtom elemType 0 currentEnv [] vg3'  // Pass tuple type
                                |> Result.map (fun (env, bindings, vg') -> (env, bindings, vg', None))
                            | AST.PLiteral n ->
                                // Compare head value to literal - guard check
                                let (guardVar, vg4) = ANF.freshVar vg3'
                                let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral (ANF.Int64 n))
                                Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                            | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                        headEnvResult
                        |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                            let tailEnvResult =
                                match tailPattern with
                                | AST.PVar name -> Ok (Map.add name (typedTailVar, listType) envWithHead, vg4)  // Use typed tail var with correct list type
                                | AST.PWildcard -> Ok (envWithHead, vg4)
                                | _ -> Error "Tail pattern must be variable or wildcard"

                            tailEnvResult
                            |> Result.bind (fun (finalEnv, vg5) ->
                                toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg6) ->
                                    let withTupleBindings = wrapBindings tupleBindings bodyExpr
                                    let withTailBinding = wrapBindings tailBindings withTupleBindings
                                    // If there's a guard (literal pattern), add check AFTER head bindings
                                    // because guardExpr uses headVar which is defined in headBindingsWithType
                                    let withGuard =
                                        match guardOpt with
                                        | Some (guardVar, guardExpr) ->
                                            // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                            let ifGuard = ANF.If (ANF.Var guardVar, withTailBinding, elseExpr)
                                            let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                            wrapBindings headBindingsWithType withGuardBinding
                                        | None -> wrapBindings headBindingsWithType withTailBinding
                                    (withGuard, vg6))))

                    // Build the combined expression with branching
                    compileSingleBranch vg4
                    |> Result.bind (fun (singleBranchExpr, vg5) ->
                        compileDeepBranch vg5
                        |> Result.map (fun (deepBranchExpr, vg6) ->
                            // If SINGLE then singleBranch else deepBranch
                            let tagBranchExpr = ANF.If (ANF.Var isSingleVar, singleBranchExpr, deepBranchExpr)
                            // Wrap inner bindings (tag, ptr, isSingle) around the tag branch
                            let withInnerBindings = wrapBindings innerBindings tagBranchExpr
                            // If not empty then execute inner bindings + branch else elseExpr
                            let ifExpr = ANF.If (ANF.Var notEmptyVar, withInnerBindings, elseExpr)
                            // Bind notEmptyVar BEFORE the If
                            let finalExpr = wrapBindings condBindings ifExpr
                            (finalExpr, vg6)))

                | _ ->
                    // Multiple head patterns [a, b, ...t]
                    // Check length >= number of head patterns
                    let numHeads = List.length headPatterns
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthExpr = ANF.Call ("Stdlib.FingerTree.length_i64", [listAtom])
                    let (lengthCheckVar, vg2) = ANF.freshVar vg1
                    let lengthCheckExpr = ANF.Prim (ANF.Gte, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 numHeads)))

                    // Extract head elements and final tail using head/tail calls
                    // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                    // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                    let rec extractElements (pats: AST.Pattern list) (currentListVar: ANF.TempId) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.TempId * ANF.VarGen, string> =
                        match pats with
                        | [] ->
                            // No more head patterns, currentListVar is the tail
                            Ok (env, bindings, currentListVar, vg)
                        | pat :: rest ->
                            // Call head to get current element
                            let (headResultVar, vg1) = ANF.freshVar vg
                            let headCallExpr = ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [ANF.Var currentListVar])
                            // Call tail to get rest
                            let (tailResultVar, vg2) = ANF.freshVar vg1
                            let tailCallExpr = ANF.Call ("Stdlib.FingerTree.tail_i64", [ANF.Var currentListVar])
                            let newBindings = bindings @ [(headResultVar, headCallExpr); (tailResultVar, tailCallExpr)]

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (headResultVar, elemType) env  // Use element type
                                extractElements rest tailResultVar newEnv newBindings vg2
                            | AST.PWildcard ->
                                extractElements rest tailResultVar env newBindings vg2
                            | AST.PLiteral _ | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ ->
                                Error $"Unsupported head pattern in multi-element list cons: {pat}"

                    // Get initial list variable
                    let (initialListVar, vg3) = ANF.freshVar vg2
                    let initialListExpr = ANF.Atom listAtom

                    extractElements headPatterns initialListVar currentEnv [(initialListVar, initialListExpr)] vg3
                    |> Result.bind (fun (envAfterHeads, headBindings, finalTailVar, vg4) ->
                        // Bind tail pattern
                        let tailEnvResult =
                            match tailPattern with
                            | AST.PVar name -> Ok (Map.add name (finalTailVar, listType) envAfterHeads, vg4)  // Use actual list type
                            | AST.PWildcard -> Ok (envAfterHeads, vg4)
                            | _ -> Error "Tail pattern must be variable or wildcard"

                        tailEnvResult
                        |> Result.bind (fun (finalEnv, vg5) ->
                            toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg6) ->
                                // Wrap bindings around body
                                let withHeadBindings = wrapBindings headBindings bodyExpr
                                // Check length condition
                                let ifExpr = ANF.If (ANF.Var lengthCheckVar, withHeadBindings, elseExpr)
                                let withLengthCheck = ANF.Let (lengthCheckVar, lengthCheckExpr, ifExpr)
                                let finalExpr = ANF.Let (lengthVar, lengthExpr, withLengthCheck)
                                (finalExpr, vg6))))

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
                    let pattern = AST.NonEmptyList.head mc.Patterns
                    let body = mc.Body
                    // For non-empty list patterns, we still need proper length checking
                    match pattern with
                    | AST.PList (_ :: _ as listPatterns) ->
                        // Explicit list pattern like [a, b] as the only/last case with no wildcard
                        // This is non-exhaustive - reject at compile time
                        Error "Non-exhaustive pattern match: list pattern requires a catch-all case"
                    | AST.PListCons (headPatterns, tailPattern) ->
                        // List cons pattern as last case
                        // Fallback should crash at runtime since it's unreachable code
                        // Dereference null pointer (address 0) to trigger SIGSEGV
                        let (crashVar, vg') = ANF.freshVar vg
                        let crashExpr = ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None)
                        let fallbackExpr = ANF.Let (crashVar, crashExpr, ANF.Return (ANF.Var crashVar))
                        compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body fallbackExpr vg'
                    | _ ->
                        // Other patterns - original behavior
                        // Handle guard if present
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody pattern body scrutineeAtom' scrutType env vg
                        | Some guardExpr ->
                            // With guard: compile pattern match with guard check
                            // Fallback should crash at runtime since it's unreachable code
                            // Dereference null pointer (address 0) to trigger SIGSEGV
                            let (crashVar, vg') = ANF.freshVar vg
                            let crashExpr = ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None)
                            let fallbackExpr = ANF.Let (crashVar, crashExpr, ANF.Return (ANF.Var crashVar))
                            extractAndCompileBodyWithGuard pattern guardExpr body scrutineeAtom' scrutType env vg' fallbackExpr
                | mc :: rest ->
                    // For pattern grouping, use first pattern for bindings but OR all patterns for comparison
                    let firstPattern = AST.NonEmptyList.head mc.Patterns
                    let body = mc.Body
                    if patternAlwaysMatches firstPattern then
                        // Wildcard or var - matches everything, but may still need guard
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                        | Some guardExpr ->
                            // Wildcard with guard - still need to check guard, fall through if false
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                    else
                        // Non-empty list patterns need special handling with interleaved checks
                        match firstPattern with
                        | AST.PList (_ :: _ as listPatterns) ->
                            // Build the else branch first (rest of cases)
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                // Use the new interleaved check-and-extract function
                                compileListPatternWithChecks listPatterns scrutineeAtom' scrutType env body elseExpr vg1)
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // List cons pattern - needs interleaved checks
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body elseExpr vg1)
                        | _ ->
                            // Use pattern grouping: OR all patterns in the group
                            buildPatternGroupComparison (AST.NonEmptyList.toList mc.Patterns) scrutineeAtom' vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                                    | Some guardExpr ->
                                        buildChain rest vg
                                        |> Result.bind (fun (elseExpr, vg1) ->
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                                | Some (condAtom, bindings, vg1) ->
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg1
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
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg2 elseExpr
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
            toANF (partToExpr single) varGen env typeReg variantLookup funcReg moduleRegistry
        | first :: rest ->
            // Multiple parts → fold with StringConcat
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry

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
                toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry
        | AST.Var name ->
            // Calling a variable that might hold a closure
            match Map.tryFind name env with
            | Some (tempId, _) ->
                // Variable exists - treat as closure call
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    // Generate closure call
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Return (ANF.Var resultId), varGen2)
                    |> Result.map (fun (expr, vg) ->
                        (wrapBindings finalBindings expr, vg)))
            | None ->
                Error $"Cannot apply variable '{name}' as function - variable not in scope"

        | AST.Apply (_, _) ->
            // Nested application: ((x) => (y) => ...)(a)(b)(c)...
            // Flatten all nested applies first, then desugar from innermost out
            let rec flattenApplies expr argLists =
                match expr with
                | AST.Apply (innerFunc, innerArgs) -> flattenApplies innerFunc (innerArgs :: argLists)
                | other -> (other, argLists)

            let (baseFunc, allArgLists) = flattenApplies func [args]
            // allArgLists is a list of arg lists, from innermost to outermost
            // e.g., for f(1)(2)(3), we get ([1], [2], [3])

            match baseFunc with
            | AST.Lambda _ ->
                // Desugar all nested lambda applications at once
                let rec desugaAll (currentFunc: AST.Expr) (remainingArgLists: AST.Expr list list) : AST.Expr =
                    match remainingArgLists with
                    | [] -> currentFunc
                    | currentArgs :: restArgLists ->
                        match currentFunc with
                        | AST.Lambda (lambdaParams, body) ->
                            if List.length currentArgs <> List.length lambdaParams then
                                // Will error later, just wrap in Apply for now
                                desugaAll (AST.Apply (currentFunc, currentArgs)) restArgLists
                            else
                                // Desugar: let p1 = a1 in let p2 = a2 in ... body
                                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                                    match ps, as' with
                                    | [], [] -> body
                                    | (pName, _) :: restPs, argExpr :: restAs ->
                                        AST.Let (pName, argExpr, buildLets restPs restAs)
                                    | _ -> body
                                let desugared = buildLets lambdaParams currentArgs
                                desugaAll desugared restArgLists
                        | AST.Let (name, value, innerBody) ->
                            // Float let out: Apply(let x = v in body, args) → let x = v in Apply(body, args)
                            AST.Let (name, value, desugaAll innerBody (currentArgs :: restArgLists))
                        | _ ->
                            // Non-lambda function - wrap remaining in Apply
                            let applied = AST.Apply (currentFunc, currentArgs)
                            desugaAll applied restArgLists

                let desugared = desugaAll baseFunc allArgLists
                toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry

            | _ ->
                // Base function is not a lambda - use toAtom which handles nested applies
                // Reconstruct the full nested apply, then delegate to toAtom
                let rec applyAll (currentExpr: AST.Expr) (remainingArgLists: AST.Expr list list) : AST.Expr =
                    match remainingArgLists with
                    | [] -> currentExpr
                    | currentArgs :: rest -> applyAll (AST.Apply (currentExpr, currentArgs)) rest
                let fullApply = applyAll baseFunc allArgLists

                toAtom fullApply varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (resultAtom, bindings, vg) ->
                    (wrapBindings bindings (ANF.Return resultAtom), vg))

        | AST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) → let x = v in Apply(body, args)
            // Float the let binding out
            toANF (AST.Let (letName, letValue, AST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Closure (funcName, captures) ->
            // Closure being called directly - convert to ClosureCall
            // First, convert captures to atoms
            let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | cap :: rest ->
                    toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                // Allocate closure
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                // Convert args
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    // Generate closure call
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let allBindings = captureBindings @ [(closureId, closureAlloc)] @ argBindings @ [(resultId, closureCall)]
                    Ok (wrapBindings allBindings (ANF.Return (ANF.Var resultId)), varGen4)))

        | _ ->
            // Other complex function expressions not yet supported
            Error $"Complex function application not yet fully implemented: {func}"

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        Ok (ANF.UnitLiteral, [], varGen)

    | AST.IntLiteral n ->
        Ok (ANF.IntLiteral (ANF.Int64 n), [], varGen)

    | AST.Int8Literal n ->
        Ok (ANF.IntLiteral (ANF.Int8 n), [], varGen)

    | AST.Int16Literal n ->
        Ok (ANF.IntLiteral (ANF.Int16 n), [], varGen)

    | AST.Int32Literal n ->
        Ok (ANF.IntLiteral (ANF.Int32 n), [], varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt8 n), [], varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt16 n), [], varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt32 n), [], varGen)

    | AST.UInt64Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt64 n), [], varGen)

    | AST.BoolLiteral b ->
        Ok (ANF.BoolLiteral b, [], varGen)

    | AST.StringLiteral s ->
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.CharLiteral s ->
        // Char literal uses same representation as string
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.FloatLiteral f ->
        Ok (ANF.FloatLiteral f, [], varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some (tempId, _) -> Ok (ANF.Var tempId, [], varGen)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (_, _) ->
                // Module function reference - wrap in closure for uniform calling convention
                // Note: name should already be resolved by the type checker
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
                toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
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
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toAtom body varGen2 env' typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                    // All bindings: valueBindings + binding tempId to value + bodyBindings
                    let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
                    (bodyAtom, allBindings, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue), [], varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.FloatLiteral (-f), [], varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.UnaryOp (AST.BitNot, innerExpr) ->
        // Bitwise NOT: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.BitNot, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (rightAtom, rightBindings, varGen2) ->
                // Check if this is an equality comparison on compound types
                let typeEnv = typeEnvFromVarEnv env
                match op with
                | AST.Eq | AST.Neq ->
                    match inferType left typeEnv typeReg variantLookup funcReg moduleRegistry with
                    | Ok operandType when isCompoundType operandType ->
                        // Generate structural equality
                        let (eqBindings, eqResultAtom, varGen3) =
                            generateStructuralEquality leftAtom rightAtom operandType varGen2 typeReg variantLookup
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                            else
                                (eqResultAtom, eqBindings, varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | Ok AST.TString ->
                        // String equality - use StringEq
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.StringEq (leftAtom, rightAtom)
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                            else
                                (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | _ ->
                        // Primitive type - simple comparison
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen3)
                | AST.StringConcat ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.StringConcat (leftAtom, rightAtom)
                    let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen3)
                // Arithmetic, bitwise, and comparison operators - use simple primitive
                | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
                | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                | AST.And | AST.Or ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                    let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
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
                toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
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
                        // Check if it's a Float intrinsic
                        match tryFloatIntrinsic funcName argAtoms with
                        | Some intrinsicExpr ->
                            // Float intrinsic call
                            let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2)
                        | None ->
                            // Check if it's a constant-fold intrinsic (Platform, Path)
                            match tryConstantFoldIntrinsic funcName argAtoms with
                            | Some intrinsicExpr ->
                                // Constant-folded intrinsic
                                let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                Ok (ANF.Var tempVar, allBindings, varGen2)
                            | None ->
                                // Check if it's a random intrinsic
                                match tryRandomIntrinsic funcName argAtoms with
                                | Some intrinsicExpr ->
                                    // Random intrinsic call
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
                toAtom elem vg env typeReg variantLookup funcReg moduleRegistry
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
        toAtom tupleExpr varGen env typeReg variantLookup funcReg moduleRegistry
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
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.RecordUpdate (recordExpr, updates) ->
        // Desugar to RecordLiteral: build new record with updated fields
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | Some typeFields ->
                    let updateMap = Map.ofList updates
                    let newFields =
                        typeFields
                        |> List.map (fun (fname, _) ->
                            match Map.tryFind fname updateMap with
                            | Some updateExpr -> (fname, updateExpr)
                            | None -> (fname, AST.RecordAccess (recordExpr, fname)))
                    toAtom (AST.RecordLiteral (typeName, newFields)) varGen env typeReg variantLookup funcReg moduleRegistry
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error "Cannot use record update syntax on non-record type")

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
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
                Ok (ANF.IntLiteral (ANF.Int64 (int64 tag)), [], varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag, 0] for uniform 2-element structure
                // This enables consistent structural equality comparison
                let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                let (tempVar, varGen1) = ANF.freshVar varGen
                let tupleCExpr = ANF.TupleAlloc [tagAtom; dummyPayload]
                Ok (ANF.Var tempVar, [(tempVar, tupleCExpr)], varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as FingerTree in atom position
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), elemAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 5L))  // tag 5 = LEAF
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a SINGLE node containing a TreeNode
        let allocSingle (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 1L))  // tag 1 = SINGLE
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a DEEP node
        let allocDeep (measure: int) (prefixNodes: ANF.Atom list) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let prefixCount = List.length prefixNodes
            let suffixCount = List.length suffixNodes
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 96L))  // 12 fields * 8 bytes

            // Build all the set operations
            let setAt offset value vg bindings =
                let (setVar, vg') = ANF.freshVar vg
                let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value, None)
                (vg', bindings @ [(setVar, setExpr)])

            let (vg2, bindings2) = setAt 0 (ANF.IntLiteral (ANF.Int64 (int64 measure))) vg1 (bindings @ [(ptrVar, allocExpr)])
            let (vg3, bindings3) = setAt 8 (ANF.IntLiteral (ANF.Int64 (int64 prefixCount))) vg2 bindings2

            // Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
            let rec setPrefix nodes offset vg bindings =
                match nodes with
                | [] -> (vg, bindings)
                | n :: rest ->
                    let (vg', bindings') = setAt offset n vg bindings
                    setPrefix rest (offset + 8) vg' bindings'
            let (vg4, bindings4) = setPrefix prefixNodes 16 vg3 bindings3

            // Set middle = EMPTY (0) at offset 48
            let (vg5, bindings5) = setAt 48 (ANF.IntLiteral (ANF.Int64 0L)) vg4 bindings4

            // Set suffix count at offset 56
            let (vg6, bindings6) = setAt 56 (ANF.IntLiteral (ANF.Int64 (int64 suffixCount))) vg5 bindings5

            // Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
            let (vg7, bindings7) = setPrefix suffixNodes 64 vg6 bindings6

            // Tag with DEEP (2)
            let (taggedVar, vg8) = ANF.freshVar vg7
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 2L))
            (ANF.Var taggedVar, bindings7 @ [(taggedVar, tagExpr)], vg8)

        if List.isEmpty elements then
            // Empty list is EMPTY (represented as 0)
            Ok (ANF.IntLiteral (ANF.Int64 0L), [], varGen)
        else
            // Convert all elements to atoms first
            let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    toAtom e vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (atom, bindings, vg') ->
                        convertElements rest vg' ((atom, bindings) :: acc))

            convertElements elements varGen []
            |> Result.bind (fun (atomsWithBindings, varGen1) ->
                let count = List.length atomsWithBindings

                // Flatten all element bindings
                let elemBindings = atomsWithBindings |> List.collect snd
                let elemAtoms = atomsWithBindings |> List.map fst

                // Create LEAF nodes for all elements
                let rec createLeaves (atoms: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) (acc: ANF.Atom list) =
                    match atoms with
                    | [] -> (List.rev acc, bindings, vg)
                    | a :: rest ->
                        let (leafAtom, bindings', vg') = allocLeaf a vg bindings
                        createLeaves rest vg' bindings' (leafAtom :: acc)

                let (leafAtoms, leafBindings, varGen2) = createLeaves elemAtoms varGen1 elemBindings []

                if count = 1 then
                    // Single element: SINGLE(LEAF(elem))
                    let (resultAtom, resultBindings, varGen3) = allocSingle (List.head leafAtoms) varGen2 leafBindings
                    Ok (resultAtom, resultBindings, varGen3)
                else
                    // Multiple elements: DEEP with prefix and suffix
                    // Split into prefix (first element) and suffix (rest, up to 4)
                    let prefixNodes = [List.head leafAtoms]
                    let suffixNodes = List.tail leafAtoms |> List.truncate 4
                    // For more than 5 elements, we'd need the middle spine, but for now support up to 5
                    let (resultAtom, resultBindings, varGen3) = allocDeep count prefixNodes suffixNodes varGen2 leafBindings
                    Ok (resultAtom, resultBindings, varGen3))

    | AST.ListCons (headElements, tail) ->
        // Compile list cons in atom position: [a, b, ...tail] prepends elements to tail
        // Use Stdlib.FingerTree.push to prepend each element
        toAtom tail varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            // Build list by prepending elements from right to left
            // [a, b, ...tail] means push(push(tail, b), a)
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentList: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentList, allBindings, vg)
                | elem :: rest ->
                    // First build the rest of the list, then prepend this element
                    buildList rest vg currentList allBindings
                    |> Result.bind (fun (restList, restBindings, vg1) ->
                        toAtom elem vg1 env typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (pushVar, vg3) = ANF.freshVar vg2
                            // Call Stdlib.FingerTree.push to prepend element
                            let pushExpr = ANF.Call ("Stdlib.FingerTree.push_i64", [restList; elemAtom])
                            let newBindings = restBindings @ elemBindings @ [(pushVar, pushExpr)]
                            (ANF.Var pushVar, newBindings, vg3)))

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
            toAtom (partToExpr single) varGen env typeReg variantLookup funcReg moduleRegistry
        | first :: rest ->
            // Multiple parts → desugar to StringConcat and convert
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toAtom desugared varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANF (AST.Match (scrutinee, cases)) varGen env typeReg variantLookup funcReg moduleRegistry
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
                toAtom desugared varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Apply (innerFunc, innerArgs) ->
            // Nested application in atom position: ((x) => (y) => ...)(a)(b)
            match innerFunc with
            | AST.Lambda (innerParams, innerBody) ->
                if List.length innerArgs <> List.length innerParams then
                    Error $"Inner lambda expects {List.length innerParams} arguments, got {List.length innerArgs}"
                else
                    let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                        match ps, as' with
                        | [], [] -> innerBody
                        | (pName, _) :: restPs, argExpr :: restAs ->
                            AST.Let (pName, argExpr, buildLets restPs restAs)
                        | _ -> innerBody
                    let desugaredInner = buildLets innerParams innerArgs
                    toAtom (AST.Apply (desugaredInner, args)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                // Inner is complex - evaluate inner, then call as closure
                toAtom (AST.Apply (innerFunc, innerArgs)) varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (closureAtom, closureBindings, varGen1) ->
                    let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                        match remaining with
                        | [] -> Ok (List.rev acc, vg)
                        | arg :: rest ->
                            toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                            |> Result.bind (fun (argAtom, argBindings, vg') ->
                                convertArgs rest vg' ((argAtom, argBindings) :: acc))
                    convertArgs args varGen1 []
                    |> Result.bind (fun (argResults, varGen2) ->
                        let argAtoms = argResults |> List.map fst
                        let argBindings = argResults |> List.collect snd
                        let (resultId, varGen3) = ANF.freshVar varGen2
                        let closureCall = ANF.ClosureCall (closureAtom, argAtoms)
                        let allBindings = closureBindings @ argBindings @ [(resultId, closureCall)]
                        Ok (ANF.Var resultId, allBindings, varGen3)))

        | AST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) in atom position
            // Float the let out and recurse
            toAtom (AST.Let (letName, letValue, AST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Var name ->
            // Variable call in atom position - treat as closure call
            match Map.tryFind name env with
            | Some (tempId, _) ->
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, finalBindings, varGen2))
            | None ->
                Error $"Cannot apply variable '{name}' as function in atom position - variable not in scope"

        | AST.Closure (funcName, captures) ->
            // Closure call in atom position
            let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | cap :: rest ->
                    toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let allBindings = captureBindings @ [(closureId, closureAlloc)] @ argBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, allBindings, varGen4)))

        | _ ->
            Error $"Complex function application in atom position not yet supported: {func}"

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
/// VarGen is passed in and out to maintain globally unique TempIds across functions
/// (needed for TypeMap which maps TempId -> Type across the whole program)
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Function * ANF.VarGen, string> =
    // Allocate TempIds for parameters, bundled with their types
    let (typedParams, varGen1) =
        funcDef.Params
        |> List.fold (fun (acc, vg) (_, typ) ->
            let (tempId, vg') = ANF.freshVar vg
            (acc @ [{ ANF.TypedParam.Id = tempId; Type = typ }], vg')) ([], varGen)

    // Build environment mapping param names to (TempId, Type)
    let paramEnv : VarEnv =
        List.zip funcDef.Params typedParams
        |> List.map (fun ((name, _), typedParam) -> (name, (typedParam.Id, typedParam.Type)))
        |> Map.ofList

    // Convert body
    toANF funcDef.Body varGen1 paramEnv typeReg variantLookup funcReg moduleRegistry
    |> Result.map (fun (body, varGen2) ->
        ({ Name = funcDef.Name; TypedParams = typedParams; ReturnType = funcDef.ReturnType; Body = body }, varGen2))

/// Result type that includes registries needed for later passes
type ConversionResult = {
    Program: ANF.Program
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> param list with types
    ModuleRegistry: AST.ModuleRegistry
}

/// Result type for user-only ANF conversion (functions not merged with stdlib)
/// Used for Phase 4 optimization: cache stdlib MIR separately
type UserOnlyResult = {
    UserFunctions: ANF.Function list   // Only user functions, not merged with stdlib
    MainExpr: ANF.AExpr                // User's main expression
    TypeReg: TypeRegistry              // Merged registries (for lookups)
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    ModuleRegistry: AST.ModuleRegistry
    SpecializedFuncNames: Set<string>  // Names of specialized stdlib functions from monomorphization
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

    // Build alias registry from type alias definitions
    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, [], targetType)) -> Some (name, targetType)
            | _ -> None)
        |> Map.ofList

    // Expand typeReg with alias entries so "Vec" can be looked up when it aliases "Point"
    let typeReg = expandTypeRegWithAliases typeReg aliasReg

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

    // Convert all functions, passing VarGen between them for globally unique TempIds
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup funcReg moduleRegistry
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
            toANF expr varGen1 emptyEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.map (fun (anfExpr, _) ->
                { Program = ANF.Program (anfFuncs, anfExpr)
                  TypeReg = typeReg
                  VariantLookup = variantLookup
                  FuncReg = funcReg
                  FuncParams = funcParams
                  ModuleRegistry = moduleRegistry })
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))

/// Merge two maps, with overlay taking precedence on conflicts
let private mergeMaps m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2

/// Convert user program to ANF and concatenate with pre-compiled stdlib ANF.
/// This avoids re-processing stdlib functions during user compilation.
/// stdlibGenericDefs: Generic function definitions from stdlib needed for specialization
/// when user code calls generic stdlib functions like Stdlib.List.length<Int64>.
let convertUserWithStdlib
    (stdlibGenericDefs: GenericFuncDefs)
    (stdlibResult: ConversionResult)
    (userProgram: AST.Program) : Result<ConversionResult, string> =
    // 1. Run transformations on user code only (with access to stdlib generics)
    let monomorphizedProgram = monomorphizeWithExternalDefs stdlibGenericDefs userProgram
    let inlinedProgram = inlineLambdasInProgram monomorphizedProgram
    liftLambdasInProgram inlinedProgram
    |> Result.bind (fun liftedProgram ->
    let (AST.Program topLevels) = liftedProgram
    let varGen = ANF.VarGen 0

    // 2. Build registries from user code only
    let userTypeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    let userVariantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Build alias registry from type alias definitions
    let userAliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, [], targetType)) -> Some (name, targetType)
            | _ -> None)
        |> Map.ofList

    // Expand userTypeReg with alias entries so "Vec" can be looked up when it aliases "Point"
    let userTypeReg = expandTypeRegWithAliases userTypeRegBase userAliasReg

    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    let userFuncReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f -> (f.Name, f.Params))
        |> Map.ofList

    // 3. Merge with stdlib registries (stdlib first, user overlays)
    let mergedTypeReg = mergeMaps stdlibResult.TypeReg userTypeReg
    let mergedVariantLookup = mergeMaps stdlibResult.VariantLookup userVariantLookup
    let mergedFuncReg = mergeMaps stdlibResult.FuncReg userFuncReg

    // Get module function parameters from cached stdlib result
    let moduleRegistry = stdlibResult.ModuleRegistry
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let mergedFuncParams = mergeMaps stdlibResult.FuncParams (mergeMaps userFuncParams moduleFuncParams)

    // 4. Convert user functions with merged registries for lookups
    // Pass VarGen between functions for globally unique TempIds
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    convertFunctions functions varGen []
    |> Result.bind (fun (userAnfFuncs, varGen1) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
            |> Result.map (fun (anfExpr, _) ->
                // 5. Concatenate stdlib ANF functions with user ANF functions
                let (ANF.Program (stdlibFuncs, _stdlibMain)) = stdlibResult.Program
                { Program = ANF.Program (stdlibFuncs @ userAnfFuncs, anfExpr)
                  TypeReg = mergedTypeReg
                  VariantLookup = mergedVariantLookup
                  FuncReg = mergedFuncReg
                  FuncParams = mergedFuncParams
                  ModuleRegistry = moduleRegistry })
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))

/// Convert user program to ANF WITHOUT merging with stdlib functions.
/// Returns user functions separately for MIR-level caching optimization.
/// The registries are still merged to allow proper lookups during conversion.
let convertUserOnly
    (stdlibGenericDefs: GenericFuncDefs)
    (stdlibResult: ConversionResult)
    (userProgram: AST.Program) : Result<UserOnlyResult, string> =
    // 1. Run transformations on user code only (with access to stdlib generics)
    let monomorphizedProgram = monomorphizeWithExternalDefs stdlibGenericDefs userProgram
    let inlinedProgram = inlineLambdasInProgram monomorphizedProgram
    liftLambdasInProgram inlinedProgram
    |> Result.bind (fun liftedProgram ->
    let (AST.Program topLevels) = liftedProgram
    let varGen = ANF.VarGen 0

    // 2. Build registries from user code only
    let userTypeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    let userVariantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Build alias registry from type alias definitions
    let userAliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, [], targetType)) -> Some (name, targetType)
            | _ -> None)
        |> Map.ofList

    // Expand userTypeReg with alias entries so "Vec" can be looked up when it aliases "Point"
    let userTypeReg = expandTypeRegWithAliases userTypeRegBase userAliasReg

    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    let userFuncReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f -> (f.Name, f.Params))
        |> Map.ofList

    // 3. Merge with stdlib registries (stdlib first, user overlays)
    let mergedTypeReg = mergeMaps stdlibResult.TypeReg userTypeReg
    let mergedVariantLookup = mergeMaps stdlibResult.VariantLookup userVariantLookup
    let mergedFuncReg = mergeMaps stdlibResult.FuncReg userFuncReg

    // Get module function parameters from cached stdlib result
    let moduleRegistry = stdlibResult.ModuleRegistry
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let mergedFuncParams = mergeMaps stdlibResult.FuncParams (mergeMaps userFuncParams moduleFuncParams)

    // 4. Convert user functions with merged registries for lookups
    // Pass VarGen between functions for globally unique TempIds
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    convertFunctions functions varGen []
    |> Result.bind (fun (userAnfFuncs, varGen1) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
            |> Result.map (fun (anfExpr, _) ->
                // Return user functions ONLY (not merged with stdlib)
                { UserFunctions = userAnfFuncs
                  MainExpr = anfExpr
                  TypeReg = mergedTypeReg
                  VariantLookup = mergedVariantLookup
                  FuncReg = mergedFuncReg
                  FuncParams = mergedFuncParams
                  ModuleRegistry = moduleRegistry
                  SpecializedFuncNames = Set.empty })  // Non-cached path has no specialization tracking
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))

/// Convert user program to ANF with specialization caching.
/// Uses the provided cache to avoid re-monomorphizing the same generic functions.
/// Also caches converted ANF functions to avoid re-converting the same function across tests.
let convertUserOnlyCached
    (cache: SpecializationCache)
    (anfFuncCache: ANFFunctionCache)
    (sourceFile: string)
    (funcLineMap: Map<string, int>)
    (stdlibGenericDefs: GenericFuncDefs)
    (stdlibResult: ConversionResult)
    (userProgram: AST.Program) : Result<UserOnlyResult, string> =
    // 1. Run transformations on user code only (with access to stdlib generics)
    // Use cached monomorphization to avoid re-specializing the same functions
    let (monomorphizedProgram, specializedFuncNames) = monomorphizeWithExternalDefsCached cache stdlibGenericDefs userProgram
    let inlinedProgram = inlineLambdasInProgram monomorphizedProgram
    liftLambdasInProgram inlinedProgram
    |> Result.bind (fun liftedProgram ->
    let (AST.Program topLevels) = liftedProgram
    let varGen = ANF.VarGen 0

    // 2. Build registries from user code only
    let userTypeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    let userVariantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Build alias registry from type alias definitions
    let userAliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, [], targetType)) -> Some (name, targetType)
            | _ -> None)
        |> Map.ofList

    // Expand userTypeReg with alias entries so "Vec" can be looked up when it aliases "Point"
    let userTypeReg = expandTypeRegWithAliases userTypeRegBase userAliasReg

    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    let userFuncReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f -> (f.Name, f.Params))
        |> Map.ofList

    // 3. Merge with stdlib registries (stdlib first, user overlays)
    let mergedTypeReg = mergeMaps stdlibResult.TypeReg userTypeReg
    let mergedVariantLookup = mergeMaps stdlibResult.VariantLookup userVariantLookup
    let mergedFuncReg = mergeMaps stdlibResult.FuncReg userFuncReg

    // Get module function parameters from cached stdlib result
    let moduleRegistry = stdlibResult.ModuleRegistry
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let mergedFuncParams = mergeMaps stdlibResult.FuncParams (mergeMaps userFuncParams moduleFuncParams)

    // 4. Convert user functions with merged registries for lookups
    // Uses cache to avoid re-converting the same function across tests
    // Only cache non-specialized functions (no __ in name) since specialized functions
    // are generated during monomorphization and may have context-dependent behavior
    // Pass VarGen between functions for globally unique TempIds
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            // Only cache non-specialized functions (those without __ in name)
            let isSpecialized = func.Name.Contains("__")
            let lineNum = Map.tryFind func.Name funcLineMap |> Option.defaultValue 0
            let cacheKey = (sourceFile, lineNum, func.Name)
            // Only use cache for non-specialized functions with valid line numbers
            let canCache = not isSpecialized && lineNum > 0 && sourceFile <> ""
            match canCache, anfFuncCache.TryGetValue(cacheKey) with
            | true, (true, (cachedFunc, cachedEndVg)) ->
                // Cache hit - reuse cached ANF function
                // Use max of current VarGen and cached ending VarGen to avoid TempId collisions
                let (ANF.VarGen currentId) = vg
                let (ANF.VarGen cachedId) = cachedEndVg
                let newVg = ANF.VarGen (max currentId cachedId)
                convertFunctions rest newVg (cachedFunc :: acc)
            | _ ->
                // Cache miss or not cacheable - compile (and maybe cache)
                convertFunction func vg mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
                |> Result.bind (fun (anfFunc, vg') ->
                    if canCache then
                        anfFuncCache.TryAdd(cacheKey, (anfFunc, vg')) |> ignore
                    convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    convertFunctions functions varGen []
    |> Result.bind (fun (userAnfFuncs, varGen1) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv mergedTypeReg mergedVariantLookup mergedFuncReg moduleRegistry
            |> Result.map (fun (anfExpr, _) ->
                // Return user functions ONLY (not merged with stdlib)
                { UserFunctions = userAnfFuncs
                  MainExpr = anfExpr
                  TypeReg = mergedTypeReg
                  VariantLookup = mergedVariantLookup
                  FuncReg = mergedFuncReg
                  FuncParams = mergedFuncParams
                  ModuleRegistry = moduleRegistry
                  SpecializedFuncNames = specializedFuncNames })
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))

/// Convert a program to ANF
let convertProgram (program: AST.Program) : Result<ANF.Program, string> =
    // Build module registry once for the entire conversion
    let moduleRegistry = Stdlib.buildModuleRegistry ()

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

    // Build alias registry from type alias definitions
    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, [], targetType)) -> Some (name, targetType)
            | _ -> None)
        |> Map.ofList

    // Expand typeReg with alias entries so "Vec" can be looked up when it aliases "Point"
    let typeReg = expandTypeRegWithAliases typeReg aliasReg

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

    // Convert all functions - VarGen is passed between functions for globally unique TempIds
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    // Validate no function is named "main" (reserved for synthesized entry point)
    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    else

    // Stdlib functions are now loaded from stdlib.dark and included as regular functions
    convertFunctions functions ANF.initialVarGen []
    |> Result.bind (fun (anfFuncs, vg) ->
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            // Main expression continues with VarGen from functions for globally unique TempIds
            toANF expr vg emptyEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.map (fun (anfExpr, _) ->
                ANF.Program (anfFuncs, anfExpr))
        | [] ->
            Error "Program must have a main expression"
        | _ ->
            Error "Multiple top-level expressions not allowed"))
