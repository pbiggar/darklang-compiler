// Stdlib.fs - Standard Library Module Definitions
//
// Defines the built-in Stdlib modules that are available to all programs.
// These modules provide primitive operations wrapped as functions.

module Stdlib

open AST

/// Stdlib.Int64 module - integer operations
let int64Module : ModuleDef = {
    Name = "Stdlib.Int64"
    Functions = [
        { Name = "add"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "sub"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "mul"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "div"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "max"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "min"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "toFloat"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TFloat64 }
    ]
}

/// Stdlib.Float module - floating-point operations
let floatModule : ModuleDef = {
    Name = "Stdlib.Float"
    Functions = [
        { Name = "sqrt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "abs"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "negate"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "toInt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TInt64 }
    ]
}

/// Helper to create Result<T, String> type
let resultType (okType: Type) : Type =
    TSum ("Stdlib.Result.Result", [okType; TString])

/// Stdlib.String module - string operations
let stringModule : ModuleDef = {
    Name = "Stdlib.String"
    Functions = [
        { Name = "length"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt64 }
    ]
}

/// Stdlib.File module - file I/O operations (intrinsics)
/// These are special-cased in the compiler and generate syscalls
let fileModule : ModuleDef = {
    Name = "Stdlib.File"
    Functions = [
        // readText : (String) -> Result<String, String>
        { Name = "readText"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TString }
        // exists : (String) -> Bool
        { Name = "exists"; TypeParams = []; ParamTypes = [TString]; ReturnType = TBool }
        // writeText : (String, String) -> Result<Unit, String>
        { Name = "writeText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // appendText : (String, String) -> Result<Unit, String>
        { Name = "appendText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
    ]
}

/// Raw memory intrinsics - internal only for HAMT implementation
/// These functions bypass the type system and should only be used in stdlib code
/// The names start with __ to indicate they are internal
let rawMemoryIntrinsics : ModuleFunc list = [
    // __raw_alloc : (Int64) -> RawPtr - allocate raw bytes
    { Name = "__raw_alloc"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TRawPtr }
    // __raw_free : (RawPtr) -> Unit - free raw memory
    { Name = "__raw_free"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TUnit }
    // __raw_get<v> : (RawPtr, Int64) -> v - read 8 bytes at offset, typed as v
    { Name = "__raw_get"; TypeParams = ["v"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TVar "v" }
    // __raw_set<v> : (RawPtr, Int64, v) -> Unit - write 8 bytes at offset
    { Name = "__raw_set"; TypeParams = ["v"]; ParamTypes = [TRawPtr; TInt64; TVar "v"]; ReturnType = TUnit }
    // __rawptr_to_int64 : (RawPtr) -> Int64 - cast pointer to int (for tagging)
    { Name = "__rawptr_to_int64"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TInt64 }
    // __int64_to_rawptr : (Int64) -> RawPtr - cast int to pointer (for memory ops)
    { Name = "__int64_to_rawptr"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TRawPtr }
    // __string_hash : (String) -> Int64 - FNV-1a hash of string contents
    { Name = "__string_hash"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt64 }
    // __string_eq : (String, String) -> Bool - byte-wise string equality
    { Name = "__string_eq"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = TBool }
    // __refcount_inc_string : (String) -> Unit - increment string refcount
    { Name = "__refcount_inc_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __refcount_dec_string : (String) -> Unit - decrement string refcount, free if 0
    { Name = "__refcount_dec_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __string_to_int64 : (String) -> Int64 - cast string pointer to int (for storage)
    { Name = "__string_to_int64"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt64 }
    // __int64_to_string : (Int64) -> String - cast int to string pointer (for retrieval)
    { Name = "__int64_to_string"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TString }
]

/// All available Stdlib modules
let allModules : ModuleDef list = [
    int64Module
    floatModule
    stringModule
    fileModule
]

/// Build the module registry from all modules
/// Maps qualified function names (e.g., "Stdlib.Int64.add") to their definitions
let buildModuleRegistry () : ModuleRegistry =
    let moduleFuncs =
        allModules
        |> List.collect (fun m ->
            m.Functions
            |> List.map (fun f -> ($"{m.Name}.{f.Name}", f)))
    // Add raw memory intrinsics directly (no module prefix)
    let rawMemFuncs =
        rawMemoryIntrinsics
        |> List.map (fun f -> (f.Name, f))
    (moduleFuncs @ rawMemFuncs)
    |> Map.ofList

/// Get a function from the registry by its full qualified name
let tryGetFunction (registry: ModuleRegistry) (qualifiedName: string) : ModuleFunc option =
    Map.tryFind qualifiedName registry

/// Get the type of a module function as an AST.Type
let getFunctionType (func: ModuleFunc) : Type =
    TFunction (func.ParamTypes, func.ReturnType)

/// Check if a function name is a file I/O intrinsic
/// These are special-cased in the compiler to generate syscalls
let isFileIntrinsic (qualifiedName: string) : bool =
    qualifiedName = "Stdlib.File.readText" ||
    qualifiedName = "Stdlib.File.exists" ||
    qualifiedName = "Stdlib.File.writeText" ||
    qualifiedName = "Stdlib.File.appendText"
