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
        { Name = "add"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "sub"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "mul"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "div"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "max"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "min"; ParamTypes = [TInt64; TInt64]; ReturnType = TInt64 }
        { Name = "toFloat"; ParamTypes = [TInt64]; ReturnType = TFloat64 }
    ]
}

/// Stdlib.Float module - floating-point operations
let floatModule : ModuleDef = {
    Name = "Stdlib.Float"
    Functions = [
        { Name = "sqrt"; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "abs"; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "negate"; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        { Name = "toInt"; ParamTypes = [TFloat64]; ReturnType = TInt64 }
    ]
}

/// Helper to create Result<T, String> type
let resultType (okType: Type) : Type =
    TSum ("Stdlib.Result.Result", [okType; TString])

/// Stdlib.File module - file I/O operations (intrinsics)
/// These are special-cased in the compiler and generate syscalls
let fileModule : ModuleDef = {
    Name = "Stdlib.File"
    Functions = [
        // readText : (String) -> Result<String, String>
        { Name = "readText"; ParamTypes = [TString]; ReturnType = resultType TString }
        // exists : (String) -> Bool
        { Name = "exists"; ParamTypes = [TString]; ReturnType = TBool }
        // writeText : (String, String) -> Result<Unit, String>
        { Name = "writeText"; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // appendText : (String, String) -> Result<Unit, String>
        { Name = "appendText"; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
    ]
}

/// Raw memory intrinsics - internal only for HAMT implementation
/// These functions bypass the type system and should only be used in stdlib code
/// The names start with __ to indicate they are internal
let rawMemoryIntrinsics : ModuleFunc list = [
    // __raw_alloc : (Int64) -> RawPtr - allocate raw bytes
    { Name = "__raw_alloc"; ParamTypes = [TInt64]; ReturnType = TRawPtr }
    // __raw_free : (RawPtr) -> Unit - free raw memory
    { Name = "__raw_free"; ParamTypes = [TRawPtr]; ReturnType = TUnit }
    // __raw_get : (RawPtr, Int64) -> Int64 - read 8 bytes at offset
    { Name = "__raw_get"; ParamTypes = [TRawPtr; TInt64]; ReturnType = TInt64 }
    // __raw_set : (RawPtr, Int64, Int64) -> Unit - write 8 bytes at offset
    { Name = "__raw_set"; ParamTypes = [TRawPtr; TInt64; TInt64]; ReturnType = TUnit }
    // __rawptr_to_int64 : (RawPtr) -> Int64 - cast pointer to int (for tagging)
    { Name = "__rawptr_to_int64"; ParamTypes = [TRawPtr]; ReturnType = TInt64 }
    // __int64_to_rawptr : (Int64) -> RawPtr - cast int to pointer (for memory ops)
    { Name = "__int64_to_rawptr"; ParamTypes = [TInt64]; ReturnType = TRawPtr }
]

/// All available Stdlib modules
let allModules : ModuleDef list = [
    int64Module
    floatModule
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
