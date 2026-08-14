// Stdlib.fs - Standard Library Module Definitions
//
// Defines intrinsic Stdlib module signatures used directly by the compiler.
// Non-intrinsic stdlib functions are loaded from stdlib/*.dark.

module Stdlib

open AST

/// Intrinsic Stdlib.Int64 functions
let int64IntrinsicModule : ModuleDef = {
    Name = "Stdlib.Int64"
    Functions = [
        // toFloat : (Int64) -> Float
        { Name = "toFloat"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TFloat64 }
    ]
}

/// Intrinsic Stdlib.Float functions
let floatIntrinsicModule : ModuleDef = {
    Name = "Stdlib.Float"
    Functions = [
        // sqrt : (Float) -> Float
        { Name = "sqrt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // abs : (Float) -> Float
        { Name = "abs"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // negate : (Float) -> Float
        { Name = "negate"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // toInt : (Float) -> Int64
        { Name = "toInt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TInt64 }
        // toBits : (Float) -> UInt64
        { Name = "toBits"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TUInt64 }
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
        { Name = "readText"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TString }
        // exists : (String) -> Bool
        { Name = "exists"; TypeParams = []; ParamTypes = [TString]; ReturnType = TBool }
        // writeText : (String, String) -> Result<Unit, String>
        { Name = "writeText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // appendText : (String, String) -> Result<Unit, String>
        { Name = "appendText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // delete : (String) -> Result<Unit, String>
        { Name = "delete"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TUnit }
        // setExecutable : (String) -> Result<Unit, String>
        { Name = "setExecutable"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TUnit }
        // writeFromPtr : (String, RawPtr, Int64) -> Bool - write raw bytes to file
        { Name = "writeFromPtr"; TypeParams = []; ParamTypes = [TString; TRawPtr; TInt64]; ReturnType = TBool }
    ]
}

/// Stdlib.Path module - path operations
/// combine is defined in stdlib/Path.dark, tempDir is constant-folded at compile time
let pathModule : ModuleDef = {
    Name = "Stdlib.Path"
    Functions = [
        // tempDir : () -> String - returns system temp directory
        { Name = "tempDir"; TypeParams = []; ParamTypes = []; ReturnType = TString }
        // combine is defined in stdlib/Path.dark
    ]
}

/// Internal native operations supporting the public Stdlib.Cli modules.
/// Portable policy stays in Dark; these typed effects are lowered by the compiler.
let cliIntrinsicModule : ModuleDef = {
    Name = "Stdlib.Cli"
    Functions = [
        { Name = "__execute"; TypeParams = []; ParamTypes = [TString]; ReturnType = TRecord ("Stdlib.Cli.NativeOutput", []) }
        { Name = "__hostOSCode"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
        { Name = "__getenv"; TypeParams = []; ParamTypes = [TString]; ReturnType = TSum ("Stdlib.Option.Option", [TString]) }
        { Name = "__kill"; TypeParams = []; ParamTypes = [TInt64; TInt64]; ReturnType = TSum ("Stdlib.Result.Result", [TUnit; TRecord ("Stdlib.Cli.NativePosixError", [])]) }
        { Name = "__sleep"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TUnit }
        { Name = "__getpid"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
        { Name = "__getuid"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
        { Name = "__cpuCount"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
        { Name = "__currentUser"; TypeParams = []; ParamTypes = []; ReturnType = TSum ("Stdlib.Option.Option", [TString]) }
        { Name = "__spawnProcess"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt64 }
        { Name = "__processIO"; TypeParams = []; ParamTypes = [TInt64; TString]; ReturnType = TRecord ("Stdlib.Cli.NativeOutput", []) }
        { Name = "__terminateProcess"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TRecord ("Stdlib.Cli.NativeOutput", []) }
    ]
}

/// Stdlib.Random module - random number generation (intrinsics)
/// These are special-cased in the compiler and generate syscalls
let randomModule : ModuleDef = {
    Name = "Stdlib.Random"
    Functions = [
        // int64 : () -> Int64 - returns 8 random bytes as Int64
        { Name = "int64"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
    ]
}

/// Internal typed operations used by the portable Stdlib.DateTime module.
let dateTimeModule : ModuleDef = {
    Name = "Stdlib.DateTime"
    Functions = [
        { Name = "__now"; TypeParams = []; ParamTypes = []; ReturnType = TDateTime }
        { Name = "__fromUnixTimeTicks"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TDateTime }
        { Name = "__toUnixTimeTicks"; TypeParams = []; ParamTypes = [TDateTime]; ReturnType = TInt64 }
    ]
}

/// Explicit CLI presentation primitives. These are compiler intrinsics rather
/// than host-library calls, so their effects are visible throughout the IR.
let builtinPresentationModule : ModuleDef = {
    Name = "Builtin"
    Functions = [
        { Name = "print"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
        { Name = "printLine"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
        { Name = "stdinReadLine"; TypeParams = []; ParamTypes = [TUnit]; ReturnType = TString }
    ]
}

/// Narrow package lookup surface consumed by ValueSearch. Compilation replaces
/// these signatures with catalog-backed Dark functions for each concrete AOT
/// specialization; there is no live package-manager service in native output.
let packageCatalogModule : ModuleDef = {
    Name = "Builtin"
    Functions = [
        { Name = "pmFindValuesByValueType"
          TypeParams = []
          ParamTypes = [TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])]
          ReturnType = TList (TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])) }
        { Name = "pmGetLocationsByValue"
          TypeParams = []
          ParamTypes = [TString; TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])]
          ReturnType = TList (TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])) }
        { Name = "pmEvaluateValue"
          TypeParams = ["a"]
          ParamTypes = [TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])]
          ReturnType = TSum ("Stdlib.Option.Option", [TVar "a"]) }
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
    // __raw_write_word : (RawPtr, Int64, Int64) -> Unit - write 8 unmanaged bytes at offset
    { Name = "__raw_write_word"; TypeParams = []; ParamTypes = [TRawPtr; TInt64; TInt64]; ReturnType = TUnit }
    // __raw_get_byte : (RawPtr, Int64) -> Int64 - read 1 byte at offset, zero-extended
    { Name = "__raw_get_byte"; TypeParams = []; ParamTypes = [TRawPtr; TInt64]; ReturnType = TInt64 }
    // __raw_write_byte : (RawPtr, Int64, Int64) -> Unit - write 1 unmanaged byte at offset
    { Name = "__raw_write_byte"; TypeParams = []; ParamTypes = [TRawPtr; TInt64; TInt64]; ReturnType = TUnit }
    // __raw_slot_init<v> : (RawPtr, Int64, v) -> Unit - initialize a typed slot edge
    { Name = "__raw_slot_init"; TypeParams = ["v"]; ParamTypes = [TRawPtr; TInt64; TVar "v"]; ReturnType = TUnit }
    // __refcount_inc_string : (String) -> Unit - increment string refcount
    { Name = "__refcount_inc_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __refcount_dec_string : (String) -> Unit - decrement string refcount, free if 0
    { Name = "__refcount_dec_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __string_to_rawptr : (String) -> RawPtr - borrow string backing pointer
    { Name = "__string_to_rawptr"; TypeParams = []; ParamTypes = [TString]; ReturnType = TRawPtr }
    // __rawptr_to_string : (RawPtr) -> String - reinterpret initialized raw allocation as String
    { Name = "__rawptr_to_string"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TString }
    // Int uses the same canonical decimal dynamic-buffer representation as String.
    { Name = "__int_to_string"; TypeParams = []; ParamTypes = [TInt]; ReturnType = TString }
    { Name = "__string_to_int"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt }
    { Name = "__int128_to_int"; TypeParams = []; ParamTypes = [TInt128]; ReturnType = TInt }
    { Name = "__uint128_to_int"; TypeParams = []; ParamTypes = [TUInt128]; ReturnType = TInt }
    { Name = "__int_to_int128"; TypeParams = []; ParamTypes = [TInt]; ReturnType = TInt128 }
    { Name = "__int_to_uint128"; TypeParams = []; ParamTypes = [TInt]; ReturnType = TUInt128 }
    { Name = "__int64_to_int8"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TInt8 }
    { Name = "__int64_to_int16"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TInt16 }
    { Name = "__int64_to_int32"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TInt32 }
    { Name = "__int64_to_uint8"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TUInt8 }
    { Name = "__int64_to_uint16"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TUInt16 }
    { Name = "__int64_to_uint32"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TUInt32 }

    // Blob intrinsics - for byte array operations
    // __blob_to_rawptr : (Blob) -> RawPtr - borrow bytes backing pointer
    { Name = "__blob_to_rawptr"; TypeParams = []; ParamTypes = [TBlob]; ReturnType = TRawPtr }
    // __rawptr_to_blob : (RawPtr) -> Blob - reinterpret initialized raw allocation as Blob
    { Name = "__rawptr_to_blob"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TBlob }

    // Dict intrinsics - for type-safe Dict<k, v> operations
    // __empty_dict<k, v> : () -> Dict<k, v> - create empty dict (null pointer)
    { Name = "__empty_dict"; TypeParams = ["k"; "v"]; ParamTypes = []; ReturnType = TDict(TVar "k", TVar "v") }
    // __dict_is_null<k, v> : (Dict<k, v>) -> Bool - check if dict is empty/null
    { Name = "__dict_is_null"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TBool }
    // __dict_get_tag<k, v> : (Dict<k, v>) -> Int64 - get tag bits from dict pointer
    { Name = "__dict_get_tag"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TInt64 }
    // __dict_to_rawptr<k, v> : (Dict<k, v>) -> RawPtr - convert dict to raw pointer (strips tag)
    { Name = "__dict_to_rawptr"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TRawPtr }
    // __rawptr_to_dict<k, v> : (RawPtr, Int64) -> Dict<k, v> - create dict from pointer + tag
    { Name = "__rawptr_to_dict"; TypeParams = ["k"; "v"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TDict(TVar "k", TVar "v") }

    // Key intrinsics - for generic key hashing and comparison
    // __hash<k> : (k) -> Int64 - hash any key type
    { Name = "__hash"; TypeParams = ["k"]; ParamTypes = [TVar "k"]; ReturnType = TInt64 }
    // __key_eq<k> : (k, k) -> Bool - compare two keys for equality
    { Name = "__key_eq"; TypeParams = ["k"]; ParamTypes = [TVar "k"; TVar "k"]; ReturnType = TBool }
    // __compare<a> is an AOT-only dispatch marker. Type checking replaces every
    // concrete use with a synthesized canonical three-way comparison helper.
    { Name = "__compare"; TypeParams = ["a"]; ParamTypes = [TVar "a"; TVar "a"]; ReturnType = TInt64 }

    // List intrinsics for the direct-payload skew RAL implementation.
    // __list_empty<a> : () -> List<a> - create empty list (null pointer with tag 0)
    { Name = "__list_empty"; TypeParams = ["a"]; ParamTypes = []; ReturnType = TList(TVar "a") }
    // __list_is_null<a> : (List<a>) -> Bool - check if list is empty/null
    { Name = "__list_is_null"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TBool }
    // __list_get_tag<a> : (List<a>) -> Int64 - get tag bits from list pointer (low 3 bits)
    { Name = "__list_get_tag"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TInt64 }
    // __list_to_rawptr<a> : (List<a>) -> RawPtr - convert list to raw pointer (strips tag)
    { Name = "__list_to_rawptr"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TRawPtr }
    // __rawptr_to_list<a> : (RawPtr, Int64) -> List<a> - create list from pointer + tag
    { Name = "__rawptr_to_list"; TypeParams = ["a"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TList(TVar "a") }
]

/// All intrinsic Stdlib modules
let allModules : ModuleDef list = [
    int64IntrinsicModule
    floatIntrinsicModule
    fileModule
    pathModule
    cliIntrinsicModule
    randomModule
    dateTimeModule
    builtinPresentationModule
    packageCatalogModule
]

/// Compiler-visible values are registered separately from functions so a value
/// cannot accidentally acquire nullary-call semantics.
let allValues : ModuleValue list = [
    { Name = "Stdlib.Blob.empty"; Type = TBlob }
    { Name = "Darklang.LanguageTools.PackageManager.PickContext.empty"
      Type = TRecord ("Darklang.LanguageTools.PackageManager.PickContext", []) }
    { Name = "Stdlib.List.empty"; Type = TList(TVar "a") }
    { Name = "Stdlib.List.empty_v0"; Type = TList(TVar "a") }
]

let private valueRegistry : ModuleValueRegistry =
    allValues |> List.map (fun value -> (value.Name, value)) |> Map.ofList

let tryGetValue (qualifiedName: string) : ModuleValue option =
    Map.tryFind qualifiedName valueRegistry

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

/// Get a function by the exact identity attached during type checking.
let tryGetFunction (registry: ModuleRegistry) (qualifiedName: string) : (ModuleFunc * string) option =
    Map.tryFind qualifiedName registry |> Option.map (fun func -> (func, qualifiedName))

/// Get the type of a module function as an AST.Type
let getFunctionType (func: ModuleFunc) : Type =
    TFunction (func.ParamTypes, func.ReturnType)
