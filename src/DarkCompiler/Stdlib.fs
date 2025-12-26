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
    ]
}

/// All available Stdlib modules
let allModules : ModuleDef list = [
    int64Module
]

/// Build the module registry from all modules
/// Maps qualified function names (e.g., "Stdlib.Int64.add") to their definitions
let buildModuleRegistry () : ModuleRegistry =
    allModules
    |> List.collect (fun m ->
        m.Functions
        |> List.map (fun f -> ($"{m.Name}.{f.Name}", f)))
    |> Map.ofList

/// Get a function from the registry by its full qualified name
let tryGetFunction (registry: ModuleRegistry) (qualifiedName: string) : ModuleFunc option =
    Map.tryFind qualifiedName registry

/// Get the type of a module function as an AST.Type
let getFunctionType (func: ModuleFunc) : Type =
    TFunction (func.ParamTypes, func.ReturnType)
