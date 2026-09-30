// FunctionTreeShaking.fs - Function Tree Shaking Pass
//
// Prunes unused user and stdlib functions by walking call graphs.
// This keeps code generation and binary size focused on reachable functions.

module FunctionTreeShaking

/// Build root set for user reachability (explicit entry when provided)
let private buildUserRoots (entryName: string option) (functions: LIR.Function list) : Set<AST.FunctionId> =
    match entryName with
    | Some name ->
        functions
        |> List.tryFind (fun f -> f.Name = name)
        |> Option.map (fun functionDef -> Set.singleton functionDef.Id)
        |> Option.defaultWith (fun () ->
            Crash.crash $"FunctionTreeShaking: entry '{name}' not found in user functions")
    | None ->
        functions |> List.map (fun f -> f.Id) |> Set.ofList

/// Filter user functions to only include reachable ones
let filterUserFunctionsWithCallGraph
    (entryName: string option)
    (callGraph: FunctionIdMap<Set<AST.FunctionId>>)
    (functions: LIR.Function list)
    : LIR.Function list =
    let roots = buildUserRoots entryName functions
    let reachableNames = DeadCodeElimination.findReachable callGraph roots
    functions |> List.filter (fun f -> Set.contains f.Id reachableNames)

/// Filter user functions to only include reachable ones
let filterUserFunctions (entryName: string option) (functions: LIR.Function list) : LIR.Function list =
    let ids = functions |> List.map (fun func -> func.Name, func.Id) |> Map.ofList
    let callGraph = DeadCodeElimination.buildCallGraph ids functions
    filterUserFunctionsWithCallGraph entryName callGraph functions

/// Filter stdlib functions using a precomputed user call graph.
let filterStdlibFunctionsWithUserCallGraph
    (stdlibCallGraph: FunctionIdMap<Set<AST.FunctionId>>)
    (userCallGraph: FunctionIdMap<Set<AST.FunctionId>>)
    (userFunctions: LIR.Function list)
    (stdlibFunctions: LIR.Function list)
    : LIR.Function list =
    DeadCodeElimination.filterFunctionsWithUserCallGraph
        stdlibCallGraph
        userCallGraph
        userFunctions
        stdlibFunctions

/// Filter stdlib functions to only include those reachable from user code
let filterStdlibFunctions
    (stdlibCallGraph: FunctionIdMap<Set<AST.FunctionId>>)
    (userFunctions: LIR.Function list)
    (stdlibFunctions: LIR.Function list)
    : LIR.Function list =
    let ids =
        userFunctions @ stdlibFunctions
        |> List.map (fun func -> func.Name, func.Id)
        |> Map.ofList
    DeadCodeElimination.filterFunctions stdlibCallGraph ids userFunctions stdlibFunctions

/// Compute reachable stdlib function names from a user ANF program
let getReachableStdlibNames
    (stdlibCallGraph: FunctionIdMap<Set<AST.FunctionId>>)
    (userProgram: ANF.Program)
    : Set<AST.FunctionId> =
    let (ANF.Program (userFuncs, userMainExpr)) = userProgram
    let startId =
        AST.allocateFunctionIds
            (seq {
                yield! userFuncs |> Seq.map (fun func -> func.Id)
                yield! stdlibCallGraph |> FunctionIdMap.keys
            })
            ["__dark_tree_shaking_start"]
        |> Map.tryFind "__dark_tree_shaking_start"
        |> Option.defaultWith (fun () ->
            Crash.crash "Tree-shaking start identity was not allocated")
    let startFunc : ANF.Function =
        { Id = startId
          Name = "_start"
          TypedParams = []
          ReturnType = AST.TUnit
          ReturnOwnership = ANF.OwnedReturn
          Body = userMainExpr }
    let userFuncsWithStart = startFunc :: userFuncs
    ANFDeadCodeElimination.getReachableStdlib stdlibCallGraph userFuncsWithStart
