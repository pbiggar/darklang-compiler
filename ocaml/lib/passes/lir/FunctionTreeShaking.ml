(*
   Prunes unused user and stdlib functions by walking call graphs.
   This keeps code generation and binary size focused on reachable functions.
*)
(* FunctionTreeShaking.fs - Function Tree Shaking Pass. *)
module FS = SpecializationIdentity.FunctionSet
(*
   Build root set for user reachability (explicit entry when provided)
*)
let buildUserRoots entryName functions = match entryName with
 | Some name -> (match List.find_opt (fun (func : LIR.functionDef) -> func.LIR.name = name) functions with Some func -> FS.singleton func.LIR.id | None -> Crash.crash ("FunctionTreeShaking: entry '" ^ name ^ "' not found in user functions"))
 | None -> FS.of_list (List.map (fun (func : LIR.functionDef) -> func.LIR.id) functions)
(*
   Filter user functions to only include reachable ones
*)
let filterUserFunctionsWithCallGraph entryName callGraph functions =
 let roots = buildUserRoots entryName functions in
 let reachableNames = DeadCodeElimination.findReachable callGraph roots in
 List.filter (fun (func : LIR.functionDef) -> FS.mem func.LIR.id reachableNames) functions
(*
   Filter user functions to only include reachable ones
*)
let filterUserFunctions entryName functions =
 let ids = StringOrder.Map.of_list (List.map (fun (func : LIR.functionDef) -> func.LIR.name,func.LIR.id) functions) in
 filterUserFunctionsWithCallGraph entryName (DeadCodeElimination.buildCallGraph ids functions) functions
(*
   Filter stdlib functions using a precomputed user call graph.
*)
let filterStdlibFunctionsWithUserCallGraph = DeadCodeElimination.filterFunctionsWithUserCallGraph
(*
   Filter stdlib functions to only include those reachable from user code
*)
let filterStdlibFunctions stdlibCallGraph userFunctions stdlibFunctions =
 let ids = StringOrder.Map.of_list (List.map (fun (func : LIR.functionDef) -> func.LIR.name,func.LIR.id) (userFunctions @ stdlibFunctions)) in
 DeadCodeElimination.filterFunctions stdlibCallGraph ids userFunctions stdlibFunctions
(*
   Compute reachable stdlib function names from a user ANF program
*)
let getReachableStdlibNames stdlibCallGraph (ANF.Program (userFuncs, userMainExpr)) =
 let existing = Seq.append (List.to_seq (List.map (fun (func : ANF.functionDef) -> func.ANF.id) userFuncs)) (FunctionIdMap.keys stdlibCallGraph) in
 let startId = match StringOrder.Map.find_opt "__dark_tree_shaking_start" (AST.allocateFunctionIds existing (List.to_seq ["__dark_tree_shaking_start"])) with Some id -> id | None -> Crash.crash "Tree-shaking start identity was not allocated" in
 let startFunc : ANF.functionDef = {ANF.id=startId;name="_start";typedParams=[];returnType=AST.TUnit;returnOwnership=ANF.OwnedReturn;body=userMainExpr} in
 ANFDeadCodeElimination.getReachableStdlib stdlibCallGraph (startFunc :: userFuncs)
