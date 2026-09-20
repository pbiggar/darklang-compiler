// TestIds.fs - Allocate deterministic semantic identities for isolated compiler fixtures.

module TestIds

let functionIdForName (name: string) : AST.FunctionId =
    match name with
    | "_start" -> AST.functionId 0
    | "__dark_compiler_program_entry" -> AST.functionId 1
    | _ ->
        let rec hash index current =
            if index = name.Length then current
            else hash (index + 1) ((current * 16777619) ^^^ int name[index])
        AST.functionId (hash 0 -2128831035)
