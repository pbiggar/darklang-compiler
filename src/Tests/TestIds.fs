// TestIds.fs - Allocate deterministic semantic identities for isolated compiler fixtures.

module TestIds

let functionIdForName (name: string) : AST.FunctionId =
    AST.functionIdForName name
