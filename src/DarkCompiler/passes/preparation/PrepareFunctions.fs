// PrepareFunctions.fs - Expose whole-program generic preparation entry points.

module PrepareFunctions

open ANF
open SpecializationIdentity
open Monomorphization
open ClosureAnalysis
open LiftExpressions
open LiftFunctions

let monomorphize (program: CheckedAST.Program) : CheckedAST.Program =
    monomorphizeWithGenericFuncDefs (extractGenericFuncDefs program) program

/// Monomorphize a program with access to external generic function definitions.
/// Used when user code needs to specialize stdlib generics - the stdlib generic
/// function bodies are passed in as externalGenericDefs so they can be specialized
/// without merging the full stdlib AST with user code.
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
let monomorphizeWithExternalDefs (externalGenericDefs: GenericFuncDefs) (program: CheckedAST.Program) : CheckedAST.Program =
    let localGenericDefs =
        extractGenericFuncDefs program

    // Merge external defs with local defs (local takes precedence)
    let genericFuncDefs =
        Map.fold (fun acc k v -> Map.add k v acc) externalGenericDefs localGenericDefs

    monomorphizeWithGenericFuncDefs genericFuncDefs program

/// Convert CheckedAST.BinOp to ANF.BinOp
/// Note: StringConcat is handled separately as ANF.StringConcat CExpr
