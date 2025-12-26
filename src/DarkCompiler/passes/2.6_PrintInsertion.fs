// 2.6_PrintInsertion.fs - Print Insertion Pass
//
// Inserts a Print instruction at the end of the main expression.
// This ensures the program's result is printed before exiting.
//
// This pass runs after RC insertion and before ANF-to-MIR conversion.

module PrintInsertion

open ANF

/// Wrap the return value with a Print instruction
/// Transforms: Return atom  →  Let (_, Print (atom, type), Return atom)
let rec wrapReturnWithPrint (programType: AST.Type) (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
    match expr with
    | Return atom ->
        // Insert Print before Return
        let (printTmp, varGen') = freshVar varGen
        (Let (printTmp, Print (atom, programType), Return atom), varGen')
    | Let (tempId, cexpr, body) ->
        // Recurse into body
        let (body', varGen') = wrapReturnWithPrint programType varGen body
        (Let (tempId, cexpr, body'), varGen')
    | If (cond, thenBranch, elseBranch) ->
        // Wrap both branches
        let (thenBranch', varGen1) = wrapReturnWithPrint programType varGen thenBranch
        let (elseBranch', varGen2) = wrapReturnWithPrint programType varGen1 elseBranch
        (If (cond, thenBranch', elseBranch'), varGen2)

/// Insert Print at the end of the main expression
let insertPrint (functions: ANF.Function list) (mainExpr: ANF.AExpr) (programType: AST.Type) : ANF.Program =
    let varGen = VarGen 2000  // Start high to avoid conflicts
    let (exprWithPrint, _) = wrapReturnWithPrint programType varGen mainExpr
    ANF.Program (functions, exprWithPrint)
