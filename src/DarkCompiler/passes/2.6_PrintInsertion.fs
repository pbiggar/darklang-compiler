// 2.6_PrintInsertion.fs - Print Insertion Pass
//
// Inserts a Print instruction at the end of the main expression.
// This ensures the program's result is printed before exiting.
//
// This pass runs after RC insertion and before ANF-to-MIR conversion.

module PrintInsertion

open ANF

/// Get the toDisplayString function name for list element type
let getListDisplayStringFunc (elemType: AST.Type) : string option =
    match elemType with
    | AST.TInt64 -> Some "Stdlib.List.toDisplayString_i64"
    | AST.TBool -> Some "Stdlib.List.toDisplayString_bool"
    | AST.TString -> Some "Stdlib.List.toDisplayString_str"
    | AST.TFloat64 -> Some "Stdlib.List.toDisplayString_f64"
    | _ -> None

/// Wrap the return value with a Print instruction
/// Transforms: Return atom  →  Let (_, Print (atom, type), Return atom)
/// For list types, generates: Call toDisplayString, then Print the string
let rec wrapReturnWithPrint (programType: AST.Type) (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
    match expr with
    | Return atom ->
        // For list types, call toDisplayString first
        match programType with
        | AST.TList elemType ->
            match getListDisplayStringFunc elemType with
            | Some toDisplayStringName ->
                // Generate: let strTmp = Call(toDisplayString, [list]) in
                //           let _ = Print(strTmp, String) in Return atom
                let (strTmp, varGen1) = freshVar varGen
                let (printTmp, varGen2) = freshVar varGen1
                let callExpr = Call (toDisplayStringName, [atom])
                let printExpr = Print (Var strTmp, AST.TString)
                (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
            | None ->
                // Unsupported element type, fall back to simple print
                let (printTmp, varGen') = freshVar varGen
                (Let (printTmp, Print (atom, programType), Return atom), varGen')
        | _ ->
            // Non-list types: simple print
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
