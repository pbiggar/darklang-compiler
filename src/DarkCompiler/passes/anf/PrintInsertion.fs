// PrintInsertion.fs - Print Insertion Pass
//
// Inserts a Print instruction at the end of the main expression.
// This ensures the program's result is printed before exiting.
//
// This pass runs before RC insertion so generated output and its final managed
// uses participate in the same ownership analysis as source operations.

module PrintInsertion

open ANF

let unsupportedListDisplay (elemType: AST.SemanticType) : 'a =
    Crash.crash $"Unsupported list result display element type: {CheckingDiagnostics.typeToString elemType}"

/// Wrap the return value with a Print instruction
/// Transforms: Return atom  →  Let (_, Print (atom, type), Return atom)
/// For list types, generates: Call toDisplayString, then Print the string
let rec wrapReturnWithPrint
    (resolveFunction: string -> AST.FunctionId)
    (programType: AST.SemanticType)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    let defaultPrintType =
        match programType with
        // Builtin.testRuntimeError has a bottom-like compile-time type.
        // Printing should stay concrete so downstream passes never see it.
        | AST.TNever -> AST.TUnit
        | _ -> programType

    match expr with
    | Return atom ->
        // Dead-code elimination can reduce a typed expression branch to `()`
        // (for example, `Builtin.testRuntimeError` in a selected match arm).
        // Printing must follow the runtime atom shape, not only the original program type.
        let printType =
            match atom with
            | ANF.UnitLiteral -> AST.TUnit
            | _ -> defaultPrintType

        // For list types, call toDisplayString first
        match printType with
        | AST.TUnit ->
            // Explicit output functions return Unit. Matching the interpreter,
            // a final Unit has no implicit textual representation.
            (Return atom, varGen)
        | AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TList elemType]) ->
            match ListDisplay.getDisplayStringFunc elemType with
            | Some toDisplayStringName ->
                // Keep the display helper reachable so tree shaking doesn't drop it.
                let (keepFunc, varGen1) = freshVar varGen
                let (printTmp, varGen2) = freshVar varGen1
                let keepExpr = Atom (FuncRef (resolveFunction toDisplayStringName))
                let printExpr = Print (atom, printType)
                (Let (keepFunc, keepExpr, Let (printTmp, printExpr, Return atom)), varGen2)
            | None ->
                unsupportedListDisplay elemType
        | AST.TList elemType ->
            match ListDisplay.getDisplayStringFunc elemType with
            | Some toDisplayStringName ->
                // Generate: let strTmp = Call(toDisplayString, [list]) in
                //           let _ = Print(strTmp, String) in Return atom
                let (strTmp, varGen1) = freshVar varGen
                let (printTmp, varGen2) = freshVar varGen1
                let callExpr = Call (resolveFunction toDisplayStringName, [atom])
                let printExpr = Print (Var strTmp, AST.TString)
                (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
            | None ->
                unsupportedListDisplay elemType
        | AST.TFloat64 ->
            // For Float64, call Float.toString first, then print the string
            let (strTmp, varGen1) = freshVar varGen
            let (printTmp, varGen2) = freshVar varGen1
            let callExpr =
                Call (resolveFunction "Darklang.Stdlib.Float.toString", [atom])
            let printExpr = Print (Var strTmp, AST.TString)
            (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
        | AST.TDateTime ->
            // DateTime is an opaque immediate; display it through its public formatter.
            let (strTmp, varGen1) = freshVar varGen
            let (printTmp, varGen2) = freshVar varGen1
            let callExpr =
                Call (resolveFunction "Darklang.Stdlib.DateTime.toString", [atom])
            let printExpr = Print (Var strTmp, AST.TString)
            (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
        | AST.TSum ("Uuid", []) ->
            // UUID is an ordinary sum, but public output is its canonical text.
            let (strTmp, varGen1) = freshVar varGen
            let (printTmp, varGen2) = freshVar varGen1
            let callExpr =
                Call (resolveFunction "Darklang.Stdlib.Uuid.toString", [atom])
            let printExpr = Print (Var strTmp, AST.TString)
            (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
        | _ ->
            // Non-list types: simple print
            let (printTmp, varGen') = freshVar varGen
            (Let (printTmp, Print (atom, printType), Return atom), varGen')
    | Let (tempId, cexpr, body) ->
        // Recurse into body
        let (body', varGen') = wrapReturnWithPrint resolveFunction programType varGen body
        (Let (tempId, cexpr, body'), varGen')
    | Jump _ -> (expr, varGen)
    | Join (parameter, continuation, entry) ->
        let continuation', next = wrapReturnWithPrint resolveFunction programType varGen continuation
        let entry', final = wrapReturnWithPrint resolveFunction programType next entry
        (Join (parameter, continuation', entry'), final)
    | If (cond, thenBranch, elseBranch) ->
        // Wrap both branches
        let (thenBranch', varGen1) = wrapReturnWithPrint resolveFunction programType varGen thenBranch
        let (elseBranch', varGen2) = wrapReturnWithPrint resolveFunction programType varGen1 elseBranch
        (If (cond, thenBranch', elseBranch'), varGen2)

/// Insert Print at the end of the main expression
let insertPrint (functions: ANF.Function list) (mainExpr: ANF.AExpr) (programType: AST.SemanticType) : ANF.Program =
    let resolveFunction = AST.functionIdForName
    let varGen = VarGen 2000  // Start high to avoid conflicts
    let (exprWithPrint, _) = wrapReturnWithPrint resolveFunction programType varGen mainExpr
    ANF.Program (functions, exprWithPrint)

/// Insert Print into a named entry function
let insertPrintInEntry
    (entryName: string)
    (programType: AST.SemanticType)
    (functions: ANF.Function list)
    : Result<ANF.Function list, string> =
    let resolveFunction = AST.functionIdForName
    let varGen = VarGen 2000  // Start high to avoid conflicts
    let rec update found remaining =
        match remaining with
        | [] ->
            if found then Ok []
            else Error $"Entry function '{entryName}' not found for print insertion"
        | f :: rest ->
            if f.Name = entryName then
                let (bodyWithPrint, _) = wrapReturnWithPrint resolveFunction programType varGen f.Body
                update true rest
                |> Result.map (fun updatedTail -> { f with Body = bodyWithPrint } :: updatedTail)
            else
                update found rest
                |> Result.map (fun updatedTail -> f :: updatedTail)
    update false functions

/// Observe the source value immediately before the generated value renderer
/// consumes it. The ordinary result printer sees only the rendered string.
let insertRootWordProbeInEntry
    (entryName: string)
    (functions: ANF.Function list)
    : Result<ANF.Function list, string> =
    let rec probeReturns (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
        match expr with
        | Return _ -> (expr, varGen)
        | Let (id, Call (callee, [value]), body)
            when (AST.functionIdValue callee).StartsWith("__dark_render_value_") ->
            let probeId, next = freshVar varGen
            let body', final = probeReturns next body
            (Let (probeId, Print (value, AST.TInt64), Let (id, Call (callee, [value]), body')), final)
        | Let (id, value, body) ->
            let body', next = probeReturns varGen body
            (Let (id, value, body'), next)
        | If (condition, yes, no) ->
            let yes', afterYes = probeReturns varGen yes
            let no', afterNo = probeReturns afterYes no
            (If (condition, yes', no'), afterNo)
        | Join (parameter, continuation, entry) ->
            let continuation', afterContinuation = probeReturns varGen continuation
            let entry', afterEntry = probeReturns afterContinuation entry
            (Join (parameter, continuation', entry'), afterEntry)
        | Jump _ -> (expr, varGen)

    let rec update found remaining =
        match remaining with
        | [] ->
            if found then Ok []
            else Error $"Entry function '{entryName}' not found for root word probe"
        | func :: rest when func.Name = entryName ->
            let body, _ = probeReturns (VarGen 3000) func.Body
            update true rest
            |> Result.map (fun tail -> { func with Body = body } :: tail)
        | func :: rest ->
            update found rest
            |> Result.map (fun tail -> func :: tail)
    update false functions
