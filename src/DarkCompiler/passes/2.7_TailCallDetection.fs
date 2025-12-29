// 2.7_TailCallDetection.fs - Tail Call Detection Pass
//
// Detects tail calls in ANF and transforms them to tail call variants:
// - Call → TailCall
// - IndirectCall → IndirectTailCall
// - ClosureCall → ClosureTailCall
//
// A call is in tail position if:
// - It's in a Let binding where the body is Return of the same variable
// - Both branches of an If are in tail position if the If itself is
//
// This runs AFTER RefCountInsertion, so RefCountDec operations are already
// inserted. For safety in V1, we only optimize calls that are immediately
// followed by Return (no intervening RefCountDec).

module TailCallDetection

open ANF

/// Check if an expression is a simple Return of a specific TempId
let isReturnOf (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return (Var tid) when tid = tempId -> true
    | _ -> false

/// Transform a Call to TailCall if it's in tail position
let convertToTailCall (cexpr: CExpr) : CExpr =
    match cexpr with
    | Call (funcName, args) -> TailCall (funcName, args)
    | IndirectCall (func, args) -> IndirectTailCall (func, args)
    | ClosureCall (closure, args) -> ClosureTailCall (closure, args)
    | _ -> cexpr

/// Check if a CExpr is a call (direct, indirect, or closure)
let isCallExpr (cexpr: CExpr) : bool =
    match cexpr with
    | Call _ | IndirectCall _ | ClosureCall _ -> true
    | _ -> false

/// Detect and transform tail calls in an expression.
/// The 'inTailPosition' parameter indicates if the current expression
/// is in tail position (its result is directly returned).
let rec detectTailCalls (inTailPosition: bool) (expr: AExpr) : AExpr =
    match expr with
    | Return atom ->
        // Return is always a base case - just return it
        Return atom

    | Let (tempId, cexpr, body) ->
        // Check if this is a tail call pattern:
        // Let (t, Call(...), Return (Var t))
        if inTailPosition && isCallExpr cexpr && isReturnOf tempId body then
            // This is a tail call! Convert the call to tail call variant
            Let (tempId, convertToTailCall cexpr, body)
        else
            // Not a tail call - recurse into body
            // Body is in tail position if current expression is
            let body' = detectTailCalls inTailPosition body
            Let (tempId, cexpr, body')

    | If (cond, thenBranch, elseBranch) ->
        // If expression: both branches are in tail position if If is
        let thenBranch' = detectTailCalls inTailPosition thenBranch
        let elseBranch' = detectTailCalls inTailPosition elseBranch
        If (cond, thenBranch', elseBranch')

/// Detect tail calls in a function
let detectTailCallsInFunction (func: Function) : Function =
    // Function body is always in tail position
    let body' = detectTailCalls true func.Body
    { func with Body = body' }

/// Detect tail calls in a program
let detectTailCallsInProgram (program: ANF.Program) : ANF.Program =
    let (ANF.Program (functions, mainExpr)) = program

    // Process all functions
    let functions' = List.map detectTailCallsInFunction functions

    // DON'T apply tail call optimization to main expression
    // _start has a different stack frame structure and exits via syscall, not RET
    // So tail calls from main would break the program
    ANF.Program (functions', mainExpr)
