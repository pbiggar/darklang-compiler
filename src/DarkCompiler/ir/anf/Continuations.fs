// Continuations.fs - Substitute ANF return continuations without changing lexical joins.

module ANFContinuations

let isSupportedJoinArgumentType = function
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TBool | AST.TDateTime | AST.TUnit
    | AST.TInternalRawPtr -> true
    | _ -> false

let rec bindReturns (expr: ANF.AExpr) (k: ANF.Atom -> ANF.AExpr) : ANF.AExpr =
    match expr with
    | ANF.Jump _ -> expr
    | ANF.Join (parameter, continuation, entry) ->
        ANF.Join (parameter, bindReturns continuation k, bindReturns entry k)
    | ANF.Return atom ->
        k atom
    | ANF.Let (id, cexpr, rest) ->
        ANF.Let (id, cexpr, bindReturns rest k)
    | ANF.If (cond, thenBranch, elseBranch) ->
        ANF.If (cond, bindReturns thenBranch k, bindReturns elseBranch k)

let wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr
