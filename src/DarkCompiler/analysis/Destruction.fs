// Destruction.fs - Prove inert destruction locally and through function scope contracts.

module DestructionAnalysis

/// Structural proof that releasing a value cannot invoke user code. This is
/// not an effect/purity claim about evaluating it. Nominal payloads and opaque
/// closures require layout/capture evidence unavailable from the type alone.
let rec hasInertDestruction = function
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TInt | AST.TInt128 | AST.TUInt128
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TChar
    | AST.TBlob | AST.TUnit | AST.TDateTime | AST.TInternalRawPtr | AST.TNever -> true
    | AST.TList element -> hasInertDestruction element
    | AST.TTuple elements -> List.forall hasInertDestruction elements
    | AST.TDict (key, value) -> hasInertDestruction key && hasInertDestruction value
    | _ -> false

type ScopeDestruction = InertScope | UnprovenScope

type FunctionScopeContract = {
    LocalDestruction: ScopeDestruction
    Calls: Set<AST.FunctionId>
}

/// These compiler primitives can perform effects, but neither owns a value
/// whose destruction invokes user code. Unknown external calls remain unproven.
let private inertPrimitiveNames = Set.ofList ["Builtin.printLine"; "Builtin.print"]

/// Reject callers transitively from locally unproven scopes and unavailable
/// callees. Safe recursive components are accepted without unfolding paths.
let inertFunctionScopes
    (functionNames: Map<AST.FunctionId, string>)
    (contracts: Map<AST.FunctionId, FunctionScopeContract>) =
    let names = contracts |> Map.keys |> Set.ofSeq
    let inertPrimitives =
        functionNames
        |> Map.toSeq
        |> Seq.choose (fun (id, name) -> if Set.contains name inertPrimitiveNames then Some id else None)
        |> Set.ofSeq
    let primitives = Set.difference inertPrimitives names
    let unavailable calls = not (Set.isSubset calls (Set.union names primitives))
    let unproven =
        contracts |> Map.toSeq |> Seq.choose (fun (name, contract) ->
            match contract.LocalDestruction with
            | UnprovenScope -> Some name
            | InertScope when unavailable contract.Calls -> Some name
            | InertScope -> None) |> Set.ofSeq
    let callers =
        contracts |> Map.fold (fun callers name contract ->
            contract.Calls |> Set.fold (fun callers target ->
                Map.change target (fun previous -> Some (Set.add name (Option.defaultValue Set.empty previous))) callers) callers) Map.empty
    let rejected = CallGraphReachability.findReachable callers unproven
    Set.union primitives (Set.difference names rejected)
