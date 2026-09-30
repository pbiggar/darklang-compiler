// FunctionIdMap.fs - Keep sparse function tables typed while comparing scalar ordinals.

namespace global

/// The representation is private: compiler passes supply semantic identities,
/// while the persistent tree compares uint64 keys without boxing their wrappers.
[<Struct>]
type FunctionIdMap<'value> = private FunctionIdMap of Map<uint64, 'value>

[<RequireQualifiedAccess>]
module FunctionIdMap =
    let empty = FunctionIdMap Map.empty

    let isEmpty (FunctionIdMap entries) = Map.isEmpty entries
    let count (FunctionIdMap entries) = Map.count entries

    let add id value (FunctionIdMap entries) =
        FunctionIdMap (Map.add (AST.functionIdValue id) value entries)

    let remove id (FunctionIdMap entries) =
        FunctionIdMap (Map.remove (AST.functionIdValue id) entries)

    let change id update (FunctionIdMap entries) =
        FunctionIdMap (Map.change (AST.functionIdValue id) update entries)

    let tryFind id (FunctionIdMap entries) =
        Map.tryFind (AST.functionIdValue id) entries

    let containsKey id (FunctionIdMap entries) =
        Map.containsKey (AST.functionIdValue id) entries

    let find id entries =
        tryFind id entries
        |> Option.defaultWith (fun () ->
            Crash.crash $"Required function identity {AST.functionIdValue id} is absent")

    let ofSeq entries =
        entries
        |> Seq.map (fun (id, value) -> AST.functionIdValue id, value)
        |> Map.ofSeq
        |> FunctionIdMap

    let ofList entries = ofSeq entries
    let ofArray entries = ofSeq entries

    let toSeq (FunctionIdMap entries) =
        entries |> Map.toSeq |> Seq.map (fun (ordinal, value) -> AST.functionId ordinal, value)

    let toList (FunctionIdMap entries) =
        entries |> Map.toList |> List.map (fun (ordinal, value) -> AST.functionId ordinal, value)

    let keys (FunctionIdMap entries) = entries |> Map.keys |> Seq.map AST.functionId
    let values (FunctionIdMap entries) = Map.values entries

    let fold folder state (FunctionIdMap entries) =
        Map.fold (fun state ordinal value -> folder state (AST.functionId ordinal) value) state entries

    let iter action (FunctionIdMap entries) =
        Map.iter (fun ordinal value -> action (AST.functionId ordinal) value) entries

    let map mapping (FunctionIdMap entries) =
        entries
        |> Map.map (fun ordinal value -> mapping (AST.functionId ordinal) value)
        |> FunctionIdMap

    let filter predicate (FunctionIdMap entries) =
        entries
        |> Map.filter (fun ordinal value -> predicate (AST.functionId ordinal) value)
        |> FunctionIdMap

    let exists predicate (FunctionIdMap entries) =
        Map.exists (fun ordinal value -> predicate (AST.functionId ordinal) value) entries

    let forall predicate (FunctionIdMap entries) =
        Map.forall (fun ordinal value -> predicate (AST.functionId ordinal) value) entries

    let maxKeyValue (FunctionIdMap entries) =
        if Map.isEmpty entries then Crash.crash "Empty function table has no greatest identity"
        let ordinal, value = Map.maxKeyValue entries
        AST.functionId ordinal, value

    /// Overlay entries take precedence, matching the catalog merge convention.
    let merge baseEntries overlay = fold (fun entries id value -> add id value entries) baseEntries overlay
