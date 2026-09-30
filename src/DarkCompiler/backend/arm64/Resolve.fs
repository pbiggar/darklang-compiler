// Resolve.fs - ARM64 symbolic literal-pool collection before offset assignment.

module ARM64_Resolve

let collectPoolsFromLabelRefs
    (labelRefs: seq<ARM64Symbolic.LabelRef>)
    : LiteralPool.StringPool * LiteralPool.FloatPool =
    let strings, floats =
        labelRefs
        |> Seq.fold (fun (strings, floats) labelRef ->
            match labelRef with
            | ARM64Symbolic.DataLabel (ARM64Symbolic.StringLiteral value) ->
                value :: strings, floats
            | ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral value) ->
                strings, value :: floats
            | _ -> strings, floats) ([], [])
    (LiteralPool.createStringPool (List.rev strings),
     LiteralPool.createFloatPool (List.rev floats))

let collectPools
    (instructions: ARM64Symbolic.Instr list)
    : LiteralPool.StringPool * LiteralPool.FloatPool =
    instructions
    |> Seq.choose (function
        | ARM64Symbolic.ADRP (_, labelRef)
        | ARM64Symbolic.ADD_label (_, _, labelRef)
        | ARM64Symbolic.ADR (_, labelRef) -> Some labelRef
        | _ -> None)
    |> collectPoolsFromLabelRefs
