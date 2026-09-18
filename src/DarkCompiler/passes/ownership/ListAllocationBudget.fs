// ListAllocationBudget.fs - Piecewise allocation accounting across shared continuations.

module ListAllocationBudget

open HIR
open OwnedIR

open ListRegion

let allocationBudget (OwnedRegion (block, layouts)) : AllocationBudget =
    let empty = { Allocations = 0; AllocatedBytes = constantBytes 0L; Copies = 0; ReusedTransforms = 0; Releases = 0 }
    let rec budget block =
        loop empty block.Body.Operations
    and loop summary operations =
        match operations with
        | [] -> Complete summary
        | Drop _ :: rest -> loop { summary with Releases = summary.Releases + 1 } rest
        | Dup _ :: rest -> loop summary rest
        | Evaluate (Branch (_, _, yes, no)) :: rest ->
            Conditional (summary, budget yes, budget no, loop empty rest)
        | Evaluate operation :: rest ->
            let allocations, bytes, copies, reused =
                match operation with
                | Leaf (Construct (output, _))
                | Leaf (Transform (output, _, (_, BorrowAndCopy))) ->
                    let layout =
                        match Map.tryFind output.Id layouts with
                        | Some layout -> layout
                        | None -> Crash.crash "List HIR: missing allocation layout"
                    1, requestedBytes layout, (match operation with Leaf (Transform _) -> 1 | _ -> 0), 0
                | Leaf (Transform (_, _, (_, Consume))) -> 0, constantBytes 0L, 0, 1
                | _ -> 0, constantBytes 0L, 0, 0
            loop { Allocations = summary.Allocations + allocations
                   AllocatedBytes = addBytes summary.AllocatedBytes bytes
                   Copies = summary.Copies + copies
                   ReusedTransforms = summary.ReusedTransforms + reused
                   Releases = summary.Releases } rest
    budget block
