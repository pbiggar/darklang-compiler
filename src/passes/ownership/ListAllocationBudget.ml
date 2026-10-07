(* ListAllocationBudget.ml - Piecewise allocation accounting across shared continuations. *)
[@@@warning "-4"]

module H = HIR
module L = ListRegion
module O = OwnedIR

let add left right =
  Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))

let allocationBudget (L.OwnedRegion (block, layouts)) =
  let empty : L.allocationSummary =
    {
      L.allocations = 0;
      allocatedBytes = L.constantBytes 0L;
      copies = 0;
      reusedTransforms = 0;
      releases = 0;
    }
  in
  let rec budget (block : L.ownedBlock) = loop empty block.O.body.H.operations
  and loop (summary : L.allocationSummary) operations =
    match operations with
    | [] -> L.Complete summary
    | O.Drop _ :: rest ->
        loop { summary with L.releases = add summary.L.releases 1 } rest
    | O.Dup _ :: rest -> loop summary rest
    | O.Evaluate (H.Branch (_, _, yes, no)) :: rest ->
        let yes = budget yes in
        let no = budget no in
        let rest = loop empty rest in
        L.Conditional (summary, yes, no, rest)
    | O.Evaluate (H.Leaf (L.Transform (output, _, (_, L.ConsumeOrCopy))))
      :: rest ->
        let layout =
          match H.ValueMap.find_opt output.H.id layouts with
          | Some layout -> layout
          | None -> Crash.crash "List HIR: missing runtime-copy layout"
        in
        let reused = { empty with L.reusedTransforms = 1 } in
        let copied =
          {
            empty with
            L.allocations = 1;
            allocatedBytes = L.requestedBytes layout;
            copies = 1;
          }
        in
        L.RuntimeConditional
          (summary, L.Complete reused, L.Complete copied, loop empty rest)
    | O.Evaluate operation :: rest ->
        let allocations, bytes, copies, reused =
          match operation with
          | H.Leaf (L.Construct (output, _))
          | H.Leaf (L.Transform (output, _, (_, L.BorrowAndCopy))) ->
              let layout =
                match H.ValueMap.find_opt output.H.id layouts with
                | Some layout -> layout
                | None -> Crash.crash "List HIR: missing allocation layout"
              in
              ( 1,
                L.requestedBytes layout,
                (match operation with H.Leaf (L.Transform _) -> 1 | _ -> 0),
                0 )
          | H.Leaf (L.Transform (_, _, (_, L.Consume))) ->
              (0, L.constantBytes 0L, 0, 1)
          | H.Leaf (L.Transform (_, _, (_, L.ConsumeOrCopy))) ->
              Crash.crash
                "List HIR: runtime transform handled before static accounting"
          | _ -> (0, L.constantBytes 0L, 0, 0)
        in
        loop
          {
            L.allocations = add summary.L.allocations allocations;
            allocatedBytes = L.addBytes summary.L.allocatedBytes bytes;
            copies = add summary.L.copies copies;
            reusedTransforms = add summary.L.reusedTransforms reused;
            releases = summary.L.releases;
          }
          rest
  in
  budget block
