(* SSAOwnershipTestHelpers.ml - Exercise ownership insertion on the production CFG. *)
[@@@warning "-4-42"]

open Dark_compiler

let transform ctx func =
  Result.map
    (RcSSARefCountInsertion.insertBlockLocal ctx
       (RefCountInsertion.ownedDictionaryFrontierParams func))
    (SSAANF.convertFunctionBeforeRC
       (ANF_to_MIR.maxTempIdInFunction func)
       ctx func)

let hasOperation predicate (func : SSAANF.functionDef) =
  SSAANF.LabelMap.exists
    (fun _ (block : SSAANF.block) ->
      List.exists
        (fun (_, operation) -> predicate operation)
        block.SSAANF.operations)
    func.SSAANF.blocks

let hasRefCountIncForTemp target =
  hasOperation (function
    | ANF.RefCountInc (ANF.Var id, _, _, _) when id = target -> true
    | _ -> false)

let hasRefCountDecForTemp target =
  hasOperation (function
    | ANF.RefCountDec (ANF.Var id, _, _, _) when id = target -> true
    | _ -> false)

let tryRefCountDecSourceTypeForTemp target (func : SSAANF.functionDef) =
  SSAANF.LabelMap.bindings func.SSAANF.blocks
  |> List.find_map (fun (_, (block : SSAANF.block)) ->
      List.find_map
        (function
          | _, ANF.RefCountDec (ANF.Var id, _, _, metadata) when id = target ->
              Option.bind metadata (fun value -> value.MemoryModel.sourceType)
          | _ -> None)
        block.SSAANF.operations)
