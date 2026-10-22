(* AsyncSumStorage.ml - Preserve constructor-specific payload types across async interfaces. *)
[@@@warning "-4"]

module M = StringOrder.Map
module A = ANF

let physicalPrefix = "__dark_async_sum_"

type layout = {
  source : A.sumDescriptor;
  payloads : (int * AST.semanticType) list;
  emptyTags : int list;
  unaryTags : int list;
}

let layout shapes source =
  match M.find_opt source.A.sumName shapes with
  | Some shape
    when List.length shape.MemoryModel.typeParams = List.length source.A.sumArgs
    ->
      let substitutions =
        M.of_list (List.combine shape.MemoryModel.typeParams source.A.sumArgs)
      in
      let payloads =
        List.filter_map
          (fun (tag, payload) ->
            Option.map
              (fun typ ->
                (tag, TypeSubstitution.applySubstToType substitutions typ))
              payload)
          shape.MemoryModel.payloads
        |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      in
      let emptyTags =
        List.filter_map
          (fun (tag, payload) ->
            if Option.is_none payload then Some tag else None)
          shape.MemoryModel.payloads
        |> List.sort Int.compare
      in
      Ok
        {
          source;
          payloads;
          emptyTags;
          unaryTags =
            MemoryModel.IntSet.elements shape.MemoryModel.unaryPayloadTags;
        }
  | None | Some _ ->
      Error
        ("Async sum payload metadata is missing or inconsistent for "
       ^ source.A.sumName)

let payloadIndex layout tag =
  List.find_map
    (fun (index, (candidate, _)) ->
      if candidate = tag then Some index else None)
    (List.mapi (fun index payload -> (index, payload)) layout.payloads)

let physicalNamed ~name layout types =
  if List.length types <> List.length layout.payloads then
    Crash.crash "Async sum layout has an inconsistent payload count";
  if not (String.starts_with ~prefix:physicalPrefix name) then
    Crash.crash "Async sum layout has a non-private physical name";
  let payloads =
    List.map2 (fun (tag, _) typ -> (tag, Some typ)) layout.payloads types
    @ List.map (fun tag -> (tag, None)) layout.emptyTags
    |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
  in
  ( { A.sumName = name; sumArgs = [] },
    {
      MemoryModel.typeParams = [];
      payloads;
      unaryPayloadTags = MemoryModel.IntSet.of_list layout.unaryTags;
    } )

let physical layout types =
  let source = AST.TSum (layout.source.A.sumName, layout.source.A.sumArgs) in
  let identity =
    SpecializationIdentity.specName "sum_payloads" (source :: types)
  in
  let name = physicalPrefix ^ Digest.to_hex (Digest.string identity) in
  physicalNamed ~name layout types

let representationIndex shapes =
  M.mapi
    (fun owner shape ->
      List.fold_left
        (fun cases (tag, payload) ->
          let fields =
            match payload with
            | None -> []
            | Some (AST.TTuple types)
              when not
                     (MemoryModel.IntSet.mem tag
                        shape.MemoryModel.unaryPayloadTags) ->
                types
            | Some typ -> [ typ ]
          in
          M.add
            (owner ^ ".__case_" ^ string_of_int tag)
            {
              LoweringPrimitives.typeParams = shape.MemoryModel.typeParams;
              tag;
              fields;
            }
            cases)
        M.empty shape.MemoryModel.payloads)
    shapes

let includePhysicalShapes ~into shapes =
  M.fold
    (fun name shape result ->
      Result.bind result (fun combined ->
          if not (String.starts_with ~prefix:physicalPrefix name) then
            Ok combined
          else
            match M.find_opt name combined with
            | None -> Ok (M.add name shape combined)
            | Some existing
              when existing.MemoryModel.typeParams
                   = shape.MemoryModel.typeParams
                   && existing.MemoryModel.payloads = shape.MemoryModel.payloads
                   && MemoryModel.IntSet.equal
                        existing.MemoryModel.unaryPayloadTags
                        shape.MemoryModel.unaryPayloadTags ->
                Ok combined
            | Some _ ->
                Error
                  ("Async sum physical layouts have conflicting metadata for "
                 ^ name)))
    shapes (Ok into)

let physicalVariantLookup shapes =
  let physical =
    M.filter
      (fun name _ -> String.starts_with ~prefix:physicalPrefix name)
      shapes
    |> representationIndex
  in
  M.fold
    (fun owner cases lookup ->
      M.fold
        (fun name case lookup ->
          M.add name
            ( owner,
              case.LoweringPrimitives.typeParams,
              case.LoweringPrimitives.tag,
              case.LoweringPrimitives.fields )
            lookup)
        cases lookup)
    physical M.empty
