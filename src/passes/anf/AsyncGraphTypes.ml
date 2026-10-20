(* AsyncGraphTypes.ml - Reserve cyclic sum identities before resolving component payload types. *)
[@@@warning "-4"]
module G = AsyncStorageGraph
module K = AsyncStorageKind
module Keys = Map.Make (struct
  type t = int * AST.semanticType
  let compare = Stdlib.compare
end)
module Seen = Set.Make (struct
  type t = int * AST.semanticType
  let compare = Stdlib.compare
end)
module Components = G.Nodes

type slot = { source : AST.semanticType; pending : bool; nested : Keys.key option }
type node = { kind : K.t; slots : slot list }
type resolved = { types : AST.semanticType Keys.t; shapes : MemoryModel.rcSumShapeRegistry }

let ( let* ) = Result.bind
let find map key =
  match Keys.find_opt key map with
  | Some value -> value
  | None -> Crash.crash "Async typed graph references an absent node"

let carrier source = AST.TSum ("Darklang.Stdlib.__Async.__Value", [source])

let typedNodes graph source =
  let rec visit seen nodes ((id, source) as key) =
    if Seen.mem key seen then (seen, nodes)
    else
      let seen = Seen.add key seen in
      let original = G.node (G.at graph id) in
      let types = K.sourceFields original.G.kind source in
      if List.length types <> List.length original.G.slots then
        Crash.crash "Async typed graph has inconsistent field arities";
      let slots =
        List.map2
          (fun source slot ->
            { source; pending = Option.is_some slot.G.pending;
              nested = Option.map (fun id -> id, source) slot.G.nested })
          types original.G.slots
      in
      let seen, nodes =
        List.fold_left
          (fun (seen, nodes) slot ->
            match slot.nested with
            | None -> seen, nodes
            | Some child -> visit seen nodes child)
          (seen, nodes) slots
      in
      seen, Keys.add key { kind = original.G.kind; slots } nodes
  in
  snd (visit Seen.empty Keys.empty (G.root graph, source))

let rebuild kind types =
  match kind, types with
  | K.TupleStorage, _ | K.RecordStorage _, _ -> AST.TTuple types
  | K.ListStorage _, [element] -> AST.TList element
  | K.DictStorage (key, _), [value] -> AST.TDict (key, value)
  | _ -> Crash.crash "Async physical graph has an inconsistent storage kind"

let resolve graph source =
  let nodes = typedNodes graph source in
  let keys = List.map fst (Keys.bindings nodes) in
  let indices =
    List.mapi (fun index key -> key, index) keys |> Keys.of_list
  in
  let edges =
    List.map
      (fun key ->
        List.filter_map
          (fun slot -> Option.map (find indices) slot.nested)
          (find nodes key).slots)
      keys |> Array.of_list
  in
  let classes = StronglyConnectedComponents.classify edges |> Array.to_list in
  let membership = Keys.of_list (List.combine keys classes) in
  let components =
    List.fold_left2
      (fun components key component ->
        let existing =
          match Components.find_opt component components with Some values -> values | None -> []
        in
        Components.add component (key :: existing) components)
      Components.empty keys classes
  in
  let componentKeys component =
    match Components.find_opt component components with
    | Some keys -> keys
    | None -> Crash.crash "Async physical graph references an absent component"
  in
  let rec slotTypes ?(literal = Fun.id) resolveType state slots =
    match slots with
    | [] -> Ok ([], state)
    | slot :: rest ->
        let* typ, state =
          match slot.nested with
          | None -> Ok (literal slot.source, state)
          | Some key -> resolveType state key
        in
        let typ = if slot.pending then carrier typ else typ in
        let* rest, state = slotTypes ~literal resolveType state rest in
        Ok (typ :: rest, state)
  in
  let includeShape state sum shape =
    let* shapes =
      AsyncSumStorage.includePhysicalShapes ~into:state.shapes
        (StringOrder.Map.singleton sum.ANF.sumName shape)
    in
    Ok { state with shapes }
  in
  let rec component state id =
    let members = componentKeys id in
    if List.for_all (fun key -> Keys.mem key state.types) members then Ok state
    else
      let* state =
        List.fold_left
          (fun result key ->
            let* state = result in
            List.fold_left
              (fun result slot ->
                let* state = result in
                match slot.nested with
                | Some child when find membership child <> id ->
                    component state (find membership child)
                | None | Some _ -> Ok state)
              (Ok state) (find nodes key).slots)
          (Ok state) members
      in
      let cyclic =
        match members with
        | [key] -> List.exists (fun slot -> slot.nested = Some key) (find nodes key).slots
        | [] -> Crash.crash "Async physical graph has an empty component"
        | _ -> true
      in
      if not cyclic then
        List.fold_left
          (fun result key ->
            let* state = result in
            let node = find nodes key in
            let* types, state = slotTypes known state node.slots in
            match node.kind with
            | K.SumStorage layout ->
                let sum, shape = AsyncSumStorage.physical layout types in
                let* state = includeShape state sum shape in
                Ok { state with types = Keys.add key (AST.TSum (sum.ANF.sumName, sum.ANF.sumArgs)) state.types }
            | kind -> Ok { state with types = Keys.add key (rebuild kind types) state.types })
          (Ok state) members
      else cyclicComponent state members
  and known state key =
    match Keys.find_opt key state.types with
    | Some typ -> Ok (typ, state)
    | None -> Crash.crash "Async physical dependency was not resolved"
  and cyclicComponent state members =
    let sourceIdentity (_, source) = SpecializationIdentity.specName "source" [source] in
    let anchors =
      List.filter
        (fun key -> match (find nodes key).kind with K.SumStorage _ -> true | _ -> false)
        members
      |> List.sort (fun left right -> StringOrder.compare (sourceIdentity left) (sourceIdentity right))
    in
    if anchors = [] then Error "Async recursive aggregate layout has no sum anchor"
    else
      let symbolic =
        List.mapi
          (fun index key -> key, AST.TSum ("__dark_async_cycle_ref_" ^ string_of_int index, []))
          anchors |> Keys.of_list
      in
      let rec expand literal substitutions visiting state key =
        match Keys.find_opt key substitutions with
        | Some typ -> Ok (typ, state)
        | None -> (
            match Keys.find_opt key state.types with
            | Some typ -> Ok (typ, state)
            | None when Seen.mem key visiting ->
                Error "Async recursive aggregate layout has a cycle without a sum anchor"
            | None ->
                let node = find nodes key in
                let* types, state =
                  slotTypes ~literal (expand literal substitutions (Seen.add key visiting)) state node.slots
                in
                let typ = rebuild node.kind types in
                Ok (typ, { state with types = Keys.add key typ state.types }))
      in
      (* Native types and back references occupy disjoint symbolic forms. A
         user type whose spelling resembles a marker cannot alias a cycle. *)
      let native typ = AST.TSum ("__dark_async_cycle_native", [typ]) in
      let symbolicState = { state with types = Keys.map native state.types } in
      let* signature, _ =
        List.fold_left
          (fun result key ->
            let* signature, state = result in
            let* types, state = slotTypes ~literal:native (expand native symbolic Seen.empty) state (find nodes key).slots in
            Ok (signature @ [snd key; AST.TTuple types], state))
          (Ok ([], symbolicState)) anchors
      in
      let identity = SpecializationIdentity.specName "recursive_sum_payloads" signature in
      let state =
        List.fold_left
          (fun state key ->
            let name = "__dark_async_sum_" ^ Digest.to_hex (Digest.string (identity ^ "/" ^ sourceIdentity key)) in
            { state with types = Keys.add key (AST.TSum (name, [])) state.types })
          state anchors
      in
      let* state =
        List.fold_left
          (fun result key ->
            let* state = result in
            Result.map snd (expand Fun.id Keys.empty Seen.empty state key))
          (Ok state) members
      in
      List.fold_left
        (fun result key ->
          let* state = result in
          let layout =
            match (find nodes key).kind with K.SumStorage layout -> layout
            | _ -> Crash.crash "Async sum anchor lost its source layout"
          in
          let name =
            match find state.types key with AST.TSum (name, []) -> name
            | _ -> Crash.crash "Async sum anchor lost its reserved identity"
          in
          let* types, state = slotTypes known state (find nodes key).slots in
          let sum, shape = AsyncSumStorage.physicalNamed ~name layout types in
          includeShape state sum shape)
        (Ok state) anchors
  in
  let root = G.root graph, source in
  let* state = component { types = Keys.empty; shapes = StringOrder.Map.empty } (find membership root) in
  Ok (find state.types root, state.shapes)
