(* ScheduleOwnershipVariants.ml - Drive verified ownership specialization to a bounded fixed point. *)
[@@@warning "-4"]

module O = OwnedIR
module H = HIR
module S = SelectOwnershipVariants
module G = InferOwnedFunctionGroups
module M = MaterializeOwnershipVariants
module R = InferRecursiveOwnership
module F = FunctionIdMap
module FS = SpecializationIdentity.FunctionSet
module Names = StringOrder.Map

let ( let* ) = Result.bind

type limits = {
  maxIterations : int;
  maxGeneratedGroups : int;
  maxRewrittenCalls : int;
}

let defaultLimits =
  { maxIterations = 32; maxGeneratedGroups = 256; maxRewrittenCalls = 4096 }

type iteration = {
  number : int;
  addedCalls : O.callSiteIdentity list;
  generatedGroups : int;
}

(*
   A cache consumer needs the exact source bodies as well as their canonical
   ownership identity and dependencies. Keeping this structural avoids a
   process-local or formatting-dependent hash in the compiler pipeline.
*)
type ('leaf, 'id) cacheDescriptor = {
  identity : S.candidateIdentity;
  sources : ('leaf, 'id) O.functionDef list;
  boundaries : (string * O.callSignature) list;
  internalDependencies : FS.t;
  externalTargets : FS.t;
}

type ('leaf, 'id) plan = {
  materialization : ('leaf, 'id) M.plan;
  iterations : iteration list;
  cache : ('leaf, 'id) cacheDescriptor list;
}

let materialization plan = plan.materialization
let functions plan = M.functions plan.materialization
let hirContracts plan source = M.hirContracts plan.materialization source
let iterations plan = plan.iterations
let cacheDescriptors plan = plan.cache

let rec calls (block : ('leaf, 'id) O.block) =
  List.concat_map
    (function
      | O.Evaluate (H.Call call) -> [ call ]
      | O.Evaluate (H.Branch (_, _, yes, no)) ->
          let yes = calls yes in
          let no = calls no in
          yes @ no
      | O.Evaluate (H.Leaf _ | H.ScalarBinding _) | O.Dup _ | O.Drop _ -> [])
    block.O.body.H.operations

let compareFunction first second =
  Int64.unsigned_compare
    (AST.functionIdValue first)
    (AST.functionIdValue second)

module SiteOrder = struct
  type t = O.callSiteIdentity

  let compare (first : t) (second : t) =
    let result = compareFunction first.O.caller second.O.caller in
    if result <> 0 then result
    else
      let (H.ValueId first) = first.O.result in
      let (H.ValueId second) = second.O.result in
      Int.compare first second
end

module Sites = Map.Make (SiteOrder)

let originalCalls definitions =
  Sites.of_list
    (List.concat_map
       (fun (definition : ('leaf, 'id) O.functionDef) ->
         List.map
           (fun (call : H.functionCall) ->
             ( {
                 O.caller = definition.O.definition.H.id;
                 result = call.H.result.H.id;
               },
               call ))
           (calls definition.O.definition.H.body))
       definitions)

type demandKey = {
  target : AST.functionId;
  established : O.callSignature;
  uniqueArguments : O.IntSet.t;
}

module Demands = Map.Make (struct
  type t = demandKey

  let compare first second =
    let target = compareFunction first.target second.target in
    if target <> 0 then target
    else
      let signature = Stdlib.compare first.established second.established in
      if signature <> 0 then signature
      else
        Stdlib.compare
          (O.IntSet.elements first.uniqueArguments)
          (O.IntSet.elements second.uniqueArguments)
end)

type 'id demandResolution = { selection : 'id S.selection }

module Visits = Set.Make (struct
  type t = AST.functionId * int

  let compare (first, version) (second, otherVersion) =
    let result = compareFunction first second in
    if result <> 0 then result else Int.compare version otherVersion
end)

let refinableUniqueArguments (established : O.callSignature) uniqueArguments =
  List.mapi
    (fun index ownership ->
      match ownership with
      | O.ConsumedCallParameter when O.IntSet.mem index uniqueArguments ->
          Some index
      | O.UnmanagedCallParameter | O.BorrowedCallParameter
      | O.ConsumedCallParameter | O.UniqueCallParameter ->
          None)
    established.O.parameters
  |> List.filter_map Fun.id |> O.IntSet.of_list

module Make (Identity : O.Identity) = struct
  module Ownership = O.Make (Identity)
  module Verify = VerifyOwnership.Make (Identity)
  module Inference = G.Make (Identity)
  module Verification = VerifyOwnedHIR.Make (Identity)
  module Materialize = M.Make (Identity)
  module Selector = S.Make (Identity)

  type schedulingError =
    | InvalidLimits of limits
    | InvalidFunctionBoundary of AST.functionId * Ownership.verificationError
    | InferenceFailed of Inference.inferenceError
    | CatalogFailed of S.selectionError
    | AnalysisFailed of Verification.verificationError
    | SelectionFailed of O.callSiteIdentity * S.selectionError
    | MaterializationFailed of Materialize.materializationError
    | IterationLimitExceeded of int
    | GeneratedGroupLimitExceeded of int
    | RewrittenCallLimitExceeded of int
    | MissingOriginalCall of O.callSiteIdentity

  let ownershipSemantics plan source =
    Materialize.ownershipSemantics plan.materialization source

  let candidateIdentity candidate =
    List.map
      (fun (boundary : Identity.t R.functionBoundary) ->
        match Verify.callSignatureOfFunction boundary.R.ownership with
        | Ok signature -> (boundary.R.name, signature)
        | Error _ ->
            Crash.crash "Inferred ownership candidate has an invalid boundary")
      (G.candidateBoundaries candidate)
    |> List.sort (fun (first, _) (second, _) ->
        StringOrder.compare first second)

  let cacheDescriptor definitionsByName groups identity =
    List.find_map
      (fun group ->
        List.find_map
          (fun candidate ->
            let boundaries = candidateIdentity candidate in
            if boundaries = S.identityBoundaries identity then
              let sources =
                List.map
                  (fun (name, _) ->
                    match Names.find_opt name definitionsByName with
                    | Some definition -> definition
                    | None ->
                        Crash.crash
                          "Inferred ownership candidate lost its source body")
                  boundaries
              in
              Some
                {
                  identity;
                  sources;
                  boundaries;
                  internalDependencies = G.internalDependencies group;
                  externalTargets = G.externalTargets group;
                }
            else None)
          (G.candidates group))
      groups

  let validateLimits limits =
    if
      limits.maxIterations <= 0
      || limits.maxGeneratedGroups < 0
      || limits.maxRewrittenCalls < 0
    then Error (InvalidLimits limits)
    else Ok ()

  let withInternalOwnership (semantics : 'leaf Ownership.semantics) definitions
      =
    let* registry =
      List.fold_left
        (fun result (definition : ('leaf, Identity.t) O.functionDef) ->
          let* registry = result in
          let* boundary =
            Verify.callSignatureOfFunction definition.O.ownership
            |> Result.map_error (fun error ->
                InvalidFunctionBoundary (definition.O.definition.H.id, error))
          in
          Ok (F.add definition.O.definition.H.id boundary registry))
        (Ok F.empty) definitions
    in
    Ok
      {
        semantics with
        Ownership.callOwnership =
          (fun call ->
            match F.tryFind call.H.target registry with
            | Some boundary -> Some boundary
            | None -> semantics.Ownership.callOwnership call);
      }

  (*
   Only calls in original functions to original functions are scheduling
   roots; recursive edges remain atomic inside their selected SCC clone.
*)
  let scheduleWithTrace recordTiming limits hir semantics reservedFunctions
      definitions =
    let measure name operation =
      let start = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6 in
      let result = operation () in
      let elapsed =
        (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start
      in
      Option.iter (fun record -> record name elapsed) recordTiming;
      result
    in
    let* () = validateLimits limits in
    let* programSemantics =
      measure "Ownership detail: Scheduling registry construction" (fun () ->
          withInternalOwnership semantics definitions)
    in
    let* program =
      Inference.prepareWithTrace recordTiming definitions
      |> Result.map_error (fun error -> InferenceFailed error)
    in
    let originalIds =
      FS.of_list
        (List.map
           (fun (definition : ('leaf, Identity.t) O.functionDef) ->
             definition.O.definition.H.id)
           definitions)
    in
    let definitionsById =
      F.ofList
        (List.map
           (fun (definition : ('leaf, Identity.t) O.functionDef) ->
             (definition.O.definition.H.id, definition))
           definitions)
    in
    let definitionsByName =
      Names.of_list
        (List.map
           (fun (definition : ('leaf, Identity.t) O.functionDef) ->
             (definition.O.definition.H.name, definition))
           definitions)
    in
    let sourceCalls = originalCalls definitions in
    let resolveDemand (fact : O.callSiteFacts) demandCache inferredGroups =
      let uniqueArguments =
        refinableUniqueArguments fact.O.established fact.O.uniqueArguments
      in
      let key =
        {
          target = fact.O.call.H.target;
          established = fact.O.established;
          uniqueArguments;
        }
      in
      match Demands.find_opt key demandCache with
      | Some resolution -> Ok (resolution, demandCache, inferredGroups)
      | None -> (
          let target =
            match F.tryFind fact.O.call.H.target definitionsById with
            | Some definition -> definition.O.definition.H.name
            | None ->
                Crash.crash "Filtered original call target has no definition"
          in
          let* group =
            Inference.inferDemandWithTrace recordTiming programSemantics program
              fact.O.call.H.target uniqueArguments
            |> Result.map_error (fun error -> InferenceFailed error)
          in
          match group with
          | None -> Ok (None, Demands.add key None demandCache, inferredGroups)
          | Some group -> (
              let* catalog =
                measure "Ownership detail: Candidate catalog construction"
                  (fun () -> S.create [ group ])
                |> Result.map_error (fun error -> CatalogFailed error)
              in
              let* selection =
                Selector.select catalog
                  {
                    S.target;
                    established = fact.O.established;
                    uniqueArguments = fact.O.uniqueArguments;
                  }
                |> Result.map_error (fun error ->
                    SelectionFailed (O.callSiteIdentity fact, error))
              in
              match selection with
              | S.EstablishedBoundary _ ->
                  Ok (None, Demands.add key None demandCache, inferredGroups)
              | S.InferredVariant selected
                when S.selectedCallSignature selected = fact.O.established ->
                  Ok (None, Demands.add key None demandCache, inferredGroups)
              | S.InferredVariant _ ->
                  let resolution = { selection } in
                  Ok
                    ( Some resolution,
                      Demands.add key (Some resolution) demandCache,
                      group :: inferredGroups )))
    in
    let rec loop number requests history demandCache inferredGroups cachedFacts
        changedCallers versions analyzedVersions =
      if number > limits.maxIterations then
        Error (IterationLimitExceeded limits.maxIterations)
      else
        let orderedRequests = List.map snd (Sites.bindings requests) in
        let* materialized =
          measure "Ownership detail: Scheduling materialization round"
            (fun () ->
              Materialize.materialize hir semantics reservedFunctions
                definitions orderedRequests)
          |> Result.map_error (fun error -> MaterializationFailed error)
        in
        let groupCount = List.length (M.groups materialized) in
        let rewriteCount = List.length (M.rewrites materialized) in
        if groupCount > limits.maxGeneratedGroups then
          Error (GeneratedGroupLimitExceeded limits.maxGeneratedGroups)
        else if rewriteCount > limits.maxRewrittenCalls then
          Error (RewrittenCallLimitExceeded limits.maxRewrittenCalls)
        else
          let* factsByCaller, analyzedVersions =
            measure "Ownership detail: Scheduling program analysis round"
              (fun () ->
                let currentSemantics =
                  Materialize.ownershipSemantics materialized programSemantics
                in
                List.fold_left
                  (fun result (definition : ('leaf, Identity.t) O.functionDef)
                     ->
                    let* cache, visits = result in
                    let id = definition.O.definition.H.id in
                    if FS.mem id changedCallers then
                      let version =
                        Option.value (F.tryFind id versions) ~default:0
                      in
                      let visit = (id, version) in
                      if Visits.mem visit visits then
                        Crash.crash
                          "Ownership caller was analyzed twice at one body \
                           version"
                      else
                        let* facts =
                          Verify.analyzeFunction currentSemantics definition
                          |> Result.map_error (fun error ->
                              AnalysisFailed
                                (Verification.OwnershipVerificationFailed error))
                        in
                        Ok (F.add id facts cache, Visits.add visit visits)
                    else if F.containsKey id cache then Ok (cache, visits)
                    else
                      Crash.crash
                        "Unchanged ownership caller has no cached facts")
                  (Ok (cachedFacts, analyzedVersions))
                  (M.originals materialized))
          in
          let facts =
            List.concat_map
              (fun (definition : ('leaf, Identity.t) O.functionDef) ->
                match F.tryFind definition.O.definition.H.id factsByCaller with
                | Some facts -> facts
                | None -> Crash.crash "Ownership caller facts were lost")
              (M.originals materialized)
          in
          let* additions, demandCache, inferredGroups =
            measure "Ownership detail: Scheduling call selection round"
              (fun () ->
                let selectedFacts =
                  List.filter
                    (fun (fact : O.callSiteFacts) ->
                      FS.mem fact.O.caller originalIds
                      && FS.mem fact.O.call.H.target originalIds
                      && (not
                            (G.isInternalRecursiveCall program fact.O.caller
                               fact.O.call.H.target))
                      && not (Sites.mem (O.callSiteIdentity fact) requests))
                    facts
                  |> List.sort (fun first second ->
                      SiteOrder.compare (O.callSiteIdentity first)
                        (O.callSiteIdentity second))
                in
                List.fold_left
                  (fun result (fact : O.callSiteFacts) ->
                    let* additions, demandCache, inferredGroups = result in
                    let* resolution, demandCache, inferredGroups =
                      resolveDemand fact demandCache inferredGroups
                    in
                    match resolution with
                    | None -> Ok (additions, demandCache, inferredGroups)
                    | Some resolution -> (
                        let site = O.callSiteIdentity fact in
                        match Sites.find_opt site sourceCalls with
                        | None -> Error (MissingOriginalCall site)
                        | Some original ->
                            let request : Identity.t M.request =
                              {
                                M.caller = fact.O.caller;
                                call = original;
                                selection = resolution.selection;
                              }
                            in
                            Ok
                              (request :: additions, demandCache, inferredGroups)
                        ))
                  (Ok ([], demandCache, inferredGroups))
                  selectedFacts)
          in
          let additions = List.rev additions in
          if additions = [] then
            let identities =
              List.map (fun group -> group.M.identity) (M.groups materialized)
            in
            let cache =
              measure
                "Ownership detail: Scheduling cache descriptor construction"
                (fun () ->
                  List.filter_map
                    (cacheDescriptor definitionsByName inferredGroups)
                    identities)
            in
            Ok
              {
                materialization = materialized;
                iterations = List.rev history;
                cache;
              }
          else
            let next =
              List.fold_left
                (fun state (request : Identity.t M.request) ->
                  Sites.add
                    {
                      O.caller = request.M.caller;
                      result = request.M.call.H.result.H.id;
                    }
                    request state)
                requests additions
            in
            let iteration =
              {
                number;
                addedCalls =
                  List.map
                    (fun (request : Identity.t M.request) ->
                      {
                        O.caller = request.M.caller;
                        result = request.M.call.H.result.H.id;
                      })
                    additions;
                generatedGroups = groupCount;
              }
            in
            let nextVersions =
              List.fold_left
                (fun versions (request : Identity.t M.request) ->
                  let prior =
                    Option.value
                      (F.tryFind request.M.caller versions)
                      ~default:0
                  in
                  F.add request.M.caller (prior + 1) versions)
                versions additions
            in
            loop (number + 1) next (iteration :: history) demandCache
              inferredGroups factsByCaller
              (FS.of_list
                 (List.map
                    (fun (request : Identity.t M.request) -> request.M.caller)
                    additions))
              nextVersions analyzedVersions
    in
    loop 1 Sites.empty [] Demands.empty [] F.empty originalIds F.empty
      Visits.empty

  let schedule limits hir semantics reservedSymbols definitions =
    scheduleWithTrace None limits hir semantics reservedSymbols definitions
end
