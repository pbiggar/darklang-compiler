(* InferOwnedFunctionGroups.fs - Infer uniqueness variants for owned-HIR call groups. *)
[@@@warning "-4"]
module O = OwnedIR
module R = InferRecursiveOwnership
module G = OwnedFunctionGroups
module F = FunctionIdMap
module S = SpecializationIdentity.FunctionSet
type 'id functionBoundary = 'id R.functionBoundary
type 'id candidate = Candidate of 'id functionBoundary * 'id functionBoundary list
type 'id group = Group of 'id candidate * 'id candidate list * bool * S.t * S.t
type ('leaf, 'id) program = Program of ('leaf, 'id) G.group F.t
let measure recordTiming name operation = let start = HostClock.milliseconds () in let result = operation () in let elapsed = HostClock.milliseconds () -. start in Option.iter (fun record -> record name elapsed) recordTiming; result
let inferenceLabel discovered =
 let refinableModes = List.fold_left (fun count (definition : ('leaf, 'id) O.functionDef) -> count + InferOwnershipUniqueness.refinableModeCount definition.O.ownership) 0 (G.functions discovered) in
 let variants = if InferOwnershipUniqueness.withinVariantLimit refinableModes then string_of_int (1 lsl refinableModes) else ">" ^ string_of_int InferOwnershipUniqueness.maximumVariants in
 "Ownership detail: Candidate inference (" ^ variants ^ " theoretical variants)"
let candidateBoundaries (Candidate (head, tail)) = head :: tail
let candidates (Group (head, tail, _, _, _)) = head :: tail
let isRecursive (Group (_, _, recursive, _, _)) = recursive
let internalDependencies (Group (_, _, _, dependencies, _)) = dependencies
let externalTargets (Group (_, _, _, _, targets)) = targets
let nonEmpty context values = match values with head :: tail -> {NonEmptyList.head; tail} | [] -> failwith context
let inferredGroup discovered = function
 | head :: tail -> Group (head, tail, G.isRecursive discovered, G.internalDependencies discovered, G.externalTargets discovered)
 | [] -> failwith "Ownership uniqueness inference returned no group candidates"
let singletonCandidate id name ownership = Candidate ({R.id; name; ownership}, [])
let recursiveCandidate boundary = match R.boundaryToList boundary with head :: tail -> Candidate (head, tail) | [] -> failwith "Recursive ownership inference returned an empty boundary"
(*
   Recursive edges are implementation details of an atomic SCC candidate, not
   independent external demands. Materialization rewrites them when the group
   is selected by a call entering the component.
*)
let isInternalRecursiveCall (Program groups) caller target = match F.tryFind target groups with
 | Some group when G.isRecursive group -> List.exists (fun (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.HIR.id = caller) (G.functions group)
 | Some _ | None -> false
module Make (Identity : O.Identity) = struct
 module Uniqueness = InferOwnershipUniqueness.Make (Identity)
 module Recursive = R.Make (Identity)
 module Ownership = Uniqueness.Ownership
 type inferenceError = FunctionGroupingFailed of G.groupingError | DemandTargetMissing of AST.functionId | GroupInferenceFailed of string AST.nonEmptyList * Uniqueness.inferenceError
 let inferGroup semantics discovered =
  let definitions = G.functions discovered in
  let names = nonEmpty "Owned function discovery returned an empty group" (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.HIR.name) definitions) in
  let wrap result = result |> Result.map_error (fun error -> GroupInferenceFailed (names, error)) |> Result.map (inferredGroup discovered) in
  match definitions, G.isRecursive discovered with
  | [definition], false -> Uniqueness.infer semantics definition |> Result.map (fun inferred -> List.map (singletonCandidate definition.O.definition.HIR.id definition.O.definition.HIR.name) (Uniqueness.toList inferred)) |> wrap
  | head :: tail, true -> Recursive.infer semantics {NonEmptyList.head; tail} |> Result.map (fun inferred -> List.map recursiveCandidate (R.toList inferred)) |> wrap
  | _ :: _, false -> failwith "Owned function discovery produced a nonrecursive multi-function group"
  | [], _ -> failwith "Owned function discovery returned an empty group"
(*
   Discover proof groups without inferring any variants. A function maps back
   to its complete SCC so a later concrete call demand can be solved atomically.
*)
 let prepareWithTrace recordTiming definitions =
  measure recordTiming "Ownership detail: Candidate SCC discovery" (fun () -> G.discover definitions) |> Result.map_error (fun error -> FunctionGroupingFailed error) |> Result.map (fun groups ->
   Program (List.fold_left (fun index group -> List.fold_left (fun index (definition : ('leaf, Identity.t) O.functionDef) -> F.add definition.O.definition.HIR.id group index) index (G.functions group)) F.empty groups))
 let prepare definitions = prepareWithTrace None definitions
(*
   Infer at most one best candidate for an actual call demand. Uncalled groups
   never reach uniqueness verification, and an unprovable demand safely keeps
   the established boundary.
*)
 let inferDemandWithTrace recordTiming semantics (Program groups) target uniqueArguments = match F.tryFind target groups with
 | None -> Error (DemandTargetMissing target)
 | Some discovered ->
   let definitions = G.functions discovered in
   let names = nonEmpty "Owned function demand group is empty" (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.HIR.name) definitions) in
   let wrap result = result |> Result.map_error (fun error -> GroupInferenceFailed (names, error)) |> Result.map (Option.map (fun candidate -> inferredGroup discovered [candidate])) in
   measure recordTiming (inferenceLabel discovered) (fun () -> match definitions, G.isRecursive discovered with
   | [definition], false -> Uniqueness.inferDemand semantics uniqueArguments definition |> Result.map (Option.map (singletonCandidate definition.O.definition.HIR.id definition.O.definition.HIR.name)) |> wrap
   | head :: tail, true -> Recursive.inferDemand semantics target uniqueArguments {NonEmptyList.head; tail} |> Result.map (Option.map recursiveCandidate) |> wrap
   | _ :: _, false -> failwith "Owned function discovery produced a nonrecursive multi-function group"
   | [], _ -> failwith "Owned function discovery returned an empty group")
 let inferDemand semantics program target uniqueArguments = inferDemandWithTrace None semantics program target uniqueArguments
(*
   Discover callee-first owned-HIR SCCs and infer every nondominated uniqueness
   boundary for each proof unit. Cross-group calls deliberately retain the
   ownership contracts registered in `semantics`; selecting inferred callee
   variants at call sites is a later specialization policy.
*)
 let inferWithTrace recordTiming semantics definitions =
  let rec inferGroups inferred = function [] -> Ok (List.rev inferred) | discovered :: rest ->
   match measure recordTiming (inferenceLabel discovered) (fun () -> inferGroup semantics discovered) with Error error -> Error error | Ok group -> inferGroups (group :: inferred) rest in
  measure recordTiming "Ownership detail: Candidate SCC discovery" (fun () -> G.discover definitions) |> Result.map_error (fun error -> FunctionGroupingFailed error) |> fun result -> Result.bind result (inferGroups [])
 let infer semantics definitions = inferWithTrace None semantics definitions
end
