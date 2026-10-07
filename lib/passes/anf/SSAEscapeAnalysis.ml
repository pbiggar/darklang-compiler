(* SSAEscapeAnalysis.fs - Scalar replacement and unique fixed-block reuse on SSA ANF. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module M = RcTypeFacts.TempMap
module Set = ANFEffects.TempSet
module E = EscapeAnalysisFacts
type aggregate = {fields : A.atom list}
let atomUses id atom = ANFEffects.atomsUseTemp id [atom]
let operationUses = ANFEffects.cexprUsesTemp
let terminatorUses id = function S.Return atom -> atomUses id atom | S.Jump (_, args) -> ANFEffects.atomsUseTemp id args | S.Branch (condition, _, _) -> atomUses id condition
let allOperations (func : S.functionDef) = S.LabelMap.bindings func.S.blocks |> List.concat_map (fun (_, block) -> block.S.operations)
let scalarAtom types = function
 | A.UnitLiteral | A.IntLiteral _ | A.BoolLiteral _ | A.FloatLiteral _ -> true
 | A.Var id -> Option.fold ~none:false ~some:E.isScalarType (M.find_opt id types)
 | A.StringLiteral _ | A.FuncRef _ -> false
let scalarAggregate types = function
 | A.TupleAlloc fields when List.for_all (scalarAtom types) fields -> Some {fields}
 | A.RecordAlloc (descriptor, fields) | A.RecordClone (descriptor, _, fields) when List.length descriptor.A.fields = List.length fields && List.for_all (fun (_, typ) -> E.isScalarType typ) descriptor.A.fields && List.for_all (scalarAtom types) fields -> Some {fields}
 | _ -> None
let aliasesOf operations source =
 let rec collect known = let next = List.fold_left (fun current (id, operation) -> match operation with A.Atom (A.Var origin) | A.TypedAtom (A.Var origin, _) when Set.mem origin current -> Set.add id current | _ -> current) known operations in if Set.equal next known then known else collect next in collect (Set.singleton source)
let aggregateHasOnlyLocalUses (func : S.functionDef) operations source =
 let tracked = aliasesOf operations source in
 let valid (id, operation) = if id = source || not (Set.exists (fun id -> operationUses id operation) tracked) then true else match operation with
  | A.Atom (A.Var origin) | A.TypedAtom (A.Var origin, _) when Set.mem origin tracked -> true
  | A.TupleGet (A.Var origin, _) | A.RecordGet (_, A.Var origin, _) when Set.mem origin tracked -> true
  | A.RecordClone (_, A.Var origin, fields) when Set.mem origin tracked -> not (Set.exists (fun id -> ANFEffects.atomsUseTemp id fields) tracked)
  | _ -> false in
 List.for_all valid operations && S.LabelMap.for_all (fun _ block -> not (Set.exists (fun id -> terminatorUses id block.S.terminator) tracked)) func.S.blocks
let tryField index aggregate = match (if index < 0 then None else List.nth_opt aggregate.fields index) with Some field -> field | None -> Crash.crash ("SSA escape analysis: projection index " ^ string_of_int index ^ " is out of bounds")
let replaceScalars (func : S.functionDef) =
 let operations = allOperations func in
 let aggregates = List.filter_map (fun (id, operation) -> Option.bind (scalarAggregate func.S.freshValueTypes operation) (fun aggregate -> if aggregateHasOnlyLocalUses func operations id then Some (id, aggregate) else None)) operations |> M.of_list in
 let aliases = M.fold (fun source aggregate known -> Set.fold (fun id current -> M.add id aggregate current) (aliasesOf operations source) known) aggregates M.empty in
 let rewrite (id, operation) = if M.mem id aliases then None else
  let replacement = match operation with
   | A.TupleGet (A.Var source, index) | A.RecordGet (_, A.Var source, index) -> Option.map (fun aggregate -> A.Atom (tryField index aggregate)) (M.find_opt source aliases)
   | A.RecordClone (descriptor, A.Var source, fields) when M.mem source aliases -> Some (A.RecordAlloc (descriptor, fields))
   | _ -> None in Some (id, Option.value ~default:operation replacement) in
 {func with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.filter_map rewrite block.S.operations}) func.S.blocks}
let usesAny ids operation = Set.exists (fun id -> operationUses id operation) ids
let increment value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
(*
   The ANF reuse rule stops at a branch or join. The matching SSA scope is
   one block, with a whole-function use check to rule out escaping aliases.
*)
let reusesInBlock typeReg sumReg (func : S.functionDef) (block : S.block) =
 let rec visit processed = function
  | [] -> List.rev processed
  | (sourceId, sourceOperation) :: tail ->
    let descriptor = match sourceOperation with A.RecordAlloc (descriptor, _) | A.RecordClone (descriptor, _, _) | A.RecordReuse (_, descriptor, _, _) -> Some descriptor | _ -> None in
    let replacement = Option.bind descriptor (fun sourceDescriptor ->
     if not (E.descriptorHasNonObservableDestruction typeReg sumReg sourceDescriptor) then None else
     let isSum = match sourceDescriptor.A.valueType with AST.TSum _ -> true | _ -> false in
     let rec scan index tracked projections = function
      | [] -> None
      | (candidateId, candidate) :: later ->
        let target = match candidate with
         | A.RecordClone (target, A.Var origin, fields) when not isSum && target = sourceDescriptor && Set.mem origin tracked && not (Set.exists (fun id -> ANFEffects.atomsUseTemp id fields) tracked) -> Some (target, A.Var origin, fields)
         | A.RecordAlloc (target, fields) when isSum && target.A.valueType = sourceDescriptor.A.valueType && List.length target.A.fields = List.length sourceDescriptor.A.fields && List.for_all (fun (_, typ) -> E.hasNonObservableDestruction typeReg sumReg true typ) target.A.fields && not (usesAny tracked candidate) -> Some (target, A.Var sourceId, fields)
         | _ -> None in
        match target with
        | Some (target, origin, fields) ->
          let ids = Set.union tracked projections in
          let escaped = S.LabelMap.exists (fun label other -> label <> block.S.label && (List.exists (fun (_, operation) -> usesAny ids operation) other.S.operations || Set.exists (fun id -> terminatorUses id other.S.terminator) ids)) func.S.blocks in
          let live = List.exists (fun (_, operation) -> usesAny tracked operation) later || List.exists (fun (_, operation) -> usesAny projections operation) later || Set.exists (fun id -> terminatorUses id block.S.terminator) ids || escaped in
          if live then None else Some (index, A.RecordReuse (sourceDescriptor, target, origin, fields))
        | None -> match candidate with
          | A.Atom (A.Var origin) | A.TypedAtom (A.Var origin, _) when Set.mem origin tracked -> scan (increment index) (Set.add candidateId tracked) projections later
          | A.Atom (A.Var origin) | A.TypedAtom (A.Var origin, _) when Set.mem origin projections -> scan (increment index) tracked (Set.add candidateId projections) later
          | A.TupleGet (A.Var origin, _) | A.RecordGet (_, A.Var origin, _) when Set.mem origin tracked -> scan (increment index) tracked (Set.add candidateId projections) later
          | _ when not (usesAny tracked candidate) -> scan (increment index) tracked projections later
          | _ -> None in
     scan 0 (Set.singleton sourceId) Set.empty tail) in
    let tail = match replacement with None -> tail | Some (target, operation) -> List.mapi (fun index (id, current) -> id, if index = target then operation else current) tail in
    visit ((sourceId, sourceOperation) :: processed) tail in
 {block with S.operations = visit [] block.S.operations}
let optimizeFunction typeReg sumReg func =
 let replaced = replaceScalars func in
 {replaced with S.blocks = S.LabelMap.map (reusesInBlock typeReg sumReg replaced) replaced.S.blocks}
