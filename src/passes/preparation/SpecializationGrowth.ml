(* SpecializationGrowth.ml - Reject reachable constructor-growing generic type-flow cycles. *)
module S = SpecializationIdentity
module C = CheckedAST
module M = StringOrder.Map

module Slot = struct
  type t = string * string

  let compare = Stdlib.compare
end

module Slots = Set.Make (Slot)
module Graph = Map.Make (Slot)

type edge = { source : Slot.t; target : Slot.t; grows : bool }

(* A parameter permutation preserves depth; a constructor around a parameter
   increases it. Constants carry no flow, so a concrete reset breaks a cycle. *)
let rec variables nested = function
  | AST.TVar name -> [ (name, nested) ]
  | AST.TFunction (parameters, result) ->
      List.concat_map (variables true) (result :: parameters)
  | AST.TTuple types | AST.TRecord (_, types) | AST.TSum (_, types) ->
      List.concat_map (variables true) types
  | AST.TList typ | AST.TStream typ -> variables true typ
  | AST.TDict (key, value) -> variables true key @ variables true value
  | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
  | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
  | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar
  | AST.TDateTime | AST.TUnit | AST.TNever | AST.TInferenceVar _
  | AST.TInternalRawPtr ->
      []

let check definitions calls initial =
  let rec collect pending visited edges =
    match StringOrder.Set.min_elt_opt pending with
    | None -> edges
    | Some name ->
      let pending = StringOrder.Set.remove name pending in
      if StringOrder.Set.mem name visited then collect pending visited edges
      else
        let visited = StringOrder.Set.add name visited in
        match M.find_opt name definitions with
        | None -> collect pending visited edges
        | Some (artifact : S.genericFunctionArtifact) ->
            let pending, edges =
              S.SpecSet.fold
                (fun (callee, types) (pending, edges) ->
                  let pending = StringOrder.Set.add callee pending in
                  match M.find_opt callee definitions with
                  | None -> (pending, edges)
                  | Some (target : S.genericFunctionArtifact) ->
                      let flows =
                        List.combine target.S.func.C.typeParams types
                        |> List.concat_map (fun (parameter, typ) ->
                            variables false typ
                            |> List.filter_map (fun (source, grows) ->
                                if List.mem source artifact.S.func.C.typeParams
                                then
                                  Some
                                    {
                                      source = (name, source);
                                      target = (callee, parameter);
                                      grows;
                                    }
                                else None))
                      in
                      (pending, flows @ edges))
                (calls artifact) (pending, edges)
            in
            collect pending visited edges
  in
  let pending =
    S.SpecSet.fold
      (fun (name, _) names -> StringOrder.Set.add name names)
      initial StringOrder.Set.empty
  in
  let edges = collect pending StringOrder.Set.empty [] in
  let graph =
    List.fold_left
      (fun graph edge ->
        Graph.update edge.source
          (fun targets ->
            Some
              (Slots.add edge.target
                 (Option.value targets ~default:Slots.empty)))
          graph)
      Graph.empty edges
  in
  let rec reaches target pending visited =
    match Slots.min_elt_opt pending with
    | None -> false
    | Some current ->
      if Slot.compare current target = 0 then true
      else
        let pending = Slots.remove current pending in
        if Slots.mem current visited then reaches target pending visited
        else
          let successors =
            Option.value (Graph.find_opt current graph) ~default:Slots.empty
          in
          reaches target (Slots.union pending successors)
            (Slots.add current visited)
  in
  match
    List.find_opt
      (fun edge ->
        edge.grows
        && reaches edge.source (Slots.singleton edge.target) Slots.empty)
      edges
  with
  | None -> Ok ()
  | Some edge ->
      let name, parameter = edge.source in
      Error
        ("Unbounded generic specialization: recursive type argument '"
       ^ parameter ^ "' of '" ^ name
       ^ "' grows through a type constructor; native compilation would require infinitely many instances")
