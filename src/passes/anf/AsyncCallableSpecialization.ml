(* AsyncCallableSpecialization.ml - Expose known source callbacks before transport inference. *)
[@@@warning "-4"]

module A = ANF
module S = SSAANF
module F = FunctionIdMap
module Ids = SpecializationIdentity.FunctionSet
module M = InliningCommon.TempMap
module N = StringOrder.Map

let ( let* ) = Result.bind

type prepared = {
  functions : A.functionDef list;
  externalDefinitions : A.functionDef N.t;
  nextFunctionOrdinal : int64;
}

let sourceHelper (fn : A.functionDef) =
  (not (String.starts_with ~prefix:"Darklang.Stdlib." fn.A.name))
  && not (String.starts_with ~prefix:"__dark_async_" fn.A.name)

let runtime (fn : A.functionDef) =
  List.exists
    (fun prefix -> String.starts_with ~prefix fn.A.name)
    [
      "Darklang.Stdlib.__Task.";
      "Darklang.Stdlib.__Async.";
      "Darklang.Stdlib.__Network.";
      "__dark_async_";
    ]

let callbackParameter (fn : A.functionDef) =
  List.exists
    (fun (parameter : A.typedParam) ->
      match parameter.A.typ with AST.TFunction _ -> true | _ -> false)
    fn.A.typedParams

let rec callbackTargets found = function
  | A.Return atom | A.Jump (_, atom) -> (
      match atom with A.FuncRef target -> Ids.add target found | _ -> found)
  | A.If (_, yes, no) -> callbackTargets (callbackTargets found yes) no
  | A.Join (_, continuation, entry) ->
      callbackTargets (callbackTargets found continuation) entry
  | A.Let (_, operation, rest) ->
      let atom found = function
        | A.FuncRef id -> Ids.add id found
        | _ -> found
      in
      let found =
        match operation with
        | A.ClosureAlloc (target, captures) ->
            List.fold_left atom (Ids.add target found) captures
        | A.Atom value | A.TypedAtom (value, _) -> atom found value
        | A.Call (_, args)
        | A.BorrowedCall (_, args)
        | A.TailCall (_, args)
        | A.ClosureCall (_, args)
        | A.ClosureTailCall (_, args) ->
            List.fold_left atom found args
        | _ -> found
      in
      callbackTargets found rest

(* This specialization changes operations and function signatures, while keeping
   every block edge and parameter intact. Replay it on the matching structured
   source rather than introducing a general SSA-to-ANF conversion. *)
let restore source view (rewritten : S.functionDef) =
  if
    not
      (S.LabelMap.equal
         (fun left right ->
           left.S.parameters = right.S.parameters
           && left.S.terminator = right.S.terminator)
         view.S.blocks rewritten.S.blocks)
  then
    Crash.crash "Async callback specialization changed structured control flow";
  let operations =
    S.LabelMap.fold
      (fun _ block operations ->
        List.fold_left
          (fun operations (id, expression) -> M.add id expression operations)
          operations block.S.operations)
      rewritten.S.blocks M.empty
  in
  let rec body = function
    | A.Let (id, _, rest) ->
        let operation =
          match M.find_opt id operations with
          | Some operation -> operation
          | None ->
              Crash.crash
                "Async callback specialization removed a source definition"
        in
        A.Let (id, operation, body rest)
    | A.If (condition, yes, no) -> A.If (condition, body yes, body no)
    | A.Join (parameter, continuation, entry) ->
        A.Join (parameter, body continuation, body entry)
    | (A.Return _ | A.Jump _) as expression -> expression
  in
  {
    A.id = rewritten.S.id;
    name = rewritten.S.name;
    typedParams = rewritten.S.typedParams;
    returnType = rewritten.S.returnType;
    returnOwnership = rewritten.S.returnOwnership;
    body = body source.A.body;
  }

let freshen functions =
  let maximum =
    List.fold_left
      (fun maximum fn -> max maximum (ANF_to_MIR.maxTempIdInFunction fn))
      4000 functions
  in
  let reversed, _ =
    List.fold_left
      (fun (functions, gen) (fn : A.functionDef) ->
        let params, substitutions, gen =
          List.fold_left
            (fun (params, substitutions, gen) (parameter : A.typedParam) ->
              let id, gen = A.freshVar gen in
              ( { parameter with A.id } :: params,
                M.add parameter.A.id id substitutions,
                gen ))
            ([], M.empty, gen) fn.A.typedParams
        in
        let body, gen = InliningCommon.renameExpr substitutions gen fn.A.body in
        ({ fn with A.typedParams = List.rev params; body } :: functions, gen))
      ([], A.VarGen (maximum + 1))
      functions
  in
  List.rev reversed

let prepare reservedIds nextFunctionOrdinal context externalDefinitions
    functions =
  let unchanged () =
    Ok { functions; externalDefinitions; nextFunctionOrdinal }
  in
  if
    not
      (N.exists
         (fun name _ -> AsyncSourceHooks.isProducerName name)
         reservedIds)
  then unchanged ()
  else
    let definitions =
      List.fold_left
        (fun definitions (fn : A.functionDef) -> F.add fn.A.id fn definitions)
        (N.fold
           (fun _ (fn : A.functionDef) definitions ->
             F.add fn.A.id fn definitions)
           externalDefinitions F.empty)
        functions
    in
    let eligible id =
      match F.tryFind id definitions with
      | Some fn -> not (runtime fn)
      | None -> false
    in
    let rec reachable pending seen =
      if Ids.is_empty pending then seen
      else
        let id = Ids.min_elt pending in
        let pending = Ids.remove id pending in
        if Ids.mem id seen || not (eligible id) then reachable pending seen
        else
          let fn =
            match F.tryFind id definitions with
            | Some fn -> fn
            | None -> Crash.crash "Async callback traversal lost its definition"
          in
          let direct =
            ANFDeadCodeElimination.getCalledFunctions fn
            |> Ids.filter (fun id ->
                match F.tryFind id definitions with
                | Some fn -> sourceHelper fn
                | None -> false)
          in
          let called = Ids.union direct (callbackTargets Ids.empty fn.A.body) in
          reachable (Ids.union pending called) (Ids.add id seen)
    in
    let selected =
      reachable
        (Ids.of_list (List.map (fun (fn : A.functionDef) -> fn.A.id) functions))
        Ids.empty
    in
    (* Precompiled stdlib interfaces retain their existing physical adapters.
       Ordinary source helpers have no such boundary and can expose callbacks. *)
    let helpers =
      Ids.filter
        (fun id ->
          match F.tryFind id definitions with
          | Some fn -> sourceHelper fn && callbackParameter fn
          | None -> false)
        selected
    in
    if Ids.is_empty helpers then unchanged ()
    else
      let context = context () in
      let* sources =
        Ids.fold
          (fun id result ->
            let* prepared = result in
            let fn =
              match F.tryFind id definitions with
              | Some fn -> fn
              | None -> Crash.crash "Async callback preparation lost its source"
            in
            let* source, view =
              S.prepareFunctionBeforeRC
                (ANF_to_MIR.maxTempIdInFunction fn)
                context fn
            in
            Ok (F.add id (source, view) prepared))
          selected (Ok F.empty)
      in
      let views = F.toList sources |> List.map (fun (_, (_, view)) -> view) in
      let specialized =
        SSAHigherOrderSpecialization
        .specializeProgramWithExternalFunctionsAndNames
          ~purpose:(SSAHigherOrderSpecialization.AsyncInterfaces helpers)
          reservedIds nextFunctionOrdinal [] views
      in
      if F.isEmpty specialized.SSAHigherOrderSpecialization.cloneOrigins then
        unchanged ()
      else
        let rewritten =
          List.fold_left
            (fun rewritten (fn : S.functionDef) ->
              let original =
                match
                  F.tryFind fn.S.id
                    specialized.SSAHigherOrderSpecialization.cloneOrigins
                with
                | Some original -> original
                | None -> fn.S.id
              in
              let source, view =
                match F.tryFind original sources with
                | Some pair -> pair
                | None ->
                    Crash.crash
                      "Async callback specialization lost its structured source"
              in
              F.add fn.S.id (restore source view fn) rewritten)
            F.empty specialized.SSAHigherOrderSpecialization.functions
        in
        let local =
          Ids.of_list (List.map (fun (fn : A.functionDef) -> fn.A.id) functions)
        in
        let generated =
          F.toList rewritten
          |> List.filter_map (fun (id, fn) ->
              if F.containsKey id definitions then None else Some fn)
        in
        let functions =
          List.map
            (fun (fn : A.functionDef) ->
              match F.tryFind fn.A.id rewritten with
              | Some fn -> fn
              | None -> fn)
            functions
        in
        let externalDefinitions =
          F.fold
            (fun externalDefinitions id fn ->
              if Ids.mem id local || not (F.containsKey id definitions) then
                externalDefinitions
              else N.add fn.A.name fn externalDefinitions)
            externalDefinitions rewritten
        in
        let functions = freshen (functions @ generated) in
        let nextFunctionOrdinal =
          List.fold_left
            (fun next (fn : A.functionDef) ->
              let after =
                AST.nextFunctionIdOrdinal (AST.functionIdValue fn.A.id)
              in
              if Int64.unsigned_compare next after >= 0 then next else after)
            nextFunctionOrdinal functions
        in
        Ok { functions; externalDefinitions; nextFunctionOrdinal }
