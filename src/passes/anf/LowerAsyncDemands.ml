(* LowerAsyncDemands.ml - Infer pending call interfaces and demand concrete values before ownership. *)
[@@@warning "-4"]

module A = ANF
module M = InliningCommon.TempMap
module F = FunctionIdMap
module S = SpecializationIdentity.FunctionSet
module T = ANFEffects.TempSet
module Names = StringOrder.Map

let ( let* ) = Result.bind

type pending = { payload : AST.semanticType; force : AST.functionId }
type summary = { parameters : bool list; returnsPending : bool; joins : T.t }
type version = { id : AST.functionId; name : string }
type lowered = { functions : A.functionDef list; nextFunctionOrdinal : int64 }

let carrier payload = AST.TSum ("Darklang.Stdlib.__Async.__Value", [ payload ])
let transportName = "Darklang.Stdlib.__Task.__transport"
let delayName = "Darklang.Stdlib.__Task.__delayTransparent"
let specialized base name = String.starts_with ~prefix:(base ^ "_") name
let lookup env = function A.Var id -> M.find_opt id env | _ -> None

let nativeRuntime name =
  List.exists
    (fun prefix -> String.starts_with ~prefix name)
    [
      "Darklang.Stdlib.__Task.";
      "Darklang.Stdlib.__Async.";
      "Darklang.Stdlib.__Network.";
      "__dark_async_";
    ]

let lower registry names ids nextFunctionOrdinal externalDefinitions functions =
  let name id = F.tryFind id names in
  let producer id =
    match name id with
    | Some name -> specialized transportName name || specialized delayName name
    | None -> false
  in
  if not (F.exists (fun id _ -> producer id) names) then
    Ok { functions; nextFunctionOrdinal }
  else
    let definitions =
      Names.fold
        (fun _ (fn : A.functionDef) map -> F.add fn.A.id fn map)
        externalDefinitions F.empty
      |> fun map ->
      List.fold_left
        (fun map (fn : A.functionDef) -> F.add fn.A.id fn map)
        map functions
    in
    let originalResult id =
      match F.tryFind id definitions with
      | Some fn -> fn.A.returnType
      | None -> (
          match F.tryFind id registry with
          | Some (_, AST.TFunction (_, result)) -> result
          | _ -> Crash.crash "Async source operation has no typed signature")
    in
    let helper base payload =
      let name = SpecializationIdentity.specName base [ payload ] in
      match Names.find_opt name ids with
      | Some id -> Ok id
      | None -> Error ("Async runtime specialization is missing: " ^ name)
    in
    let info payload =
      let* force = helper "Darklang.Stdlib.__Task.__forceCurrent" payload in
      Ok { payload; force }
    in
    let summary state id =
      match F.tryFind id state with
      | Some value -> value
      | None -> Crash.crash "Async inference lost a function summary"
    in
    let initial =
      F.map
        (fun _ (fn : A.functionDef) ->
          {
            parameters = List.map (fun _ -> false) fn.A.typedParams;
            returnsPending = false;
            joins = T.empty;
          })
        definitions
    in
    let eligible id =
      match F.tryFind id definitions with
      | Some fn -> not (nativeRuntime fn.A.name)
      | None -> false
    in
    let called (fn : A.functionDef) =
      ANFDeadCodeElimination.getCalledFunctions fn
    in
    let starts =
      F.fold
        (fun starts id fn ->
          if eligible id && S.exists producer (called fn) then S.add id starts
          else starts)
        S.empty definitions
    in
    let rec reach current =
      let next =
        F.fold
          (fun found id fn ->
            if eligible id && not (S.is_empty (S.inter current (called fn)))
            then S.add id found
            else found)
          current definitions
      in
      if S.equal current next then current else reach next
    in
    let startsAsync = reach starts in
    let sourceCall env callee arguments =
      eligible callee
      && (S.mem callee startsAsync
         || List.exists (fun atom -> Option.is_some (lookup env atom)) arguments
         )
    in
    let rec infer (fn : A.functionDef) env body state =
      let update transform =
        let current = summary state fn.A.id in
        F.add fn.A.id (transform current) state
      in
      match body with
      | A.Return atom ->
          if Option.is_some (lookup env atom) then
            update (fun current -> { current with returnsPending = true })
          else state
      | A.Jump (target, atom) ->
          if Option.is_some (lookup env atom) then
            update (fun current ->
                { current with joins = T.add target current.joins })
          else state
      | A.If (_, yes, no) -> infer fn env no (infer fn env yes state)
      | A.Join (parameter, continuation, entry) ->
          let state = infer fn env entry state in
          let env =
            if T.mem parameter.A.id (summary state fn.A.id).joins then
              M.add parameter.A.id parameter.A.typ env
            else M.remove parameter.A.id env
          in
          infer fn env continuation state
      | A.Let (id, operation, rest) ->
          let state, result =
            match operation with
            | A.Call (callee, _) when producer callee ->
                (state, Some (originalResult callee))
            | A.Atom atom -> (state, lookup env atom)
            | A.TypedAtom (atom, typ) ->
                ( state,
                  match lookup env atom with
                  | Some source when source = typ -> Some typ
                  | _ -> None )
            | A.IfValue (_, yes, no) ->
                ( state,
                  match lookup env yes with
                  | Some _ as value -> value
                  | None -> lookup env no )
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when sourceCall env callee arguments ->
                let calleeSummary = summary state callee in
                let actual =
                  List.map
                    (fun atom -> Option.is_some (lookup env atom))
                    arguments
                in
                if List.length actual <> List.length calleeSummary.parameters
                then
                  Crash.crash
                    "Async inference found an invalid direct-call arity";
                let parameters =
                  List.map2 ( || ) actual calleeSummary.parameters
                in
                let state =
                  F.add callee { calleeSummary with parameters } state
                in
                ( state,
                  if calleeSummary.returnsPending then
                    Some (originalResult callee)
                  else None )
            | _ -> (state, None)
          in
          let env =
            match result with
            | Some typ -> M.add id typ env
            | None -> M.remove id env
          in
          infer fn env rest state
    in
    let equal left right =
      left.parameters = right.parameters
      && left.returnsPending = right.returnsPending
      && T.equal left.joins right.joins
    in
    let rec converge state =
      let next =
        F.fold
          (fun state id fn ->
            if not (eligible id) then state
            else
              let current = summary state id in
              let env =
                List.fold_left2
                  (fun env (param : A.typedParam) pending ->
                    if pending then M.add param.A.id param.A.typ env else env)
                  M.empty fn.A.typedParams current.parameters
              in
              infer fn env fn.A.body state)
          state definitions
      in
      if F.forall (fun id value -> equal value (summary state id)) next then
        next
      else converge next
    in
    let summaries = converge initial in
    let modeChanges id =
      let value = summary summaries id in
      value.returnsPending || List.exists Fun.id value.parameters
    in
    let target bindings = function
      | A.FuncRef id -> Some id
      | A.Var id -> M.find_opt id bindings
      | _ -> None
    in
    let nativeEntry id =
      match name id with
      | Some name ->
          List.exists
            (fun base -> specialized ("Darklang.Stdlib.__Task." ^ base) name)
            [ "__runMain"; "__startCurrent"; "__startValueCurrent" ]
      | None -> false
    in
    let rec entries bindings found = function
      | A.Return _ | A.Jump _ -> found
      | A.If (_, yes, no) -> entries bindings (entries bindings found yes) no
      | A.Join (_, continuation, entry) ->
          entries bindings (entries bindings found continuation) entry
      | A.Let (id, expression, rest) ->
          let found =
            match expression with
            | A.Call (callee, [ entry ]) when nativeEntry callee -> (
                match target bindings entry with
                | Some entry -> S.add entry found
                | None -> found)
            | _ -> found
          in
          let bindings =
            match expression with
            | A.ClosureAlloc (callee, _) -> M.add id callee bindings
            | A.Atom atom | A.TypedAtom (atom, _) -> (
                match target bindings atom with
                | Some callee -> M.add id callee bindings
                | None -> M.remove id bindings)
            | _ -> M.remove id bindings
          in
          entries bindings found rest
    in
    let nativeEntries =
      F.fold
        (fun found _ fn -> entries M.empty found fn.A.body)
        S.empty definitions
    in
    let local =
      List.fold_left
        (fun set (fn : A.functionDef) -> S.add fn.A.id set)
        S.empty functions
    in
    let boundary (fn : A.functionDef) =
      S.mem fn.A.id nativeEntries || fn.A.name = "__dark_compiler_program_entry"
    in
    let versions, nativeVersions, nextFunctionOrdinal =
      F.fold
        (fun (versions, nativeVersions, next) id fn ->
          if not (eligible id) then (versions, nativeVersions, next)
          else
            let imported = not (S.mem id local) in
            let needs = modeChanges id || (imported && S.mem id startsAsync) in
            let versions, next =
              if not needs then (versions, next)
              else
                let version =
                  {
                    id = AST.functionId next;
                    name =
                      "__dark_async_values_" ^ Int64.to_string next ^ "."
                      ^ fn.A.name;
                  }
                in
                (F.add id version versions, Int64.succ next)
            in
            if imported && boundary fn && S.mem id startsAsync then
              let version =
                {
                  id = AST.functionId next;
                  name =
                    "__dark_async_native_" ^ Int64.to_string next ^ "."
                    ^ fn.A.name;
                }
              in
              (versions, F.add id version nativeVersions, Int64.succ next)
            else (versions, nativeVersions, next))
        (F.empty, F.empty, nextFunctionOrdinal)
        definitions
    in
    let unsupported functionName where =
      Error
        ("Async transport across " ^ where ^ " is not implemented in "
       ^ functionName)
    in
    let wraps bindings body =
      List.fold_right
        (fun (id, value) body -> A.Let (id, value, body))
        bindings body
    in
    let ready payload atom gen =
      let raw, gen = A.freshVar gen in
      let typed, gen = A.freshVar gen in
      ( A.Var typed,
        [
          (raw, A.TupleAlloc [ A.IntLiteral (A.Int64 0L); atom ]);
          (typed, A.TypedAtom (A.Var raw, carrier payload));
        ],
        gen )
    in
    let forceAtom env atom gen =
      match lookup env atom with
      | None -> (atom, [], gen)
      | Some value ->
          let id, gen = A.freshVar gen in
          (A.Var id, [ (id, A.Call (value.force, [ atom ])) ], gen)
    in
    let forceUses env operation gen =
      let substitutions, before, gen =
        T.fold
          (fun id (substitutions, before, gen) ->
            match M.find_opt id env with
            | None -> (substitutions, before, gen)
            | Some value ->
                let forced, gen = A.freshVar gen in
                ( M.add id forced substitutions,
                  (forced, A.Call (value.force, [ A.Var id ])) :: before,
                  gen ))
          (ANFEffects.cexprTempUses operation)
          (M.empty, [], gen)
      in
      (InliningCommon.renameCExpr substitutions operation, List.rev before, gen)
    in
    let rec joinTypes types = function
      | A.Return _ | A.Jump _ -> types
      | A.If (_, yes, no) -> joinTypes (joinTypes types yes) no
      | A.Let (_, _, rest) -> joinTypes types rest
      | A.Join (parameter, continuation, entry) ->
          joinTypes
            (joinTypes
               (M.add parameter.A.id parameter.A.typ types)
               continuation)
            entry
    in
    let rec expression (fn : A.functionDef) nativeReturn env body gen =
      let signature = summary summaries fn.A.id in
      let returnsCarrier = signature.returnsPending && not nativeReturn in
      let joinPayloads = joinTypes M.empty fn.A.body in
      let prepare payload atom gen =
        match lookup env atom with
        | Some _ -> (atom, [], gen)
        | None -> ready payload atom gen
      in
      match body with
      | A.Return atom -> (
          if returnsCarrier then
            let atom, before, gen = prepare fn.A.returnType atom gen in
            Ok (wraps before (A.Return atom), gen)
          else
            match lookup env atom with
            | None -> Ok (body, gen)
            | Some _ when nativeReturn ->
                let atom, before, gen = forceAtom env atom gen in
                Ok (wraps before (A.Return atom), gen)
            | Some _ -> Error "Async return inference lost a pending result")
      | A.Jump (target, atom) ->
          if T.mem target signature.joins then
            let payload =
              match M.find_opt target joinPayloads with
              | Some typ -> typ
              | None -> Crash.crash "Async join lost its parameter type"
            in
            let atom, before, gen = prepare payload atom gen in
            Ok (wraps before (A.Jump (target, atom)), gen)
          else if Option.is_some (lookup env atom) then
            Error "Async join inference lost a pending argument"
          else Ok (body, gen)
      | A.If (condition, yes, no) ->
          let condition, before, gen = forceAtom env condition gen in
          let* yes, gen = expression fn nativeReturn env yes gen in
          let* no, gen = expression fn nativeReturn env no gen in
          Ok (wraps before (A.If (condition, yes, no)), gen)
      | A.Join (parameter, continuation, entry) ->
          let* value =
            if T.mem parameter.A.id signature.joins then
              Result.map Option.some (info parameter.A.typ)
            else Ok None
          in
          let continuationEnv =
            match value with
            | Some value -> M.add parameter.A.id value env
            | None -> M.remove parameter.A.id env
          in
          let* continuation, gen =
            expression fn nativeReturn continuationEnv continuation gen
          in
          let* entry, gen = expression fn nativeReturn env entry gen in
          let parameter =
            if Option.is_some value then
              { parameter with A.typ = carrier parameter.A.typ }
            else parameter
          in
          Ok (A.Join (parameter, continuation, entry), gen)
      | A.Let (id, operation, rest) ->
          let* operation, before, value, gen =
            match operation with
            | A.Call (callee, arguments) when producer callee ->
                let* value = info (originalResult callee) in
                let* operation, before, gen =
                  match (name callee, arguments) with
                  | Some name, [ argument ] when specialized transportName name
                    ->
                      if Option.is_some (lookup env argument) then
                        unsupported fn.A.name "nested source transport"
                      else
                        Ok
                          ( A.TypedAtom (argument, carrier value.payload),
                            [],
                            gen )
                  | Some name, [ delay; argument ]
                    when specialized delayName name ->
                      let delay, before, gen = forceAtom env delay gen in
                      let argument, added, gen =
                        prepare value.payload argument gen
                      in
                      let* delayHelper =
                        helper "Darklang.Stdlib.__Task.__delayValueCurrent"
                          value.payload
                      in
                      Ok
                        ( A.Call (delayHelper, [ delay; argument ]),
                          before @ added,
                          gen )
                  | _ -> Error "Invalid async source operation arguments"
                in
                Ok (operation, before, Some value, gen)
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when sourceCall env callee arguments ->
                let calleeDefinition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None -> Crash.crash "Async direct call lost its body"
                in
                let calleeSummary = summary summaries callee in
                let rec argumentsLoop arguments parameters modes reversed before
                    gen =
                  match (arguments, parameters, modes) with
                  | [], [], [] -> Ok (List.rev reversed, before, gen)
                  | ( argument :: rest,
                      (parameter : A.typedParam) :: parameters,
                      mode :: modes ) ->
                      if mode then
                        let argument, added, gen =
                          prepare parameter.A.typ argument gen
                        in
                        argumentsLoop rest parameters modes
                          (argument :: reversed) (before @ added) gen
                      else if Option.is_some (lookup env argument) then
                        Error "Async call inference lost a pending parameter"
                      else
                        argumentsLoop rest parameters modes
                          (argument :: reversed) before gen
                  | _ ->
                      Error
                        "Async direct call has an invalid argument interface"
                in
                let* arguments, before, gen =
                  argumentsLoop arguments calleeDefinition.A.typedParams
                    calleeSummary.parameters [] [] gen
                in
                let callee =
                  match F.tryFind callee versions with
                  | Some version -> version.id
                  | None -> callee
                in
                let* value =
                  if calleeSummary.returnsPending then
                    Result.map Option.some (info calleeDefinition.A.returnType)
                  else Ok None
                in
                Ok (A.Call (callee, arguments), before, value, gen)
            | A.ClosureAlloc (callee, captures) ->
                if
                  List.exists
                    (fun atom -> Option.is_some (lookup env atom))
                    captures
                then unsupported fn.A.name "a closure capture"
                else
                  let* callee =
                    if S.mem callee nativeEntries then
                      Ok
                        (match F.tryFind callee nativeVersions with
                        | Some version -> version.id
                        | None -> callee)
                    else if
                      eligible callee && modeChanges callee
                      && S.mem callee startsAsync
                    then unsupported fn.A.name "a callable interface"
                    else if eligible callee && S.mem callee startsAsync then
                      Ok
                        (match F.tryFind callee versions with
                        | Some version -> version.id
                        | None -> callee)
                    else Ok callee
                  in
                  Ok (A.ClosureAlloc (callee, captures), [], None, gen)
            | A.Atom atom -> Ok (operation, [], lookup env atom, gen)
            | A.TypedAtom (atom, typ) -> (
                match lookup env atom with
                | Some value when typ = value.payload ->
                    Ok (A.TypedAtom (atom, carrier typ), [], Some value, gen)
                | _ ->
                    let operation, before, gen = forceUses env operation gen in
                    Ok (operation, before, None, gen))
            | A.IfValue (condition, yes, no) -> (
                let condition, before, gen = forceAtom env condition gen in
                let value =
                  match lookup env yes with
                  | Some _ as value -> value
                  | None -> lookup env no
                in
                match value with
                | None -> Ok (A.IfValue (condition, yes, no), before, None, gen)
                | Some value ->
                    let yes, left, gen = prepare value.payload yes gen in
                    let no, right, gen = prepare value.payload no gen in
                    Ok
                      ( A.IfValue (condition, yes, no),
                        before @ left @ right,
                        Some value,
                        gen ))
            | A.ClosureCall (callee, arguments)
            | A.IndirectCall (callee, arguments) ->
                if
                  List.exists
                    (fun atom -> Option.is_some (lookup env atom))
                    arguments
                then unsupported fn.A.name "an indirect argument interface"
                else
                  let callee, before, gen = forceAtom env callee gen in
                  let operation =
                    match operation with
                    | A.ClosureCall _ -> A.ClosureCall (callee, arguments)
                    | _ -> A.IndirectCall (callee, arguments)
                  in
                  Ok (operation, before, None, gen)
            | _ ->
                let hasPending =
                  T.exists
                    (fun id -> M.mem id env)
                    (ANFEffects.cexprTempUses operation)
                in
                if not hasPending then Ok (operation, [], None, gen)
                else
                  let* () =
                    match operation with
                    | A.Prim _ | A.UnaryPrim _ | A.TupleGet _ | A.RecordGet _
                    | A.StringConcat _ | A.CanonicalBufferEq _ | A.Print _
                    | A.StdoutWrite _ | A.RuntimeErrorString _ | A.FloatSqrt _
                    | A.FloatAbs _ | A.FloatNeg _ | A.Int64ToFloat _
                    | A.FloatToInt64 _ | A.FloatToBits _ | A.FloatToString _
                    | A.RawAlloc _ | A.MappedAlloc _ | A.RawFree _
                    | A.MappedFree _ | A.RawGet _ | A.RawTake _ | A.RawGetByte _
                    | A.RawWriteWord _ | A.RawWriteByte _ | A.StringToRawPtr _
                    | A.RawPtrToString _ | A.BlobToRawPtr _ | A.RawPtrToBlob _
                    | A.RawPtrToInt128 _ | A.RawPtrToUInt128 _
                    | A.DictToRawPtr _ | A.ListToRawPtr _
                    | A.FixedBlockToRawPtr _ | A.RawPtrToDict _
                    | A.RawPtrToList _ | A.Sleep _ | A.CliNative _
                    | A.FileReadBlob _ | A.FileExists _ | A.FileWriteBlob _
                    | A.FileAppendText _ | A.FileDelete _
                    | A.FileCreateDirectory _ | A.FileSetExecutable _
                    | A.FileWriteFromPtr _ ->
                        Ok ()
                    | _ -> unsupported fn.A.name "this call or storage boundary"
                  in
                  let operation, before, gen = forceUses env operation gen in
                  Ok (operation, before, None, gen)
          in
          let env =
            match value with
            | Some value -> M.add id value env
            | None -> M.remove id env
          in
          let* rest, gen = expression fn nativeReturn env rest gen in
          Ok (wraps before (A.Let (id, operation, rest)), gen)
    in
    let initialTemp =
      1
      + F.fold
          (fun maximum _ fn -> max maximum (ANF_to_MIR.maxTempIdInFunction fn))
          0 definitions
    in
    let rewrite (fn : A.functionDef) nativeReturn modes gen =
      let* env =
        List.fold_left2
          (fun result (parameter : A.typedParam) mode ->
            let* env = result in
            if mode then
              let* value = info parameter.A.typ in
              Ok (M.add parameter.A.id value env)
            else Ok env)
          (Ok M.empty) fn.A.typedParams modes
      in
      expression fn nativeReturn env fn.A.body gen
    in
    let* originals, gen =
      List.fold_left
        (fun result (fn : A.functionDef) ->
          let* values, gen = result in
          if
            nativeRuntime fn.A.name || (modeChanges fn.A.id && not (boundary fn))
          then Ok (fn :: values, gen)
          else
            let* body, gen =
              rewrite fn (boundary fn)
                (List.map (fun _ -> false) fn.A.typedParams)
                gen
            in
            Ok ({ fn with A.body } :: values, gen))
        (Ok ([], A.VarGen initialTemp))
        functions
    in
    let clone nativeReturn (fn : A.functionDef) (version : version) gen =
      let state = summary summaries fn.A.id in
      let modes =
        if nativeReturn then List.map (fun _ -> false) state.parameters
        else state.parameters
      in
      let* body, gen = rewrite fn nativeReturn modes gen in
      let parameters, substitutions, gen =
        List.fold_left2
          (fun (parameters, substitutions, gen) (parameter : A.typedParam) mode
             ->
            let id, gen = A.freshVar gen in
            let typ =
              if mode then carrier parameter.A.typ else parameter.A.typ
            in
            ( { A.id; typ } :: parameters,
              M.add parameter.A.id id substitutions,
              gen ))
          ([], M.empty, gen) fn.A.typedParams modes
      in
      let body, gen = InliningCommon.renameExpr substitutions gen body in
      let returnType =
        if state.returnsPending && not nativeReturn then carrier fn.A.returnType
        else fn.A.returnType
      in
      Ok
        ( {
            fn with
            A.id = version.id;
            name = version.name;
            typedParams = List.rev parameters;
            returnType;
            returnOwnership = A.OwnedReturn;
            body;
          },
          gen )
    in
    let addVersions nativeReturn versions result =
      F.fold
        (fun result source version ->
          let* values, gen = result in
          let definition =
            match F.tryFind source definitions with
            | Some definition -> definition
            | None -> Crash.crash "Async version lost its source definition"
          in
          let* value, gen = clone nativeReturn definition version gen in
          Ok (value :: values, gen))
        result versions
    in
    let* cloned, gen = addVersions false versions (Ok ([], gen)) in
    let* cloned, _ = addVersions true nativeVersions (Ok (cloned, gen)) in
    let functions =
      ANFDeadCodeElimination.filterReachableFunctions local
        (List.rev originals @ cloned)
    in
    Ok { functions; nextFunctionOrdinal }
