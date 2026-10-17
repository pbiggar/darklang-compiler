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

type summary = {
  parameters : bool list;
  captures : bool list;
  returnsPending : bool;
  joins : T.t;
}

type version = { id : AST.functionId; name : string }
type lowered = { functions : A.functionDef list; nextFunctionOrdinal : int64 }

let carrier payload = AST.TSum ("Darklang.Stdlib.__Async.__Value", [ payload ])
let transportName = "Darklang.Stdlib.__Task.__transport"
let delayName = "Darklang.Stdlib.__Task.__delayTransparent"
let specialized base name = String.starts_with ~prefix:(base ^ "_") name
let lookup env = function A.Var id -> M.find_opt id env | _ -> None

(* Stored fields retain independent pending layouts. Nominal records use a
   structural physical type while local; native adapters rebuild their source
   descriptor after materializing fields. Ordinary aggregate interfaces still
   require a coordinated signature migration. *)
type storageKind = TupleStorage | RecordStorage of A.recordDescriptor

type 'a storedSlot = PendingSlot of 'a | NestedStorage of 'a storage
and 'a storage = { kind : storageKind; fields : 'a storedSlot option list }

let storageField storage index =
  match
    List.find_opt
      (fun (offset, _) -> offset = index)
      (List.mapi (fun i field -> (i, field)) storage.fields)
  with
  | Some (_, field) -> field
  | None -> Crash.crash "Async storage projection is outside its layout"

let storageFacts env stores operation =
  let allocation kind values =
    let fields =
      List.map
        (fun atom ->
          match lookup env atom with
          | Some value -> Some (PendingSlot value)
          | None ->
              Option.map
                (fun fields -> NestedStorage fields)
                (lookup stores atom))
        values
    in
    if List.exists Option.is_some fields then Some { kind; fields } else None
  in
  match operation with
  | A.TupleAlloc values -> allocation TupleStorage values
  | A.RecordAlloc (({ A.valueType = AST.TRecord _; _ } as descriptor), values)
  | A.RecordClone (({ A.valueType = AST.TRecord _; _ } as descriptor), _, values)
    ->
      allocation (RecordStorage descriptor) values
  | A.Atom atom | A.TypedAtom (atom, _) -> lookup stores atom
  | A.TupleGet (atom, index) | A.RecordGet (_, atom, index) -> (
      match lookup stores atom with
      | Some fields -> (
          match storageField fields index with
          | Some (NestedStorage fields) -> Some fields
          | _ -> None)
      | None -> None)
  | _ -> None

let bindStorage env stores id operation =
  match storageFacts env stores operation with
  | Some fields -> M.add id fields stores
  | None -> M.remove id stores

let rec fieldRepresentations storage types =
  if List.length types <> List.length storage.fields then
    Crash.crash "Async typed storage has an inconsistent layout";
  List.map2
    (fun typ slot ->
      match slot with
      | None -> typ
      | Some (PendingSlot _) -> carrier typ
      | Some (NestedStorage storage) -> storageType storage typ)
    types storage.fields

and storageType storage source =
  let types =
    match (storage.kind, source) with
    | TupleStorage, AST.TTuple types -> types
    | RecordStorage descriptor, AST.TRecord _ ->
        List.map snd descriptor.A.fields
    | _ -> Crash.crash "Async storage source type does not match its layout"
  in
  AST.TTuple (fieldRepresentations storage types)

let storageDescriptor storage (descriptor : A.recordDescriptor) =
  let types = fieldRepresentations storage (List.map snd descriptor.A.fields) in
  {
    descriptor with
    A.fields =
      List.map2 (fun (name, _) typ -> (name, typ)) descriptor.A.fields types;
    valueType = AST.TTuple types;
  }

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
    let initial =
      F.map
        (fun _ (fn : A.functionDef) ->
          {
            parameters = List.map (fun _ -> false) fn.A.typedParams;
            captures =
              (match fn.A.typedParams with
              | { A.typ = AST.TTuple (AST.TInt64 :: fields); _ } :: _ ->
                  List.map (fun _ -> false) fields
              | _ -> []);
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
    let rec closureBindings bindings = function
      | A.Return _ | A.Jump _ -> bindings
      | A.If (_, yes, no) -> closureBindings (closureBindings bindings yes) no
      | A.Join (_, continuation, entry) ->
          closureBindings (closureBindings bindings entry) continuation
      | A.Let (id, operation, rest) ->
          let callee =
            match operation with
            | A.ClosureAlloc (callee, _) -> Some callee
            | A.Atom atom | A.TypedAtom (atom, _) -> target bindings atom
            | _ -> None
          in
          let bindings =
            match callee with
            | Some callee -> M.add id callee bindings
            | None -> bindings
          in
          closureBindings bindings rest
    in
    let callables =
      F.map (fun _ fn -> closureBindings M.empty fn.A.body) definitions
    in
    let callable (fn : A.functionDef) atom =
      match F.tryFind fn.A.id callables with
      | Some bindings -> target bindings atom
      | None -> Crash.crash "Async inference lost callable bindings"
    in
    let captureType (fn : A.functionDef) state atom index =
      match (fn.A.typedParams, atom) with
      | { A.id; typ = AST.TTuple (_ :: fields) } :: _, A.Var source
        when id = source && index > 0 ->
          let modes = (summary state fn.A.id).captures in
          if not (List.exists Fun.id modes) then None
          else
            List.combine fields modes
            |> List.mapi (fun offset value -> (offset + 1, value))
            |> List.find_map (fun (offset, (typ, pending)) ->
                if offset = index && pending then Some typ else None)
      | _ -> None
    in
    let rec infer (fn : A.functionDef) env stores body state =
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
      | A.If (_, yes, no) ->
          infer fn env stores no (infer fn env stores yes state)
      | A.Join (parameter, continuation, entry) ->
          let state = infer fn env stores entry state in
          let env =
            if T.mem parameter.A.id (summary state fn.A.id).joins then
              M.add parameter.A.id parameter.A.typ env
            else M.remove parameter.A.id env
          in
          infer fn env stores continuation state
      | A.Let (id, operation, rest) ->
          let state, result =
            match operation with
            | A.Call (callee, _) when producer callee ->
                (state, Some (originalResult callee))
            | A.ClosureAlloc (callee, captures) when eligible callee ->
                let current = summary state callee in
                let actual =
                  List.map
                    (fun atom -> Option.is_some (lookup env atom))
                    captures
                in
                if List.length actual <> List.length current.captures then
                  Crash.crash "Async closure capture layout is inconsistent";
                let captures = List.map2 ( || ) actual current.captures in
                (F.add callee { current with captures } state, None)
            | A.TupleGet (atom, index) ->
                ( state,
                  match lookup stores atom with
                  | Some fields -> (
                      match storageField fields index with
                      | Some (PendingSlot value) -> Some value
                      | _ -> None)
                  | None -> captureType fn state atom index )
            | A.RecordGet (_, atom, index) ->
                ( state,
                  match lookup stores atom with
                  | Some fields -> (
                      match storageField fields index with
                      | Some (PendingSlot value) -> Some value
                      | _ -> None)
                  | None -> None )
            | A.ClosureCall (closure, arguments) -> (
                match callable fn closure with
                | Some callee when eligible callee ->
                    let current = summary state callee in
                    let actual =
                      false
                      :: List.map
                           (fun atom -> Option.is_some (lookup env atom))
                           arguments
                    in
                    if List.length actual <> List.length current.parameters then
                      Crash.crash
                        "Async closure argument layout is inconsistent";
                    let parameters =
                      List.map2 ( || ) actual current.parameters
                    in
                    ( F.add callee { current with parameters } state,
                      if
                        current.returnsPending
                        && not (S.mem callee nativeEntries)
                      then Some (originalResult callee)
                      else None )
                | _ -> (state, None))
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
          let stores = bindStorage env stores id operation in
          let env =
            match result with
            | Some typ -> M.add id typ env
            | None -> M.remove id env
          in
          infer fn env stores rest state
    in
    let equal left right =
      left.parameters = right.parameters
      && left.captures = right.captures
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
              infer fn env M.empty fn.A.body state)
          state definitions
      in
      if F.forall (fun id value -> equal value (summary state id)) next then
        next
      else converge next
    in
    let summaries = converge initial in
    let modeChanges id =
      let value = summary summaries id in
      value.returnsPending
      || List.exists Fun.id value.parameters
      || List.exists Fun.id value.captures
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
            if
              boundary fn
              && ((imported && S.mem id startsAsync)
                 || List.exists Fun.id (summary summaries id).captures)
            then
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
    let changedCallable fn atom =
      match callable fn atom with
      | Some callee when eligible callee && not (S.mem callee nativeEntries)
        -> (
          let state = summary summaries callee in
          state.returnsPending
          ||
          match state.parameters with
          | _ :: values -> List.exists Fun.id values
          | [] -> false)
      | _ -> false
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
    let rec materializeStorage fields atom gen =
      let values, before, gen =
        List.fold_left
          (fun (values, before, gen) (index, pending) ->
            let projected, gen = A.freshVar gen in
            let projection =
              match fields.kind with
              | TupleStorage -> A.TupleGet (atom, index)
              | RecordStorage descriptor ->
                  A.RecordGet (storageDescriptor fields descriptor, atom, index)
            in
            let before = before @ [ (projected, projection) ] in
            match pending with
            | None -> (A.Var projected :: values, before, gen)
            | Some (PendingSlot value) ->
                let forced, gen = A.freshVar gen in
                ( A.Var forced :: values,
                  before
                  @ [ (forced, A.Call (value.force, [ A.Var projected ])) ],
                  gen )
            | Some (NestedStorage fields) ->
                let value, added, gen =
                  materializeStorage fields (A.Var projected) gen
                in
                (value :: values, before @ added, gen))
          ([], [], gen)
          (List.mapi (fun i value -> (i, value)) fields.fields)
      in
      let result, gen = A.freshVar gen in
      let allocation =
        match fields.kind with
        | TupleStorage -> A.TupleAlloc (List.rev values)
        | RecordStorage descriptor -> A.RecordAlloc (descriptor, List.rev values)
      in
      (A.Var result, before @ [ (result, allocation) ], gen)
    in
    let forceUses env stores operation gen =
      let substitutions, before, gen =
        T.fold
          (fun id (substitutions, before, gen) ->
            let atom, added, gen =
              match M.find_opt id env with
              | Some value ->
                  let forced, gen = A.freshVar gen in
                  ( A.Var forced,
                    [ (forced, A.Call (value.force, [ A.Var id ])) ],
                    gen )
              | None -> (
                  match M.find_opt id stores with
                  | Some fields -> materializeStorage fields (A.Var id) gen
                  | None -> (A.Var id, [], gen))
            in
            match atom with
            | A.Var forced when forced <> id ->
                (M.add id forced substitutions, before @ added, gen)
            | _ -> (substitutions, before, gen))
          (ANFEffects.cexprTempUses operation)
          (M.empty, [], gen)
      in
      (InliningCommon.renameCExpr substitutions operation, before, gen)
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
    let rec expression (fn : A.functionDef) nativeReturn env stores body gen =
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
          if Option.is_some (lookup stores atom) then
            match lookup stores atom with
            | Some fields when nativeReturn ->
                let atom, before, gen = materializeStorage fields atom gen in
                Ok (wraps before (A.Return atom), gen)
            | _ -> unsupported fn.A.name "an aggregate result interface"
          else if changedCallable fn atom then
            unsupported fn.A.name "an escaping callable result"
          else if returnsCarrier then
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
          if Option.is_some (lookup stores atom) then
            unsupported fn.A.name "an aggregate join"
          else if changedCallable fn atom then
            unsupported fn.A.name "a callable join"
          else if T.mem target signature.joins then
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
          let* yes, gen = expression fn nativeReturn env stores yes gen in
          let* no, gen = expression fn nativeReturn env stores no gen in
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
            expression fn nativeReturn continuationEnv stores continuation gen
          in
          let* entry, gen = expression fn nativeReturn env stores entry gen in
          let parameter =
            if Option.is_some value then
              { parameter with A.typ = carrier parameter.A.typ }
            else parameter
          in
          Ok (A.Join (parameter, continuation, entry), gen)
      | A.Let (id, operation, rest) ->
          let* () =
            let storageEscape =
              match operation with
              | A.Call (_, args)
              | A.BorrowedCall (_, args)
              | A.TailCall (_, args)
              | A.ClosureAlloc (_, args)
              | A.ClosureCall (_, args)
              | A.IndirectCall (_, args) ->
                  List.exists
                    (fun atom -> Option.is_some (lookup stores atom))
                    args
              | A.IfValue (_, yes, no) ->
                  Option.is_some (lookup stores yes)
                  || Option.is_some (lookup stores no)
              | A.TypedAtom (atom, typ) -> (
                  Option.is_some (lookup stores atom)
                  &&
                  match (lookup stores atom, typ) with
                  | Some { kind = TupleStorage; _ }, AST.TTuple _ -> false
                  | Some { kind = RecordStorage _; _ }, AST.TRecord _ -> false
                  | _ -> true)
              | _ -> false
            in
            if storageEscape then
              unsupported fn.A.name "an aggregate storage or call interface"
            else Ok ()
          in
          let storedValue = storageFacts env stores operation in
          let* () =
            let escape =
              match operation with
              | A.Call (_, args)
              | A.BorrowedCall (_, args)
              | A.TailCall (_, args)
              | A.ClosureAlloc (_, args)
              | A.TupleAlloc args
              | A.RecordAlloc (_, args) ->
                  List.exists (changedCallable fn) args
              | _ -> false
            in
            if escape then
              unsupported fn.A.name "an escaping callable interface"
            else Ok ()
          in
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
            | A.ClosureAlloc (callee, captures) when eligible callee ->
                let definition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None -> Crash.crash "Async closure lost its source body"
                in
                let fields =
                  match definition.A.typedParams with
                  | { A.typ = AST.TTuple (_ :: fields); _ } :: _ -> fields
                  | _ ->
                      Crash.crash "Async closure has no environment parameter"
                in
                let modes = (summary summaries callee).captures in
                let captures, before, gen =
                  List.fold_left2
                    (fun (values, before, gen) (atom, typ) mode ->
                      let atom, added, gen =
                        if mode then prepare typ atom gen else (atom, [], gen)
                      in
                      (atom :: values, before @ added, gen))
                    ([], [], gen)
                    (if List.length captures = List.length fields then
                       List.combine captures fields
                     else
                       Crash.crash
                         ("Async capture count mismatch in " ^ definition.A.name))
                    modes
                in
                let versions =
                  if S.mem callee nativeEntries then nativeVersions
                  else versions
                in
                let callee =
                  match F.tryFind callee versions with
                  | Some version -> version.id
                  | None -> callee
                in
                Ok
                  (A.ClosureAlloc (callee, List.rev captures), before, None, gen)
            | A.RecordAlloc (descriptor, values)
            | A.RecordClone (descriptor, _, values)
              when Option.is_some storedValue ->
                let storage =
                  match storedValue with
                  | Some storage -> storage
                  | None ->
                      Crash.crash "Async record allocation lost its layout"
                in
                Ok
                  ( A.RecordAlloc (storageDescriptor storage descriptor, values),
                    [],
                    None,
                    gen )
            | A.RecordClone (descriptor, original, values)
              when Option.is_some (lookup stores original) ->
                Ok (A.RecordAlloc (descriptor, values), [], None, gen)
            | A.RecordGet (descriptor, atom, index)
              when Option.is_some (lookup stores atom) ->
                let storage =
                  match lookup stores atom with
                  | Some storage -> storage
                  | None ->
                      Crash.crash "Async record projection lost its layout"
                in
                let value =
                  match storageField storage index with
                  | Some (PendingSlot value) -> Some value
                  | _ -> None
                in
                Ok
                  ( A.RecordGet
                      (storageDescriptor storage descriptor, atom, index),
                    [],
                    value,
                    gen )
            | A.TupleAlloc _ -> Ok (operation, [], None, gen)
            | A.TupleGet (atom, index) -> (
                let value =
                  match lookup stores atom with
                  | Some fields -> (
                      match storageField fields index with
                      | Some (PendingSlot value) -> Some value
                      | _ -> None)
                  | None -> None
                in
                match (value, lookup stores atom) with
                | Some value, _ -> Ok (operation, [], Some value, gen)
                | None, Some _ -> Ok (operation, [], None, gen)
                | None, None -> (
                    match captureType fn summaries atom index with
                    | Some payload ->
                        let* value = info payload in
                        Ok (operation, [], Some value, gen)
                    | None ->
                        let operation, before, gen =
                          forceUses env stores operation gen
                        in
                        Ok (operation, before, None, gen)))
            | A.Atom atom -> Ok (operation, [], lookup env atom, gen)
            | A.TypedAtom (atom, typ) when Option.is_some (lookup stores atom)
              ->
                let fields =
                  match lookup stores atom with
                  | Some fields -> fields
                  | None -> Crash.crash "Async typed storage lost its fields"
                in
                let typ = storageType fields typ in
                Ok (A.TypedAtom (atom, typ), [], None, gen)
            | A.TypedAtom (atom, typ) -> (
                match lookup env atom with
                | Some value when typ = value.payload ->
                    Ok (A.TypedAtom (atom, carrier typ), [], Some value, gen)
                | _ ->
                    let operation, before, gen =
                      forceUses env stores operation gen
                    in
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
            | A.ClosureCall (closure, arguments)
              when match callable fn closure with
                   | Some callee -> eligible callee
                   | None -> false ->
                let callee =
                  match callable fn closure with
                  | Some callee -> callee
                  | None -> Crash.crash "Async closure call lost its target"
                in
                let state = summary summaries callee in
                let definition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None ->
                      Crash.crash "Async closure call lost its source body"
                in
                let parameters, modes =
                  match (definition.A.typedParams, state.parameters) with
                  | _ :: parameters, _ :: modes -> (parameters, modes)
                  | _ ->
                      Crash.crash
                        "Async closure call has no environment parameter"
                in
                let arguments, before, gen =
                  List.fold_left2
                    (fun (values, before, gen)
                         (atom, (parameter : A.typedParam)) mode ->
                      let atom, added, gen =
                        if mode then prepare parameter.A.typ atom gen
                        else (atom, [], gen)
                      in
                      (atom :: values, before @ added, gen))
                    ([], [], gen)
                    (List.combine arguments parameters)
                    modes
                in
                let* value =
                  if state.returnsPending && not (S.mem callee nativeEntries)
                  then Result.map Option.some (info definition.A.returnType)
                  else Ok None
                in
                Ok
                  ( A.ClosureCall (closure, List.rev arguments),
                    before,
                    value,
                    gen )
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
                    (fun id -> M.mem id env || M.mem id stores)
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
                  let operation, before, gen =
                    forceUses env stores operation gen
                  in
                  Ok (operation, before, None, gen)
          in
          let stores =
            match storedValue with
            | Some fields -> M.add id fields stores
            | None -> M.remove id stores
          in
          let env =
            match value with
            | Some value -> M.add id value env
            | None -> M.remove id env
          in
          let* rest, gen = expression fn nativeReturn env stores rest gen in
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
      expression fn nativeReturn env M.empty fn.A.body gen
    in
    let* originals, gen =
      List.fold_left
        (fun result (fn : A.functionDef) ->
          let* values, gen = result in
          if
            nativeRuntime fn.A.name
            || modeChanges fn.A.id
               && ((not (boundary fn))
                  || List.exists Fun.id (summary summaries fn.A.id).captures)
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
              if mode then carrier parameter.A.typ
              else if
                parameter.A.id
                =
                match fn.A.typedParams with
                | first :: _ -> first.A.id
                | [] -> Crash.crash "Async clone has no parameter"
              then
                match parameter.A.typ with
                | AST.TTuple (code :: fields)
                  when List.length fields = List.length state.captures ->
                    AST.TTuple
                      (code
                      :: List.map2
                           (fun typ pending ->
                             if pending then carrier typ else typ)
                           fields state.captures)
                | typ -> typ
              else parameter.A.typ
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
