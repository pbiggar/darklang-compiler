(* LowerAsyncDemands.ml - Lower source demands before ownership and native ABI selection. *)
[@@@warning "-4"]

module A = ANF
module M = InliningCommon.TempMap
module S = SpecializationIdentity.FunctionSet
module Names = StringOrder.Map

let ( let* ) = Result.bind

type pending = { payload : AST.semanticType; force : AST.functionId }

let carrier payload = AST.TSum ("Darklang.Stdlib.__Async.__Value", [ payload ])
let transportName = "Darklang.Stdlib.__Task.__transport"
let delayName = "Darklang.Stdlib.__Task.__delayTransparent"
let specialized base name = String.starts_with ~prefix:(base ^ "_") name

let lower registry names ids functions =
  let name id = FunctionIdMap.tryFind id names in
  let producer id =
    match name id with
    | Some name -> specialized transportName name || specialized delayName name
    | None -> false
  in
  if not (FunctionIdMap.exists (fun id _ -> producer id) names) then
    Ok functions
  else
    let helper base payload =
      let name = SpecializationIdentity.specName base [ payload ] in
      match Names.find_opt name ids with
      | Some id -> Ok id
      | None -> Error ("Async runtime specialization is missing: " ^ name)
    in
    let info id =
      match FunctionIdMap.tryFind id registry with
      | Some (_, AST.TFunction (_, payload)) ->
          let* force = helper "Darklang.Stdlib.__Task.__forceCurrent" payload in
          Ok { payload; force }
      | _ -> Error "Async source operation has no typed signature"
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
      List.fold_left
        (fun found (fn : A.functionDef) -> entries M.empty found fn.A.body)
        S.empty functions
    in
    let pending env = function A.Var id -> M.find_opt id env | _ -> None in
    let unsupported functionName boundary =
      Error
        ("Async transport across " ^ boundary ^ " is not implemented in "
       ^ functionName)
    in
    let forceAtom env atom gen =
      match pending env atom with
      | None -> (atom, [], gen)
      | Some value ->
          let id, gen = A.freshVar gen in
          (A.Var id, [ (id, A.Call (value.force, [ atom ])) ], gen)
    in
    let forceUses env expression gen =
      let replacements, bindings, gen =
        ANFEffects.TempSet.fold
          (fun id (replacements, bindings, gen) ->
            match M.find_opt id env with
            | None -> (replacements, bindings, gen)
            | Some value ->
                let forced, gen = A.freshVar gen in
                ( M.add id forced replacements,
                  (forced, A.Call (value.force, [ A.Var id ])) :: bindings,
                  gen ))
          (ANFEffects.cexprTempUses expression)
          (M.empty, [], gen)
      in
      ( InliningCommon.renameCExpr replacements expression,
        List.rev bindings,
        gen )
    in
    let wraps bindings body =
      List.fold_right
        (fun (id, value) body -> A.Let (id, value, body))
        bindings body
    in
    let rec expression functionName nativeReturn env body gen =
      match body with
      | A.Return atom -> (
          match pending env atom with
          | None -> Ok (body, gen)
          | Some _ when nativeReturn ->
              let atom, bindings, gen = forceAtom env atom gen in
              Ok (wraps bindings (A.Return atom), gen)
          | Some _ -> unsupported functionName "an ordinary return")
      | A.Jump (_, atom) ->
          if Option.is_some (pending env atom) then
            unsupported functionName "a control-flow join"
          else Ok (body, gen)
      | A.If (condition, yes, no) ->
          let condition, bindings, gen = forceAtom env condition gen in
          let* yes, gen = expression functionName nativeReturn env yes gen in
          let* no, gen = expression functionName nativeReturn env no gen in
          Ok (wraps bindings (A.If (condition, yes, no)), gen)
      | A.Join (parameter, continuation, entry) ->
          let* continuation, gen =
            expression functionName nativeReturn
              (M.remove parameter.A.id env)
              continuation gen
          in
          let* entry, gen =
            expression functionName nativeReturn env entry gen
          in
          Ok (A.Join (parameter, continuation, entry), gen)
      | A.Let (id, operation, rest) ->
          let* operation, before, value, gen =
            match operation with
            | A.Call (callee, arguments) when producer callee ->
                let* value = info callee in
                let* operation, before, gen =
                  match (name callee, arguments) with
                  | Some name, [ argument ] when specialized transportName name
                    ->
                      if Option.is_some (pending env argument) then
                        unsupported functionName "nested source transport"
                      else
                        Ok
                          ( A.TypedAtom (argument, carrier value.payload),
                            [],
                            gen )
                  | Some name, [ delay; argument ]
                    when specialized delayName name ->
                      let delayAtom, before, gen = forceAtom env delay gen in
                      let* argument, before, gen =
                        match pending env argument with
                        | Some _ -> Ok (argument, before, gen)
                        | None ->
                            let* ready =
                              helper "Darklang.Stdlib.__Async.__ready"
                                value.payload
                            in
                            let wrapped, gen = A.freshVar gen in
                            Ok
                              ( A.Var wrapped,
                                before
                                @ [ (wrapped, A.Call (ready, [ argument ])) ],
                                gen )
                      in
                      let* delayHelper =
                        helper "Darklang.Stdlib.__Task.__delayValueCurrent"
                          value.payload
                      in
                      Ok
                        ( A.Call (delayHelper, [ delayAtom; argument ]),
                          before,
                          gen )
                  | _ -> Error "Invalid async source operation arguments"
                in
                Ok (operation, before, Some value, gen)
            | A.Atom atom -> Ok (operation, [], pending env atom, gen)
            | A.TypedAtom (atom, typ) -> (
                match pending env atom with
                | Some value when typ = value.payload ->
                    Ok (A.TypedAtom (atom, carrier typ), [], Some value, gen)
                | _ ->
                    let operation, before, gen = forceUses env operation gen in
                    Ok (operation, before, None, gen))
            | _ ->
                let uses = ANFEffects.cexprTempUses operation in
                let hasPending =
                  ANFEffects.TempSet.exists (fun id -> M.mem id env) uses
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
                    | _ ->
                        unsupported functionName "this call or storage boundary"
                  in
                  let operation, before, gen = forceUses env operation gen in
                  Ok (operation, before, None, gen)
          in
          let env =
            match value with
            | Some value -> M.add id value env
            | None -> M.remove id env
          in
          let* rest, gen = expression functionName nativeReturn env rest gen in
          Ok (wraps before (A.Let (id, operation, rest)), gen)
    in
    let initial =
      1
      + List.fold_left
          (fun maximum fn -> max maximum (ANF_to_MIR.maxTempIdInFunction fn))
          0 functions
    in
    let* reversed, _ =
      List.fold_left
        (fun result (fn : A.functionDef) ->
          let* functions, gen = result in
          let* body, gen =
            expression fn.A.name
              (S.mem fn.A.id nativeEntries)
              M.empty fn.A.body gen
          in
          Ok ({ fn with A.body } :: functions, gen))
        (Ok ([], A.VarGen initial))
        functions
    in
    Ok (List.rev reversed)
