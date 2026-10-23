(* AutomaticTaskEntry.ml - Give CLI computations one scheduler-owned native stack. *)
module X = CompilationContexts
module C = CheckedAST
module I = SpecializationIdentity
module M = StringOrder.Map

let ( let* ) = Result.bind

let prepare materialize (context : X.pipelineContext) functions =
  let entryName = "__dark_compiler_program_entry" in
  let bodyName = "__dark_compiler_program_body" in
  if
    M.mem bodyName context.X.registries.AST_to_ANF.functionIds
    || List.exists
         (fun (fn : ANF.functionDef) -> fn.ANF.name = bodyName)
         functions
  then Error ("Function name '" ^ bodyName ^ "' is reserved")
  else
    let entry =
      match
        List.filter
          (fun (fn : ANF.functionDef) -> fn.ANF.name = entryName)
          functions
      with
      | [ entry ] -> entry
      | [] | _ :: _ ->
          Crash.crash
            "Automatic CLI task entry requires exactly one program entry"
    in
    let bodyId, symbols = C.internFunction bodyName context.X.symbols in
    let context = { context with X.symbols } in
    let base = "Darklang.Stdlib.__Task.__runMain" in
    let helperName = I.specName base [ entry.ANF.returnType ] in
    let* context, helpers =
      if M.mem helperName context.X.registries.AST_to_ANF.functionIds then
        Ok (context, [])
      else
        materialize context
          (I.SpecSet.singleton (base, [ entry.ANF.returnType ]))
    in
    let helperId =
      match
        M.find_opt helperName context.X.registries.AST_to_ANF.functionIds
      with
      | Some id -> id
      | None ->
          Crash.crash "Automatic CLI task entry lost its typed scheduler helper"
    in
    let maximum =
      List.fold_left
        (fun maximum fn -> max maximum (ANF_to_MIR.maxTempIdInFunction fn))
        0 functions
    in
    let environmentId = ANF.TempId (maximum + 1) in
    let unitId = ANF.TempId (maximum + 2) in
    let closureId = ANF.TempId (maximum + 3) in
    let resultId = ANF.TempId (maximum + 4) in
    let body =
      {
        entry with
        ANF.id = bodyId;
        name = bodyName;
        typedParams =
          [
            { ANF.id = environmentId; typ = AST.TTuple [ AST.TInt64 ] };
            { ANF.id = unitId; typ = AST.TUnit };
          ];
      }
    in
    let entry =
      {
        entry with
        ANF.body =
          ANF.Let
            ( closureId,
              ANF.ClosureAlloc (bodyId, []),
              ANF.Let
                ( resultId,
                  ANF.Call (helperId, [ ANF.Var closureId ]),
                  ANF.Return (ANF.Var resultId) ) );
      }
    in
    let functions =
      List.map
        (fun (fn : ANF.functionDef) ->
          if fn.ANF.id = entry.ANF.id then entry else fn)
        functions
    in
    Ok (context, functions @ [ body ], helpers)
