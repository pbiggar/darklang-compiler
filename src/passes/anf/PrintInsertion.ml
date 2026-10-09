(* PrintInsertion.ml - Print Insertion Pass
   Inserts a Print instruction at the end of the main expression.
   This ensures the program's result is printed before exiting.
   This pass runs before RC insertion so generated output and its final managed
   uses participate in the same ownership analysis as source operations. *)
[@@@warning "-4"]

open ANF

let unsupportedListDisplay elemType =
  Crash.crash
    ("Unsupported list result display element type: "
    ^ CheckingDiagnostics.typeToString elemType)

(* Wrap the return value with a Print instruction
   Transforms: Return atom  →  Let (_, Print (atom, type), Return atom)
   For list types, generates: Call toDisplayString, then Print the string *)
(*
   For list types, call toDisplayString first
*)
let rec wrapReturnWithPrint resolveFunction programType gen expr =
  let defaultPrintType =
    match programType with
    (* Builtin.crash has a bottom-like compile-time type.
    Printing should stay concrete so downstream passes never see it. *)
    | AST.TNever -> AST.TUnit
    | _ -> programType
  in
  match expr with
  | Return atom -> (
      (* Dead-code elimination can reduce a typed expression branch to `()`
      (for example, `Builtin.crash` in a selected match arm).
      Printing must follow the runtime atom shape, not only the original program type. *)
      let printType =
        match atom with UnitLiteral -> AST.TUnit | _ -> defaultPrintType
      in
      let displayHelper elemType =
        match ListDisplay.getDisplayStringFunc elemType with
        | Some name -> name
        | None -> unsupportedListDisplay elemType
      in
      let format name =
        let strTmp, gen = freshVar gen in
        let printTmp, gen = freshVar gen in
        let callExpr = Call (resolveFunction name, [ atom ]) in
        let printExpr = Print (Var strTmp, AST.TString) in
        (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), gen)
      in
      match printType with
      | AST.TUnit ->
          (* Explicit output functions return Unit. Matching the interpreter,
        a final Unit has no implicit textual representation. *)
          (Return atom, gen)
      | AST.TSum ("Darklang.Stdlib.Option.Option", [ AST.TList elemType ]) ->
          let helper = displayHelper elemType in
          (* Keep the display helper reachable so tree shaking doesn't drop it. *)
          let keepFunc, gen = freshVar gen in
          let printTmp, gen = freshVar gen in
          let keepExpr = Atom (FuncRef (resolveFunction helper)) in
          let printExpr = Print (atom, printType) in
          (Let (keepFunc, keepExpr, Let (printTmp, printExpr, Return atom)), gen)
      | AST.TList elemType ->
          (* Generate: let strTmp = Call(toDisplayString, [list]) in
                let _ = Print(strTmp, String) in Return atom *)
          format (displayHelper elemType)
      | AST.TFloat64 ->
          (* For Float64, call Float.toString first, then print the string *)
          format "Darklang.Stdlib.Float.toString"
      | AST.TDateTime ->
          (* DateTime is an opaque immediate; display it through its public formatter. *)
          format "Darklang.Stdlib.DateTime.toString"
      | AST.TSum ("Uuid", []) ->
          (* UUID is an ordinary sum, but public output is its canonical text. *)
          format "Darklang.Stdlib.Uuid.toString"
      | _ ->
          (* Non-list types: simple print *)
          let id, gen = freshVar gen in
          (Let (id, Print (atom, printType), Return atom), gen))
  | Let (id, value, body) ->
      (* Recurse into body *)
      let body, gen =
        wrapReturnWithPrint resolveFunction programType gen body
      in
      (Let (id, value, body), gen)
  | Jump _ -> (expr, gen)
  | Join (parameter, continuation, entry) ->
      let continuation, gen =
        wrapReturnWithPrint resolveFunction programType gen continuation
      in
      let entry, gen =
        wrapReturnWithPrint resolveFunction programType gen entry
      in
      (Join (parameter, continuation, entry), gen)
  | If (condition, yes, no) ->
      (* Wrap both branches *)
      let yes, gen = wrapReturnWithPrint resolveFunction programType gen yes in
      let no, gen = wrapReturnWithPrint resolveFunction programType gen no in
      (If (condition, yes, no), gen)

let resolveFunction functionIds name =
  match StringOrder.Map.find_opt name functionIds with
  | Some id -> id
  | None -> Crash.crash ("Print helper '" ^ name ^ "' has no allocated identity")

(* Insert Print at the end of the main expression *)
let insertPrint functionIds functions mainExpr programType =
  let gen = VarGen 2000 in
  (* Start high to avoid conflicts *)
  let body, _ =
    wrapReturnWithPrint (resolveFunction functionIds) programType gen mainExpr
  in
  Program (functions, body)

(* Insert Print into a named entry function *)
let insertPrintInEntry functionIds entryName programType functions =
  let gen = VarGen 2000 in
  (* Start high to avoid conflicts *)
  let rec update found = function
    | [] ->
        if found then Ok []
        else
          Error
            ("Entry function '" ^ entryName ^ "' not found for print insertion")
    | (func : functionDef) :: rest ->
        if func.name = entryName then
          let body, _ =
            wrapReturnWithPrint
              (resolveFunction functionIds)
              programType gen func.body
          in
          Result.map (fun tail -> { func with body } :: tail) (update true rest)
        else Result.map (fun tail -> func :: tail) (update found rest)
  in
  update false functions

(* Observe the source value immediately before the generated value renderer
   consumes it. The ordinary result printer sees only the rendered string. *)
let insertRootWordProbeInEntry functionNames entryName tupleWords functions =
  let rec probeReturns gen expr =
    match expr with
    | Return _ -> (expr, gen)
    | Let (id, Call (callee, [ value ]), body)
      when Option.fold ~none:false
             ~some:(String.starts_with ~prefix:"__dark_render_value_")
             (FunctionIdMap.tryFind callee functionNames) ->
        if tupleWords then
          let field0, gen = freshVar gen in
          let print0, gen = freshVar gen in
          let separator, gen = freshVar gen in
          let field1, gen = freshVar gen in
          let print1, gen = freshVar gen in
          let body, gen = probeReturns gen body in
          ( Let
              ( field0,
                TupleGet (value, 0),
                Let
                  ( print0,
                    Print (Var field0, AST.TInt64),
                    Let
                      ( separator,
                        StdoutWrite (StringLiteral "|", false),
                        Let
                          ( field1,
                            TupleGet (value, 1),
                            Let
                              ( print1,
                                Print (Var field1, AST.TInt64),
                                Let (id, Call (callee, [ value ]), body) ) ) )
                  ) ),
            gen )
        else
          let probeId, gen = freshVar gen in
          let body, gen = probeReturns gen body in
          ( Let
              ( probeId,
                Print (value, AST.TInt64),
                Let (id, Call (callee, [ value ]), body) ),
            gen )
    | Let (id, value, body) ->
        let body, gen = probeReturns gen body in
        (Let (id, value, body), gen)
    | If (condition, yes, no) ->
        let yes, gen = probeReturns gen yes in
        let no, gen = probeReturns gen no in
        (If (condition, yes, no), gen)
    | Join (parameter, continuation, entry) ->
        let continuation, gen = probeReturns gen continuation in
        let entry, gen = probeReturns gen entry in
        (Join (parameter, continuation, entry), gen)
    | Jump _ -> (expr, gen)
  in
  let rec update found = function
    | [] ->
        if found then Ok []
        else
          Error
            ("Entry function '" ^ entryName ^ "' not found for root word probe")
    | (func : functionDef) :: rest when func.name = entryName ->
        let body, _ = probeReturns (VarGen 3000) func.body in
        Result.map (fun tail -> { func with body } :: tail) (update true rest)
    | func :: rest -> Result.map (fun tail -> func :: tail) (update found rest)
  in
  update false functions
