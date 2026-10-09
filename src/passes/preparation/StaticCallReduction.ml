(* Remove statically unused generic callbacks before representation selection.
   Only substitute values: argument evaluation and effects remain in source
   order. Unknown matches remain intact, and recursion never expands itself. *)
[@@@warning "-4"]

module C = CheckedAST
module B = C.BindingIdMap
module M = StringOrder.Map
module S = StringOrder.Set

let rec value = function
  | C.Local _ | C.FuncRef _ | C.GenericFuncRef _ | C.Lambda _ | C.UnitLiteral
  | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _ | C.Int8Literal _
  | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _ | C.UInt16Literal _
  | C.UInt32Literal _ | C.UInt64Literal _ | C.UInt128Literal _ | C.BoolLiteral _
  | C.StringLiteral _ | C.BlobLiteral _ | C.CharLiteral _ | C.FloatLiteral _ ->
      true
  | C.TupleLiteral fields -> List.for_all value (C.tupleElementsToList fields)
  | C.ListLiteral fields | C.Constructor (_, fields) ->
      List.for_all value fields
  | _ -> false

type selection = Selected of C.expr B.t | Miss | Unknown

let rec matches pattern expression bindings =
  let many patterns expressions =
    if List.length patterns <> List.length expressions then Miss
    else
      List.fold_left
        (fun previous (pattern, expression) ->
          match (previous, matches pattern expression bindings) with
          | Miss, _ | _, Miss -> Miss
          | Unknown, _ | _, Unknown -> Unknown
          | Selected previous, Selected next ->
              Selected (B.fold B.add next previous))
        (Selected bindings)
        (List.combine patterns expressions)
  in
  match (pattern, expression) with
  | C.PWildcard, _ -> Selected bindings
  | C.PVariable id, _ -> Selected (B.add id expression bindings)
  | C.PConstructor (id, fields), C.Constructor (reference, values) ->
      if AST.compareConstructorId id reference.C.constructorId <> 0 then Miss
      else many fields values
  | C.PTuple fields, C.TupleLiteral values ->
      many fields (C.tupleElementsToList values)
  | C.PList fields, C.ListLiteral values -> many fields values
  | C.PBool a, C.BoolLiteral b -> if a = b then Selected bindings else Miss
  | C.PInt64 a, C.Int64Literal b -> if a = b then Selected bindings else Miss
  | C.PString a, C.StringLiteral b -> if a = b then Selected bindings else Miss
  | _ -> Unknown

let reduce definitions program =
  let rec expression active env symbols expr =
    let one child build =
      let child, symbols = expression active env symbols child in
      (build child, symbols)
    in
    let many symbols items =
      let reversed, symbols =
        List.fold_left
          (fun (reversed, symbols) item ->
            let item, symbols = expression active env symbols item in
            (item :: reversed, symbols))
          ([], symbols) items
      in
      (List.rev reversed, symbols)
    in
    let two left right build =
      let left, symbols = expression active env symbols left in
      let right, symbols = expression active env symbols right in
      (build left right, symbols)
    in
    match expr with
    | C.Local id when not (S.is_empty active) ->
        (Option.value (B.find_opt id env) ~default:expr, symbols)
    | C.Let (C.LPVariable id, bound, body) ->
        let bound, symbols = expression active env symbols bound in
        let child =
          if value bound then B.add id bound env else B.remove id env
        in
        let body, symbols = expression active child symbols body in
        let removable =
          match bound with
          | C.Lambda (parameters, annotation, _) ->
              List.exists
                (fun (parameter : C.lambdaParameter) ->
                  Unification.containsTVar (C.semanticType parameter.C.typ))
                (NonEmptyList.toList parameters)
              || Option.fold ~none:false
                   ~some:(fun typ ->
                     Unification.containsTVar (C.semanticType typ))
                   annotation
          | _ -> false
        in
        if
          value bound
          && (removable || not (S.is_empty active))
          && not (InlineLambdas.varOccursInExpr id body)
        then (body, symbols)
        else (C.Let (C.LPVariable id, bound, body), symbols)
    | C.TypeApp (id, types, arguments) -> (
        let args, symbols = many symbols (NonEmptyList.toList arguments) in
        let original = C.TypeApp (id, types, NonEmptyList.fromList args) in
        let name = C.functionName id symbols in
        match name with
        | Some name
          when (not (S.mem name active))
               && List.exists
                    (fun typ -> Unification.containsTVar (C.semanticType typ))
                    types
               && List.for_all value args -> (
            match M.find_opt name definitions with
            | Some artifact
              when NonEmptyList.length
                     artifact.SpecializationIdentity.func.C.params
                   = List.length args -> (
                let func =
                  TypeSubstitution.specializeFunction id
                    artifact.SpecializationIdentity.func
                    (C.semanticTypeArgs types)
                in
                let importedSymbols, imported =
                  C.composeTopLevels artifact.SpecializationIdentity.symbols
                    symbols [ C.FunctionDef func ]
                in
                match imported with
                | [ C.FunctionDef func ] ->
                    let bindings =
                      List.fold_left
                        (fun env ((id, _), argument) -> B.add id argument env)
                        B.empty
                        (List.combine (NonEmptyList.toList func.C.params) args)
                    in
                    let reduced, reducedSymbols =
                      expression (S.add name active) bindings importedSymbols
                        func.C.body
                    in
                    (* A known return value proves that no callback invocation
                       survives. Unknown control flow keeps the original call. *)
                    if value reduced then (reduced, reducedSymbols)
                    else (original, symbols)
                | _ ->
                    Crash.crash "Static call import changed declaration shape")
            | _ -> (original, symbols))
        | _ -> (original, symbols))
    | C.Match (scrutinee, cases) -> (
        let scrutinee, symbols = expression active env symbols scrutinee in
        let rec select = function
          | [] -> None
          | (case : C.matchCase) :: rest -> (
              let result =
                NonEmptyList.toList case.C.patterns
                |> List.fold_left
                     (fun result pattern ->
                       match result with
                       | Miss -> matches pattern scrutinee env
                       | _ -> result)
                     Miss
              in
              match (result, case.C.guard) with
              | Miss, _ -> select rest
              | Selected bindings, None -> Some (bindings, case.C.body)
              | _ -> None)
        in
        match select (NonEmptyList.toList cases) with
        | Some (bindings, body) when value scrutinee && not (S.is_empty active)
          ->
            expression active bindings symbols body
        | _ ->
            let reversed, symbols =
              List.fold_left
                (fun (reversed, symbols) (case : C.matchCase) ->
                  let guard, symbols =
                    match case.C.guard with
                    | None -> (None, symbols)
                    | Some guard ->
                        let guard, symbols =
                          expression active env symbols guard
                        in
                        (Some guard, symbols)
                  in
                  let body, symbols =
                    expression active env symbols case.C.body
                  in
                  ({ case with C.guard; body } :: reversed, symbols))
                ([], symbols)
                (NonEmptyList.toList cases)
            in
            ( C.Match (scrutinee, NonEmptyList.fromList (List.rev reversed)),
              symbols ))
    | C.If (condition, yes, no) -> (
        let condition, symbols = expression active env symbols condition in
        match condition with
        | C.BoolLiteral true when not (S.is_empty active) ->
            expression active env symbols yes
        | C.BoolLiteral false when not (S.is_empty active) ->
            expression active env symbols no
        | _ ->
            let yes, symbols = expression active env symbols yes in
            let no, symbols = expression active env symbols no in
            (C.If (condition, yes, no), symbols))
    | C.BinOp (op, left, right) ->
        two left right (fun left right -> C.BinOp (op, left, right))
    | C.Sequence (left, right) ->
        two left right (fun left right -> C.Sequence (left, right))
    | C.Let (pattern, bound, body) ->
        two bound body (fun bound body -> C.Let (pattern, bound, body))
    | C.RecursiveLet (member, bound, body) ->
        two bound body (fun bound body -> C.RecursiveLet (member, bound, body))
    | C.BoundaryRender (id, child) ->
        one child (fun child -> C.BoundaryRender (id, child))
    | C.UnaryOp (op, child) -> one child (fun child -> C.UnaryOp (op, child))
    | C.TupleAccess (child, index) ->
        one child (fun child -> C.TupleAccess (child, index))
    | C.RecordAccess (child, id) ->
        one child (fun child -> C.RecordAccess (child, id))
    | C.Call (id, args) ->
        let args, symbols = many symbols (NonEmptyList.toList args) in
        (C.Call (id, NonEmptyList.fromList args), symbols)
    | C.Apply (func, args) | C.IndirectApply (func, args) ->
        let func, symbols = expression active env symbols func in
        let args, symbols = many symbols (NonEmptyList.toList args) in
        let args = NonEmptyList.fromList args in
        ( (match expr with
          | C.Apply _ -> C.Apply (func, args)
          | _ -> C.IndirectApply (func, args)),
          symbols )
    | C.Lambda (parameters, annotation, body) ->
        one body (fun body -> C.Lambda (parameters, annotation, body))
    | C.TupleLiteral fields ->
        let fields, symbols = many symbols (C.tupleElementsToList fields) in
        (C.TupleLiteral (C.tupleElementsOfList fields), symbols)
    | C.ListLiteral fields ->
        let fields, symbols = many symbols fields in
        (C.ListLiteral fields, symbols)
    | C.Constructor (reference, fields) ->
        let fields, symbols = many symbols fields in
        (C.Constructor (reference, fields), symbols)
    | C.Closure (id, fields) ->
        let fields, symbols = many symbols fields in
        (C.Closure (id, fields), symbols)
    | C.RecordLiteral (reference, fields) ->
        let fields, symbols =
          C.mapFoldRecordFields
            (fun symbols field -> expression active env symbols field)
            symbols fields
        in
        (C.RecordLiteral (reference, fields), symbols)
    | C.RecordUpdate (record, fields) ->
        let record, symbols = expression active env symbols record in
        let fields, symbols =
          List.fold_left
            (fun (reversed, symbols) (name, child) ->
              let child, symbols = expression active env symbols child in
              ((name, child) :: reversed, symbols))
            ([], symbols) fields
        in
        (C.RecordUpdate (record, List.rev fields), symbols)
    | C.DictLiteral (key, typ, fields) ->
        let fields, symbols =
          List.fold_left
            (fun (reversed, symbols) (key, child) ->
              let key, symbols = expression active env symbols key in
              let child, symbols = expression active env symbols child in
              ((key, child) :: reversed, symbols))
            ([], symbols) fields
        in
        (C.DictLiteral (key, typ, List.rev fields), symbols)
    | C.InterpolatedString parts ->
        let parts, symbols =
          List.fold_left
            (fun (reversed, symbols) -> function
              | C.StringText _ as part -> (part :: reversed, symbols)
              | C.StringExpr child ->
                  let child, symbols = expression active env symbols child in
                  (C.StringExpr child :: reversed, symbols))
            ([], symbols) parts
        in
        (C.InterpolatedString (List.rev parts), symbols)
    | _ -> (expr, symbols)
  in
  let tops, symbols =
    List.fold_left
      (fun (reversed, symbols) top ->
        let rewrite body = expression S.empty B.empty symbols body in
        let top, symbols =
          match top with
          | C.FunctionDef func ->
              let body, symbols = rewrite func.C.body in
              (C.FunctionDef { func with C.body }, symbols)
          | C.ValueDef definition ->
              let body, symbols = rewrite definition.C.body in
              (C.ValueDef { definition with C.body }, symbols)
          | C.Expression body ->
              let body, symbols = rewrite body in
              (C.Expression body, symbols)
          | C.TypeDef _ -> (top, symbols)
        in
        (top :: reversed, symbols))
      ([], C.programSymbols program)
      (C.programTopLevels program)
  in
  C.programFromCheckedParts (symbols, List.rev tops)
