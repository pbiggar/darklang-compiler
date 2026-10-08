(* Immutable source-checking state: symbol allocation and flexible type constraints. *)
[@@@warning "-4"]

module M = StringOrder.Map
module S = StringOrder.Set

type t = {
  symbols : CheckedAST.symbols;
  substitution : Types.substitution;
  flexible : S.t;
  nextVariable : int;
}

let create symbols =
  { symbols; substitution = M.empty; flexible = S.empty; nextVariable = 0 }

let symbols state = state.symbols
let resolve state = Types.applySubst state.substitution

let resolveExpression state =
  TypeSubstitution.applySubstToExpr (M.map (resolve state) state.substitution)

let constrain first second state =
  let first = resolve state first and second = resolve state second in
  let matched =
    if first = AST.TNever || second = AST.TNever then Ok []
    else Unification.matchTypes first second
  in
  Result.bind matched (fun bindings ->
      let bindings =
        List.filter (fun (name, _) -> S.mem name state.flexible) bindings
      in
      Result.map
        (fun substitution -> { state with substitution })
        (Unification.consolidateBindings
           (M.bindings state.substitution @ bindings)))

let freshenTypes rigid types state =
  let rec freshen mapping state typ =
    let many mapping state types =
      let reversed, mapping, state =
        List.fold_left
          (fun (reversed, mapping, state) typ ->
            let typ, mapping, state = freshen mapping state typ in
            (typ :: reversed, mapping, state))
          ([], mapping, state) types
      in
      (List.rev reversed, mapping, state)
    in
    match typ with
    | AST.TVar name when S.mem name rigid -> (typ, mapping, state)
    | (AST.TVar name | AST.TInferenceVar (_, name))
      when S.mem name state.flexible ->
        (typ, mapping, state)
    | AST.TVar name | AST.TInferenceVar (_, name) -> (
        match M.find_opt name mapping with
        | Some typ -> (typ, mapping, state)
        | None ->
            let fresh =
              "t$constraint_"
              ^ string_of_int (CheckedAST.nextBindingOrdinal state.symbols)
              ^ "_"
              ^ string_of_int state.nextVariable
            in
            let typ = AST.TInferenceVar (fresh, fresh) in
            ( typ,
              M.add name typ mapping,
              {
                state with
                nextVariable = state.nextVariable + 1;
                flexible = S.add fresh state.flexible;
              } ))
    | AST.TFunction (args, result) ->
        let args, mapping, state = many mapping state args in
        let result, mapping, state = freshen mapping state result in
        (AST.TFunction (args, result), mapping, state)
    | AST.TTuple args ->
        let args, mapping, state = many mapping state args in
        (AST.TTuple args, mapping, state)
    | AST.TRecord (name, args) | AST.TSum (name, args) ->
        let args, mapping, state = many mapping state args in
        let typ =
          match typ with
          | AST.TRecord _ -> AST.TRecord (name, args)
          | _ -> AST.TSum (name, args)
        in
        (typ, mapping, state)
    | AST.TList value | AST.TStream value ->
        let value, mapping, state = freshen mapping state value in
        let typ =
          match typ with
          | AST.TList _ -> AST.TList value
          | _ -> AST.TStream value
        in
        (typ, mapping, state)
    | AST.TDict (key, value) ->
        let key, mapping, state = freshen mapping state key in
        let value, mapping, state = freshen mapping state value in
        (AST.TDict (key, value), mapping, state)
    | _ -> (typ, mapping, state)
  in
  let reversed, _, state =
    List.fold_left
      (fun (reversed, mapping, state) typ ->
        let typ, mapping, state = freshen mapping state (resolve state typ) in
        (typ :: reversed, mapping, state))
      ([], M.empty, state) types
  in
  (List.rev reversed, state)

let update operation state =
  let value, symbols = operation state.symbols in
  (value, { state with symbols })

let allocateBinding name = update (CheckedAST.allocateBinding name)
let internFunction name = update (CheckedAST.internFunction name)
let internType name = update (CheckedAST.internType name)

let internField owner name index =
  update (CheckedAST.internField owner name index)

let internConstructor owner name tag =
  update (CheckedAST.internConstructor owner name tag)

let nextBindingOrdinal state = CheckedAST.nextBindingOrdinal state.symbols
let constructorInfo id state = CheckedAST.constructorInfo id state.symbols
