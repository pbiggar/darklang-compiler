(* Immutable source-checking state: symbol allocation and flexible type constraints. *)
[@@@warning "-4"]

module M = StringOrder.Map
module S = StringOrder.Set

type t = {
  symbols : CheckedAST.symbols;
  substitution : Types.substitution;
  flexible : S.t;
  nextVariable : int;
  operators : (AST.binOp * AST.semanticType) list;
}

let create symbols =
  { symbols; substitution = M.empty; flexible = S.empty; nextVariable = 0;
    operators = [] }

let symbols state = state.symbols
let resolve state = Types.applySubst state.substitution

let resolveExpression state =
  TypeSubstitution.applySubstToExpr (M.map (resolve state) state.substitution)

(* Unknown operands carry an operator requirement, rather than choosing a
   numeric representation. Later unification must satisfy that requirement. *)
let validateOperator operation = function
  | AST.TVar _ | AST.TInferenceVar _ | AST.TNever -> Ok ()
  | typ ->
      let integer =
        List.mem typ [AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32;
          AST.TInt64; AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32;
          AST.TUInt64; AST.TUInt128]
      in
      let valid = match operation with
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> integer
        | AST.Pow -> (integer || typ = AST.TFloat64)
            && typ <> AST.TInt128 && typ <> AST.TUInt128
        | _ -> integer || typ = AST.TFloat64
      in
      if valid then Ok () else Error "Operator is unavailable for this type"

let requireOperator operation typ state =
  let typ = resolve state typ in
  Result.map
    (fun () -> match typ with
      | AST.TVar _ | AST.TInferenceVar _ ->
          {state with operators = (operation, typ) :: state.operators}
      | _ -> state)
    (validateOperator operation typ)

let validateOperators state =
  List.fold_left
    (fun result (operation, typ) ->
      Result.bind result (fun () -> validateOperator operation (resolve state typ)))
    (Ok ()) state.operators

let constrain first second state =
  let first = resolve state first and second = resolve state second in
  match Unification.reconcileTypes None first second with
  | None ->
      Error
        ("Expected "
        ^ StructuralFormat.semanticType first
        ^ ", got "
        ^ StructuralFormat.semanticType second)
  | Some unified ->
      Result.bind (Unification.matchTypes first unified) (fun firstBindings ->
          Result.bind (Unification.matchTypes second unified)
            (fun secondBindings ->
              let bindings =
                List.filter
                  (fun (name, _) -> S.mem name state.flexible)
                  (firstBindings @ secondBindings)
              in
              Result.bind
                (Unification.consolidateBindings
                   (M.bindings state.substitution @ bindings))
                (fun substitution ->
                  let state = {state with substitution} in
                  Result.map (fun () -> state) (validateOperators state))))

let freshenType rigid typ state =
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
  let typ, _, state = freshen M.empty state (resolve state typ) in
  (typ, state)

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
