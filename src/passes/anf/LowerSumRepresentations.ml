(* LowerSumRepresentations.ml - Select compact or boxed layouts from final sum payload types. *)
[@@@warning "-4"]

module A = ANF
module P = LoweringPrimitives
module M = StringOrder.Map

let integer value = A.IntLiteral (A.Int64 value)
let tag value = integer (Int64.of_int value)
let sumType sum = AST.TSum (sum.A.sumName, sum.A.sumArgs)

let cases representations sum =
  match M.find_opt sum.A.sumName representations with
  | Some cases -> M.bindings cases |> List.map snd
  | None -> Crash.crash "Sum representation metadata is absent after checking"

let descriptor sum payloadType =
  {
    A.sourceTypeName = sum.A.sumName;
    runtimeTypeName = sum.A.sumName;
    typeArgs = sum.A.sumArgs;
    fields = [ ("$tag", AST.TInt64); ("$payload", payloadType) ];
    valueType = sumType sum;
  }

let lowerProgram representations (A.Program (functions, main)) =
  let transparent sum =
    P.transparentSumPayloadType sum.A.sumName sum.A.sumArgs representations
  in
  let absent sum =
    match
      P.nullablePointerSumPayloadType sum.A.sumName sum.A.sumArgs
        representations
    with
    | Some _ -> Some 0L
    | None ->
        P.spareImmediateSumSentinel sum.A.sumName sum.A.sumArgs representations
  in
  let boxed id sum constructor payload payloadType gen =
    let raw, gen = A.freshVar gen in
    ( [
        ( raw,
          A.RecordAlloc
            (descriptor sum payloadType, [ tag constructor; payload ]) );
        (id, A.TypedAtom (A.Var raw, sumType sum));
      ],
      gen )
  in
  let operation id expression gen =
    match expression with
    | A.SumConstant (sum, constructor) -> (
        match absent sum with
        | Some word -> ([ (id, A.TypedAtom (integer word, sumType sum)) ], gen)
        | None ->
            if
              List.exists
                (fun case -> case.P.fields <> [])
                (cases representations sum)
            then boxed id sum constructor (integer 0L) AST.TInt64 gen
            else ([ (id, A.TypedAtom (tag constructor, sumType sum)) ], gen))
    | A.SumAlloc (variant, payload) ->
        if
          Option.is_some (transparent variant.A.sum)
          || Option.is_some (absent variant.A.sum)
        then ([ (id, A.TypedAtom (payload, sumType variant.A.sum)) ], gen)
        else
          boxed id variant.A.sum variant.A.variantTag payload
            variant.A.payloadType gen
    | A.SumPayload (variant, value) ->
        let expression =
          if
            Option.is_some (transparent variant.A.sum)
            || Option.is_some (absent variant.A.sum)
          then A.TypedAtom (value, variant.A.payloadType)
          else
            A.RecordGet
              (descriptor variant.A.sum variant.A.payloadType, value, 1)
        in
        ([ (id, expression) ], gen)
    | A.SumTest (sum, expected, value) -> (
        let variants = cases representations sum in
        match variants with
        | [ variant ] ->
            ([ (id, A.Atom (A.BoolLiteral (variant.P.tag = expected))) ], gen)
        | _ -> (
            match absent sum with
            | Some word -> (
                match
                  ( List.filter (fun case -> case.P.fields = []) variants,
                    List.filter (fun case -> case.P.fields <> []) variants )
                with
                | [ empty ], [ present ] ->
                    let expression =
                      if expected = empty.P.tag then
                        A.Prim (A.Eq, value, integer word)
                      else if expected = present.P.tag then
                        A.Prim (A.Neq, value, integer word)
                      else A.Atom (A.BoolLiteral false)
                    in
                    ([ (id, expression) ], gen)
                | _ ->
                    Crash.crash
                      "Compact sum has inconsistent constructor metadata")
            | None ->
                if List.exists (fun case -> case.P.fields <> []) variants then
                  let actual, gen = A.freshVar gen in
                  ( [
                      (actual, A.TupleGet (value, 0));
                      (id, A.Prim (A.Eq, A.Var actual, tag expected));
                    ],
                    gen )
                else ([ (id, A.Prim (A.Eq, value, tag expected)) ], gen)))
    | _ -> ([ (id, expression) ], gen)
  in
  let rec body gen = function
    | A.Let (id, expression, rest) ->
        let bindings, gen = operation id expression gen in
        let rest, gen = body gen rest in
        ( List.fold_right
            (fun (id, expression) rest -> A.Let (id, expression, rest))
            bindings rest,
          gen )
    | A.If (condition, yes, no) ->
        let yes, gen = body gen yes in
        let no, gen = body gen no in
        (A.If (condition, yes, no), gen)
    | A.Join (parameter, continuation, entry) ->
        let continuation, gen = body gen continuation in
        let entry, gen = body gen entry in
        (A.Join (parameter, continuation, entry), gen)
    | (A.Return _ | A.Jump _) as expression -> (expression, gen)
  in
  let maximum = ANF_to_MIR.maxTempIdInProgram (A.Program (functions, main)) in
  let functions, gen =
    List.fold_left
      (fun (functions, gen) (fn : A.functionDef) ->
        let body, gen = body gen fn.A.body in
        ({ fn with A.body } :: functions, gen))
      ([], A.VarGen (maximum + 1))
      functions
  in
  let main, _ = body gen main in
  A.Program (List.rev functions, main)

let lower representations functions =
  let (A.Program (functions, _)) =
    lowerProgram representations (A.Program (functions, A.Return A.UnitLiteral))
  in
  functions
