(* ANF_Intrinsics.ml - Give named fixed-width arithmetic and operators one ANF operation. *)
[@@@warning "-4"]

open ANF

type arithmeticIntrinsic = { operandType : AST.semanticType; operation : binOp }

let nativeIntegerTypes =
  [
    ("Int8", AST.TInt8);
    ("Int16", AST.TInt16);
    ("Int32", AST.TInt32);
    ("Int64", AST.TInt64);
    ("UInt8", AST.TUInt8);
    ("UInt16", AST.TUInt16);
    ("UInt32", AST.TUInt32);
    ("UInt64", AST.TUInt64);
  ]

let arithmeticOperations =
  [ ("add", Add); ("subtract", Sub); ("multiply", Mul); ("divide", Div) ]

let intrinsicFunctions functionIds functions =
  FunctionIdMap.ofList
    (List.concat_map
       (fun (typeName, operandType) ->
         List.filter_map
           (fun (operationName, operation) ->
             let name = "Darklang.Stdlib." ^ typeName ^ "." ^ operationName in
             Option.map
               (fun id ->
                 match FunctionIdMap.tryFind id functions with
                 | Some (_, AST.TFunction ([ left; right ], result))
                   when left = operandType && right = operandType
                        && result = operandType ->
                     (id, { operandType; operation })
                 | _ ->
                     Crash.crash
                       ("Arithmetic intrinsic " ^ name
                      ^ " has an unexpected signature"))
               (StringOrder.Map.find_opt name functionIds))
           arithmeticOperations)
       nativeIntegerTypes)

let canonicalizeCExpr intrinsics = function
  | ( Call (target, [ left; right ])
    | BorrowedCall (target, [ left; right ])
    | TailCall (target, [ left; right ]) ) as cexpr -> (
      match FunctionIdMap.tryFind target intrinsics with
      | Some intrinsic -> Prim (intrinsic.operation, left, right)
      | None -> cexpr)
  | cexpr -> cexpr

let rec canonicalizeExpr intrinsics = function
  | Let (result, operation, continuation) ->
      Let
        ( result,
          canonicalizeCExpr intrinsics operation,
          canonicalizeExpr intrinsics continuation )
  | Join (parameter, continuation, entry) ->
      Join
        ( parameter,
          canonicalizeExpr intrinsics continuation,
          canonicalizeExpr intrinsics entry )
  | If (condition, yes, no) ->
      If
        ( condition,
          canonicalizeExpr intrinsics yes,
          canonicalizeExpr intrinsics no )
  | (Return _ | Jump _) as expr -> expr

(*
   A function reference keeps its resolved ID. Its compiled definition is the
   callable adapter below; direct calls and the adapter both use the same Prim.
*)
let canonicalizeProgram functionIds functions program =
  let intrinsics = intrinsicFunctions functionIds functions in
  if FunctionIdMap.isEmpty intrinsics then program
  else
    let (Program (definitions, main)) = program in
    let initialVarGen =
      if
        List.exists
          (fun (definition : functionDef) ->
            FunctionIdMap.containsKey definition.id intrinsics)
          definitions
      then ANFExpressionOptimization.freshVarGenForProgram program
      else initialVarGen
    in
    let reversed, _ =
      List.fold_left
        (fun (acc, varGen) (definition : functionDef) ->
          match FunctionIdMap.tryFind definition.id intrinsics with
          | None ->
              ( {
                  definition with
                  body = canonicalizeExpr intrinsics definition.body;
                }
                :: acc,
                varGen )
          | Some intrinsic -> (
              match definition.typedParams with
              | [ left; right ]
                when left.typ = intrinsic.operandType
                     && right.typ = intrinsic.operandType
                     && definition.returnType = intrinsic.operandType ->
                  let result, next = freshVar varGen in
                  ( {
                      definition with
                      body =
                        Let
                          ( result,
                            Prim (intrinsic.operation, Var left.id, Var right.id),
                            Return (Var result) );
                    }
                    :: acc,
                    next )
              | _ ->
                  Crash.crash
                    ("Arithmetic intrinsic adapter " ^ definition.name
                   ^ " has an unexpected ANF signature")))
        ([], initialVarGen) definitions
    in
    Program (List.rev reversed, canonicalizeExpr intrinsics main)
