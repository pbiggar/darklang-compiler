(*
   These tests cover optimizer decisions that must agree with the RcShape
   metadata used by ownership insertion and backend helper selection.
*)
(* ANFOptimizeTests.ml - Test scalar and ownership-sensitive production SSA optimization. *)
[@@@warning "-4-42"]

open Dark_compiler
open MemoryModel
open ANF
open ANFConstants
module FS = SpecializationIdentity.FunctionSet
module TM = TempMap

let dceOnlyOptions =
  {
    defaultOptimizeOptions with
    enableConstFolding = false;
    enableConstProp = false;
    enableCopyProp = false;
    enableDCE = true;
    enableStrengthReduction = false;
  }

let ( let* ) = Result.bind

let optimizeFunction context options (func : functionDef) =
  let* ssa =
    SSAANF.convertFunction (ANF_to_MIR.maxTempIdInFunction func)
      ANF.TypeMap.empty func
  in
  match SSAOptimization.optimizeFunctions context options [ ssa ] with
  | [ optimized ] -> Ok optimized
  | _ -> Error "SSA optimization changed the number of functions"

let optimizeMain context expr =
  optimizeFunction context dceOnlyOptions
    {
      id = AST.functionId 1L;
      name = "dce";
      typedParams = [];
      returnType = AST.TUnit;
      returnOwnership = OwnedReturn;
      body = expr;
    }

let singleBlock (func : SSAANF.functionDef) =
  match SSAANF.LabelMap.bindings func.SSAANF.blocks with
  | [ (_, block) ] -> Ok block
  | _ -> Error "Expected one optimized SSA block"

let emptyContext : optimizeContext =
  {
    typeReg = StringOrder.Map.empty;
    recordTypeParams = StringOrder.Map.empty;
    sumShapeReg = StringOrder.Map.empty;
    functionNames = FunctionIdMap.empty;
    functionIds = StringOrder.Map.empty;
  }

let optimizationFunctionNames =
  FunctionIdMap.ofList
    (List.map
       (fun name ->
         let fullName = "Darklang.Stdlib." ^ name in
         (TestIds.functionIdForName fullName, fullName))
       [
         "Int.fromInt64";
         "String.getByteAt";
         "String.__getByteAtInt64";
         "String.__byteLength";
         "String.__byteAtUnchecked";
       ])

let markerContext : optimizeContext =
  {
    typeReg = StringOrder.Map.empty;
    recordTypeParams = StringOrder.Map.empty;
    functionNames = optimizationFunctionNames;
    functionIds =
      StringOrder.Map.of_list
        (List.map
           (fun (id, name) -> (name, id))
           (FunctionIdMap.toList optimizationFunctionNames));
    sumShapeReg =
      StringOrder.Map.of_list
        [
          ( "Marker",
            {
              typeParams = [ "a" ];
              payloads = [ (0, None); (1, None) ];
              unaryPayloadTags = IntSet.empty;
            } );
          ( "Box",
            {
              typeParams = [ "a" ];
              payloads = [ (0, Some (AST.TVar "a")) ];
              unaryPayloadTags = IntSet.singleton 0;
            } );
        ];
  }

let optimizeExpression typeEnv returnType body =
  optimizeFunction markerContext defaultOptimizeOptions
    {
      id = AST.functionId 1L;
      name = "expression";
      typedParams =
        List.map (fun (id, typ) -> { id; typ }) (TM.bindings typeEnv);
      returnType;
      returnOwnership = OwnedReturn;
      body;
    }

let expressionCalls (func : SSAANF.functionDef) =
  SSAANF.LabelMap.fold
    (fun _ (block : SSAANF.block) calls ->
      List.fold_left
        (fun calls (_, operation) ->
          match operation with
          | Call (name, _) -> FS.add name calls
          | _ -> calls)
        calls block.SSAANF.operations)
    func.SSAANF.blocks FS.empty

let formatFunction func =
  ANFPrinter.formatSSAFunctions optimizationFunctionNames [ func ]

let stdlibFunction name = TestIds.functionIdForName ("Darklang.Stdlib." ^ name)

let testStringByteIndexConversionFusesWithLookup () =
  let fromInt64 = stdlibFunction "Int.fromInt64"
  and getByteAt = stdlibFunction "String.getByteAt"
  and getByteAtInt64 = stdlibFunction "String.__getByteAtInt64" in
  let expr =
    Let
      ( TempId 2,
        Call (fromInt64, [ Var (TempId 0) ]),
        Let
          ( TempId 3,
            Call (getByteAt, [ Var (TempId 1); Var (TempId 2) ]),
            Return (Var (TempId 3)) ) )
  in
  let* optimized =
    optimizeExpression
      (TM.of_list [ (TempId 0, AST.TInt64); (TempId 1, AST.TString) ])
      (AST.TSum ("Darklang.Stdlib.Option.Option", [ AST.TUInt8 ]))
      expr
  in
  let calls = expressionCalls optimized in
  if
    FS.mem getByteAtInt64 calls
    && (not (FS.mem fromInt64 calls))
    && not (FS.mem getByteAt calls)
  then Ok ()
  else
    Error
      ("Expected index conversion and byte lookup to fuse, got "
      ^ formatFunction optimized)

let byteOptionMatch usePayload =
  let someBranch =
    if usePayload then
      Let
        ( TempId 5,
          TypedAtom (Var (TempId 2), AST.TUInt8),
          Return (Var (TempId 5)) )
    else Return (IntLiteral (Int64 1L))
  in
  let noneBranch =
    Return (IntLiteral (if usePayload then UInt8 0 else Int64 0L))
  in
  (* UInt8 options reserve 256 for None; Some carries the byte directly. *)
  Let
    ( TempId 2,
      Call
        ( stdlibFunction "String.__getByteAtInt64",
          [ Var (TempId 0); Var (TempId 1) ] ),
      Let
        ( TempId 4,
          Prim (Neq, Var (TempId 2), IntLiteral (Int64 256L)),
          If (Var (TempId 4), someBranch, noneBranch) ) )

let testDeadStringBytePayloadLowersToBoundsCheck () =
  let original = stdlibFunction "String.__getByteAtInt64"
  and byteLength = stdlibFunction "String.__byteLength" in
  let* optimized =
    optimizeExpression
      (TM.of_list [ (TempId 0, AST.TString); (TempId 1, AST.TInt64) ])
      AST.TInt64
      (byteOptionMatch false)
  in
  let calls = expressionCalls optimized in
  if FS.mem byteLength calls && not (FS.mem original calls) then Ok ()
  else
    Error
      ("Expected dead Option payload construction to lower to a bounds check, \
        got "
      ^ formatFunction optimized)

let testLiveStringBytePayloadLowersToUncheckedLoad () =
  let original = stdlibFunction "String.__getByteAtInt64"
  and byteLength = stdlibFunction "String.__byteLength"
  and uncheckedLoad = stdlibFunction "String.__byteAtUnchecked" in
  let* optimized =
    optimizeExpression
      (TM.of_list [ (TempId 0, AST.TString); (TempId 1, AST.TInt64) ])
      AST.TUInt8
      (byteOptionMatch true)
  in
  let calls = expressionCalls optimized in
  if
    FS.mem byteLength calls && FS.mem uncheckedLoad calls
    && not (FS.mem original calls)
  then Ok ()
  else
    Error
      ("Expected live Option payload construction to lower to a guarded \
        unchecked load, got "
      ^ formatFunction optimized)

let testLiteralFloatAbsoluteValueFolds () =
  let* optimized =
    optimizeExpression TM.empty AST.TFloat64
      (Let (TempId 0, FloatAbs (FloatLiteral (-3.5)), Return (Var (TempId 0))))
  in
  let* block = singleBlock optimized in
  match (block.SSAANF.operations, block.SSAANF.terminator) with
  | [], SSAANF.Return (FloatLiteral value) when value = 3.5 -> Ok ()
  | _ ->
      Error
        ("Expected literal FloatAbs to fold to 3.5, got "
        ^ formatFunction optimized)

let testLiteralFloatToInt64Folds () =
  let* optimized =
    optimizeExpression TM.empty AST.TInt64
      (Let
         (TempId 0, FloatToInt64 (FloatLiteral (-3.75)), Return (Var (TempId 0))))
  in
  let* block = singleBlock optimized in
  match (block.SSAANF.operations, block.SSAANF.terminator) with
  | [], SSAANF.Return (IntLiteral (Int64 -3L)) -> Ok ()
  | _ ->
      Error
        ("Expected literal FloatToInt64 to truncate to -3, got "
        ^ formatFunction optimized)

let testLiteralBitNotFolds () =
  let* optimized =
    optimizeExpression TM.empty AST.TInt64
      (Let
         ( TempId 0,
           UnaryPrim (BitNot, IntLiteral (Int64 1L)),
           Return (Var (TempId 0)) ))
  in
  let* block = singleBlock optimized in
  match (block.SSAANF.operations, block.SSAANF.terminator) with
  | [], SSAANF.Return (IntLiteral (Int64 -2L)) -> Ok ()
  | _ ->
      Error
        ("Expected literal BitNot to fold to -2, got "
        ^ formatFunction optimized)

let testDceDropsUnusedPureGenericSumTypedAtom () =
  let markerType = AST.TSum ("Marker", [ AST.TString ]) in
  let* optimized =
    optimizeMain markerContext
      (Let
         ( TempId 0,
           TypedAtom (IntLiteral (Int64 0L), markerType),
           Return UnitLiteral ))
  in
  let* block = singleBlock optimized in
  match
    (block.SSAANF.operations, block.SSAANF.terminator)
  with
  | [], SSAANF.Return UnitLiteral -> Ok ()
  | _ ->
      Error
        ("Expected unused pure generic sum TypedAtom to be removed, got "
        ^ ANFPrinter.formatSSAFunctions FunctionIdMap.empty [ optimized ])

let testDcePreservesUnusedHeapGenericSumTypedAtom () =
  let boxType = AST.TSum ("Box", [ AST.TString ]) in
  let* optimized =
    optimizeMain markerContext
      (Let
         ( TempId 0,
           TypedAtom (IntLiteral (Int64 0L), boxType),
           Return UnitLiteral ))
  in
  let* block = singleBlock optimized in
  match
    (block.SSAANF.operations, block.SSAANF.terminator)
  with
  | [ (TempId 0, TypedAtom (_, typ)) ], SSAANF.Return UnitLiteral
    when typ = boxType ->
      Ok ()
  | _ ->
      Error
        ("Expected heap generic sum TypedAtom to be preserved, got "
        ^ ANFPrinter.formatSSAFunctions FunctionIdMap.empty [ optimized ])

let testCseReusesFloatAbsoluteValue () =
  let parameter : typedParam = { id = TempId 0; typ = AST.TFloat64 } in
  let func : functionDef =
    {
      id = TestIds.functionIdForName "floatAbsCse";
      name = "floatAbsCse";
      typedParams = [ parameter ];
      returnType = AST.TTuple [ AST.TFloat64; AST.TFloat64 ];
      returnOwnership = OwnedReturn;
      body =
        Let
          ( TempId 1,
            FloatAbs (Var parameter.id),
            Let
              ( TempId 2,
                FloatAbs (Var parameter.id),
                Let
                  ( TempId 3,
                    TupleAlloc [ Var (TempId 1); Var (TempId 2) ],
                    Return (Var (TempId 3)) ) ) );
    }
  in
  let* optimized = optimizeFunction emptyContext defaultOptimizeOptions func in
  let* block = singleBlock optimized in
  match (block.SSAANF.operations, block.SSAANF.terminator) with
  | ( [
        (TempId 1, FloatAbs (Var (TempId 0)));
        (TempId 3, TupleAlloc [ Var left; Var right ]);
      ],
      SSAANF.Return (Var (TempId 3)) )
    when left = TempId 1 && right = TempId 1 ->
      Ok ()
  | _ ->
      Error "Expected duplicate FloatAbs expressions to reuse the first result"

let testCanonicalBufferSelfEqualityFoldsForBothRepresentations () =
  let check kind =
    let parameterType =
      match kind with
      | Utf8String -> AST.TString
      | NullableUtf8String -> AST.TSum ("NullableText", [])
      | GraphemeCluster -> AST.TChar
      | NullableGraphemeCluster -> AST.TSum ("NullableChar", [])
    in
    let parameter : typedParam = { id = TempId 0; typ = parameterType } in
    let func : functionDef =
      {
        id = TestIds.functionIdForName "bufferEquality";
        name = "bufferEquality";
        typedParams = [ parameter ];
        returnType = AST.TBool;
        returnOwnership = OwnedReturn;
        body =
          Let
            ( TempId 1,
              CanonicalBufferEq (kind, Var parameter.id, Var parameter.id),
              Return (Var (TempId 1)) );
      }
    in
    let* optimized = optimizeFunction emptyContext defaultOptimizeOptions func in
    let* block = singleBlock optimized in
    match (block.SSAANF.operations, block.SSAANF.terminator) with
    | [], SSAANF.Return (BoolLiteral true) -> Ok ()
    | _ ->
        let name =
          match kind with
          | Utf8String -> "Utf8String"
          | NullableUtf8String -> "NullableUtf8String"
          | GraphemeCluster -> "GraphemeCluster"
          | NullableGraphemeCluster -> "NullableGraphemeCluster"
        in
        Error ("Expected " ^ name ^ " self-equality to fold to true")
  in
  List.fold_left
    (fun result kind -> Result.bind result (fun () -> check kind))
    (Ok ())
    [ Utf8String; NullableUtf8String; GraphemeCluster; NullableGraphemeCluster ]

let tests =
  [
    ( "DCE drops unused pure generic sum TypedAtom",
      testDceDropsUnusedPureGenericSumTypedAtom );
    ( "DCE preserves unused heap generic sum TypedAtom",
      testDcePreservesUnusedHeapGenericSumTypedAtom );
    ( "String byte lookup fuses Int64 index conversion",
      testStringByteIndexConversionFusesWithLookup );
    ( "String byte lookup with dead payload lowers to bounds checks",
      testDeadStringBytePayloadLowersToBoundsCheck );
    ( "String byte lookup with live payload lowers to unchecked load",
      testLiveStringBytePayloadLowersToUncheckedLoad );
    ("Literal FloatAbs folds", testLiteralFloatAbsoluteValueFolds);
    ("Literal FloatToInt64 folds", testLiteralFloatToInt64Folds);
    ("Literal BitNot folds", testLiteralBitNotFolds);
    ("SSA CSE reuses FloatAbs", testCseReusesFloatAbsoluteValue);
    ( "Canonical buffer self-equality folds for String and Char storage",
      testCanonicalBufferSelfEqualityFoldsForBothRepresentations );
  ]
