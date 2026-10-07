(*
   ANFToMIRTests.ml - Unit tests for ANF to MIR lowering behavior.
   Covers pass-local edge cases that are not reachable from public E2E programs.
*)
(* Unit tests for ANF to MIR lowering behavior. *)
[@@@warning "-4-42"]

open Dark_compiler
module A = ANF
module M = MIR
module S = SSAANF
module R = ANF_to_MIR

type testResult = (unit, string) result

let ( let* ) = Result.bind

let sameSet expected actual =
  Option.equal RcReturnAnalysis.TempSet.equal expected actual

let contains message fragment =
  let length = String.length fragment in
  let rec scan n =
    n + length <= String.length message
    && (String.sub message n length = fragment || scan (n + 1))
  in
  scan 0

let testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64 () =
  try
    let _ = R.tryGetIntrinsicReturnType "__raw_get_str" in
    Error "Expected __raw_get_str fallback return type to crash"
  with
  | Failure message
    when contains message "monomorphized raw_get return type missing" ->
      Ok ()
  | Failure message -> Error ("Expected raw_get fallback crash, got: " ^ message)

let testBuildVariantRegistryRejectsInconsistentTypeParams () =
  try
    let lookup =
      StringOrder.Map.empty
      |> StringOrder.Map.add "Some" ("Option", [ "a" ], 0, [ AST.TVar "a" ])
      |> StringOrder.Map.add "None" ("Option", [], 1, [])
    in
    let _ = R.buildVariantRegistry lookup in
    Error "Expected inconsistent type parameters to crash"
  with
  | Failure message when contains message "inconsistent type parameters" ->
      Ok ()
  | Failure message ->
      Error ("Expected inconsistent type parameter crash, got: " ^ message)

(*
   Native record descriptors are compile-time metadata: fields occupy the
   complete heap payload and lowering must not materialize a descriptor word.
*)
let testRecordAllocationStartsFieldsAtOffsetZero () =
  let descriptor : A.recordDescriptor =
    {
      A.sourceTypeName = "LayoutRecord";
      runtimeTypeName = "LayoutRecord";
      typeArgs = [];
      fields = [ ("left", AST.TInt64); ("right", AST.TInt64) ];
      valueType = AST.TRecord ("LayoutRecord", []);
    }
  in
  let program =
    A.Program
      ( [],
        A.Let
          ( A.TempId 0,
            A.RecordAlloc
              ( descriptor,
                [ A.IntLiteral (A.Int64 10L); A.IntLiteral (A.Int64 20L) ] ),
            A.Return (A.Var (A.TempId 0)) ) )
  in
  let types =
    A.TypeMap.ofSeq
      (List.to_seq [ (A.TempId 0, AST.TRecord ("LayoutRecord", [])) ])
  in
  match
    R.toMIR program types StringOrder.Map.empty
      (AST.TRecord ("LayoutRecord", []))
      StringOrder.Map.empty StringOrder.Map.empty false FunctionIdMap.empty
      (FunctionIdMap.ofList [ (AST.functionId 0L, "_start") ])
  with
  | Error message -> Error ("Unexpected record lowering error: " ^ message)
  | Ok (M.Program (functions, _, _)) -> (
      match
        List.find_opt
          (fun (func : M.functionDef) -> func.M.name = "_start")
          functions
      with
      | None -> Error "Expected synthetic _start function"
      | Some start -> (
          match
            M.LabelMap.find_opt start.M.cfg.M.entry start.M.cfg.M.blocks
          with
          | None -> Error "Expected _start entry block"
          | Some block -> (
              match block.M.instrs with
              | [
               M.HeapAlloc (_, 16);
               M.HeapStore (_, 0, M.Int64Const 10L, None);
               M.HeapStore (_, 8, M.Int64Const 20L, None);
              ] ->
                  Ok ()
              | _ ->
                  Error
                    "Expected a 16-byte record with fields at offsets 0 and 8"))
      )

(*
   Nested branches whose alternatives both loop cannot reach an enclosing
   value continuation. Lowering must not invent an unreachable return block
   or treat the non-returning subtree as an integer-valued predecessor.
*)
let testNestedTerminalBranchesHaveNoInventedReturn () =
  let name = "terminalBranches" in
  let first = A.TempId 0 in
  let second = A.TempId 1 in
  let identity = TestIds.functionIdForName name in
  let loop id =
    A.Let
      ( A.TempId id,
        A.TailCall (identity, [ A.BoolLiteral false; A.Var first ]),
        A.Return (A.Var (A.TempId id)) )
  in
  let func : A.functionDef =
    {
      A.id = identity;
      name;
      typedParams =
        [
          { A.id = first; typ = AST.TBool }; { A.id = second; typ = AST.TBool };
        ];
      returnType = AST.TFloat64;
      returnOwnership = A.OwnedReturn;
      body =
        A.If
          ( A.Var first,
            A.If (A.Var second, loop 2, loop 3),
            A.Return (A.FloatLiteral 3.5) );
    }
  in
  let types =
    A.TypeMap.ofSeq
      (List.to_seq
         [
           (first, AST.TBool);
           (second, AST.TBool);
           (A.TempId 2, AST.TFloat64);
           (A.TempId 3, AST.TFloat64);
         ])
  in
  let* lowered =
    R.convertANFFunction func types StringOrder.Map.empty
      (FunctionIdMap.ofList [ (identity, AST.TFloat64) ])
      (FunctionIdMap.ofList [ (identity, name) ])
      false
  in
  let rec visit seen = function
    | [] -> Ok seen
    | label :: rest when M.LabelSet.mem label seen -> visit seen rest
    | label :: rest -> (
        match M.LabelMap.find_opt label lowered.M.cfg.M.blocks with
        | None -> Error "Missing branch target"
        | Some block ->
            let successors =
              match block.M.terminator with
              | M.Ret _ -> []
              | M.Jump target -> [ target ]
              | M.Branch (_, yes, no) -> [ yes; no ]
            in
            visit (M.LabelSet.add label seen) (successors @ rest))
  in
  let* () = MIR_SSA_Verify.verifyFunction lowered in
  let* reachable = visit M.LabelSet.empty [ lowered.M.cfg.M.entry ] in
  let all =
    M.LabelSet.of_list
      (List.map fst (M.LabelMap.bindings lowered.M.cfg.M.blocks))
  in
  if M.LabelSet.equal reachable all then Ok ()
  else Error "Terminal branches invented unreachable blocks"

let testPhiEdgesPreserveDistinctPredecessors () =
  let entry = M.Label "entry" in
  let yes = M.Label "yes" in
  let no = M.Label "no" in
  let join = M.Label "join" in
  let condition = M.VReg 0 in
  let result = M.VReg 1 in
  let block label instrs terminator : M.basicBlock =
    { M.label; instrs; terminator }
  in
  let func : M.functionDef =
    {
      M.id = TestIds.functionIdForName "invalidPhiEdges";
      name = "invalidPhiEdges";
      typedParams = [ { M.reg = condition; typ = AST.TBool } ];
      returnType = AST.TInt64;
      cfg =
        {
          M.entry;
          blocks =
            M.LabelMap.of_list
              [
                ( entry,
                  block entry [] (M.Branch (M.Register condition, yes, no)) );
                (yes, block yes [] (M.Jump join));
                (no, block no [] (M.Jump join));
                ( join,
                  block join
                    [
                      M.Phi
                        ( result,
                          [ (M.Int64Const 1L, yes); (M.Int64Const 2L, yes) ],
                          Some AST.TInt64 );
                    ]
                    (M.Ret (M.Register result)) );
              ];
        };
      floatRegs = M.IntSet.empty;
    }
  in
  match MIR_SSA_Verify.verifyFunction func with
  | Error message when contains message "phi edges disagree" -> Ok ()
  | Error message -> Error ("Expected invalid phi edge error, got " ^ message)
  | Ok () -> Error "Expected duplicate phi predecessor to be rejected"

(*
   Branch-local ANF identities can be reused with different value types.
   The pre-RC SSA builder must type each definition before MIR lowering.
*)
let testPreRcSsaTypesBranchLocalDefinitions () =
  let name = "branchLocalTypes" in
  let identity = TestIds.functionIdForName name in
  let condition = A.TempId 0 in
  let reused = A.TempId 2 in
  let func : A.functionDef =
    {
      A.id = identity;
      name;
      typedParams = [ { A.id = condition; typ = AST.TBool } ];
      returnType = AST.TInt64;
      returnOwnership = A.OwnedReturn;
      body =
        A.If
          ( A.Var condition,
            A.Let
              ( reused,
                A.TypedAtom (A.FloatLiteral 1., AST.TFloat64),
                A.Return (A.IntLiteral (A.Int64 0L)) ),
            A.Let
              ( reused,
                A.TypedAtom (A.BoolLiteral true, AST.TBool),
                A.Return (A.IntLiteral (A.Int64 0L)) ) );
    }
  in
  let ctx : RcTypeFacts.typeContext =
    {
      RcTypeFacts.typeReg = StringOrder.Map.empty;
      variantLookup = StringOrder.Map.empty;
      sumShapeReg = StringOrder.Map.empty;
      funcReg =
        FunctionIdMap.ofList
          [ (identity, (name, AST.TFunction ([ AST.TBool ], AST.TInt64))) ];
      funcParams = StringOrder.Map.empty;
      tempTypes = RcTypeFacts.TempMap.empty;
      closureFuncs = RcTypeFacts.TempMap.empty;
      typePlanning = RcTypeFacts.createRcTypePlanningContext ();
    }
  in
  let* ssa = SSAANF.convertFunctionBeforeRC 2 ctx func in
  let* mir =
    R.convertSSAANFFunction ssa
      (A.TypeMap.ofSeq (List.to_seq [ (A.TempId 0, AST.TBool) ]))
      StringOrder.Map.empty
      (FunctionIdMap.ofList [ (identity, AST.TInt64) ])
      (FunctionIdMap.ofList [ (identity, name) ])
      false
  in
  MIR_SSA_Verify.verifyFunction mir

(*
   A returned join value refers to each predecessor's edge argument. The
   backedge also requires a fixed point through the loop parameter.
*)
let testSsaReturnFlowAcrossJoinAndLoop () =
  let id n = A.TempId n in
  let label n = S.Label n in
  let block n parameters operations terminator : S.block =
    { S.label = label n; parameters; operations; terminator }
  in
  let func : S.functionDef =
    {
      S.id = TestIds.functionIdForName "returnFlow";
      name = "returnFlow";
      typedParams =
        [ { A.id = id 0; typ = AST.TBool }; { A.id = id 1; typ = AST.TString } ];
      returnType = AST.TString;
      returnOwnership = A.OwnedReturn;
      entry = label 0;
      freshValueTypes = RcTypeFacts.TempMap.empty;
      blocks =
        S.LabelMap.of_list
          [
            (label 0, block 0 [] [] (S.Branch (A.Var (id 0), label 1, label 2)));
            (label 1, block 1 [] [] (S.Jump (label 3, [ A.Var (id 1) ])));
            ( label 2,
              block 2 [] [] (S.Jump (label 3, [ A.StringLiteral "other" ])) );
            ( label 3,
              block 3
                [ { A.id = id 3; typ = AST.TString } ]
                [ (id 4, A.Atom (A.Var (id 3))) ]
                (S.Branch (A.Var (id 0), label 4, label 5)) );
            (label 4, block 4 [] [] (S.Jump (label 3, [ A.Var (id 4) ])));
            (label 5, block 5 [] [] (S.Return (A.Var (id 4))));
          ];
    }
  in
  let facts = RcSSAReturnAnalysis.analyze func in
  let live = RcSSAValueLiveness.analyze func in
  let expected =
    S.LabelMap.of_list
      [
        (label 0, RcReturnAnalysis.TempSet.singleton (id 1));
        (label 1, RcReturnAnalysis.TempSet.singleton (id 1));
        (label 2, RcReturnAnalysis.TempSet.empty);
        (label 3, RcReturnAnalysis.TempSet.singleton (id 3));
        (label 4, RcReturnAnalysis.TempSet.singleton (id 4));
        (label 5, RcReturnAnalysis.TempSet.singleton (id 4));
      ]
  in
  if
    not
      (S.LabelMap.equal RcReturnAnalysis.TempSet.equal
         facts.RcSSAReturnAnalysis.atEntry expected)
  then Error "Unexpected SSA return flow"
  else if
    not
      (sameSet
         (Some (RcReturnAnalysis.TempSet.of_list [ id 0; id 3 ]))
         (S.LabelMap.find_opt (label 3) live.RcSSAValueLiveness.atEntry))
  then Error "Unexpected liveness at loop header"
  else if
    not
      (sameSet
         (Some (RcReturnAnalysis.TempSet.of_list [ id 0; id 4 ]))
         (RcTypeFacts.TempMap.find_opt (id 4)
            live.RcSSAValueLiveness.afterDefinition))
  then Error "Unexpected liveness after loop alias"
  else Ok ()

(*
   Edge arguments are substituted in parallel. Sequential substitution loses
   one value when a backedge swaps two block parameters.
*)
let testSsaReturnFlowThroughSwappedParameters () =
  let id n = A.TempId n in
  let label n = S.Label n in
  let param n : A.typedParam = { A.id = id n; typ = AST.TString } in
  let block n parameters terminator : S.block =
    { S.label = label n; parameters; operations = []; terminator }
  in
  let func : S.functionDef =
    {
      S.id = TestIds.functionIdForName "swappedReturnFlow";
      name = "swappedReturnFlow";
      typedParams =
        [
          { A.id = id 0; typ = AST.TBool };
          { A.id = id 1; typ = AST.TString };
          { A.id = id 2; typ = AST.TString };
        ];
      returnType = AST.TString;
      returnOwnership = A.OwnedReturn;
      entry = label 0;
      freshValueTypes = RcTypeFacts.TempMap.empty;
      blocks =
        S.LabelMap.of_list
          [
            ( label 0,
              block 0 [] (S.Jump (label 1, [ A.Var (id 1); A.Var (id 2) ])) );
            ( label 1,
              block 1
                [ param 3; param 4 ]
                (S.Branch (A.Var (id 0), label 2, label 3)) );
            ( label 2,
              block 2 [] (S.Jump (label 1, [ A.Var (id 4); A.Var (id 3) ])) );
            (label 3, block 3 [] (S.Return (A.Var (id 3))));
          ];
    }
  in
  let returned = RcSSAReturnAnalysis.analyze func in
  let live = RcSSAValueLiveness.analyze func in
  let both = RcReturnAnalysis.TempSet.of_list [ id 3; id 4 ] in
  if
    not
      (sameSet (Some both)
         (S.LabelMap.find_opt (label 1) returned.RcSSAReturnAnalysis.atEntry))
  then Error "Swapped return parameters were lost"
  else if
    not
      (sameSet
         (Some (RcReturnAnalysis.TempSet.add (id 0) both))
         (S.LabelMap.find_opt (label 2) live.RcSSAValueLiveness.atEntry))
  then Error "Swapped live parameters were lost"
  else Ok ()

(*
   Imported units can start at high IDs, leave gaps, and refine earlier types.
*)
let testTypeInformationComposition () =
  let original =
    A.TypeMap.ofSeq
      (List.to_seq
         [
           (A.TempId 4002, AST.TString);
           (A.TempId 4000, AST.TInt64);
           (A.TempId 4002, AST.TBool);
         ])
  in
  let imported =
    A.TypeMap.ofSeq
      (List.to_seq
         [
           (A.TempId 3999, AST.TUnit);
           (A.TempId 4002, AST.TFloat64);
           (A.TempId 4004, AST.TString);
         ])
  in
  let merged = A.TypeMap.merge original imported in
  let expected =
    [
      (3998, None);
      (3999, Some AST.TUnit);
      (4000, Some AST.TInt64);
      (4001, None);
      (4002, Some AST.TFloat64);
      (4003, None);
      (4004, Some AST.TString);
      (4005, None);
    ]
  in
  if
    List.exists
      (fun (id, typ) -> A.TypeMap.tryFind (A.TempId id) merged <> typ)
      expected
  then
    Error "Composed type information lost a gap, boundary, or later definition"
  else if A.TypeMap.tryFind (A.TempId 4002) original <> Some AST.TBool then
    Error "Composition changed the original unit's type information"
  else if A.TypeMap.tryFind (A.TempId 0) A.TypeMap.empty <> None then
    Error "An empty unit contains type information"
  else if
    A.TypeMap.merge original A.TypeMap.empty <> original
    || A.TypeMap.merge A.TypeMap.empty imported <> imported
  then Error "Composition with an empty unit lost type information"
  else Ok ()

let tests =
  [
    ( "type information composition preserves gaps and unit boundaries",
      testTypeInformationComposition );
    ( "raw_get intrinsic fallback crashes instead of defaulting to Int64",
      testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64 );
    ( "variant registry rejects inconsistent type parameters",
      testBuildVariantRegistryRejectsInconsistentTypeParams );
    ( "record allocation starts fields at offset zero",
      testRecordAllocationStartsFieldsAtOffsetZero );
    ( "nested terminal branches have no invented return",
      testNestedTerminalBranchesHaveNoInventedReturn );
    ( "phi edges preserve distinct predecessors",
      testPhiEdgesPreserveDistinctPredecessors );
    ( "pre-RC SSA types branch-local definitions",
      testPreRcSsaTypesBranchLocalDefinitions );
    ( "SSA return flow across joins and loops",
      testSsaReturnFlowAcrossJoinAndLoop );
    ( "SSA return flow through swapped parameters",
      testSsaReturnFlowThroughSwappedParameters );
  ]
