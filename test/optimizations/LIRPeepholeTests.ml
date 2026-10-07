(*
   These tests cover post-register-allocation cleanup that is not directly
   visible through the source-to-optimized-LIR test runner.
*)
(* LIRPeepholeTests.ml - Unit tests for local LIR cleanup helpers. *)
[@@@warning "-4-42"]

open Dark_compiler
open LIR
open LIR_Peephole

type testResult = (unit, string) result

let check message actual expected =
  if actual = expected then Ok () else Error message

let fixture name instrs : functionDef =
  let label = Label "entry" in
  let block : basicBlock = { label; instrs; terminator = Ret } in
  {
    id = TestIds.functionIdForName name;
    name;
    typedParams = [];
    cfg = { entry = label; blocks = LabelMap.singleton label block };
    stackSize = 0;
    usedCalleeSaved = [];
    codegenFacts = None;
  }

let optimizeBlockInstrs instrs =
  let block : basicBlock =
    { label = Label "entry"; instrs; terminator = Ret }
  in
  (fst (optimizeBlock block)).instrs

let testRemoveSelfMovesFromAllocatedFunction () =
  let instrs =
    [
      Mov (Physical X1, Reg (Physical X1));
      Mov (Physical X2, Reg (Physical X3));
      Mov (Virtual 4, Reg (Virtual 4));
      FMov (FPhysical D3, FPhysical D3);
      Add (Physical X4, Physical X4, Imm 0L);
    ]
  in
  check "Expected cleanup to preserve the entry block"
    (LabelMap.find (Label "entry")
       (removeSelfMovesFromFunction (fixture "self_move_cleanup" instrs)).cfg
         .blocks)
      .instrs
    [
      Mov (Physical X2, Reg (Physical X3));
      Add (Physical X4, Physical X4, Imm 0L);
    ]

let testRemoveFloatingCopyBackMovesFromAllocatedFunction () =
  let instrs =
    [
      FMov (FPhysical D3, FPhysical D5);
      FMov (FPhysical D2, FPhysical D4);
      FMov (FPhysical D5, FPhysical D3);
      FMov (FPhysical D4, FPhysical D2);
      FAdd (FPhysical D0, FPhysical D3, FPhysical D2);
    ]
  in
  check "Expected cleanup to preserve the entry block"
    (LabelMap.find (Label "entry")
       (removePostAllocationMovesFromFunction
          (fixture "floating_copy_back_cleanup" instrs))
         .cfg
         .blocks)
      .instrs
    [
      FMov (FPhysical D3, FPhysical D5);
      FMov (FPhysical D2, FPhysical D4);
      FAdd (FPhysical D0, FPhysical D3, FPhysical D2);
    ]

let testFloatingCopyBackKeepsMoveAfterFPhiWritesSource () =
  let sourceLabel = Label "source" in
  let instrs =
    [
      FMov (FPhysical D3, FPhysical D5);
      FPhi (FPhysical D5, [ (FPhysical D2, sourceLabel) ]);
      FMov (FPhysical D3, FPhysical D5);
    ]
  in
  check "Expected cleanup to preserve the entry block"
    (LabelMap.find (Label "entry")
       (removePostAllocationMovesFromFunction
          (fixture "floating_copy_back_cleanup" instrs))
         .cfg
         .blocks)
      .instrs
    instrs

let testFNegMoveChainFusesWhenTempDies () =
  let instrs =
    [
      FNeg (FPhysical D0, FPhysical D2);
      FMov (FPhysical D2, FPhysical D0);
      PrintInt64 (Physical X0);
    ]
  in
  check "Expected dead FNeg/FMov chain to fuse"
    (removeSelfMovesFromInstrs instrs)
    [ FNeg (FPhysical D2, FPhysical D2); PrintInt64 (Physical X0) ]

let testFloatingArithmeticMoveChainsFuseWhenTempsDie () =
  let instrs =
    [
      FAdd (FVirtual 1, FVirtual 2, FVirtual 3);
      FMov (FVirtual 4, FVirtual 1);
      FSub (FVirtual 5, FVirtual 6, FVirtual 7);
      FMov (FVirtual 8, FVirtual 5);
      FMul (FVirtual 9, FVirtual 10, FVirtual 11);
      FMov (FVirtual 12, FVirtual 9);
      FDiv (FVirtual 13, FVirtual 14, FVirtual 15);
      FMov (FVirtual 16, FVirtual 13);
    ]
  in
  check "Expected dead floating arithmetic copies to fold"
    (optimizeInstrs instrs)
    [
      FAdd (FVirtual 4, FVirtual 2, FVirtual 3);
      FSub (FVirtual 8, FVirtual 6, FVirtual 7);
      FMul (FVirtual 12, FVirtual 10, FVirtual 11);
      FDiv (FVirtual 16, FVirtual 14, FVirtual 15);
    ]

let testFloatingArithmeticMoveChainKeepsLiveTemp () =
  let instrs =
    [
      FAdd (FVirtual 1, FVirtual 2, FVirtual 3);
      FMov (FVirtual 4, FVirtual 1);
      PrintFloat (FVirtual 1);
    ]
  in
  check "Expected live floating arithmetic temporary to stay available"
    (optimizeInstrs instrs) instrs

let testSeparatedFloatAddKeepsLiveTemporary () =
  let instrs =
    [
      FAdd (FVirtual 1, FVirtual 2, FVirtual 3);
      Mov (Virtual 10, Imm 1L);
      FMov (FVirtual 4, FVirtual 1);
      PrintFloat (FVirtual 1);
    ]
  in
  check "Expected separated FAdd with a live temporary to stay unchanged"
    (retargetSeparatedDeadFAdds instrs)
    instrs

let testSinkSeparatedAllocatedFloatAdd () =
  let instrs =
    [
      FAdd (FPhysical D4, FPhysical D4, FPhysical D0);
      FAdd (FPhysical D2, FPhysical D2, FPhysical D2);
      FMul (FPhysical D2, FPhysical D2, FPhysical D3);
      FAdd (FPhysical D3, FPhysical D2, FPhysical D1);
      Add (Physical X1, Physical X1, Imm 1L);
      FMov (FPhysical D2, FPhysical D4);
    ]
  in
  check "Expected allocated FAdd to replace its separated copy"
    (sinkSeparatedAllocatedFAdds instrs)
    [
      FAdd (FPhysical D2, FPhysical D2, FPhysical D2);
      FMul (FPhysical D2, FPhysical D2, FPhysical D3);
      FAdd (FPhysical D3, FPhysical D2, FPhysical D1);
      Add (Physical X1, Physical X1, Imm 1L);
      FAdd (FPhysical D2, FPhysical D4, FPhysical D0);
    ]

let testSinkImmediateCounterUpdatePastAccumulator () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Add (Physical X2, Physical X2, Reg (Physical X1));
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Add (Physical X2, Physical X2, Reg (Physical X1));
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastSubtraction () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Sub (Physical X2, Physical X2, Reg (Physical X1));
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected subtraction counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Sub (Physical X2, Physical X2, Reg (Physical X1));
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastDivision () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Sdiv (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected division counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Sdiv (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastProduct () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Mul (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected product counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Mul (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastMultiplyAdd () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Madd (Physical X2, Physical X1, Physical X1, Physical X2);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected multiply-add counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Madd (Physical X2, Physical X1, Physical X1, Physical X2);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastMultiplySubtract () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Msub (Physical X2, Physical X1, Physical X1, Physical X2);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check
    "Expected multiply-subtract counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Msub (Physical X2, Physical X1, Physical X1, Physical X2);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastXor () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Eor (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected XOR counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Eor (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastAnd () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      And (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected AND counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         And (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastOr () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Orr (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected OR counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Orr (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastLeftShift () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Lsl (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected left-shift counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Lsl (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testSinkImmediateCounterUpdatePastRightShift () =
  let instrs =
    [
      Sub (Physical X3, Physical X1, Imm 1L);
      Lsr (Physical X2, Physical X2, Physical X1);
      Mov (Physical X1, Reg (Physical X3));
    ]
  in
  check "Expected right-shift counter update to replace its copy-back move"
    (sinkImmediateCounterUpdate instrs)
    (Some
       [
         Lsr (Physical X2, Physical X2, Physical X1);
         Sub (Physical X1, Physical X1, Imm 1L);
       ])

let testMulAddFusionKeepsLiveTempForPrint () =
  let instrs =
    [
      Mul (Virtual 1, Virtual 2, Virtual 3);
      Add (Virtual 4, Virtual 1, Reg (Virtual 5));
      PrintInt64 (Virtual 1);
    ]
  in
  check "Expected MUL temp used by PrintInt64 to stay available"
    (tryFuseMulAdd instrs) instrs

let testMulSubFusionReplacesDeadTemp () =
  let instrs =
    [
      Mul (Virtual 1, Virtual 2, Virtual 3);
      Sub (Virtual 4, Virtual 5, Reg (Virtual 1));
    ]
  in
  check "Expected dead MUL/SUB temporary to fuse into MSUB"
    (optimizeBlockInstrs instrs)
    [ Msub (Virtual 4, Virtual 2, Virtual 3, Virtual 5) ]

let testMulSubFusionKeepsLiveTempForPrint () =
  let instrs =
    [
      Mul (Virtual 1, Virtual 2, Virtual 3);
      Sub (Virtual 4, Virtual 5, Reg (Virtual 1));
      PrintInt64 (Virtual 1);
    ]
  in
  check "Expected MUL temp used by PrintInt64 to stay available"
    (optimizeBlockInstrs instrs)
    instrs

let testMulConstantKeepsLiveConstRegister () =
  let instrs =
    [
      Mov (Physical X1, Imm 3L);
      Mul (Physical X2, Physical X3, Physical X1);
      PrintInt64 (Physical X1);
    ]
  in
  check "Expected live constant register to block strength reduction"
    (tryMulByConstant instrs) instrs

let testFloatMultiplyAddCombineAndTargetDecision () =
  let func =
    fixture "float_multiply_add"
      [
        FMul (FVirtual 1, FVirtual 2, FVirtual 3);
        FAdd (FVirtual 4, FVirtual 1, FVirtual 5);
      ]
  in
  let label = Label "entry" in
  let block = LabelMap.find label func.cfg.blocks in
  let combined = fst (tryFuseFloatMultiplyAdd block.instrs) in
  let armBlock =
    LabelMap.find_opt label (optimizeFunctionFor Platform.ARM64 func).cfg.blocks
  in
  let x64Block =
    LabelMap.find_opt label
      (optimizeFunctionFor Platform.X86_64 func).cfg.blocks
  in
  match (combined, armBlock, x64Block) with
  | ( [ FMadd (FVirtual 4, FVirtual 2, FVirtual 3, FVirtual 5) ],
      Some arm,
      Some x64 )
    when arm.instrs = block.instrs && x64.instrs = block.instrs ->
      Ok ()
  | _ ->
      Error
        "Expected an available FMADD combine rejected by strict target policies"

let testScalarDiamondFormsSelect () =
  let entry = Label "entry" in
  let trueLabel = Label "true" in
  let falseLabel = Label "false" in
  let join = Label "join" in
  let blocks =
    LabelMap.of_list
      [
        ( entry,
          {
            label = entry;
            instrs = [ Cmp (Virtual 0, Imm 0L) ];
            terminator = CondBranch (GT, trueLabel, falseLabel);
          } );
        (trueLabel, { label = trueLabel; instrs = []; terminator = Jump join });
        (falseLabel, { label = falseLabel; instrs = []; terminator = Jump join });
        ( join,
          {
            label = join;
            instrs =
              [
                Phi
                  ( Virtual 3,
                    [
                      (Reg (Virtual 1), trueLabel); (Reg (Virtual 2), falseLabel);
                    ],
                    Some AST.TInt64 );
              ];
            terminator = Ret;
          } );
      ]
  in
  let optimized = optimizeCFG { entry; blocks } in
  match
    ( LabelMap.find_opt entry optimized.blocks,
      LabelMap.find_opt join optimized.blocks )
  with
  | Some entryBlock, Some joinBlock
    when entryBlock.instrs
         = [
             Cmp (Virtual 0, Imm 0L);
             Select (Virtual 3, Virtual 1, Virtual 2, GT);
           ]
         && entryBlock.terminator = Jump join
         && joinBlock.instrs = []
         && (not (LabelMap.mem trueLabel optimized.blocks))
         && not (LabelMap.mem falseLabel optimized.blocks) ->
      Ok ()
  | _ -> Error "Expected empty scalar diamond to become Select"

let testBranchZeroDiamondFormsMultipleNarrowSelects () =
  let entry = Label "zero_entry" in
  let zeroLabel = Label "zero_arm" in
  let nonzeroLabel = Label "nonzero_arm" in
  let join = Label "narrow_join" in
  let blocks =
    LabelMap.of_list
      [
        ( entry,
          {
            label = entry;
            instrs = [];
            terminator = BranchZero (Virtual 0, zeroLabel, nonzeroLabel);
          } );
        (zeroLabel, { label = zeroLabel; instrs = []; terminator = Jump join });
        ( nonzeroLabel,
          { label = nonzeroLabel; instrs = []; terminator = Jump join } );
        ( join,
          {
            label = join;
            instrs =
              [
                Phi
                  ( Virtual 5,
                    [
                      (Reg (Virtual 1), zeroLabel);
                      (Reg (Virtual 2), nonzeroLabel);
                    ],
                    Some AST.TUInt8 );
                Phi
                  ( Virtual 6,
                    [
                      (Reg (Virtual 3), zeroLabel);
                      (Reg (Virtual 4), nonzeroLabel);
                    ],
                    Some AST.TBool );
              ];
            terminator = Ret;
          } );
      ]
  in
  let optimized = optimizeCFG { entry; blocks } in
  match
    ( LabelMap.find_opt entry optimized.blocks,
      LabelMap.find_opt join optimized.blocks )
  with
  | Some entryBlock, Some joinBlock
    when entryBlock.instrs
         = [
             Cmp (Virtual 0, Imm 0L);
             Select (Virtual 5, Virtual 1, Virtual 2, EQ);
             Select (Virtual 6, Virtual 3, Virtual 4, EQ);
           ]
         && entryBlock.terminator = Jump join
         && joinBlock.instrs = [] ->
      Ok ()
  | _ ->
      Error "Expected BranchZero with UInt8 and Bool phis to form two selects"

let testMaterializedBooleanDiamondFormsSelect () =
  let entry = Label "bool_entry" in
  let trueLabel = Label "bool_true" in
  let falseLabel = Label "bool_false" in
  let join = Label "bool_join" in
  let blocks =
    LabelMap.of_list
      [
        ( entry,
          {
            label = entry;
            instrs = [];
            terminator = Branch (Virtual 0, trueLabel, falseLabel);
          } );
        (trueLabel, { label = trueLabel; instrs = []; terminator = Jump join });
        (falseLabel, { label = falseLabel; instrs = []; terminator = Jump join });
        ( join,
          {
            label = join;
            instrs =
              [
                Phi
                  ( Virtual 3,
                    [
                      (Reg (Virtual 1), trueLabel); (Reg (Virtual 2), falseLabel);
                    ],
                    Some AST.TInt64 );
              ];
            terminator = Ret;
          } );
      ]
  in
  let optimized = optimizeCFG { entry; blocks } in
  match
    ( LabelMap.find_opt entry optimized.blocks,
      LabelMap.find_opt join optimized.blocks )
  with
  | Some entryBlock, Some joinBlock
    when entryBlock.instrs
         = [
             Cmp (Virtual 0, Imm 0L);
             Select (Virtual 3, Virtual 1, Virtual 2, NE);
           ]
         && entryBlock.terminator = Jump join
         && joinBlock.instrs = [] ->
      Ok ()
  | _ -> Error "Expected a materialized Boolean Branch diamond to become Select"

let testBooleanNotBranchSwapsSuccessors () =
  let condition = Virtual 1 in
  let negated = Virtual 2 in
  let trueLabel = Label "true" in
  let falseLabel = Label "false" in
  let block : basicBlock =
    {
      label = Label "entry";
      instrs = [ Mov (negated, Imm 1L); Sub (negated, negated, Reg condition) ];
      terminator = Branch (negated, trueLabel, falseLabel);
    }
  in
  let optimized = fst (optimizeBlock block) in
  if
    optimized.instrs = []
    && optimized.terminator = Branch (condition, falseLabel, trueLabel)
  then Ok ()
  else Error "Expected Boolean negation branch to swap successors"

let testConditionalBranchKeepsBooleanUsedInSuccessor () =
  let entry = Label "entry" in
  let trueLabel = Label "true" in
  let falseLabel = Label "false" in
  let condition = Virtual 1 in
  let entryBlock : basicBlock =
    {
      label = entry;
      instrs = [ Cmp (Virtual 2, Reg (Virtual 3)); Cset (condition, LT) ];
      terminator = Branch (condition, trueLabel, falseLabel);
    }
  in
  let trueBlock : basicBlock =
    { label = trueLabel; instrs = [ PrintBool condition ]; terminator = Ret }
  in
  let falseBlock : basicBlock =
    { label = falseLabel; instrs = []; terminator = Ret }
  in
  let cfg : cfg =
    {
      entry;
      blocks =
        LabelMap.of_list
          [
            (entry, entryBlock); (trueLabel, trueBlock); (falseLabel, falseBlock);
          ];
    }
  in
  match LabelMap.find_opt entry (optimizeCFG cfg).blocks with
  | None -> Error "Expected optimization to preserve the entry block"
  | Some optimizedEntry when optimizedEntry = entryBlock -> Ok ()
  | Some _ -> Error "Expected successor-visible Boolean to stay materialized"

let testOptimizeCFGRejectsMissingSuccessorLabel () =
  let entry = Label "entry" in
  let missing = Label "missing" in
  let block : basicBlock =
    { label = entry; instrs = []; terminator = Jump missing }
  in
  let cfg : cfg = { entry; blocks = LabelMap.singleton entry block } in
  try
    ignore (optimizeCFG cfg);
    Error "Expected LIR peephole to reject a missing successor label"
  with Failure message | Invalid_argument message ->
    if
      let needle = "successor label" in
      let rec contains index =
        index + String.length needle <= String.length message
        && (String.sub message index (String.length needle) = needle
           || contains (index + 1))
      in
      contains 0
    then Ok ()
    else Error ("Expected missing successor label crash, got: " ^ message)

(* Typed projections of all unchanged lir-peepholes.liropt inputs and expectations.
   The complete DSL parser and runner remain separate inventory components. *)
let dslFixtures =
  [
    ( "mul_add_left_fuses_to_madd",
      [
        Mul (Virtual 1, Virtual 2, Virtual 3);
        Add (Virtual 4, Virtual 1, Reg (Virtual 5));
      ],
      [ Madd (Virtual 4, Virtual 2, Virtual 3, Virtual 5) ] );
    ( "mul_add_right_fuses_to_madd",
      [
        Mul (Virtual 1, Virtual 2, Virtual 3);
        Add (Virtual 4, Virtual 5, Reg (Virtual 1));
      ],
      [ Madd (Virtual 4, Virtual 2, Virtual 3, Virtual 5) ] );
    ( "live_multiply_result_prevents_madd",
      [
        Mul (Virtual 1, Virtual 2, Virtual 3);
        Add (Virtual 4, Virtual 1, Reg (Virtual 5));
        PrintInt64 (Virtual 1);
      ],
      [
        Mul (Virtual 1, Virtual 2, Virtual 3);
        Add (Virtual 4, Virtual 1, Reg (Virtual 5));
        PrintInt64 (Virtual 1);
      ] );
    ( "add_zero_retargets_destination",
      [ Add (Virtual 2, Virtual 1, Imm 0L) ],
      [ Mov (Virtual 2, Reg (Virtual 1)) ] );
    ( "add_zero_same_destination_disappears",
      [ Add (Virtual 1, Virtual 1, Imm 0L) ],
      [] );
    ( "subtract_zero_retargets_destination",
      [ Sub (Virtual 2, Virtual 1, Imm 0L) ],
      [ Mov (Virtual 2, Reg (Virtual 1)) ] );
    ( "subtract_zero_same_destination_disappears",
      [ Sub (Virtual 1, Virtual 1, Imm 0L) ],
      [] );
    ( "multiply_by_power_plus_one_right_strength_reduces",
      [ Mov (Virtual 1, Imm 3L); Mul (Virtual 4, Virtual 2, Virtual 1) ],
      [
        Lsl_imm (Virtual 1, Virtual 2, 1);
        Add (Virtual 4, Virtual 2, Reg (Virtual 1));
      ] );
    ( "multiply_by_power_plus_one_left_strength_reduces",
      [ Mov (Virtual 1, Imm 3L); Mul (Virtual 4, Virtual 1, Virtual 2) ],
      [
        Lsl_imm (Virtual 1, Virtual 2, 1);
        Add (Virtual 4, Virtual 2, Reg (Virtual 1));
      ] );
    ( "multiply_by_power_minus_one_right_strength_reduces",
      [ Mov (Virtual 1, Imm 7L); Mul (Virtual 4, Virtual 2, Virtual 1) ],
      [
        Lsl_imm (Virtual 1, Virtual 2, 3);
        Sub (Virtual 4, Virtual 1, Reg (Virtual 2));
      ] );
    ( "multiply_by_power_minus_one_left_strength_reduces",
      [ Mov (Virtual 1, Imm 7L); Mul (Virtual 4, Virtual 1, Virtual 2) ],
      [
        Lsl_imm (Virtual 1, Virtual 2, 3);
        Sub (Virtual 4, Virtual 1, Reg (Virtual 2));
      ] );
  ]

let tests =
  [
    ( "LIR peephole removes self-moves from allocated function",
      testRemoveSelfMovesFromAllocatedFunction );
    ( "LIR peephole removes floating copy-back moves",
      testRemoveFloatingCopyBackMovesFromAllocatedFunction );
    ( "LIR peephole keeps floating copy-back after FPhi writes source",
      testFloatingCopyBackKeepsMoveAfterFPhiWritesSource );
    ( "LIR peephole fuses FNeg followed by dead-temp FMov",
      testFNegMoveChainFusesWhenTempDies );
    ( "LIR peephole folds dead floating arithmetic copies",
      testFloatingArithmeticMoveChainsFuseWhenTempsDie );
    ( "LIR peephole keeps live floating arithmetic temporaries",
      testFloatingArithmeticMoveChainKeepsLiveTemp );
    ( "LIR peephole keeps live separated FAdd temporaries",
      testSeparatedFloatAddKeepsLiveTemporary );
    ( "LIR peephole sinks separated allocated FAdd",
      testSinkSeparatedAllocatedFloatAdd );
    ( "LIR peephole sinks immediate counter update",
      testSinkImmediateCounterUpdatePastAccumulator );
    ( "LIR peephole sinks immediate counter update past subtraction",
      testSinkImmediateCounterUpdatePastSubtraction );
    ( "LIR peephole sinks immediate counter update past division",
      testSinkImmediateCounterUpdatePastDivision );
    ( "LIR peephole sinks immediate counter update past product",
      testSinkImmediateCounterUpdatePastProduct );
    ( "LIR peephole sinks immediate counter update past multiply-add",
      testSinkImmediateCounterUpdatePastMultiplyAdd );
    ( "LIR peephole sinks immediate counter update past multiply-subtract",
      testSinkImmediateCounterUpdatePastMultiplySubtract );
    ( "LIR peephole sinks immediate counter update past XOR",
      testSinkImmediateCounterUpdatePastXor );
    ( "LIR peephole sinks immediate counter update past AND",
      testSinkImmediateCounterUpdatePastAnd );
    ( "LIR peephole sinks immediate counter update past OR",
      testSinkImmediateCounterUpdatePastOr );
    ( "LIR peephole sinks immediate counter update past left shift",
      testSinkImmediateCounterUpdatePastLeftShift );
    ( "LIR peephole sinks immediate counter update past right shift",
      testSinkImmediateCounterUpdatePastRightShift );
    ( "LIR peephole keeps MUL temp used by later print",
      testMulAddFusionKeepsLiveTempForPrint );
    ( "LIR peephole fuses dead MUL/SUB temporary into MSUB",
      testMulSubFusionReplacesDeadTemp );
    ( "LIR peephole keeps MUL/SUB temporary used by later print",
      testMulSubFusionKeepsLiveTempForPrint );
    ( "LIR peephole exposes FMADD combine but preserves strict target rounding",
      testFloatMultiplyAddCombineAndTargetDecision );
    ( "LIR peephole forms scalar selects from empty diamonds",
      testScalarDiamondFormsSelect );
    ( "LIR peephole forms multiple narrow selects from BranchZero diamonds",
      testBranchZeroDiamondFormsMultipleNarrowSelects );
    ( "LIR peephole forms selects from materialized Boolean diamonds",
      testMaterializedBooleanDiamondFormsSelect );
    ( "LIR peephole keeps multiply constants that are used later",
      testMulConstantKeepsLiveConstRegister );
    ( "LIR peephole swaps Boolean negation branch successors",
      testBooleanNotBranchSwapsSuccessors );
    ( "LIR peephole keeps branch Boolean used by a successor",
      testConditionalBranchKeepsBooleanUsedInSuccessor );
    ( "LIR peephole rejects missing successor labels",
      testOptimizeCFGRejectsMissingSuccessorLabel );
  ]

let dslTests =
  List.map
    (fun (name, input, expected) ->
      ( "lir-peepholes.liropt: " ^ name,
        fun () ->
          check "Existing LIR DSL expectation differs"
            ( (optimizeFunction (fixture "_start" input)).cfg.blocks
            |> LabelMap.find (Label "entry")
            |> fun block -> block.instrs )
            expected ))
    dslFixtures
