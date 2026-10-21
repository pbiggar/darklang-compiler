(*
   These tests cover internal CFG invariants that cannot be exercised cleanly
   through source-level end-to-end programs.
*)
(* MIRGraphAnalysisTests.ml - Test MIR control-flow and register-use analyses. *)
[@@@warning "-4-42"]

open Dark_compiler
open MIR
open MIRGraphAnalysis

type testResult = (unit, string) result

let label name = Label name
let vreg id = VReg id

let makeBlock label instrs terminator : basicBlock =
  { label; instrs; terminator }

let format value = StructuralFormat.format value

let uses values =
  format
    (StructuralValue.Union
       ( "set",
         [
           StructuralValue.Sequence
             (List.map MIRTestFormatting.vReg (VRegSet.elements values));
         ] ))

let labels values =
  format
    (StructuralValue.Union
       ( "map",
         [
           StructuralValue.Sequence
             (List.map
                (fun (left, right) ->
                  StructuralValue.Tuple
                    [
                      MIRTestFormatting.label left;
                      MIRTestFormatting.label right;
                    ])
                (LabelMap.bindings values));
         ] ))

let testGetBlockUsesCoversEveryOperandPosition () =
  let destination = vreg 0
  and first = vreg 1
  and second = vreg 2
  and third = vreg 3 in
  let register reg = Register reg in
  let expected regs = VRegSet.of_list regs in
  let target = label "target" in
  let instructionCases =
    [
      ( "Mov",
        Mov (destination, register first, Some AST.TInt64),
        expected [ first ] );
      ( "BinOp",
        BinOp (destination, Add, register first, register second, AST.TInt64),
        expected [ first; second ] );
      ("UnaryOp", UnaryOp (destination, Neg, register first), expected [ first ]);
      ( "Call",
        Call
          ( destination,
            TestIds.functionIdForName "callee",
            [ register first; Int64Const 1L; register second ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ],
            AST.TInt64 ),
        expected [ first; second ] );
      ( "TailCall",
        TailCall
          ( TestIds.functionIdForName "callee",
            [ register first; Int64Const 1L; register second ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ],
            AST.TInt64 ),
        expected [ first; second ] );
      ( "IndirectCall",
        IndirectCall
          ( destination,
            register first,
            [ register second; Int64Const 1L; register third ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ],
            AST.TInt64 ),
        expected [ first; second; third ] );
      ( "IndirectTailCall",
        IndirectTailCall
          ( register first,
            [ register second; Int64Const 1L; register third ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ],
            AST.TInt64 ),
        expected [ first; second; third ] );
      ( "ClosureAlloc",
        ClosureAlloc
          ( destination,
            TestIds.functionIdForName "callee",
            [ register first; Int64Const 1L; register second ] ),
        expected [ first; second ] );
      ( "ClosureCall",
        ClosureCall
          ( destination,
            register first,
            [ register second; Int64Const 1L; register third ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ],
            AST.TInt64 ),
        expected [ first; second; third ] );
      ( "ClosureTailCall",
        ClosureTailCall
          ( register first,
            [ register second; Int64Const 1L; register third ],
            [ AST.TInt64; AST.TInt64; AST.TInt64 ] ),
        expected [ first; second; third ] );
      ( "HeapStore",
        HeapStore (first, 0, register second, Some AST.TInt64),
        expected [ first; second ] );
      ( "HeapLoad",
        HeapLoad (destination, first, 0, Some AST.TInt64),
        expected [ first ] );
      ( "StringConcat",
        StringConcat (destination, register first, register second, []),
        expected [ first; second ] );
      ( "RefCountInc",
        RefCountInc (first, 8, GenericHeap, None),
        expected [ first ] );
      ( "RefCountDec",
        RefCountDec (first, 8, GenericHeap, None),
        expected [ first ] );
      ("Print", Print (register first, AST.TInt64), expected [ first ]);
      ( "FileReadBlob",
        FileReadBlob (destination, register first),
        expected [ first ] );
      ( "FileExists",
        FileExists (destination, register first),
        expected [ first ] );
      ( "FileWriteBlob",
        FileWriteBlob (destination, register first, register second),
        expected [ first; second ] );
      ( "FileAppendText",
        FileAppendText (destination, register first, register second),
        expected [ first; second ] );
      ( "FileDelete",
        FileDelete (destination, register first),
        expected [ first ] );
      ( "FileSetExecutable",
        FileSetExecutable (destination, register first),
        expected [ first ] );
      ( "FileWriteFromPtr",
        FileWriteFromPtr
          (destination, register first, register second, register third),
        expected [ first; second; third ] );
      ( "Phi",
        Phi
          ( destination,
            [
              (register first, target);
              (Int64Const 1L, target);
              (register second, target);
            ],
            Some AST.TInt64 ),
        expected [ first; second ] );
      ("RawAlloc", RawAlloc (destination, register first), expected [ first ]);
      ("RawFree", RawFree (register first), expected [ first ]);
      ( "RawGet",
        RawGet (destination, register first, register second, Some AST.TInt64),
        expected [ first; second ] );
      ( "RawGetByte",
        RawGetByte (destination, register first, register second),
        expected [ first; second ] );
      ( "RawWriteWord",
        RawWriteWord (register first, register second, register third),
        expected [ first; second; third ] );
      ( "RawWriteByte",
        RawWriteByte (register first, register second, register third),
        expected [ first; second; third ] );
      ( "RawSlotInit",
        RawSlotInit (register first, register second, register third, AST.TInt64),
        expected [ first; second; third ] );
      ( "StringToRawPtr",
        StringToRawPtr (destination, register first),
        expected [ first ] );
      ( "RawPtrToString",
        RawPtrToString (destination, register first),
        expected [ first ] );
      ( "BlobToRawPtr",
        BlobToRawPtr (destination, register first),
        expected [ first ] );
      ( "RawPtrToBlob",
        RawPtrToBlob (destination, register first),
        expected [ first ] );
      ( "DictToRawPtr",
        DictToRawPtr (destination, register first),
        expected [ first ] );
      ( "RawPtrToDict",
        RawPtrToDict (destination, register first, register second),
        expected [ first; second ] );
      ( "ListToRawPtr",
        ListToRawPtr (destination, register first),
        expected [ first ] );
      ( "RawPtrToList",
        RawPtrToList (destination, register first, register second),
        expected [ first; second ] );
      ("FloatSqrt", FloatSqrt (destination, register first), expected [ first ]);
      ("FloatAbs", FloatAbs (destination, register first), expected [ first ]);
      ("FloatNeg", FloatNeg (destination, register first), expected [ first ]);
      ( "Int64ToFloat",
        Int64ToFloat (destination, register first),
        expected [ first ] );
      ( "FloatToInt64",
        FloatToInt64 (destination, register first),
        expected [ first ] );
      ( "FloatToBits",
        FloatToBits (destination, register first),
        expected [ first ] );
      ( "RefCountIncString",
        RefCountIncString (register first),
        expected [ first ] );
      ( "RefCountDecString",
        RefCountDecString (register first),
        expected [ first ] );
      ("RefCountIncBlob", RefCountIncBlob (register first), expected [ first ]);
      ("RefCountDecBlob", RefCountDecBlob (register first), expected [ first ]);
      ( "FloatToString",
        FloatToString (destination, register first),
        expected [ first ] );
    ]
  in
  let terminators =
    [
      ("Ret", Ret (register first), expected [ first ]);
      ("Branch", Branch (register first, target, target), expected [ first ]);
      ("Jump", Jump target, VRegSet.empty);
    ]
  in
  let check name block expected =
    let actual = getBlockUses block in
    if VRegSet.equal actual expected then None
    else
      Some (name ^ ": expected uses " ^ uses expected ^ ", got " ^ uses actual)
  in
  match
    List.find_map
      (fun (name, instr, expected) ->
        check name (makeBlock (label name) [ instr ] (Jump target)) expected)
      instructionCases
  with
  | Some error -> Error error
  | None -> (
      match
        List.find_map
          (fun (name, term, expected) ->
            check name (makeBlock (label name) [] term) expected)
          terminators
      with
      | Some error -> Error error
      | None -> Ok ())

let testComputeDominatorsHandlesJoinLoopAndUnreachableBlock () =
  let entry = label "entry"
  and left = label "left"
  and right = label "right"
  and join = label "join"
  and header = label "header"
  and body = label "body"
  and exit = label "exit"
  and unreachable = label "unreachable" in
  let blocks =
    [
      makeBlock entry [] (Branch (Register (vreg 0), left, right));
      makeBlock left [] (Jump join);
      makeBlock right [] (Jump join);
      makeBlock join [] (Jump header);
      makeBlock header [] (Branch (Register (vreg 1), body, exit));
      makeBlock body [] (Jump header);
      makeBlock exit [] (Ret (Int64Const 0L));
      makeBlock unreachable [] (Ret (Int64Const 1L));
    ]
  in
  let cfg =
    {
      entry;
      blocks =
        LabelMap.of_list (List.map (fun block -> (block.label, block)) blocks);
    }
  in
  let expected =
    LabelMap.of_list
      [
        (left, entry);
        (right, entry);
        (join, entry);
        (header, join);
        (body, header);
        (exit, header);
      ]
  in
  let actual = computeDominators cfg (buildPredecessors cfg) in
  if LabelMap.equal ( = ) actual expected then Ok ()
  else
    Error
      ("Expected immediate dominators " ^ labels expected ^ ", got "
     ^ labels actual)

let tests =
  [
    ( "getBlockUses covers every operand position",
      testGetBlockUsesCoversEveryOperandPosition );
    ( "computeDominators handles join, loop, and unreachable block",
      testComputeDominatorsHandlesJoinLoopAndUnreachableBlock );
  ]
