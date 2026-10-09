(* X64ClosureReferenceCounts.ml - Generate closure and recursive-payload lifetime helpers. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
module X = X86_64

let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

let closurePayloadSizesFromAllocs functions =
  functions
  |> List.concat_map (fun (func : LIR.functionDef) ->
      LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks
      |> List.concat_map (fun (_, (block : LIR.basicBlock)) ->
          List.filter_map
            (function
              | LIR.ClosureAlloc (_, funcName, captures) ->
                  Some (funcName, mul (add (List.length captures) 1) 8)
              | _ -> None)
            block.LIR.instrs))
  |> FunctionIdMap.ofList

let closureCaptureTypesFromParams functions =
  functions
  |> List.filter_map (fun (func : LIR.functionDef) ->
      match func.LIR.typedParams with
      | { LIR.typ = AST.TTuple (_funcPtrType :: captures); _ } :: _ ->
          Some (func.LIR.name, captures)
      | _ -> None)
  |> StringOrder.Map.of_list

let closurePayloadSizesFromParams functions =
  functions
  |> List.filter_map (fun (func : LIR.functionDef) ->
      match func.LIR.typedParams with
      | { LIR.typ = AST.TTuple fields; _ } :: _ ->
          Some (func.LIR.name, mul (List.length fields) 8)
      | _ -> None)
  |> StringOrder.Map.of_list

let generateClosureRefCountIncHelper closurePayloadSizes =
  let label name = "__dark_closure_rc_inc_" ^ name in
  let helperRet = label "ret" in
  let payloadReady = label "payload_ready" in
  let payloadCases =
    StringOrder.Map.bindings closurePayloadSizes
    |> List.filter (fun (_, size) -> size <> 8)
    |> List.mapi (fun index (funcName, payloadSize) ->
        let nextCase = label ("payload_next_" ^ string_of_int index) in
        [
          X.LEA_rip (X.R10, funcName);
          X.CMP_reg (X.RDX, X.R10);
          X.Jcc (X.NE, nextCase);
          X.MOV_imm32 (X.RCX, Int32.of_int payloadSize);
          X.JMP payloadReady;
          X.Label nextCase;
        ])
    |> List.concat
  in
  [
    X.Label closureRefCountIncHelperLabel;
    X.TEST_reg (X.RAX, X.RAX);
    X.Jcc (X.EQ, helperRet);
    X.MOV_load (X.RDX, X.RAX, 0l);
    X.MOV_imm32 (X.RCX, 8l);
  ]
  @ payloadCases
  @ [
      X.Label payloadReady;
      X.MOV_reg (X.RDI, X.RAX);
      X.ADD_reg (X.RDI, X.RCX);
      X.MOV_load (X.RDX, X.RDI, 0l);
      X.ADD_imm (X.RDX, 1l);
      X.MOV_store (X.RDI, 0l, X.RDX);
      X.Label helperRet;
      X.RET;
    ]

let generateClosureRefCountDecHelper enableLeakCheck recordRegistry
    sumShapeRegistry closurePayloadSizes closureCaptureTypes =
  let label name = "__dark_closure_rc_dec_" ^ name in
  let helperRet = label "ret" in
  let payloadReady = label "payload_ready" in
  let skipFreeList = label "skip_freelist" in
  let capturesReleased = label "captures_released" in
  let leakDec =
    if enableLeakCheck then
      [
        X.PUSH scratch;
        X.PUSH X.RCX;
        X.LEA_rip (scratch, "_leak_count");
        X.MOV_load (X.RCX, scratch, 0l);
        X.SUB_imm (X.RCX, 1l);
        X.MOV_store (scratch, 0l, X.RCX);
        X.POP X.RCX;
        X.POP scratch;
      ]
    else []
  in
  let helperCtx =
    {
      functionName = closureRefCountDecHelperLabel;
      stackSize = 0;
      usedCalleeSaved = [];
      enableLeakCheck;
      recordRegistry;
      sumShapeRegistry;
      functionNames = FunctionIdMap.empty;
    }
  in
  let payloadCases =
    StringOrder.Map.bindings closurePayloadSizes
    |> List.filter (fun (_, size) -> size <> 8)
    |> List.mapi (fun index (funcName, payloadSize) ->
        let nextCase = label ("payload_next_" ^ string_of_int index) in
        [
          X.LEA_rip (X.R10, funcName);
          X.CMP_reg (X.RDX, X.R10);
          X.Jcc (X.NE, nextCase);
          X.MOV_imm32 (X.RCX, Int32.of_int payloadSize);
          X.JMP payloadReady;
          X.Label nextCase;
        ])
    |> List.concat
  in
  let releaseDynamicBufferCapture skipTagged fieldOffset suffix =
    let doneLabel = label ("capture_dynamic_done_" ^ suffix) in
    let taggedGuard =
      if skipTagged then
        [
          X.MOV_reg (X.R10, X.R9); X.AND_imm (X.R10, 1l); X.Jcc (X.NE, doneLabel);
        ]
      else []
    in
    [
      X.MOV_load (X.R9, X.RAX, Int32.of_int fieldOffset);
      X.TEST_reg (X.R9, X.R9);
      X.Jcc (X.EQ, doneLabel);
    ]
    @ taggedGuard
    @ [ X.MOV_reg (X.R10, X.R9); X.MOV_load (X.RDX, X.R10, 0l) ]
    @ loadImm64 scratch Int64.max_int
    @ [
        X.CMP_reg (X.RDX, scratch);
        X.Jcc (X.EQ, doneLabel);
        X.SUB_imm (X.RDX, 1l);
        X.MOV_store (X.R10, 0l, X.RDX);
        X.TEST_reg (X.RDX, X.RDX);
        X.Jcc (X.NE, doneLabel);
      ]
    @ leakDec @ [ X.Label doneLabel ]
  in
  let releaseHeapRootCapture fieldOffset helperLabel suffix =
    let doneLabel = label ("capture_root_done_" ^ suffix) in
    let saveRegs =
      [ X.RAX; X.RCX; X.RDX; X.RDI; X.RSI; X.R8; X.R9; X.R10; scratch ]
    in
    let saves = List.map (fun reg -> X.PUSH reg) saveRegs in
    let restores = List.map (fun reg -> X.POP reg) (List.rev saveRegs) in
    [
      X.MOV_load (X.R9, X.RAX, Int32.of_int fieldOffset);
      X.TEST_reg (X.R9, X.R9);
      X.Jcc (X.EQ, doneLabel);
    ]
    @ saves
    @ [ X.MOV_reg (X.RAX, X.R9); X.CALL helperLabel ]
    @ restores @ [ X.Label doneLabel ]
  in
   (* Sum representations can share a list/dict/stream root directly. Dispatch
    by the release plan so a captured Option<List<_>> releases its list edge. *)
 let releaseAggregateCapture fieldOffset captureType suffix =
    match
      tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType
    with
      | Some (MemoryModel.RootRelease (_,MemoryModel.TaggedList,_) as releasePlan) ->
    releaseHeapRootCapture fieldOffset (listDecHelperForReleasePlan releasePlan) (suffix^"_list")
  | Some (MemoryModel.RootRelease (_,MemoryModel.DictHeap,_) as releasePlan) ->
    releaseHeapRootCapture fieldOffset (dictDecHelperForReleasePlan releasePlan) (suffix^"_dict")
  | Some (MemoryModel.RootRelease (_,MemoryModel.StreamHeap,_)) ->
    releaseHeapRootCapture fieldOffset streamRefCountDecHelperLabel (suffix^"_stream")
  | Some (MemoryModel.RootRelease (_,MemoryModel.ClosureHeap,_)) ->
    releaseHeapRootCapture fieldOffset closureRefCountDecHelperLabel (suffix^"_closure")
  | Some (MemoryModel.DynamicBufferRelease operation) ->
    releaseDynamicBufferCapture (operation=MemoryModel.DynamicIntBuffer) fieldOffset suffix
| Some
        (MemoryModel.RootRelease
           ( payloadSize,
             MemoryModel.GenericHeap,
             ( MemoryModel.FixedBlockPayloadRelease _
             | MemoryModel.BoxedSumPayloadRelease _ ) ) as releasePlan) ->
        let release =
          genRefCountDecGenericWithPlan helperCtx X.R9 payloadSize
            (Some releasePlan)
        in
        [ X.MOV_load (X.R9, X.RAX, Int32.of_int fieldOffset); X.PUSH X.RAX ]
        @ release @ [ X.POP X.RAX ]
    | _ -> []
  in
  let releaseCaptureCases =
    StringOrder.Map.bindings closureCaptureTypes
    |> List.mapi (fun index (funcName, captureTypes) ->
        let nextCase = label ("captures_next_" ^ string_of_int index) in
        let releases =
          List.mapi
            (fun captureIndex captureType ->
              let fieldOffset = mul (add captureIndex 1) 8 in
              let suffix =
                string_of_int index ^ "_" ^ string_of_int captureIndex
              in
              match captureType with
              | AST.TString | AST.TChar | AST.TBlob ->
                  releaseDynamicBufferCapture false fieldOffset suffix
              | AST.TInt -> releaseDynamicBufferCapture true fieldOffset suffix
              | AST.TList _ ->
                  releaseHeapRootCapture fieldOffset
                    (listDecHelperForType recordRegistry sumShapeRegistry
                       captureType)
                    (suffix ^ "_list")
              | AST.TDict _ -> (
                  match
                    tryRcReleasePlanOfType recordRegistry sumShapeRegistry
                      captureType
                  with
                  | Some releasePlan ->
                      releaseHeapRootCapture fieldOffset
                        (dictDecHelperForReleasePlan releasePlan)
                        (suffix ^ "_dict")
                  | None ->
                      Crash.crash
                        ("generateClosureRefCountDecHelper: missing RC \
                          metadata for dict capture type "
                        ^ StructuralFormat.semanticType captureType))
              | AST.TFunction _ ->
                  releaseHeapRootCapture fieldOffset
                    closureRefCountDecHelperLabel (suffix ^ "_closure")
              | AST.TStream _ ->
                  releaseHeapRootCapture fieldOffset
                    streamRefCountDecHelperLabel (suffix ^ "_stream")
                 | AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> releaseAggregateCapture fieldOffset captureType suffix
              | _ -> [])
            captureTypes
          |> List.concat
        in
        [
          X.LEA_rip (X.R9, funcName);
          X.CMP_reg (X.R8, X.R9);
          X.Jcc (X.NE, nextCase);
        ]
        @ releases
        @ [ X.JMP capturesReleased; X.Label nextCase ])
    |> List.concat
  in
  [
    X.Label closureRefCountDecHelperLabel;
    X.TEST_reg (X.RAX, X.RAX);
    X.Jcc (X.EQ, helperRet);
    X.MOV_load (X.RDX, X.RAX, 0l);
    X.MOV_imm32 (X.RCX, 8l);
  ]
  @ payloadCases
  @ [
      X.Label payloadReady;
      X.MOV_reg (X.RDI, X.RAX);
      X.ADD_reg (X.RDI, X.RCX);
      X.MOV_load (X.RDX, X.RDI, 0l);
      X.SUB_imm (X.RDX, 1l);
      X.MOV_store (X.RDI, 0l, X.RDX);
      X.TEST_reg (X.RDX, X.RDX);
      X.Jcc (X.NE, helperRet);
      X.MOV_load (X.R8, X.RAX, 0l);
    ]
  @ releaseCaptureCases
  @ [
      X.Label capturesReleased;
      X.CMP_imm (X.RCX, Int32.of_int freeListSize);
      X.Jcc (X.GE, skipFreeList);
      X.MOV_reg (X.RDI, freeListBase);
      X.ADD_reg (X.RDI, X.RCX);
      X.MOV_load (X.RDX, X.RDI, 0l);
      X.MOV_store (X.RAX, 0l, X.RDX);
      X.MOV_store (X.RDI, 0l, X.RAX);
      X.Label skipFreeList;
    ]
  @ leakDec
  @ [ X.Label helperRet; X.RET ]
