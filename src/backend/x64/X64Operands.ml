(*
   X64Operands.ml - Materialize target operands, registers, and immediates.
   Resolve a LIR.FReg to x86-64 XMM register.
   Emit inline 8-byte-at-a-time copy of a UTF-8 byte array to heap memory.
   Stores bytes starting after the two-word dynamic-buffer header.
*)
[@@@warning "-4"]

let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b = Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let syscalls = Platform.linuxX86_64SyscallNumbers

let physName = function
  | LIR.X0 -> "X0"
  | LIR.X1 -> "X1"
  | LIR.X2 -> "X2"
  | LIR.X3 -> "X3"
  | LIR.X4 -> "X4"
  | LIR.X5 -> "X5"
  | LIR.X6 -> "X6"
  | LIR.X7 -> "X7"
  | LIR.X8 -> "X8"
  | LIR.X9 -> "X9"
  | LIR.X10 -> "X10"
  | LIR.X11 -> "X11"
  | LIR.X12 -> "X12"
  | LIR.X13 -> "X13"
  | LIR.X14 -> "X14"
  | LIR.X15 -> "X15"
  | LIR.X16 -> "X16"
  | LIR.X17 -> "X17"
  | LIR.X19 -> "X19"
  | LIR.X20 -> "X20"
  | LIR.X21 -> "X21"
  | LIR.X22 -> "X22"
  | LIR.X23 -> "X23"
  | LIR.X24 -> "X24"
  | LIR.X25 -> "X25"
  | LIR.X26 -> "X26"
  | LIR.X27 -> "X27"
  | LIR.X29 -> "X29"
  | LIR.X30 -> "X30"
  | LIR.SP -> "SP"

let invalidX64PhysRegReason = function
  | LIR.X22 -> Some "X22 maps to the x64 heap pointer runtime register"
  | LIR.X23 -> Some "X23 maps to the x64 free-list runtime register"
  | (LIR.X24 | LIR.X25 | LIR.X26) as reg ->
      Some (physName reg ^ " has no allocatable x64 register mapping")
  | LIR.X27 -> Some "X27 is reserved runtime state and cannot be lowered on x64"
  | LIR.X0 | LIR.X1 | LIR.X2 | LIR.X3 | LIR.X4 | LIR.X5 | LIR.X6 | LIR.X7
  | LIR.X8 | LIR.X9 | LIR.X10 | LIR.X11 | LIR.X12 | LIR.X13 | LIR.X14 | LIR.X15
  | LIR.X16 | LIR.X17 | LIR.X19 | LIR.X20 | LIR.X21 | LIR.X29 | LIR.X30 | LIR.SP
    ->
      None

(*
   Map LIR.PhysReg to x86-64 register
   Return value
   Arg 1
   Arg 2
   Arg 3 (NOT RDX — RDX is reserved for IDIV)
   Arg 4
   Arg 5
   Arg 6 / caller-saved
   Caller-saved (only used when IDIV isn't active)
   Scratch
   Scratch (shared)
   Callee-saved 1
   Callee-saved 2
   Callee-saved 3
   Frame pointer
   Link register (not applicable on x86_64)
*)
let lirRegToX86 = function
  | LIR.X0 -> X86_64.RAX
  | LIR.X1 -> X86_64.RDI
  | LIR.X2 -> X86_64.RSI
  | LIR.X3 -> X86_64.RCX
  | LIR.X4 -> X86_64.R8
  | LIR.X5 -> X86_64.R9
  | LIR.X6 -> X86_64.R10
  | LIR.X7 -> X86_64.RDX
  | LIR.X8 -> X86_64.R11
  | LIR.X9 -> X86_64.R11
  | LIR.X10 -> X86_64.R11
  | LIR.X11 -> X86_64.R11
  | LIR.X12 -> X86_64.R11
  | LIR.X13 -> X86_64.R11
  | LIR.X14 -> X86_64.R11
  | LIR.X15 -> X86_64.R11
  | LIR.X16 -> X86_64.R11
  | LIR.X17 -> X86_64.R11
  | LIR.X19 -> X86_64…6534 tokens truncated…,
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
              | AST.TTuple _ | AST.TRecord _ | AST.TSum _ ->
                  releaseAggregateCapture fieldOffset captureType suffix
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
