(* Typed formatting of complete MIR values in original test diagnostics. *)
[@@@warning "-4-42"]

open Dark_compiler
open StructuralValue

let union _ name fields = Union (name, fields)
let record _ fields = Record fields
let text value = Text value
let tuple values = Tuple values
let list encode values = Sequence (List.map encode values)

let option encode = function
  | None -> Union ("None", [])
  | Some value -> Union ("Some", [ encode value ])

let int32 value = Scalar (string_of_int value)
let int64 value = Scalar (Int64.to_string value ^ "L")
let float64 value = Scalar (FloatFormat.structural value)
let functionId = AST.DiagnosticFormatting.func

let rec vReg (value : MIR.vReg) =
  match value with MIR.VReg field0 -> union "VReg" "VReg" [ int32 field0 ]

and typedMIRParam (value : MIR.typedMIRParam) =
  record "TypedMIRParam"
    [
      ("Reg", vReg value.MIR.reg);
      ("Type", StructuralFormat.semanticValue value.MIR.typ);
    ]

and operand (value : MIR.operand) =
  match value with
  | MIR.Int64Const field0 -> union "Operand" "Int64Const" [ int64 field0 ]
  | MIR.BoolConst field0 ->
      union "Operand" "BoolConst"
        [ (fun value -> Scalar (string_of_bool value)) field0 ]
  | MIR.FloatSymbol field0 -> union "Operand" "FloatSymbol" [ float64 field0 ]
  | MIR.StringSymbol field0 -> union "Operand" "StringSymbol" [ text field0 ]
  | MIR.Register field0 -> union "Operand" "Register" [ vReg field0 ]
  | MIR.FuncAddr field0 -> union "Operand" "FuncAddr" [ functionId field0 ]

and binOp (value : MIR.binOp) =
  match value with
  | MIR.Add -> union "BinOp" "Add" []
  | MIR.Sub -> union "BinOp" "Sub" []
  | MIR.Mul -> union "BinOp" "Mul" []
  | MIR.Div -> union "BinOp" "Div" []
  | MIR.Mod -> union "BinOp" "Mod" []
  | MIR.Shl -> union "BinOp" "Shl" []
  | MIR.Shr -> union "BinOp" "Shr" []
  | MIR.BitAnd -> union "BinOp" "BitAnd" []
  | MIR.BitOr -> union "BinOp" "BitOr" []
  | MIR.BitXor -> union "BinOp" "BitXor" []
  | MIR.Eq -> union "BinOp" "Eq" []
  | MIR.Neq -> union "BinOp" "Neq" []
  | MIR.Lt -> union "BinOp" "Lt" []
  | MIR.Gt -> union "BinOp" "Gt" []
  | MIR.Lte -> union "BinOp" "Lte" []
  | MIR.Gte -> union "BinOp" "Gte" []
  | MIR.And -> union "BinOp" "And" []
  | MIR.Or -> union "BinOp" "Or" []

and unaryOp (value : MIR.unaryOp) =
  match value with
  | MIR.Neg -> union "UnaryOp" "Neg" []
  | MIR.Not -> union "UnaryOp" "Not" []
  | MIR.BitNot -> union "UnaryOp" "BitNot" []

and rcKind (value : MIR.rcKind) =
  match value with
  | MIR.GenericHeap -> union "RcKind" "GenericHeap" []
  | MIR.StreamHeap -> union "RcKind" "StreamHeap" []
  | MIR.TaggedList -> union "RcKind" "TaggedList" []
  | MIR.DictHeap -> union "RcKind" "DictHeap" []
  | MIR.ClosureHeap -> union "RcKind" "ClosureHeap" []

and cliOperation (value : MIR.cliOperation) =
  match value with
  | MIR.Execute -> union "CliOperation" "Execute" []
  | MIR.StartupStack -> union "CliOperation" "StartupStack" []
  | MIR.ExecutableState -> union "CliOperation" "ExecutableState" []
  | MIR.RunProcess -> union "CliOperation" "RunProcess" []
  | MIR.HostOS -> union "CliOperation" "HostOS" []
  | MIR.HostArchitecture -> union "CliOperation" "HostArchitecture" []
  | MIR.Hostname -> union "CliOperation" "Hostname" []
  | MIR.GetEnv -> union "CliOperation" "GetEnv" []
  | MIR.GetEnvironmentPacked -> union "CliOperation" "GetEnvironmentPacked" []
  | MIR.StdinState -> union "CliOperation" "StdinState" []
  | MIR.SetEnv -> union "CliOperation" "SetEnv" []
  | MIR.UnsetEnv -> union "CliOperation" "UnsetEnv" []
  | MIR.DirectoryCurrent -> union "CliOperation" "DirectoryCurrent" []
  | MIR.DirectoryListPacked -> union "CliOperation" "DirectoryListPacked" []
  | MIR.FileIsDirectory -> union "CliOperation" "FileIsDirectory" []
  | MIR.FileCreateExclusive -> union "CliOperation" "FileCreateExclusive" []
  | MIR.GetArgv -> union "CliOperation" "GetArgv" []
  | MIR.Kill -> union "CliOperation" "Kill" []
  | MIR.GetPid -> union "CliOperation" "GetPid" []
  | MIR.GetUid -> union "CliOperation" "GetUid" []
  | MIR.CpuCount -> union "CliOperation" "CpuCount" []
  | MIR.SpawnProcess -> union "CliOperation" "SpawnProcess" []
  | MIR.ProcessIO -> union "CliOperation" "ProcessIO" []
  | MIR.TerminateProcess -> union "CliOperation" "TerminateProcess" []
  | MIR.SocketTcp4 -> union "CliOperation" "SocketTcp4" []
  | MIR.SocketTcp6 -> union "CliOperation" "SocketTcp6" []
  | MIR.SocketUdp4 -> union "CliOperation" "SocketUdp4" []
  | MIR.SocketUdp6 -> union "CliOperation" "SocketUdp6" []
  | MIR.SocketConnect4 -> union "CliOperation" "SocketConnect4" []
  | MIR.SocketConnect6 -> union "CliOperation" "SocketConnect6" []
  | MIR.SocketSend -> union "CliOperation" "SocketSend" []
  | MIR.SocketSendTo -> union "CliOperation" "SocketSendTo" []
  | MIR.SocketReceive -> union "CliOperation" "SocketReceive" []
  | MIR.SocketReceiveFrom -> union "CliOperation" "SocketReceiveFrom" []
  | MIR.SocketReceiveTimeout -> union "CliOperation" "SocketReceiveTimeout" []
  | MIR.SocketSendTimeout -> union "CliOperation" "SocketSendTimeout" []
  | MIR.SocketClose -> union "CliOperation" "SocketClose" []
  | MIR.SocketBind4 -> union "CliOperation" "SocketBind4" []
  | MIR.SocketBind6 -> union "CliOperation" "SocketBind6" []
  | MIR.SocketListen -> union "CliOperation" "SocketListen" []
  | MIR.SocketAccept -> union "CliOperation" "SocketAccept" []
  | MIR.SocketCloexec -> union "CliOperation" "SocketCloexec" []
  | MIR.SocketReuseAddress -> union "CliOperation" "SocketReuseAddress" []
  | MIR.SocketPoll -> union "CliOperation" "SocketPoll" []
  | MIR.SignalBlock -> union "CliOperation" "SignalBlock" []
  | MIR.SignalRestore -> union "CliOperation" "SignalRestore" []
  | MIR.SignalPending -> union "CliOperation" "SignalPending" []
  | MIR.SignalWait -> union "CliOperation" "SignalWait" []
  | MIR.MonotonicTime -> union "CliOperation" "MonotonicTime" []
  | MIR.SecureRandomFill -> union "CliOperation" "SecureRandomFill" []
  | MIR.PosixOpenAt -> union "CliOperation" "PosixOpenAt" []
  | MIR.PosixRead -> union "CliOperation" "PosixRead" []
  | MIR.PosixWrite -> union "CliOperation" "PosixWrite" []
  | MIR.PosixClose -> union "CliOperation" "PosixClose" []
  | MIR.PosixSeek -> union "CliOperation" "PosixSeek" []
  | MIR.PosixStatAt -> union "CliOperation" "PosixStatAt" []
  | MIR.PosixGetCwd -> union "CliOperation" "PosixGetCwd" []
  | MIR.PosixChdir -> union "CliOperation" "PosixChdir" []
  | MIR.PosixMkdirAt -> union "CliOperation" "PosixMkdirAt" []
  | MIR.PosixUnlinkAt -> union "CliOperation" "PosixUnlinkAt" []
  | MIR.PosixRenameAt -> union "CliOperation" "PosixRenameAt" []
  | MIR.PosixChmodAt -> union "CliOperation" "PosixChmodAt" []
  | MIR.PosixChmodAt2 -> union "CliOperation" "PosixChmodAt2" []
  | MIR.PosixUtimesAt -> union "CliOperation" "PosixUtimesAt" []
  | MIR.PosixSetAttributesAt -> union "CliOperation" "PosixSetAttributesAt" []
  | MIR.PosixSymlinkAt -> union "CliOperation" "PosixSymlinkAt" []
  | MIR.PosixReadlinkAt -> union "CliOperation" "PosixReadlinkAt" []
  | MIR.PosixFlock -> union "CliOperation" "PosixFlock" []
  | MIR.PosixGetDents -> union "CliOperation" "PosixGetDents" []
  | MIR.PosixIoctl -> union "CliOperation" "PosixIoctl" []
  | MIR.PosixProcInfo -> union "CliOperation" "PosixProcInfo" []

and label (value : MIR.label) =
  match value with MIR.Label field0 -> union "Label" "Label" [ text field0 ]

and instr (value : MIR.instr) =
  match value with
  | MIR.Mov (field0, field1, field2) ->
      union "Instr" "Mov"
        [
          vReg field0;
          operand field1;
          option (fun value -> StructuralFormat.semanticValue value) field2;
        ]
  | MIR.BinOp (field0, field1, field2, field3, field4) ->
      union "Instr" "BinOp"
        [
          vReg field0;
          binOp field1;
          operand field2;
          operand field3;
          StructuralFormat.semanticValue field4;
        ]
  | MIR.UnaryOp (field0, field1, field2) ->
      union "Instr" "UnaryOp" [ vReg field0; unaryOp field1; operand field2 ]
  | MIR.Call (field0, field1, field2, field3, field4) ->
      union "Instr" "Call"
        [
          vReg field0;
          functionId field1;
          list (fun value -> operand value) field2;
          list (fun value -> StructuralFormat.semanticValue value) field3;
          StructuralFormat.semanticValue field4;
        ]
  | MIR.TailCall (field0, field1, field2, field3) ->
      union "Instr" "TailCall"
        [
          functionId field0;
          list (fun value -> operand value) field1;
          list (fun value -> StructuralFormat.semanticValue value) field2;
          StructuralFormat.semanticValue field3;
        ]
  | MIR.IndirectCall (field0, field1, field2, field3, field4) ->
      union "Instr" "IndirectCall"
        [
          vReg field0;
          operand field1;
          list (fun value -> operand value) field2;
          list (fun value -> StructuralFormat.semanticValue value) field3;
          StructuralFormat.semanticValue field4;
        ]
  | MIR.IndirectTailCall (field0, field1, field2, field3) ->
      union "Instr" "IndirectTailCall"
        [
          operand field0;
          list (fun value -> operand value) field1;
          list (fun value -> StructuralFormat.semanticValue value) field2;
          StructuralFormat.semanticValue field3;
        ]
  | MIR.ClosureAlloc (field0, field1, field2) ->
      union "Instr" "ClosureAlloc"
        [
          vReg field0;
          functionId field1;
          list (fun value -> operand value) field2;
        ]
  | MIR.ClosureCall (field0, field1, field2, field3, field4) ->
      union "Instr" "ClosureCall"
        [
          vReg field0;
          operand field1;
          list (fun value -> operand value) field2;
          list (fun value -> StructuralFormat.semanticValue value) field3;
          StructuralFormat.semanticValue field4;
        ]
  | MIR.ClosureTailCall (field0, field1, field2) ->
      union "Instr" "ClosureTailCall"
        [
          operand field0;
          list (fun value -> operand value) field1;
          list (fun value -> StructuralFormat.semanticValue value) field2;
        ]
  | MIR.HeapAlloc (field0, field1) ->
      union "Instr" "HeapAlloc" [ vReg field0; int32 field1 ]
  | MIR.HeapStore (field0, field1, field2, field3) ->
      union "Instr" "HeapStore"
        [
          vReg field0;
          int32 field1;
          operand field2;
          option (fun value -> StructuralFormat.semanticValue value) field3;
        ]
  | MIR.HeapLoad (field0, field1, field2, field3) ->
      union "Instr" "HeapLoad"
        [
          vReg field0;
          vReg field1;
          int32 field2;
          option (fun value -> StructuralFormat.semanticValue value) field3;
        ]
  | MIR.StringConcat (field0, field1, field2, field3) ->
      union "Instr" "StringConcat"
        [
          vReg field0;
          operand field1;
          operand field2;
          list (fun value -> operand value) field3;
        ]
  | MIR.CanonicalBufferEq (field0, field1, field2, field3) ->
      union "Instr" "CanonicalBufferEq"
        [
          vReg field0;
          ANFTestFormatting.memoryModel_canonicalBufferKind field1;
          operand field2;
          operand field3;
        ]
  | MIR.RefCountInc (field0, field1, field2, field3) ->
      union "Instr" "RefCountInc"
        [
          vReg field0;
          int32 field1;
          rcKind field2;
          option
            (fun value -> ANFTestFormatting.memoryModel_rcMetadata value)
            field3;
        ]
  | MIR.RefCountDec (field0, field1, field2, field3) ->
      union "Instr" "RefCountDec"
        [
          vReg field0;
          int32 field1;
          rcKind field2;
          option
            (fun value -> ANFTestFormatting.memoryModel_rcMetadata value)
            field3;
        ]
  | MIR.Print (field0, field1) ->
      union "Instr" "Print"
        [ operand field0; StructuralFormat.semanticValue field1 ]
  | MIR.StdoutWrite (field0, field1, field2) ->
      union "Instr" "StdoutWrite"
        [
          int32 field0;
          operand field1;
          (fun value -> Scalar (string_of_bool value)) field2;
        ]
  | MIR.StdinReadLine field0 -> union "Instr" "StdinReadLine" [ vReg field0 ]
  | MIR.RuntimeError field0 -> union "Instr" "RuntimeError" [ text field0 ]
  | MIR.RuntimeErrorString field0 ->
      union "Instr" "RuntimeErrorString" [ operand field0 ]
  | MIR.FileReadBlob (field0, field1) ->
      union "Instr" "FileReadBlob" [ vReg field0; operand field1 ]
  | MIR.FileExists (field0, field1) ->
      union "Instr" "FileExists" [ vReg field0; operand field1 ]
  | MIR.FileWriteBlob (field0, field1, field2) ->
      union "Instr" "FileWriteBlob"
        [ vReg field0; operand field1; operand field2 ]
  | MIR.FileAppendText (field0, field1, field2) ->
      union "Instr" "FileAppendText"
        [ vReg field0; operand field1; operand field2 ]
  | MIR.FileDelete (field0, field1) ->
      union "Instr" "FileDelete" [ vReg field0; operand field1 ]
  | MIR.FileCreateDirectory (field0, field1) ->
      union "Instr" "FileCreateDirectory" [ vReg field0; operand field1 ]
  | MIR.FileSetExecutable (field0, field1) ->
      union "Instr" "FileSetExecutable" [ vReg field0; operand field1 ]
  | MIR.FileWriteFromPtr (field0, field1, field2, field3) ->
      union "Instr" "FileWriteFromPtr"
        [ vReg field0; operand field1; operand field2; operand field3 ]
  | MIR.FloatSqrt (field0, field1) ->
      union "Instr" "FloatSqrt" [ vReg field0; operand field1 ]
  | MIR.FloatAbs (field0, field1) ->
      union "Instr" "FloatAbs" [ vReg field0; operand field1 ]
  | MIR.FloatNeg (field0, field1) ->
      union "Instr" "FloatNeg" [ vReg field0; operand field1 ]
  | MIR.Int64ToFloat (field0, field1) ->
      union "Instr" "Int64ToFloat" [ vReg field0; operand field1 ]
  | MIR.FloatToInt64 (field0, field1) ->
      union "Instr" "FloatToInt64" [ vReg field0; operand field1 ]
  | MIR.FloatToBits (field0, field1) ->
      union "Instr" "FloatToBits" [ vReg field0; operand field1 ]
  | MIR.RawAlloc (field0, field1) ->
      union "Instr" "RawAlloc" [ vReg field0; operand field1 ]
  | MIR.MappedAlloc (field0, field1) ->
      union "Instr" "MappedAlloc" [ vReg field0; operand field1 ]
  | MIR.RawFree field0 -> union "Instr" "RawFree" [ operand field0 ]
  | MIR.MappedFree field0 -> union "Instr" "MappedFree" [ operand field0 ]
  | MIR.RawGet (field0, field1, field2, field3) ->
      union "Instr" "RawGet"
        [
          vReg field0;
          operand field1;
          operand field2;
          option (fun value -> StructuralFormat.semanticValue value) field3;
        ]
  | MIR.RawGetByte (field0, field1, field2) ->
      union "Instr" "RawGetByte" [ vReg field0; operand field1; operand field2 ]
  | MIR.RawWriteWord (field0, field1, field2) ->
      union "Instr" "RawWriteWord"
        [ operand field0; operand field1; operand field2 ]
  | MIR.RawWriteByte (field0, field1, field2) ->
      union "Instr" "RawWriteByte"
        [ operand field0; operand field1; operand field2 ]
  | MIR.RawSlotInit (field0, field1, field2, field3) ->
      union "Instr" "RawSlotInit"
        [
          operand field0;
          operand field1;
          operand field2;
          StructuralFormat.semanticValue field3;
        ]
  | MIR.StringToRawPtr (field0, field1) ->
      union "Instr" "StringToRawPtr" [ vReg field0; operand field1 ]
  | MIR.RawPtrToString (field0, field1) ->
      union "Instr" "RawPtrToString" [ vReg field0; operand field1 ]
  | MIR.BlobToRawPtr (field0, field1) ->
      union "Instr" "BlobToRawPtr" [ vReg field0; operand field1 ]
  | MIR.RawPtrToBlob (field0, field1) ->
      union "Instr" "RawPtrToBlob" [ vReg field0; operand field1 ]
  | MIR.DictToRawPtr (field0, field1) ->
      union "Instr" "DictToRawPtr" [ vReg field0; operand field1 ]
  | MIR.RawPtrToDict (field0, field1, field2) ->
      union "Instr" "RawPtrToDict"
        [ vReg field0; operand field1; operand field2 ]
  | MIR.ListToRawPtr (field0, field1) ->
      union "Instr" "ListToRawPtr" [ vReg field0; operand field1 ]
  | MIR.RawPtrToList (field0, field1, field2) ->
      union "Instr" "RawPtrToList"
        [ vReg field0; operand field1; operand field2 ]
  | MIR.RefCountIncString field0 ->
      union "Instr" "RefCountIncString" [ operand field0 ]
  | MIR.RefCountDecString field0 ->
      union "Instr" "RefCountDecString" [ operand field0 ]
  | MIR.RefCountIncBlob field0 ->
      union "Instr" "RefCountIncBlob" [ operand field0 ]
  | MIR.RefCountDecBlob field0 ->
      union "Instr" "RefCountDecBlob" [ operand field0 ]
  | MIR.RefCountIncInt field0 ->
      union "Instr" "RefCountIncInt" [ operand field0 ]
  | MIR.RefCountDecInt field0 ->
      union "Instr" "RefCountDecInt" [ operand field0 ]
  | MIR.RandomInt64 field0 -> union "Instr" "RandomInt64" [ vReg field0 ]
  | MIR.DateTimeNow field0 -> union "Instr" "DateTimeNow" [ vReg field0 ]
  | MIR.Sleep (field0, field1, field2) ->
      union "Instr" "Sleep" [ int32 field0; vReg field1; operand field2 ]
  | MIR.CliNative (field0, field1, field2) ->
      union "Instr" "CliNative"
        [
          vReg field0;
          cliOperation field1;
          list (fun value -> operand value) field2;
        ]
  | MIR.FloatToString (field0, field1) ->
      union "Instr" "FloatToString" [ vReg field0; operand field1 ]
  | MIR.Phi (field0, field1, field2) ->
      union "Instr" "Phi"
        [
          vReg field0;
          list
            (fun value ->
              let part0, part1 = value in
              tuple [ operand part0; label part1 ])
            field1;
          option (fun value -> StructuralFormat.semanticValue value) field2;
        ]
  | MIR.CoverageHit field0 -> union "Instr" "CoverageHit" [ int32 field0 ]

and terminator (value : MIR.terminator) =
  match value with
  | MIR.Ret field0 -> union "Terminator" "Ret" [ operand field0 ]
  | MIR.Branch (field0, field1, field2) ->
      union "Terminator" "Branch" [ operand field0; label field1; label field2 ]
  | MIR.Jump field0 -> union "Terminator" "Jump" [ label field0 ]

and basicBlock (value : MIR.basicBlock) =
  record "BasicBlock"
    [
      ("Label", label value.MIR.label);
      ("Instrs", list (fun value -> instr value) value.MIR.instrs);
      ("Terminator", terminator value.MIR.terminator);
    ]

and cfg (value : MIR.cfg) =
  record "CFG"
    [
      ("Entry", label value.MIR.entry);
      ( "Blocks",
        Union
          ( "map",
            [
              list
                (fun (key, value) -> tuple [ label key; basicBlock value ])
                (MIR.LabelMap.bindings value.MIR.blocks);
            ] ) );
    ]

and functionDef (value : MIR.functionDef) =
  record "Function"
    [
      ("Id", functionId value.MIR.id);
      ("Name", text value.MIR.name);
      ( "TypedParams",
        list (fun value -> typedMIRParam value) value.MIR.typedParams );
      ("ReturnType", StructuralFormat.semanticValue value.MIR.returnType);
      ("CFG", cfg value.MIR.cfg);
      ( "FloatRegs",
        Union ("set", [ list int32 (MIR.IntSet.elements value.MIR.floatRegs) ])
      );
    ]

and variantInfo (value : MIR.variantInfo) =
  record "VariantInfo"
    [
      ("Name", text value.MIR.name);
      ("Tag", int32 value.MIR.tag);
      ( "Payload",
        option
          (fun value -> StructuralFormat.semanticValue value)
          value.MIR.payload );
      ("FieldCount", int32 value.MIR.fieldCount);
    ]

and typeVariants (value : MIR.typeVariants) =
  record "TypeVariants"
    [
      ("TypeParams", list (fun value -> text value) value.MIR.typeParams);
      ("Variants", list (fun value -> variantInfo value) value.MIR.variants);
    ]

and recordField (value : MIR.recordField) =
  record "RecordField"
    [
      ("Name", text value.MIR.name);
      ("Type", StructuralFormat.semanticValue value.MIR.typ);
    ]

and variantRegistry (value : MIR.variantRegistry) =
  Union
    ( "map",
      [
        list
          (fun (key, value) -> tuple [ text key; typeVariants value ])
          (StringOrder.Map.bindings value);
      ] )

and recordRegistry (value : MIR.recordRegistry) =
  Union
    ( "map",
      [
        list
          (fun (key, value) -> tuple [ text key; list recordField value ])
          (StringOrder.Map.bindings value);
      ] )

and program (value : MIR.program) =
  match value with
  | MIR.Program (field0, field1, field2) ->
      union "Program" "Program"
        [
          list (fun value -> functionDef value) field0;
          variantRegistry field1;
          recordRegistry field2;
        ]

and regGen (value : MIR.regGen) =
  match value with
  | MIR.RegGen field0 -> union "RegGen" "RegGen" [ int32 field0 ]

and labelGen (value : MIR.labelGen) =
  match value with
  | MIR.LabelGen field0 -> union "LabelGen" "LabelGen" [ int32 field0 ]
