(* Typed formatting of complete memory and ANF values in original test diagnostics. *)
[@@@warning "-42"]

open Dark_compiler
open StructuralValue

let scalar kind value =
  let suffix =
    match kind with
    | "int8" -> "y"
    | "uint8" -> "uy"
    | "int16" -> "s"
    | "uint16" -> "us"
    | "int64" -> "L"
    | "uint64" -> "UL"
    | "uint32" -> "u"
    | _ -> ""
  in
  if kind = "float64" then
    Scalar
      (FloatFormat.structural
         (Int64.float_of_bits (Int64.of_string ("0x" ^ value))))
  else Scalar (value ^ suffix)

let union _ name fields = Union (name, fields)
let record _ fields = Record fields
let text value = Text value
let tuple values = Tuple values
let list encode values = Sequence (List.map encode values)

let option encode = function
  | None -> Union ("None", [])
  | Some value -> Union ("Some", [ encode value ])

let int32 value = Scalar (string_of_int value)
let boolean value = Scalar (string_of_bool value)
let float64 value = Scalar (FloatFormat.structural value)

let unsigned value =
  Z.to_string
    (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)
     else Z.of_int64 value)

let functionId = AST.DiagnosticFormatting.func

let integerSet values =
  Union ("set", [ list int32 (MemoryModel.IntSet.elements values) ])

let coverageMap values =
  Union
    ( "map",
      [
        list
          (fun (key, value) -> tuple [ int32 key; text value ])
          (ANF.ExprIdMap.bindings values);
      ] )

let stringMap encode values =
  Union
    ( "map",
      [
        list
          (fun (key, value) -> tuple [ text key; encode value ])
          (StringOrder.Map.bindings values);
      ] )

let rec memoryModel_canonicalBufferKind
    (value : MemoryModel.canonicalBufferKind) =
  match value with
  | MemoryModel.Utf8String -> union "CanonicalBufferKind" "Utf8String" []
  | MemoryModel.NullableUtf8String ->
      union "CanonicalBufferKind" "NullableUtf8String" []
  | MemoryModel.GraphemeCluster ->
      union "CanonicalBufferKind" "GraphemeCluster" []
  | MemoryModel.NullableGraphemeCluster ->
      union "CanonicalBufferKind" "NullableGraphemeCluster" []

and memoryModel_rcKind (value : MemoryModel.rcKind) =
  match value with
  | MemoryModel.GenericHeap -> union "RcKind" "GenericHeap" []
  | MemoryModel.StreamHeap -> union "RcKind" "StreamHeap" []
  | MemoryModel.TaggedList -> union "RcKind" "TaggedList" []
  | MemoryModel.DictHeap -> union "RcKind" "DictHeap" []
  | MemoryModel.ClosureHeap -> union "RcKind" "ClosureHeap" []

and memoryModel_rcShape (value : MemoryModel.rcShape) =
  match value with
  | MemoryModel.Immediate -> union "RcShape" "Immediate" []
  | MemoryModel.FixedBlock (field0, field1) ->
      union "RcShape" "FixedBlock"
        [ int32 field0; list (fun item -> memoryModel_rcShape item) field1 ]
  | MemoryModel.StreamRoot -> union "RcShape" "StreamRoot" []
  | MemoryModel.BoxedSum (field0, field1, field2) ->
      union "RcShape" "BoxedSum"
        [
          int32 field0;
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ int32 part0; memoryModel_rcShape part1 ])
            field1;
          list (fun item -> memoryModel_rcBoxedSumVariantShape item) field2;
        ]
  | MemoryModel.RecursiveNominalRef field0 ->
      union "RcShape" "RecursiveNominalRef"
        [ StructuralFormat.semanticValue field0 ]
  | MemoryModel.TaggedListShape field0 ->
      union "RcShape" "TaggedListShape" [ memoryModel_rcShape field0 ]
  | MemoryModel.DictRoot (field0, field1) ->
      union "RcShape" "DictRoot"
        [ memoryModel_rcShape field0; memoryModel_rcShape field1 ]
  | MemoryModel.DynamicString -> union "RcShape" "DynamicString" []
  | MemoryModel.DynamicBlob -> union "RcShape" "DynamicBlob" []
  | MemoryModel.DynamicInt -> union "RcShape" "DynamicInt" []
  | MemoryModel.ClosureShape field0 ->
      union "RcShape" "ClosureShape"
        [ list (fun item -> memoryModel_rcShape item) field0 ]
  | MemoryModel.StaticString -> union "RcShape" "StaticString" []
  | MemoryModel.RawUnmanaged -> union "RcShape" "RawUnmanaged" []

and memoryModel_rcBoxedSumVariantShape
    (value : MemoryModel.rcBoxedSumVariantShape) =
  record "RcBoxedSumVariantShape"
    [
      ("Tag", int32 value.MemoryModel.tag);
      ( "FieldShapes",
        list
          (fun item ->
            let part0, part1 = item in
            tuple [ int32 part0; memoryModel_rcShape part1 ])
          value.MemoryModel.fieldShapes );
    ]

and memoryModel_rcSumShapeInfo (value : MemoryModel.rcSumShapeInfo) =
  record "RcSumShapeInfo"
    [
      ("TypeParams", list (fun item -> text item) value.MemoryModel.typeParams);
      ( "Payloads",
        list
          (fun item ->
            let part0, part1 = item in
            tuple
              [
                int32 part0;
                option (fun item -> StructuralFormat.semanticValue item) part1;
              ])
          value.MemoryModel.payloads );
      ("UnaryPayloadTags", integerSet value.MemoryModel.unaryPayloadTags);
    ]

and memoryModel_rcSumShapeRegistry (value : MemoryModel.rcSumShapeRegistry) =
  stringMap (fun item -> memoryModel_rcSumShapeInfo item) value

and memoryModel_rcOperation (value : MemoryModel.rcOperation) =
  match value with
  | MemoryModel.FixedSizeRoot (field0, field1) ->
      union "RcOperation" "FixedSizeRoot"
        [ int32 field0; memoryModel_rcKind field1 ]
  | MemoryModel.DynamicStringBuffer ->
      union "RcOperation" "DynamicStringBuffer" []
  | MemoryModel.DynamicBlobBuffer -> union "RcOperation" "DynamicBlobBuffer" []
  | MemoryModel.DynamicIntBuffer -> union "RcOperation" "DynamicIntBuffer" []

and memoryModel_rcStorageClass (value : MemoryModel.rcStorageClass) =
  match value with
  | MemoryModel.UnmanagedStorage -> union "RcStorageClass" "UnmanagedStorage" []
  | MemoryModel.ManagedDynamicBuffer field0 ->
      union "RcStorageClass" "ManagedDynamicBuffer"
        [ memoryModel_rcOperation field0 ]
  | MemoryModel.ManagedRcRoot (field0, field1) ->
      union "RcStorageClass" "ManagedRcRoot"
        [ int32 field0; memoryModel_rcKind field1 ]

and memoryModel_rcReleasePlan (value : MemoryModel.rcReleasePlan) =
  match value with
  | MemoryModel.NoReleasePlan -> union "RcReleasePlan" "NoReleasePlan" []
  | MemoryModel.DynamicBufferRelease field0 ->
      union "RcReleasePlan" "DynamicBufferRelease"
        [ memoryModel_rcOperation field0 ]
  | MemoryModel.RecursiveRelease field0 ->
      union "RcReleasePlan" "RecursiveRelease"
        [ StructuralFormat.semanticValue field0 ]
  | MemoryModel.RootRelease (field0, field1, field2) ->
      union "RcReleasePlan" "RootRelease"
        [
          int32 field0;
          memoryModel_rcKind field1;
          memoryModel_rcPayloadReleasePlan field2;
        ]

and memoryModel_rcPayloadReleasePlan (value : MemoryModel.rcPayloadReleasePlan)
    =
  match value with
  | MemoryModel.NoPayloadRelease ->
      union "RcPayloadReleasePlan" "NoPayloadRelease" []
  | MemoryModel.FixedBlockPayloadRelease (field0, field1) ->
      union "RcPayloadReleasePlan" "FixedBlockPayloadRelease"
        [
          int32 field0;
          list (fun item -> memoryModel_rcFieldRelease item) field1;
        ]
  | MemoryModel.BoxedSumPayloadRelease (field0, field1, field2) ->
      union "RcPayloadReleasePlan" "BoxedSumPayloadRelease"
        [
          int32 field0;
          list (fun item -> memoryModel_rcFieldRelease item) field1;
          list (fun item -> memoryModel_rcBoxedSumVariantRelease item) field2;
        ]
  | MemoryModel.TaggedListPayloadRelease field0 ->
      union "RcPayloadReleasePlan" "TaggedListPayloadRelease"
        [ memoryModel_rcReleasePlan field0 ]
  | MemoryModel.DictPayloadRelease (field0, field1) ->
      union "RcPayloadReleasePlan" "DictPayloadRelease"
        [ memoryModel_rcReleasePlan field0; memoryModel_rcReleasePlan field1 ]
  | MemoryModel.ClosurePayloadRelease field0 ->
      union "RcPayloadReleasePlan" "ClosurePayloadRelease"
        [ list (fun item -> memoryModel_rcFieldRelease item) field0 ]

and memoryModel_rcFieldRelease (value : MemoryModel.rcFieldRelease) =
  match value with
  | MemoryModel.FieldRelease (field0, field1) ->
      union "RcFieldRelease" "FieldRelease"
        [ int32 field0; memoryModel_rcReleasePlan field1 ]

and memoryModel_rcBoxedSumVariantRelease
    (value : MemoryModel.rcBoxedSumVariantRelease) =
  record "RcBoxedSumVariantRelease"
    [
      ("Tag", int32 value.MemoryModel.tag);
      ( "FieldReleases",
        list
          (fun item -> memoryModel_rcFieldRelease item)
          value.MemoryModel.fieldReleases );
    ]

and memoryModel_rcMetadata (value : MemoryModel.rcMetadata) =
  record "RcMetadata"
    [
      ( "ReleasePlanCacheKey",
        option (fun item -> text item) value.MemoryModel.releasePlanCacheKey );
      ( "ReleasePlan",
        option
          (fun item -> memoryModel_rcReleasePlan item)
          value.MemoryModel.releasePlan );
      ( "SourceType",
        option
          (fun item -> StructuralFormat.semanticValue item)
          value.MemoryModel.sourceType );
    ]

and aNF_tempId (value : ANF.tempId) =
  match value with
  | ANF.TempId field0 -> union "TempId" "TempId" [ int32 field0 ]

and aNF_typedParam (value : ANF.typedParam) =
  record "TypedParam"
    [
      ("Id", aNF_tempId value.ANF.id);
      ("Type", StructuralFormat.semanticValue value.ANF.typ);
    ]

and aNF_sizedInt (value : ANF.sizedInt) =
  match value with
  | ANF.Int8 field0 ->
      union "SizedInt" "Int8" [ scalar "int8" (string_of_int field0) ]
  | ANF.Int16 field0 ->
      union "SizedInt" "Int16" [ scalar "int16" (string_of_int field0) ]
  | ANF.Int32 field0 ->
      union "SizedInt" "Int32" [ scalar "int32" (Int32.to_string field0) ]
  | ANF.Int64 field0 ->
      union "SizedInt" "Int64" [ scalar "int64" (Int64.to_string field0) ]
  | ANF.UInt8 field0 ->
      union "SizedInt" "UInt8" [ scalar "uint8" (string_of_int field0) ]
  | ANF.UInt16 field0 ->
      union "SizedInt" "UInt16" [ scalar "uint16" (string_of_int field0) ]
  | ANF.UInt32 field0 ->
      union "SizedInt" "UInt32" [ scalar "uint32" (Int64.to_string field0) ]
  | ANF.UInt64 field0 ->
      union "SizedInt" "UInt64" [ scalar "uint64" (unsigned field0) ]

and aNF_atom (value : ANF.atom) =
  match value with
  | ANF.UnitLiteral -> union "Atom" "UnitLiteral" []
  | ANF.IntLiteral field0 -> union "Atom" "IntLiteral" [ aNF_sizedInt field0 ]
  | ANF.BoolLiteral field0 -> union "Atom" "BoolLiteral" [ boolean field0 ]
  | ANF.StringLiteral field0 -> union "Atom" "StringLiteral" [ text field0 ]
  | ANF.FloatLiteral field0 -> union "Atom" "FloatLiteral" [ float64 field0 ]
  | ANF.Var field0 -> union "Atom" "Var" [ aNF_tempId field0 ]
  | ANF.FuncRef field0 -> union "Atom" "FuncRef" [ functionId field0 ]

and aNF_binOp (value : ANF.binOp) =
  match value with
  | ANF.Add -> union "BinOp" "Add" []
  | ANF.Sub -> union "BinOp" "Sub" []
  | ANF.Mul -> union "BinOp" "Mul" []
  | ANF.Div -> union "BinOp" "Div" []
  | ANF.Mod -> union "BinOp" "Mod" []
  | ANF.Shl -> union "BinOp" "Shl" []
  | ANF.Shr -> union "BinOp" "Shr" []
  | ANF.BitAnd -> union "BinOp" "BitAnd" []
  | ANF.BitOr -> union "BinOp" "BitOr" []
  | ANF.BitXor -> union "BinOp" "BitXor" []
  | ANF.Eq -> union "BinOp" "Eq" []
  | ANF.Neq -> union "BinOp" "Neq" []
  | ANF.Lt -> union "BinOp" "Lt" []
  | ANF.Gt -> union "BinOp" "Gt" []
  | ANF.Lte -> union "BinOp" "Lte" []
  | ANF.Gte -> union "BinOp" "Gte" []
  | ANF.And -> union "BinOp" "And" []
  | ANF.Or -> union "BinOp" "Or" []

and aNF_unaryOp (value : ANF.unaryOp) =
  match value with
  | ANF.Neg -> union "UnaryOp" "Neg" []
  | ANF.Not -> union "UnaryOp" "Not" []
  | ANF.BitNot -> union "UnaryOp" "BitNot" []

and aNF_returnOwnership (value : ANF.returnOwnership) =
  match value with
  | ANF.OwnedReturn -> union "ReturnOwnership" "OwnedReturn" []
  | ANF.BorrowedReturn -> union "ReturnOwnership" "BorrowedReturn" []

and aNF_cliOperation (value : ANF.cliOperation) =
  match value with
  | ANF.Execute -> union "CliOperation" "Execute" []
  | ANF.RunProcess -> union "CliOperation" "RunProcess" []
  | ANF.HostOS -> union "CliOperation" "HostOS" []
  | ANF.HostArchitecture -> union "CliOperation" "HostArchitecture" []
  | ANF.Hostname -> union "CliOperation" "Hostname" []
  | ANF.GetEnv -> union "CliOperation" "GetEnv" []
  | ANF.GetEnvironmentPacked -> union "CliOperation" "GetEnvironmentPacked" []
  | ANF.StdinState -> union "CliOperation" "StdinState" []
  | ANF.SetEnv -> union "CliOperation" "SetEnv" []
  | ANF.UnsetEnv -> union "CliOperation" "UnsetEnv" []
  | ANF.DirectoryCurrent -> union "CliOperation" "DirectoryCurrent" []
  | ANF.DirectoryListPacked -> union "CliOperation" "DirectoryListPacked" []
  | ANF.FileIsDirectory -> union "CliOperation" "FileIsDirectory" []
  | ANF.FileCreateExclusive -> union "CliOperation" "FileCreateExclusive" []
  | ANF.GetArgv -> union "CliOperation" "GetArgv" []
  | ANF.Kill -> union "CliOperation" "Kill" []
  | ANF.GetPid -> union "CliOperation" "GetPid" []
  | ANF.GetUid -> union "CliOperation" "GetUid" []
  | ANF.CpuCount -> union "CliOperation" "CpuCount" []
  | ANF.SpawnProcess -> union "CliOperation" "SpawnProcess" []
  | ANF.ProcessIO -> union "CliOperation" "ProcessIO" []
  | ANF.TerminateProcess -> union "CliOperation" "TerminateProcess" []
  | ANF.SocketTcp4 -> union "CliOperation" "SocketTcp4" []
  | ANF.SocketTcp6 -> union "CliOperation" "SocketTcp6" []
  | ANF.SocketUdp4 -> union "CliOperation" "SocketUdp4" []
  | ANF.SocketUdp6 -> union "CliOperation" "SocketUdp6" []
  | ANF.SocketConnect4 -> union "CliOperation" "SocketConnect4" []
  | ANF.SocketConnect6 -> union "CliOperation" "SocketConnect6" []
  | ANF.SocketSend -> union "CliOperation" "SocketSend" []
  | ANF.SocketSendTo -> union "CliOperation" "SocketSendTo" []
  | ANF.SocketReceive -> union "CliOperation" "SocketReceive" []
  | ANF.SocketReceiveFrom -> union "CliOperation" "SocketReceiveFrom" []
  | ANF.SocketReceiveTimeout -> union "CliOperation" "SocketReceiveTimeout" []
  | ANF.SocketSendTimeout -> union "CliOperation" "SocketSendTimeout" []
  | ANF.SocketClose -> union "CliOperation" "SocketClose" []
  | ANF.SocketBind4 -> union "CliOperation" "SocketBind4" []
  | ANF.SocketBind6 -> union "CliOperation" "SocketBind6" []
  | ANF.SocketListen -> union "CliOperation" "SocketListen" []
  | ANF.SocketAccept -> union "CliOperation" "SocketAccept" []
  | ANF.SocketCloexec -> union "CliOperation" "SocketCloexec" []
  | ANF.SocketReuseAddress -> union "CliOperation" "SocketReuseAddress" []
  | ANF.SocketPoll -> union "CliOperation" "SocketPoll" []
  | ANF.SignalBlock -> union "CliOperation" "SignalBlock" []
  | ANF.SignalRestore -> union "CliOperation" "SignalRestore" []
  | ANF.SignalPending -> union "CliOperation" "SignalPending" []
  | ANF.SignalWait -> union "CliOperation" "SignalWait" []
  | ANF.MonotonicTime -> union "CliOperation" "MonotonicTime" []
  | ANF.SecureRandomFill -> union "CliOperation" "SecureRandomFill" []
  | ANF.PosixOpenAt -> union "CliOperation" "PosixOpenAt" []
  | ANF.PosixRead -> union "CliOperation" "PosixRead" []
  | ANF.PosixWrite -> union "CliOperation" "PosixWrite" []
  | ANF.PosixClose -> union "CliOperation" "PosixClose" []
  | ANF.PosixSeek -> union "CliOperation" "PosixSeek" []
  | ANF.PosixStatAt -> union "CliOperation" "PosixStatAt" []
  | ANF.PosixGetCwd -> union "CliOperation" "PosixGetCwd" []
  | ANF.PosixChdir -> union "CliOperation" "PosixChdir" []
  | ANF.PosixMkdirAt -> union "CliOperation" "PosixMkdirAt" []
  | ANF.PosixUnlinkAt -> union "CliOperation" "PosixUnlinkAt" []
  | ANF.PosixRenameAt -> union "CliOperation" "PosixRenameAt" []
  | ANF.PosixChmodAt -> union "CliOperation" "PosixChmodAt" []
  | ANF.PosixChmodAt2 -> union "CliOperation" "PosixChmodAt2" []
  | ANF.PosixUtimesAt -> union "CliOperation" "PosixUtimesAt" []
  | ANF.PosixSetAttributesAt -> union "CliOperation" "PosixSetAttributesAt" []
  | ANF.PosixSymlinkAt -> union "CliOperation" "PosixSymlinkAt" []
  | ANF.PosixReadlinkAt -> union "CliOperation" "PosixReadlinkAt" []
  | ANF.PosixFlock -> union "CliOperation" "PosixFlock" []
  | ANF.PosixGetDents -> union "CliOperation" "PosixGetDents" []
  | ANF.PosixIoctl -> union "CliOperation" "PosixIoctl" []
  | ANF.PosixProcInfo -> union "CliOperation" "PosixProcInfo" []

and aNF_recordDescriptor (value : ANF.recordDescriptor) =
  record "RecordDescriptor"
    [
      ("SourceTypeName", text value.ANF.sourceTypeName);
      ("RuntimeTypeName", text value.ANF.runtimeTypeName);
      ( "TypeArgs",
        list
          (fun item -> StructuralFormat.semanticValue item)
          value.ANF.typeArgs );
      ( "Fields",
        list
          (fun item ->
            let part0, part1 = item in
            tuple [ text part0; StructuralFormat.semanticValue part1 ])
          value.ANF.fields );
      ("ValueType", StructuralFormat.semanticValue value.ANF.valueType);
    ]

and aNF_cExpr (value : ANF.cExpr) =
  match value with
  | ANF.Atom field0 -> union "CExpr" "Atom" [ aNF_atom field0 ]
  | ANF.TypedAtom (field0, field1) ->
      union "CExpr" "TypedAtom"
        [ aNF_atom field0; StructuralFormat.semanticValue field1 ]
  | ANF.Prim (field0, field1, field2) ->
      union "CExpr" "Prim"
        [ aNF_binOp field0; aNF_atom field1; aNF_atom field2 ]
  | ANF.UnaryPrim (field0, field1) ->
      union "CExpr" "UnaryPrim" [ aNF_unaryOp field0; aNF_atom field1 ]
  | ANF.IfValue (field0, field1, field2) ->
      union "CExpr" "IfValue"
        [ aNF_atom field0; aNF_atom field1; aNF_atom field2 ]
  | ANF.Call (field0, field1) ->
      union "CExpr" "Call"
        [ functionId field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.BorrowedCall (field0, field1) ->
      union "CExpr" "BorrowedCall"
        [ functionId field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.TailCall (field0, field1) ->
      union "CExpr" "TailCall"
        [ functionId field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.IndirectCall (field0, field1) ->
      union "CExpr" "IndirectCall"
        [ aNF_atom field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.IndirectTailCall (field0, field1) ->
      union "CExpr" "IndirectTailCall"
        [ aNF_atom field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.ClosureAlloc (field0, field1) ->
      union "CExpr" "ClosureAlloc"
        [ functionId field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.ClosureCall (field0, field1) ->
      union "CExpr" "ClosureCall"
        [ aNF_atom field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.ClosureTailCall (field0, field1) ->
      union "CExpr" "ClosureTailCall"
        [ aNF_atom field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.TupleAlloc field0 ->
      union "CExpr" "TupleAlloc" [ list (fun item -> aNF_atom item) field0 ]
  | ANF.TupleGet (field0, field1) ->
      union "CExpr" "TupleGet" [ aNF_atom field0; int32 field1 ]
  | ANF.RecordAlloc (field0, field1) ->
      union "CExpr" "RecordAlloc"
        [ aNF_recordDescriptor field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.RecordGet (field0, field1, field2) ->
      union "CExpr" "RecordGet"
        [ aNF_recordDescriptor field0; aNF_atom field1; int32 field2 ]
  | ANF.RecordClone (field0, field1, field2) ->
      union "CExpr" "RecordClone"
        [
          aNF_recordDescriptor field0;
          aNF_atom field1;
          list (fun item -> aNF_atom item) field2;
        ]
  | ANF.RecordReuse (field0, field1, field2, field3) ->
      union "CExpr" "RecordReuse"
        [
          aNF_recordDescriptor field0;
          aNF_recordDescriptor field1;
          aNF_atom field2;
          list (fun item -> aNF_atom item) field3;
        ]
  | ANF.StringConcat (field0, field1, field2) ->
      union "CExpr" "StringConcat"
        [
          aNF_atom field0;
          aNF_atom field1;
          list (fun item -> aNF_atom item) field2;
        ]
  | ANF.CanonicalBufferEq (field0, field1, field2) ->
      union "CExpr" "CanonicalBufferEq"
        [
          memoryModel_canonicalBufferKind field0;
          aNF_atom field1;
          aNF_atom field2;
        ]
  | ANF.RefCountInc (field0, field1, field2, field3) ->
      union "CExpr" "RefCountInc"
        [
          aNF_atom field0;
          int32 field1;
          memoryModel_rcKind field2;
          option (fun item -> memoryModel_rcMetadata item) field3;
        ]
  | ANF.RefCountDec (field0, field1, field2, field3) ->
      union "CExpr" "RefCountDec"
        [
          aNF_atom field0;
          int32 field1;
          memoryModel_rcKind field2;
          option (fun item -> memoryModel_rcMetadata item) field3;
        ]
  | ANF.Print (field0, field1) ->
      union "CExpr" "Print"
        [ aNF_atom field0; StructuralFormat.semanticValue field1 ]
  | ANF.StdoutWrite (field0, field1) ->
      union "CExpr" "StdoutWrite" [ aNF_atom field0; boolean field1 ]
  | ANF.StdinReadLine -> union "CExpr" "StdinReadLine" []
  | ANF.RuntimeError field0 -> union "CExpr" "RuntimeError" [ text field0 ]
  | ANF.RuntimeErrorString field0 ->
      union "CExpr" "RuntimeErrorString" [ aNF_atom field0 ]
  | ANF.FileReadBlob field0 -> union "CExpr" "FileReadBlob" [ aNF_atom field0 ]
  | ANF.FileExists field0 -> union "CExpr" "FileExists" [ aNF_atom field0 ]
  | ANF.FileWriteBlob (field0, field1) ->
      union "CExpr" "FileWriteBlob" [ aNF_atom field0; aNF_atom field1 ]
  | ANF.FileAppendText (field0, field1) ->
      union "CExpr" "FileAppendText" [ aNF_atom field0; aNF_atom field1 ]
  | ANF.FileDelete field0 -> union "CExpr" "FileDelete" [ aNF_atom field0 ]
  | ANF.FileCreateDirectory field0 ->
      union "CExpr" "FileCreateDirectory" [ aNF_atom field0 ]
  | ANF.FileSetExecutable field0 ->
      union "CExpr" "FileSetExecutable" [ aNF_atom field0 ]
  | ANF.FileWriteFromPtr (field0, field1, field2) ->
      union "CExpr" "FileWriteFromPtr"
        [ aNF_atom field0; aNF_atom field1; aNF_atom field2 ]
  | ANF.FloatSqrt field0 -> union "CExpr" "FloatSqrt" [ aNF_atom field0 ]
  | ANF.FloatAbs field0 -> union "CExpr" "FloatAbs" [ aNF_atom field0 ]
  | ANF.FloatNeg field0 -> union "CExpr" "FloatNeg" [ aNF_atom field0 ]
  | ANF.Int64ToFloat field0 -> union "CExpr" "Int64ToFloat" [ aNF_atom field0 ]
  | ANF.FloatToInt64 field0 -> union "CExpr" "FloatToInt64" [ aNF_atom field0 ]
  | ANF.FloatToBits field0 -> union "CExpr" "FloatToBits" [ aNF_atom field0 ]
  | ANF.RawAlloc field0 -> union "CExpr" "RawAlloc" [ aNF_atom field0 ]
  | ANF.MappedAlloc field0 -> union "CExpr" "MappedAlloc" [ aNF_atom field0 ]
  | ANF.RawFree field0 -> union "CExpr" "RawFree" [ aNF_atom field0 ]
  | ANF.MappedFree field0 -> union "CExpr" "MappedFree" [ aNF_atom field0 ]
  | ANF.RawGet (field0, field1, field2) ->
      union "CExpr" "RawGet"
        [
          aNF_atom field0;
          aNF_atom field1;
          option (fun item -> StructuralFormat.semanticValue item) field2;
        ]
  | ANF.RawTake (field0, field1, field2) ->
      union "CExpr" "RawTake"
        [
          aNF_atom field0;
          aNF_atom field1;
          option (fun item -> StructuralFormat.semanticValue item) field2;
        ]
  | ANF.RawGetByte (field0, field1) ->
      union "CExpr" "RawGetByte" [ aNF_atom field0; aNF_atom field1 ]
  | ANF.RawWriteWord (field0, field1, field2) ->
      union "CExpr" "RawWriteWord"
        [ aNF_atom field0; aNF_atom field1; aNF_atom field2 ]
  | ANF.RawWriteByte (field0, field1, field2) ->
      union "CExpr" "RawWriteByte"
        [ aNF_atom field0; aNF_atom field1; aNF_atom field2 ]
  | ANF.RawSlotInit (field0, field1, field2, field3) ->
      union "CExpr" "RawSlotInit"
        [
          aNF_atom field0;
          aNF_atom field1;
          aNF_atom field2;
          StructuralFormat.semanticValue field3;
        ]
  | ANF.StringToRawPtr field0 ->
      union "CExpr" "StringToRawPtr" [ aNF_atom field0 ]
  | ANF.RawPtrToString field0 ->
      union "CExpr" "RawPtrToString" [ aNF_atom field0 ]
  | ANF.BlobToRawPtr field0 -> union "CExpr" "BlobToRawPtr" [ aNF_atom field0 ]
  | ANF.RawPtrToBlob field0 -> union "CExpr" "RawPtrToBlob" [ aNF_atom field0 ]
  | ANF.RawPtrToInt128 field0 ->
      union "CExpr" "RawPtrToInt128" [ aNF_atom field0 ]
  | ANF.RawPtrToUInt128 field0 ->
      union "CExpr" "RawPtrToUInt128" [ aNF_atom field0 ]
  | ANF.DictToRawPtr field0 -> union "CExpr" "DictToRawPtr" [ aNF_atom field0 ]
  | ANF.RawPtrToDict (field0, field1, field2) ->
      union "CExpr" "RawPtrToDict"
        [
          aNF_atom field0;
          aNF_atom field1;
          StructuralFormat.semanticValue field2;
        ]
  | ANF.ListToRawPtr field0 -> union "CExpr" "ListToRawPtr" [ aNF_atom field0 ]
  | ANF.FixedBlockToRawPtr field0 ->
      union "CExpr" "FixedBlockToRawPtr" [ aNF_atom field0 ]
  | ANF.RawPtrToList (field0, field1, field2) ->
      union "CExpr" "RawPtrToList"
        [
          aNF_atom field0;
          aNF_atom field1;
          StructuralFormat.semanticValue field2;
        ]
  | ANF.RefCountIncString field0 ->
      union "CExpr" "RefCountIncString" [ aNF_atom field0 ]
  | ANF.RefCountDecString field0 ->
      union "CExpr" "RefCountDecString" [ aNF_atom field0 ]
  | ANF.RefCountIncBlob field0 ->
      union "CExpr" "RefCountIncBlob" [ aNF_atom field0 ]
  | ANF.RefCountDecBlob field0 ->
      union "CExpr" "RefCountDecBlob" [ aNF_atom field0 ]
  | ANF.RefCountIncInt field0 ->
      union "CExpr" "RefCountIncInt" [ aNF_atom field0 ]
  | ANF.RefCountDecInt field0 ->
      union "CExpr" "RefCountDecInt" [ aNF_atom field0 ]
  | ANF.RandomInt64 -> union "CExpr" "RandomInt64" []
  | ANF.DateTimeNow -> union "CExpr" "DateTimeNow" []
  | ANF.Sleep field0 -> union "CExpr" "Sleep" [ aNF_atom field0 ]
  | ANF.CliNative (field0, field1) ->
      union "CExpr" "CliNative"
        [ aNF_cliOperation field0; list (fun item -> aNF_atom item) field1 ]
  | ANF.FloatToString field0 ->
      union "CExpr" "FloatToString" [ aNF_atom field0 ]

and aNF_aExpr (value : ANF.aExpr) =
  match value with
  | ANF.Let (field0, field1, field2) ->
      union "AExpr" "Let"
        [ aNF_tempId field0; aNF_cExpr field1; aNF_aExpr field2 ]
  | ANF.Return field0 -> union "AExpr" "Return" [ aNF_atom field0 ]
  | ANF.If (field0, field1, field2) ->
      union "AExpr" "If" [ aNF_atom field0; aNF_aExpr field1; aNF_aExpr field2 ]
  | ANF.Join (field0, field1, field2) ->
      union "AExpr" "Join"
        [ aNF_typedParam field0; aNF_aExpr field1; aNF_aExpr field2 ]
  | ANF.Jump (field0, field1) ->
      union "AExpr" "Jump" [ aNF_tempId field0; aNF_atom field1 ]

and aNF_functionDef (value : ANF.functionDef) =
  record "Function"
    [
      ("Id", functionId value.ANF.id);
      ("Name", text value.ANF.name);
      ( "TypedParams",
        list (fun item -> aNF_typedParam item) value.ANF.typedParams );
      ("ReturnType", StructuralFormat.semanticValue value.ANF.returnType);
      ("ReturnOwnership", aNF_returnOwnership value.ANF.returnOwnership);
      ("Body", aNF_aExpr value.ANF.body);
    ]

and aNF_program (value : ANF.program) =
  match value with
  | ANF.Program (field0, field1) ->
      union "Program" "Program"
        [ list (fun item -> aNF_functionDef item) field0; aNF_aExpr field1 ]

and aNF_varGen (value : ANF.varGen) =
  match value with
  | ANF.VarGen field0 -> union "VarGen" "VarGen" [ int32 field0 ]

and aNF_exprId (value : ANF.exprId) = int32 value

and aNF_exprIdGen (value : ANF.exprIdGen) =
  match value with
  | ANF.ExprIdGen field0 -> union "ExprIdGen" "ExprIdGen" [ int32 field0 ]

and aNF_coverageMapping (value : ANF.coverageMapping) =
  record "CoverageMapping"
    [
      ("Descriptions", coverageMap value.ANF.descriptions);
      ("TotalExpressions", int32 value.ANF.totalExpressions);
    ]

let expr value = StructuralFormat.format (aNF_aExpr value)
