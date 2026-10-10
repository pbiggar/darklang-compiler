(* Typed formatting of complete LIR values in original test diagnostics. *)
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

let boolean value = Scalar (string_of_bool value)
let int32 value = Scalar (string_of_int value)
let int64 value = Scalar (Int64.to_string value ^ "L")
let float64 value = Scalar (FloatFormat.structural value)
let functionId = AST.DiagnosticFormatting.func

let rec physReg (value : LIR.physReg) =
  match value with
  | LIR.X0 -> union "PhysReg" "X0" []
  | LIR.X1 -> union "PhysReg" "X1" []
  | LIR.X2 -> union "PhysReg" "X2" []
  | LIR.X3 -> union "PhysReg" "X3" []
  | LIR.X4 -> union "PhysReg" "X4" []
  | LIR.X5 -> union "PhysReg" "X5" []
  | LIR.X6 -> union "PhysReg" "X6" []
  | LIR.X7 -> union "PhysReg" "X7" []
  | LIR.X8 -> union "PhysReg" "X8" []
  | LIR.X9 -> union "PhysReg" "X9" []
  | LIR.X10 -> union "PhysReg" "X10" []
  | LIR.X11 -> union "PhysReg" "X11" []
  | LIR.X12 -> union "PhysReg" "X12" []
  | LIR.X13 -> union "PhysReg" "X13" []
  | LIR.X14 -> union "PhysReg" "X14" []
  | LIR.X15 -> union "PhysReg" "X15" []
  | LIR.X16 -> union "PhysReg" "X16" []
  | LIR.X17 -> union "PhysReg" "X17" []
  | LIR.X19 -> union "PhysReg" "X19" []
  | LIR.X20 -> union "PhysReg" "X20" []
  | LIR.X21 -> union "PhysReg" "X21" []
  | LIR.X22 -> union "PhysReg" "X22" []
  | LIR.X23 -> union "PhysReg" "X23" []
  | LIR.X24 -> union "PhysReg" "X24" []
  | LIR.X25 -> union "PhysReg" "X25" []
  | LIR.X26 -> union "PhysReg" "X26" []
  | LIR.X27 -> union "PhysReg" "X27" []
  | LIR.X29 -> union "PhysReg" "X29" []
  | LIR.X30 -> union "PhysReg" "X30" []
  | LIR.SP -> union "PhysReg" "SP" []

and physFPReg (value : LIR.physFPReg) =
  match value with
  | LIR.D0 -> union "PhysFPReg" "D0" []
  | LIR.D1 -> union "PhysFPReg" "D1" []
  | LIR.D2 -> union "PhysFPReg" "D2" []
  | LIR.D3 -> union "PhysFPReg" "D3" []
  | LIR.D4 -> union "PhysFPReg" "D4" []
  | LIR.D5 -> union "PhysFPReg" "D5" []
  | LIR.D6 -> union "PhysFPReg" "D6" []
  | LIR.D7 -> union "PhysFPReg" "D7" []
  | LIR.D8 -> union "PhysFPReg" "D8" []
  | LIR.D9 -> union "PhysFPReg" "D9" []
  | LIR.D10 -> union "PhysFPReg" "D10" []
  | LIR.D11 -> union "PhysFPReg" "D11" []
  | LIR.D12 -> union "PhysFPReg" "D12" []
  | LIR.D13 -> union "PhysFPReg" "D13" []
  | LIR.D14 -> union "PhysFPReg" "D14" []
  | LIR.D15 -> union "PhysFPReg" "D15" []

and reg (value : LIR.reg) =
  match value with
  | LIR.Physical field0 -> union "Reg" "Physical" [ physReg field0 ]
  | LIR.Virtual field0 -> union "Reg" "Virtual" [ int32 field0 ]

and fReg (value : LIR.fReg) =
  match value with
  | LIR.FPhysical field0 -> union "FReg" "FPhysical" [ physFPReg field0 ]
  | LIR.FVirtual field0 -> union "FReg" "FVirtual" [ int32 field0 ]

and typedLIRParam (value : LIR.typedLIRParam) =
  record "TypedLIRParam"
    [
      ("Reg", reg value.LIR.reg);
      ("Type", StructuralFormat.semanticValue value.LIR.typ);
    ]

and operand (value : LIR.operand) =
  match value with
  | LIR.Imm field0 -> union "Operand" "Imm" [ int64 field0 ]
  | LIR.FloatImm field0 -> union "Operand" "FloatImm" [ float64 field0 ]
  | LIR.Reg field0 -> union "Operand" "Reg" [ reg field0 ]
  | LIR.StackSlot field0 -> union "Operand" "StackSlot" [ int32 field0 ]
  | LIR.StringSymbol field0 -> union "Operand" "StringSymbol" [ text field0 ]
  | LIR.FloatSymbol field0 -> union "Operand" "FloatSymbol" [ float64 field0 ]
  | LIR.FuncAddr field0 -> union "Operand" "FuncAddr" [ functionId field0 ]

and condition (value : LIR.condition) =
  match value with
  | LIR.EQ -> union "Condition" "EQ" []
  | LIR.NE -> union "Condition" "NE" []
  | LIR.LT -> union "Condition" "LT" []
  | LIR.GT -> union "Condition" "GT" []
  | LIR.LE -> union "Condition" "LE" []
  | LIR.GE -> union "Condition" "GE" []
  | LIR.ULT -> union "Condition" "ULT" []
  | LIR.UGT -> union "Condition" "UGT" []
  | LIR.ULE -> union "Condition" "ULE" []
  | LIR.UGE -> union "Condition" "UGE" []

and rcKind (value : LIR.rcKind) =
  match value with
  | LIR.GenericHeap -> union "RcKind" "GenericHeap" []
  | LIR.StreamHeap -> union "RcKind" "StreamHeap" []
  | LIR.TaggedList -> union "RcKind" "TaggedList" []
  | LIR.DictHeap -> union "RcKind" "DictHeap" []
  | LIR.ClosureHeap -> union "RcKind" "ClosureHeap" []

and cliOperation (value : LIR.cliOperation) =
  match value with
  | LIR.Execute -> union "CliOperation" "Execute" []
  | LIR.StartupStack -> union "CliOperation" "StartupStack" []
  | LIR.ExecutableState -> union "CliOperation" "ExecutableState" []
  | LIR.RunProcess -> union "CliOperation" "RunProcess" []
  | LIR.HostOS -> union "CliOperation" "HostOS" []
  | LIR.HostArchitecture -> union "CliOperation" "HostArchitecture" []
  | LIR.Hostname -> union "CliOperation" "Hostname" []
  | LIR.GetEnv -> union "CliOperation" "GetEnv" []
  | LIR.GetEnvironmentPacked -> union "CliOperation" "GetEnvironmentPacked" []
  | LIR.StdinState -> union "CliOperation" "StdinState" []
  | LIR.SetEnv -> union "CliOperation" "SetEnv" []
  | LIR.UnsetEnv -> union "CliOperation" "UnsetEnv" []
  | LIR.DirectoryCurrent -> union "CliOperation" "DirectoryCurrent" []
  | LIR.DirectoryListPacked -> union "CliOperation" "DirectoryListPacked" []
  | LIR.FileIsDirectory -> union "CliOperation" "FileIsDirectory" []
  | LIR.FileCreateExclusive -> union "CliOperation" "FileCreateExclusive" []
  | LIR.GetArgv -> union "CliOperation" "GetArgv" []
  | LIR.Kill -> union "CliOperation" "Kill" []
  | LIR.GetPid -> union "CliOperation" "GetPid" []
  | LIR.GetUid -> union "CliOperation" "GetUid" []
  | LIR.CpuCount -> union "CliOperation" "CpuCount" []
  | LIR.SpawnProcess -> union "CliOperation" "SpawnProcess" []
  | LIR.ProcessIO -> union "CliOperation" "ProcessIO" []
  | LIR.TerminateProcess -> union "CliOperation" "TerminateProcess" []
  | LIR.SocketTcp4 -> union "CliOperation" "SocketTcp4" []
  | LIR.SocketTcp6 -> union "CliOperation" "SocketTcp6" []
  | LIR.SocketUdp4 -> union "CliOperation" "SocketUdp4" []
  | LIR.SocketUdp6 -> union "CliOperation" "SocketUdp6" []
  | LIR.SocketConnect4 -> union "CliOperation" "SocketConnect4" []
  | LIR.SocketConnect6 -> union "CliOperation" "SocketConnect6" []
  | LIR.SocketSend -> union "CliOperation" "SocketSend" []
  | LIR.SocketSendTo -> union "CliOperation" "SocketSendTo" []
  | LIR.SocketReceive -> union "CliOperation" "SocketReceive" []
  | LIR.SocketReceiveFrom -> union "CliOperation" "SocketReceiveFrom" []
  | LIR.SocketReceiveTimeout -> union "CliOperation" "SocketReceiveTimeout" []
  | LIR.SocketSendTimeout -> union "CliOperation" "SocketSendTimeout" []
  | LIR.SocketClose -> union "CliOperation" "SocketClose" []
  | LIR.SocketBind4 -> union "CliOperation" "SocketBind4" []
  | LIR.SocketBind6 -> union "CliOperation" "SocketBind6" []
  | LIR.SocketListen -> union "CliOperation" "SocketListen" []
  | LIR.SocketAccept -> union "CliOperation" "SocketAccept" []
  | LIR.SocketCloexec -> union "CliOperation" "SocketCloexec" []
  | LIR.SocketReuseAddress -> union "CliOperation" "SocketReuseAddress" []
  | LIR.SocketPoll -> union "CliOperation" "SocketPoll" []
  | LIR.SignalBlock -> union "CliOperation" "SignalBlock" []
  | LIR.SignalRestore -> union "CliOperation" "SignalRestore" []
  | LIR.SignalPending -> union "CliOperation" "SignalPending" []
  | LIR.SignalWait -> union "CliOperation" "SignalWait" []
  | LIR.MonotonicTime -> union "CliOperation" "MonotonicTime" []
  | LIR.SecureRandomFill -> union "CliOperation" "SecureRandomFill" []
  | LIR.PosixOpenAt -> union "CliOperation" "PosixOpenAt" []
  | LIR.PosixRead -> union "CliOperation" "PosixRead" []
  | LIR.PosixWrite -> union "CliOperation" "PosixWrite" []
  | LIR.PosixClose -> union "CliOperation" "PosixClose" []
  | LIR.PosixSeek -> union "CliOperation" "PosixSeek" []
  | LIR.PosixStatAt -> union "CliOperation" "PosixStatAt" []
  | LIR.PosixGetCwd -> union "CliOperation" "PosixGetCwd" []
  | LIR.PosixChdir -> union "CliOperation" "PosixChdir" []
  | LIR.PosixMkdirAt -> union "CliOperation" "PosixMkdirAt" []
  | LIR.PosixUnlinkAt -> union "CliOperation" "PosixUnlinkAt" []
  | LIR.PosixRenameAt -> union "CliOperation" "PosixRenameAt" []
  | LIR.PosixChmodAt -> union "CliOperation" "PosixChmodAt" []
  | LIR.PosixChmodAt2 -> union "CliOperation" "PosixChmodAt2" []
  | LIR.PosixUtimesAt -> union "CliOperation" "PosixUtimesAt" []
  | LIR.PosixSetAttributesAt -> union "CliOperation" "PosixSetAttributesAt" []
  | LIR.PosixSymlinkAt -> union "CliOperation" "PosixSymlinkAt" []
  | LIR.PosixReadlinkAt -> union "CliOperation" "PosixReadlinkAt" []
  | LIR.PosixAccessAt -> union "CliOperation" "PosixAccessAt" []
  | LIR.PosixFlock -> union "CliOperation" "PosixFlock" []
  | LIR.PosixGetDents -> union "CliOperation" "PosixGetDents" []
  | LIR.PosixIoctl -> union "CliOperation" "PosixIoctl" []
  | LIR.PosixProcInfo -> union "CliOperation" "PosixProcInfo" []

and label (value : LIR.label) =
  match value with LIR.Label field0 -> union "Label" "Label" [ text field0 ]

and instr (value : LIR.instr) =
  match value with
  | LIR.Mov (field0, field1) ->
      union "Instr" "Mov" [ reg field0; operand field1 ]
  | LIR.Phi (field0, field1, field2) ->
      union "Instr" "Phi"
        [
          reg field0;
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ operand part0; label part1 ])
            field1;
          option (fun item -> StructuralFormat.semanticValue item) field2;
        ]
  | LIR.Store (field0, field1) ->
      union "Instr" "Store" [ int32 field0; reg field1 ]
  | LIR.Add (field0, field1, field2) ->
      union "Instr" "Add" [ reg field0; reg field1; operand field2 ]
  | LIR.Sub (field0, field1, field2) ->
      union "Instr" "Sub" [ reg field0; reg field1; operand field2 ]
  | LIR.Mul (field0, field1, field2) ->
      union "Instr" "Mul" [ reg field0; reg field1; reg field2 ]
  | LIR.Sdiv (field0, field1, field2) ->
      union "Instr" "Sdiv" [ reg field0; reg field1; reg field2 ]
  | LIR.Udiv (field0, field1, field2) ->
      union "Instr" "Udiv" [ reg field0; reg field1; reg field2 ]
  | LIR.Msub (field0, field1, field2, field3) ->
      union "Instr" "Msub" [ reg field0; reg field1; reg field2; reg field3 ]
  | LIR.Madd (field0, field1, field2, field3) ->
      union "Instr" "Madd" [ reg field0; reg field1; reg field2; reg field3 ]
  | LIR.Cmp (field0, field1) ->
      union "Instr" "Cmp" [ reg field0; operand field1 ]
  | LIR.Cset (field0, field1) ->
      union "Instr" "Cset" [ reg field0; condition field1 ]
  | LIR.Select (field0, field1, field2, field3) ->
      union "Instr" "Select"
        [ reg field0; reg field1; reg field2; condition field3 ]
  | LIR.And (field0, field1, field2) ->
      union "Instr" "And" [ reg field0; reg field1; reg field2 ]
  | LIR.And_imm (field0, field1, field2) ->
      union "Instr" "And_imm" [ reg field0; reg field1; int64 field2 ]
  | LIR.Orr (field0, field1, field2) ->
      union "Instr" "Orr" [ reg field0; reg field1; reg field2 ]
  | LIR.Eor (field0, field1, field2) ->
      union "Instr" "Eor" [ reg field0; reg field1; reg field2 ]
  | LIR.Lsl (field0, field1, field2) ->
      union "Instr" "Lsl" [ reg field0; reg field1; reg field2 ]
  | LIR.Lsr (field0, field1, field2) ->
      union "Instr" "Lsr" [ reg field0; reg field1; reg field2 ]
  | LIR.Asr (field0, field1, field2) ->
      union "Instr" "Asr" [ reg field0; reg field1; reg field2 ]
  | LIR.Lsl_imm (field0, field1, field2) ->
      union "Instr" "Lsl_imm" [ reg field0; reg field1; int32 field2 ]
  | LIR.Lsr_imm (field0, field1, field2) ->
      union "Instr" "Lsr_imm" [ reg field0; reg field1; int32 field2 ]
  | LIR.Asr_imm (field0, field1, field2) ->
      union "Instr" "Asr_imm" [ reg field0; reg field1; int32 field2 ]
  | LIR.Neg (field0, field1) -> union "Instr" "Neg" [ reg field0; reg field1 ]
  | LIR.Mvn (field0, field1) -> union "Instr" "Mvn" [ reg field0; reg field1 ]
  | LIR.Sxtb (field0, field1) -> union "Instr" "Sxtb" [ reg field0; reg field1 ]
  | LIR.Sxth (field0, field1) -> union "Instr" "Sxth" [ reg field0; reg field1 ]
  | LIR.Sxtw (field0, field1) -> union "Instr" "Sxtw" [ reg field0; reg field1 ]
  | LIR.Uxtb (field0, field1) -> union "Instr" "Uxtb" [ reg field0; reg field1 ]
  | LIR.Uxth (field0, field1) -> union "Instr" "Uxth" [ reg field0; reg field1 ]
  | LIR.Uxtw (field0, field1) -> union "Instr" "Uxtw" [ reg field0; reg field1 ]
  | LIR.Call (field0, field1, field2) ->
      union "Instr" "Call"
        [
          reg field0; functionId field1; list (fun item -> operand item) field2;
        ]
  | LIR.TailCall (field0, field1) ->
      union "Instr" "TailCall"
        [ functionId field0; list (fun item -> operand item) field1 ]
  | LIR.IndirectCall (field0, field1, field2) ->
      union "Instr" "IndirectCall"
        [ reg field0; reg field1; list (fun item -> operand item) field2 ]
  | LIR.IndirectTailCall (field0, field1) ->
      union "Instr" "IndirectTailCall"
        [ reg field0; list (fun item -> operand item) field1 ]
  | LIR.ClosureAlloc (field0, field1, field2) ->
      union "Instr" "ClosureAlloc"
        [
          reg field0; functionId field1; list (fun item -> operand item) field2;
        ]
  | LIR.ClosureCall (field0, field1, field2) ->
      union "Instr" "ClosureCall"
        [ reg field0; reg field1; list (fun item -> operand item) field2 ]
  | LIR.ClosureTailCall (field0, field1) ->
      union "Instr" "ClosureTailCall"
        [ reg field0; list (fun item -> operand item) field1 ]
  | LIR.SaveRegs (field0, field1) ->
      union "Instr" "SaveRegs"
        [
          list (fun item -> physReg item) field0;
          list (fun item -> physFPReg item) field1;
        ]
  | LIR.RestoreRegs (field0, field1) ->
      union "Instr" "RestoreRegs"
        [
          list (fun item -> physReg item) field0;
          list (fun item -> physFPReg item) field1;
        ]
  | LIR.ArgMoves field0 ->
      union "Instr" "ArgMoves"
        [
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ physReg part0; operand part1 ])
            field0;
        ]
  | LIR.TailArgMoves field0 ->
      union "Instr" "TailArgMoves"
        [
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ physReg part0; operand part1 ])
            field0;
        ]
  | LIR.FArgMoves field0 ->
      union "Instr" "FArgMoves"
        [
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ physFPReg part0; fReg part1 ])
            field0;
        ]
  | LIR.PrintInt64 field0 -> union "Instr" "PrintInt64" [ reg field0 ]
  | LIR.PrintUInt64 field0 -> union "Instr" "PrintUInt64" [ reg field0 ]
  | LIR.PrintBool field0 -> union "Instr" "PrintBool" [ reg field0 ]
  | LIR.PrintInt64NoNewline field0 ->
      union "Instr" "PrintInt64NoNewline" [ reg field0 ]
  | LIR.PrintUInt64NoNewline field0 ->
      union "Instr" "PrintUInt64NoNewline" [ reg field0 ]
  | LIR.PrintBoolNoNewline field0 ->
      union "Instr" "PrintBoolNoNewline" [ reg field0 ]
  | LIR.PrintFloat field0 -> union "Instr" "PrintFloat" [ fReg field0 ]
  | LIR.PrintFloatNoNewline field0 ->
      union "Instr" "PrintFloatNoNewline" [ fReg field0 ]
  | LIR.PrintString field0 -> union "Instr" "PrintString" [ text field0 ]
  | LIR.StdoutWrite (field0, field1, field2) ->
      union "Instr" "StdoutWrite"
        [ int32 field0; operand field1; boolean field2 ]
  | LIR.StdinReadLine (field0, field1) ->
      union "Instr" "StdinReadLine" [ int32 field0; reg field1 ]
  | LIR.RuntimeError field0 -> union "Instr" "RuntimeError" [ text field0 ]
  | LIR.RuntimeErrorString field0 ->
      union "Instr" "RuntimeErrorString" [ reg field0 ]
  | LIR.PrintHeapStringNoNewline field0 ->
      union "Instr" "PrintHeapStringNoNewline" [ reg field0 ]
  | LIR.PrintChars field0 ->
      union "Instr" "PrintChars"
        [ list (fun value -> Scalar (string_of_int value ^ "uy")) field0 ]
  | LIR.PrintBlob field0 -> union "Instr" "PrintBlob" [ reg field0 ]
  | LIR.PrintList (field0, field1) ->
      union "Instr" "PrintList"
        [ reg field0; StructuralFormat.semanticValue field1 ]
  | LIR.PrintSum (field0, field1, field2) ->
      union "Instr" "PrintSum"
        [
          reg field0;
          list
            (fun item ->
              let part0, part1, part2 = item in
              tuple
                [
                  text part0;
                  int32 part1;
                  option (fun item -> StructuralFormat.semanticValue item) part2;
                ])
            field1;
          boolean field2;
        ]
  | LIR.PrintRecord (field0, field1, field2) ->
      union "Instr" "PrintRecord"
        [
          reg field0;
          text field1;
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ text part0; StructuralFormat.semanticValue part1 ])
            field2;
        ]
  | LIR.Exit -> union "Instr" "Exit" []
  | LIR.FPhi (field0, field1) ->
      union "Instr" "FPhi"
        [
          fReg field0;
          list
            (fun item ->
              let part0, part1 = item in
              tuple [ fReg part0; label part1 ])
            field1;
        ]
  | LIR.FMov (field0, field1) ->
      union "Instr" "FMov" [ fReg field0; fReg field1 ]
  | LIR.FLoad (field0, field1) ->
      union "Instr" "FLoad" [ fReg field0; float64 field1 ]
  | LIR.FSpillLoad (field0, field1) ->
      union "Instr" "FSpillLoad" [ fReg field0; int32 field1 ]
  | LIR.FSpillStore (field0, field1) ->
      union "Instr" "FSpillStore" [ int32 field0; fReg field1 ]
  | LIR.FAdd (field0, field1, field2) ->
      union "Instr" "FAdd" [ fReg field0; fReg field1; fReg field2 ]
  | LIR.FSub (field0, field1, field2) ->
      union "Instr" "FSub" [ fReg field0; fReg field1; fReg field2 ]
  | LIR.FMul (field0, field1, field2) ->
      union "Instr" "FMul" [ fReg field0; fReg field1; fReg field2 ]
  | LIR.FMadd (field0, field1, field2, field3) ->
      union "Instr" "FMadd"
        [ fReg field0; fReg field1; fReg field2; fReg field3 ]
  | LIR.FDiv (field0, field1, field2) ->
      union "Instr" "FDiv" [ fReg field0; fReg field1; fReg field2 ]
  | LIR.FNeg (field0, field1) ->
      union "Instr" "FNeg" [ fReg field0; fReg field1 ]
  | LIR.FAbs (field0, field1) ->
      union "Instr" "FAbs" [ fReg field0; fReg field1 ]
  | LIR.FSqrt (field0, field1) ->
      union "Instr" "FSqrt" [ fReg field0; fReg field1 ]
  | LIR.FCmp (field0, field1) ->
      union "Instr" "FCmp" [ fReg field0; fReg field1 ]
  | LIR.Int64ToFloat (field0, field1) ->
      union "Instr" "Int64ToFloat" [ fReg field0; reg field1 ]
  | LIR.FloatToInt64 (field0, field1) ->
      union "Instr" "FloatToInt64" [ reg field0; fReg field1 ]
  | LIR.FloatToBits (field0, field1) ->
      union "Instr" "FloatToBits" [ reg field0; fReg field1 ]
  | LIR.GpToFp (field0, field1) ->
      union "Instr" "GpToFp" [ fReg field0; reg field1 ]
  | LIR.FpToGp (field0, field1) ->
      union "Instr" "FpToGp" [ reg field0; fReg field1 ]
  | LIR.HeapAlloc (field0, field1) ->
      union "Instr" "HeapAlloc" [ reg field0; int32 field1 ]
  | LIR.HeapStore (field0, field1, field2, field3) ->
      union "Instr" "HeapStore"
        [
          reg field0;
          int32 field1;
          operand field2;
          option (fun item -> StructuralFormat.semanticValue item) field3;
        ]
  | LIR.HeapLoad (field0, field1, field2) ->
      union "Instr" "HeapLoad" [ reg field0; reg field1; int32 field2 ]
  | LIR.RefCountInc (field0, field1, field2, field3) ->
      union "Instr" "RefCountInc"
        [
          reg field0;
          int32 field1;
          rcKind field2;
          option
            (fun item -> ANFTestFormatting.memoryModel_rcMetadata item)
            field3;
        ]
  | LIR.RefCountDec (field0, field1, field2, field3) ->
      union "Instr" "RefCountDec"
        [
          reg field0;
          int32 field1;
          rcKind field2;
          option
            (fun item -> ANFTestFormatting.memoryModel_rcMetadata item)
            field3;
        ]
  | LIR.StringConcat (field0, field1, field2, field3) ->
      union "Instr" "StringConcat"
        [
          reg field0;
          operand field1;
          operand field2;
          list (fun item -> operand item) field3;
        ]
  | LIR.CanonicalBufferEq (field0, field1, field2, field3) ->
      union "Instr" "CanonicalBufferEq"
        [
          reg field0;
          ANFTestFormatting.memoryModel_canonicalBufferKind field1;
          operand field2;
          operand field3;
        ]
  | LIR.PrintHeapString field0 -> union "Instr" "PrintHeapString" [ reg field0 ]
  | LIR.LoadFuncAddr (field0, field1) ->
      union "Instr" "LoadFuncAddr" [ reg field0; functionId field1 ]
  | LIR.FileReadBlob (field0, field1) ->
      union "Instr" "FileReadBlob" [ reg field0; operand field1 ]
  | LIR.FileExists (field0, field1) ->
      union "Instr" "FileExists" [ reg field0; operand field1 ]
  | LIR.FileWriteBlob (field0, field1, field2) ->
      union "Instr" "FileWriteBlob"
        [ reg field0; operand field1; operand field2 ]
  | LIR.FileAppendText (field0, field1, field2) ->
      union "Instr" "FileAppendText"
        [ reg field0; operand field1; operand field2 ]
  | LIR.FileDelete (field0, field1) ->
      union "Instr" "FileDelete" [ reg field0; operand field1 ]
  | LIR.FileCreateDirectory (field0, field1) ->
      union "Instr" "FileCreateDirectory" [ reg field0; operand field1 ]
  | LIR.FileSetExecutable (field0, field1) ->
      union "Instr" "FileSetExecutable" [ reg field0; operand field1 ]
  | LIR.FileWriteFromPtr (field0, field1, field2, field3) ->
      union "Instr" "FileWriteFromPtr"
        [ reg field0; operand field1; reg field2; reg field3 ]
  | LIR.RawAlloc (field0, field1) ->
      union "Instr" "RawAlloc" [ reg field0; reg field1 ]
  | LIR.MappedAlloc (field0, field1) ->
      union "Instr" "MappedAlloc" [ reg field0; reg field1 ]
  | LIR.RawFree field0 -> union "Instr" "RawFree" [ reg field0 ]
  | LIR.MappedFree field0 -> union "Instr" "MappedFree" [ reg field0 ]
  | LIR.RawGet (field0, field1, field2) ->
      union "Instr" "RawGet" [ reg field0; reg field1; reg field2 ]
  | LIR.RawGetByte (field0, field1, field2) ->
      union "Instr" "RawGetByte" [ reg field0; reg field1; reg field2 ]
  | LIR.RawWriteWord (field0, field1, field2) ->
      union "Instr" "RawWriteWord" [ reg field0; reg field1; reg field2 ]
  | LIR.RawWriteByte (field0, field1, field2) ->
      union "Instr" "RawWriteByte" [ reg field0; reg field1; reg field2 ]
  | LIR.RawSlotInit (field0, field1, field2, field3) ->
      union "Instr" "RawSlotInit"
        [
          reg field0;
          reg field1;
          reg field2;
          StructuralFormat.semanticValue field3;
        ]
  | LIR.RefCountIncString field0 ->
      union "Instr" "RefCountIncString" [ operand field0 ]
  | LIR.RefCountDecString field0 ->
      union "Instr" "RefCountDecString" [ operand field0 ]
  | LIR.RefCountIncBlob field0 ->
      union "Instr" "RefCountIncBlob" [ operand field0 ]
  | LIR.RefCountDecBlob field0 ->
      union "Instr" "RefCountDecBlob" [ operand field0 ]
  | LIR.RefCountIncInt field0 ->
      union "Instr" "RefCountIncInt" [ operand field0 ]
  | LIR.RefCountDecInt field0 ->
      union "Instr" "RefCountDecInt" [ operand field0 ]
  | LIR.RandomInt64 field0 -> union "Instr" "RandomInt64" [ reg field0 ]
  | LIR.DateTimeNow field0 -> union "Instr" "DateTimeNow" [ reg field0 ]
  | LIR.Sleep (field0, field1) ->
      union "Instr" "Sleep" [ int32 field0; fReg field1 ]
  | LIR.CliNative (field0, field1, field2) ->
      union "Instr" "CliNative"
        [
          reg field0;
          cliOperation field1;
          list (fun item -> operand item) field2;
        ]
  | LIR.FloatToString (field0, field1) ->
      union "Instr" "FloatToString" [ reg field0; fReg field1 ]
  | LIR.CoverageHit field0 -> union "Instr" "CoverageHit" [ int32 field0 ]

and terminator (value : LIR.terminator) =
  match value with
  | LIR.Ret -> union "Terminator" "Ret" []
  | LIR.Branch (field0, field1, field2) ->
      union "Terminator" "Branch" [ reg field0; label field1; label field2 ]
  | LIR.BranchZero (field0, field1, field2) ->
      union "Terminator" "BranchZero" [ reg field0; label field1; label field2 ]
  | LIR.BranchBitZero (field0, field1, field2, field3) ->
      union "Terminator" "BranchBitZero"
        [ reg field0; int32 field1; label field2; label field3 ]
  | LIR.BranchBitNonZero (field0, field1, field2, field3) ->
      union "Terminator" "BranchBitNonZero"
        [ reg field0; int32 field1; label field2; label field3 ]
  | LIR.CondBranch (field0, field1, field2) ->
      union "Terminator" "CondBranch"
        [ condition field0; label field1; label field2 ]
  | LIR.Jump field0 -> union "Terminator" "Jump" [ label field0 ]

and basicBlock (value : LIR.basicBlock) =
  record "BasicBlock"
    [
      ("Label", label value.LIR.label);
      ("Instrs", list (fun item -> instr item) value.LIR.instrs);
      ("Terminator", terminator value.LIR.terminator);
    ]

and cfg (value : LIR.cfg) =
  record "CFG"
    [
      ("Entry", label value.LIR.entry);
      ( "Blocks",
        Union
          ( "map",
            [
              list
                (fun (key, item) -> tuple [ label key; basicBlock item ])
                (LIR.LabelMap.bindings value.LIR.blocks);
            ] ) );
    ]

and rcReleasePlanMemoKey (value : LIR.rcReleasePlanMemoKey) =
  match value with
  | LIR.FingerprintedReleasePlan field0 ->
      union "RcReleasePlanMemoKey" "FingerprintedReleasePlan" [ text field0 ]
  | LIR.StructuralReleasePlan field0 ->
      union "RcReleasePlanMemoKey" "StructuralReleasePlan"
        [
          option
            (fun item -> ANFTestFormatting.memoryModel_rcReleasePlan item)
            field0;
        ]

and arm64ReleasePlanSummary (value : LIR.arm64ReleasePlanSummary) =
  record "Arm64ReleasePlanSummary"
    [
      ( "ListDecHelperLabels",
        Union
          ( "set",
            [
              list text (StringOrder.Set.elements value.LIR.listDecHelperLabels);
            ] ) );
      ( "PlannedListDecHelpers",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      text key;
                      (let part0, part1 = item in
                       tuple
                         [
                           int32 part0;
                           ANFTestFormatting.memoryModel_rcReleasePlan part1;
                         ]);
                    ])
                (StringOrder.Map.bindings value.LIR.plannedListDecHelpers);
            ] ) );
      ( "ExpensiveGenericDecHelper",
        option
          (fun item ->
            let part0, part1, part2 = item in
            tuple
              [
                text part0;
                int32 part1;
                ANFTestFormatting.memoryModel_rcReleasePlan part2;
              ])
          value.LIR.expensiveGenericDecHelper );
      ( "DictDecHelperLabels",
        Union
          ( "set",
            [
              list text (StringOrder.Set.elements value.LIR.dictDecHelperLabels);
            ] ) );
      ( "PlannedDictDecHelpers",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      text key; ANFTestFormatting.memoryModel_rcReleasePlan item;
                    ])
                (StringOrder.Map.bindings value.LIR.plannedDictDecHelpers);
            ] ) );
      ("NeedsClosureRcDecHelper", boolean value.LIR.needsClosureRcDecHelper);
      ("NeedsStreamRcDecHelper", boolean value.LIR.needsStreamRcDecHelper);
    ]

and arm64PlannedGenericDecHelper (value : LIR.arm64PlannedGenericDecHelper) =
  record "Arm64PlannedGenericDecHelper"
    [
      ( "ReleasePlanMemoKeys",
        Union
          ( "set",
            [
              list rcReleasePlanMemoKey
                (LIR.RcReleasePlanMemoKeySet.elements
                   value.LIR.releasePlanMemoKeys);
            ] ) );
      ("PayloadSize", int32 value.LIR.payloadSize);
      ( "ReleasePlan",
        ANFTestFormatting.memoryModel_rcReleasePlan value.LIR.releasePlan );
      ("OwnsSinglePayloadSum", boolean value.LIR.ownsSinglePayloadSum);
    ]

and arm64RcHelperRequirements (value : LIR.arm64RcHelperRequirements) =
  record "Arm64RcHelperRequirements"
    [
      ( "ListDecHelperLabels",
        Union
          ( "set",
            [
              list text (StringOrder.Set.elements value.LIR.listDecHelperLabels);
            ] ) );
      ( "PlannedListDecHelpers",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      text key;
                      (let part0, part1 = item in
                       tuple
                         [
                           int32 part0;
                           ANFTestFormatting.memoryModel_rcReleasePlan part1;
                         ]);
                    ])
                (StringOrder.Map.bindings value.LIR.plannedListDecHelpers);
            ] ) );
      ( "PlannedGenericDecHelpers",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple [ text key; arm64PlannedGenericDecHelper item ])
                (StringOrder.Map.bindings value.LIR.plannedGenericDecHelpers);
            ] ) );
      ( "PlannedDictDecHelpers",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      text key; ANFTestFormatting.memoryModel_rcReleasePlan item;
                    ])
                (StringOrder.Map.bindings value.LIR.plannedDictDecHelpers);
            ] ) );
      ( "DictDecHelperLabels",
        Union
          ( "set",
            [
              list text (StringOrder.Set.elements value.LIR.dictDecHelperLabels);
            ] ) );
      ("NeedsListRcIncHelper", boolean value.LIR.needsListRcIncHelper);
      ("NeedsDictRcIncHelper", boolean value.LIR.needsDictRcIncHelper);
      ("NeedsClosureRcIncHelper", boolean value.LIR.needsClosureRcIncHelper);
      ("NeedsClosureRcDecHelper", boolean value.LIR.needsClosureRcDecHelper);
      ("NeedsStreamRcDecHelper", boolean value.LIR.needsStreamRcDecHelper);
      ( "ReleasePlanSummaries",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      (fun (flag, key) ->
                        tuple [ boolean flag; rcReleasePlanMemoKey key ])
                        key;
                      arm64ReleasePlanSummary item;
                    ])
                (LIR.ReleasePlanSummaryMap.bindings
                   value.LIR.releasePlanSummaries);
            ] ) );
    ]

and arm64SlotInitRootRetainTarget (value : LIR.arm64SlotInitRootRetainTarget) =
  match value with
  | LIR.SlotInitListRootRetain ->
      union "Arm64SlotInitRootRetainTarget" "SlotInitListRootRetain" []
  | LIR.SlotInitDictRootRetain ->
      union "Arm64SlotInitRootRetainTarget" "SlotInitDictRootRetain" []
  | LIR.SlotInitDynamicBufferRetain ->
      union "Arm64SlotInitRootRetainTarget" "SlotInitDynamicBufferRetain" []
  | LIR.SlotInitClosureRootRetain ->
      union "Arm64SlotInitRootRetainTarget" "SlotInitClosureRootRetain" []
  | LIR.SlotInitGenericRootRetain field0 ->
      union "Arm64SlotInitRootRetainTarget" "SlotInitGenericRootRetain"
        [ int32 field0 ]

and functionCodegenFacts (value : LIR.functionCodegenFacts) =
  record "FunctionCodegenFacts"
    [
      ( "Arm64UsedCalleeSavedF",
        list (fun item -> physFPReg item) value.LIR.arm64UsedCalleeSavedF );
      ( "ClosurePayloadSizeFromParams",
        option (fun item -> int32 item) value.LIR.closurePayloadSizeFromParams
      );
      ( "ClosureCaptureTypes",
        option
          (fun item ->
            list (fun item -> StructuralFormat.semanticValue item) item)
          value.LIR.closureCaptureTypes );
      ( "ClosurePayloadSizesFromAllocs",
        list
          (fun item ->
            let part0, part1 = item in
            tuple [ functionId part0; int32 part1 ])
          value.LIR.closurePayloadSizesFromAllocs );
      ( "RecursiveReleaseTypes",
        Union
          ( "set",
            [
              list StructuralFormat.semanticValue
                (MemoryPlanning.SemanticTypeSet.elements
                   value.LIR.recursiveReleaseTypes);
            ] ) );
      ( "RefCountDecRequirements",
        Union
          ( "map",
            [
              list
                (fun (key, item) ->
                  tuple
                    [
                      (fun (kind, key) ->
                        tuple [ rcKind kind; rcReleasePlanMemoKey key ])
                        key;
                      option
                        (fun item ->
                          ANFTestFormatting.memoryModel_rcMetadata item)
                        item;
                    ])
                (LIR.RefCountDecRequirementMap.bindings
                   value.LIR.refCountDecRequirements);
            ] ) );
      ( "RefCountIncRequirements",
        Union
          ( "set",
            [
              list rcKind
                (LIR.RcKindSet.elements value.LIR.refCountIncRequirements);
            ] ) );
      ( "RawSlotInitTypes",
        Union
          ( "set",
            [
              list StructuralFormat.semanticValue
                (MemoryPlanning.SemanticTypeSet.elements
                   value.LIR.rawSlotInitTypes);
            ] ) );
      ( "Arm64RawSlotInitRetainTargets",
        option
          (fun item ->
            Union
              ( "map",
                [
                  list
                    (fun (key, item) ->
                      tuple
                        [
                          StructuralFormat.semanticValue key;
                          option
                            (fun item -> arm64SlotInitRootRetainTarget item)
                            item;
                        ])
                    (LIR.SemanticTypeMap.bindings item);
                ] ))
          value.LIR.arm64RawSlotInitRetainTargets );
      ("NeedsCliRuntimeState", boolean value.LIR.needsCliRuntimeState);
      ("NeedsCliArgvHelper", boolean value.LIR.needsCliArgvHelper);
      ("NeedsCliExecuteHelper", boolean value.LIR.needsCliExecuteHelper);
      ("NeedsCliRunProcessHelper", boolean value.LIR.needsCliRunProcessHelper);
      ( "NeedsCliProcessLifecycleHelpers",
        boolean value.LIR.needsCliProcessLifecycleHelpers );
      ("NeedsRuntimeErrorHelper", boolean value.LIR.needsRuntimeErrorHelper);
      ( "Arm64RcHelperRequirements",
        option
          (fun item -> arm64RcHelperRequirements item)
          value.LIR.arm64RcHelperRequirements );
      ( "Arm64GenericHelperIds",
        Union
          ( "map",
            [
              list
                (fun (key, item) -> tuple [ text key; functionId item ])
                (StringOrder.Map.bindings value.LIR.arm64GenericHelperIds);
            ] ) );
    ]

and functionDef (value : LIR.functionDef) =
  record "Function"
    [
      ("Id", functionId value.LIR.id);
      ("Name", text value.LIR.name);
      ( "TypedParams",
        list (fun item -> typedLIRParam item) value.LIR.typedParams );
      ("CFG", cfg value.LIR.cfg);
      ("StackSize", int32 value.LIR.stackSize);
      ( "UsedCalleeSaved",
        list (fun item -> physReg item) value.LIR.usedCalleeSaved );
      ( "CodegenFacts",
        option (fun item -> functionCodegenFacts item) value.LIR.codegenFacts );
    ]

and recordRegistry (value : LIR.recordRegistry) =
  Union
    ( "map",
      [
        list
          (fun (key, item) ->
            tuple
              [
                text key;
                list
                  (fun item ->
                    let part0, part1 = item in
                    tuple [ text part0; StructuralFormat.semanticValue part1 ])
                  item;
              ])
          (StringOrder.Map.bindings value);
      ] )

and variantInfo (value : LIR.variantInfo) =
  record "VariantInfo"
    [
      ("Name", text value.LIR.name);
      ("Tag", int32 value.LIR.tag);
      ( "Payload",
        option
          (fun item -> StructuralFormat.semanticValue item)
          value.LIR.payload );
      ("FieldCount", int32 value.LIR.fieldCount);
    ]

and typeVariants (value : LIR.typeVariants) =
  record "TypeVariants"
    [
      ("TypeParams", list (fun item -> text item) value.LIR.typeParams);
      ("Variants", list (fun item -> variantInfo item) value.LIR.variants);
    ]

and variantRegistry (value : LIR.variantRegistry) =
  Union
    ( "map",
      [
        list
          (fun (key, item) -> tuple [ text key; typeVariants item ])
          (StringOrder.Map.bindings value);
      ] )

and program (value : LIR.program) =
  match value with
  | LIR.Program (field0, field1, field2) ->
      union "Program" "Program"
        [
          list (fun item -> functionDef item) field0;
          variantRegistry field1;
          recordRegistry field2;
        ]
