(* LIRPrinter.ml - Format LIR instructions and scoped or summarized dumps. *)
[@@@warning "-4"]

open IRPrinting

let functionId id =
  StructuralFormat.format
    (StructuralFormat.Union
       ( "FunctionId",
         [
           StructuralFormat.Scalar
             (Printf.sprintf "%LuUL" (AST.functionIdValue id));
         ] ))

let labelText (LIR.Label text) =
  StructuralFormat.format
    (StructuralFormat.Union ("Label", [ StructuralFormat.Text text ]))

let prettyPrintFunctionName names id =
  match FunctionIdMap.tryFind id names with
  | Some name -> name
  | None -> functionId id

let runtimeList format values =
  let rec first remaining = function
    | [] -> ([], false)
    | _ :: _ when remaining = 0 -> ([], true)
    | value :: rest ->
        let values, truncated = first (remaining - 1) rest in
        (format value :: values, truncated)
  in
  let values, truncated = first 3 values in
  "[" ^ String.concat "; " values ^ (if truncated then "; ... " else "") ^ "]"

let optionType = function
  | None -> ""
  | Some typ -> "Some(" ^ StructuralFormat.semanticType typ ^ ")"

let formatVariants variants =
  runtimeList
    (fun (name, tag, typ) ->
      "(" ^ name ^ ", " ^ string_of_int tag ^ ", " ^ optionType typ ^ ")")
    variants

let formatFields fields =
  runtimeList
    (fun (name, typ) ->
      "(" ^ name ^ ", " ^ StructuralFormat.semanticType typ ^ ")")
    fields

(*
   Pretty-print LIR physical register
*)
let prettyPrintLIRPhysReg = function
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

(*
   Pretty-print LIR FP physical register
*)
let prettyPrintLIRPhysFPReg = function
  | LIR.D0 -> "D0"
  | LIR.D1 -> "D1"
  | LIR.D2 -> "D2"
  | LIR.D3 -> "D3"
  | LIR.D4 -> "D4"
  | LIR.D5 -> "D5"
  | LIR.D6 -> "D6"
  | LIR.D7 -> "D7"
  | LIR.D8 -> "D8"
  | LIR.D9 -> "D9"
  | LIR.D10 -> "D10"
  | LIR.D11 -> "D11"
  | LIR.D12 -> "D12"
  | LIR.D13 -> "D13"
  | LIR.D14 -> "D14"
  | LIR.D15 -> "D15"

let prettyPrintCondition = function
  | LIR.EQ -> "EQ"
  | LIR.NE -> "NE"
  | LIR.LT -> "LT"
  | LIR.GT -> "GT"
  | LIR.LE -> "LE"
  | LIR.GE -> "GE"
  | LIR.ULT -> "ULT"
  | LIR.UGT -> "UGT"
  | LIR.ULE -> "ULE"
  | LIR.UGE -> "UGE"

let prettyPrintCliOperation = function
  | LIR.Execute -> "Execute"
  | LIR.RunProcess -> "RunProcess"
  | LIR.HostOS -> "HostOS"
  | LIR.HostArchitecture -> "HostArchitecture"
  | LIR.Hostname -> "Hostname"
  | LIR.GetEnv -> "GetEnv"
  | LIR.GetEnvironmentPacked -> "GetEnvironmentPacked"
  | LIR.StdinState -> "StdinState"
  | LIR.SetEnv -> "SetEnv"
  | LIR.UnsetEnv -> "UnsetEnv"
  | LIR.DirectoryCurrent -> "DirectoryCurrent"
  | LIR.DirectoryListPacked -> "DirectoryListPacked"
  | LIR.FileIsDirectory -> "FileIsDirectory"
  | LIR.FileCreateExclusive -> "FileCreateExclusive"
  | LIR.GetArgv -> "GetArgv"
  | LIR.Kill -> "Kill"
  | LIR.GetPid -> "GetPid"
  | LIR.GetUid -> "GetUid"
  | LIR.CpuCount -> "CpuCount"
  | LIR.SpawnProcess -> "SpawnProcess"
  | LIR.ProcessIO -> "ProcessIO"
  | LIR.TerminateProcess -> "TerminateProcess"
  | LIR.SocketTcp4 -> "SocketTcp4"
  | LIR.SocketTcp6 -> "SocketTcp6"
  | LIR.SocketUdp4 -> "SocketUdp4"
  | LIR.SocketUdp6 -> "SocketUdp6"
  | LIR.SocketConnect4 -> "SocketConnect4"
  | LIR.SocketConnect6 -> "SocketConnect6"
  | LIR.SocketSend -> "SocketSend"
  | LIR.SocketSendTo -> "SocketSendTo"
  | LIR.SocketReceive -> "SocketReceive"
  | LIR.SocketReceiveFrom -> "SocketReceiveFrom"
  | LIR.SocketReceiveTimeout -> "SocketReceiveTimeout"
  | LIR.SocketSendTimeout -> "SocketSendTimeout"
  | LIR.SocketClose -> "SocketClose"
  | LIR.SocketBind4 -> "SocketBind4"
  | LIR.SocketBind6 -> "SocketBind6"
  | LIR.SocketListen -> "SocketListen"
  | LIR.SocketAccept -> "SocketAccept"
  | LIR.SocketCloexec -> "SocketCloexec"
  | LIR.SocketReuseAddress -> "SocketReuseAddress"
  | LIR.SocketPoll -> "SocketPoll"
  | LIR.SignalBlock -> "SignalBlock"
  | LIR.SignalRestore -> "SignalRestore"
  | LIR.SignalPending -> "SignalPending"
  | LIR.SignalWait -> "SignalWait"
  | LIR.MonotonicTime -> "MonotonicTime"
  | LIR.SecureRandomFill -> "SecureRandomFill"
  | LIR.PosixOpenAt -> "PosixOpenAt"
  | LIR.PosixRead -> "PosixRead"
  | LIR.PosixWrite -> "PosixWrite"
  | LIR.PosixClose -> "PosixClose"
  | LIR.PosixSeek -> "PosixSeek"
  | LIR.PosixStatAt -> "PosixStatAt"
  | LIR.PosixGetCwd -> "PosixGetCwd"
  | LIR.PosixChdir -> "PosixChdir"
  | LIR.PosixMkdirAt -> "PosixMkdirAt"
  | LIR.PosixUnlinkAt -> "PosixUnlinkAt"
  | LIR.PosixRenameAt -> "PosixRenameAt"
  | LIR.PosixChmodAt -> "PosixChmodAt"
  | LIR.PosixChmodAt2 -> "PosixChmodAt2"
  | LIR.PosixUtimesAt -> "PosixUtimesAt"
  | LIR.PosixSetAttributesAt -> "PosixSetAttributesAt"
  | LIR.PosixSymlinkAt -> "PosixSymlinkAt"
  | LIR.PosixReadlinkAt -> "PosixReadlinkAt"
  | LIR.PosixFlock -> "PosixFlock"
  | LIR.PosixGetDents -> "PosixGetDents"
  | LIR.PosixIoctl -> "PosixIoctl"
  | LIR.PosixProcInfo -> "PosixProcInfo"

(*
   Pretty-print LIR register
*)
let prettyPrintLIRReg = function
  | LIR.Physical reg -> prettyPrintLIRPhysReg reg
  | LIR.Virtual n -> "v" ^ string_of_int n

(*
   Pretty-print LIR FP register
*)
let prettyPrintLIRFReg = function
  | LIR.FPhysical reg -> prettyPrintLIRPhysFPReg reg
  | LIR.FVirtual n -> "fv" ^ string_of_int n

(*
   Pretty-print LIR operand
*)
let prettyPrintLIROperand = function
  | LIR.Imm n -> "Imm " ^ Int64.to_string n
  | LIR.FloatImm f -> "FloatImm " ^ FloatFormat.roundTrip f
  | LIR.Reg reg -> "Reg " ^ prettyPrintLIRReg reg
  | LIR.StackSlot n -> "Stack " ^ string_of_int n
  | LIR.StringSymbol value -> "str[" ^ escapeStringContent value ^ "]"
  | LIR.FloatSymbol value -> "float[" ^ FloatFormat.roundTrip value ^ "]"
  | LIR.FuncAddr name -> "&" ^ functionId name

let prettyPrintLIROperandWithNames names = function
  | LIR.FuncAddr id -> "&" ^ prettyPrintFunctionName names id
  | operand -> prettyPrintLIROperand operand

let prettyPrintLIRRcKind = function
  | LIR.GenericHeap -> "generic"
  | LIR.StreamHeap -> "stream"
  | LIR.TaggedList -> "list"
  | LIR.DictHeap -> "dict"
  | LIR.ClosureHeap -> "closure"

let prettyPrintCanonicalBufferKind = ANFPrinter.prettyPrintCanonicalBufferKind

(*
   Pretty-print LIR instruction
*)
let prettyPrintLIRInstr functionNames instr =
  let prettyPrintLIROperand = prettyPrintLIROperandWithNames functionNames in
  match instr with
  | LIR.Mov (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Mov(" ^ prettyPrintLIROperand src ^ ")"
  | LIR.Phi (dest, sources, _) ->
      let srcs =
        commaSeparated
          (fun (op, LIR.Label lbl) ->
            "(" ^ prettyPrintLIROperand op ^ ", " ^ lbl ^ ")")
          sources
      in
      "" ^ prettyPrintLIRReg dest ^ " <- Phi([" ^ srcs ^ "])"
  | LIR.FPhi (dest, sources) ->
      let srcs =
        commaSeparated
          (fun (freg, LIR.Label lbl) ->
            "(" ^ prettyPrintLIRFReg freg ^ ", " ^ lbl ^ ")")
          sources
      in
      prettyPrintLIRFReg dest ^ " <- FPhi([" ^ srcs ^ "])"
  | LIR.Store (offset, src) ->
      "Store(Stack " ^ string_of_int offset ^ ", " ^ prettyPrintLIRReg src ^ ")"
  | LIR.Add (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Add(" ^ prettyPrintLIRReg left ^ ", "
      ^ prettyPrintLIROperand right
      ^ ")"
  | LIR.Sub (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Sub(" ^ prettyPrintLIRReg left ^ ", "
      ^ prettyPrintLIROperand right
      ^ ")"
  | LIR.Mul (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Mul(" ^ prettyPrintLIRReg left ^ ", Reg "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.Sdiv (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Sdiv(" ^ prettyPrintLIRReg left ^ ", Reg "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.Udiv (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Udiv(" ^ prettyPrintLIRReg left ^ ", Reg "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.Msub (dest, mulLeft, mulRight, sub) ->
      prettyPrintLIRReg dest ^ " <- Msub(" ^ prettyPrintLIRReg mulLeft ^ ", "
      ^ prettyPrintLIRReg mulRight ^ ", " ^ prettyPrintLIRReg sub ^ ")"
  | LIR.Madd (dest, mulLeft, mulRight, add) ->
      prettyPrintLIRReg dest ^ " <- Madd(" ^ prettyPrintLIRReg mulLeft ^ ", "
      ^ prettyPrintLIRReg mulRight ^ ", " ^ prettyPrintLIRReg add ^ ")"
  | LIR.Select (dest, whenTrue, whenFalse, cond) ->
      prettyPrintLIRReg dest ^ " <- Select(" ^ prettyPrintCondition cond ^ ", "
      ^ prettyPrintLIRReg whenTrue ^ ", "
      ^ prettyPrintLIRReg whenFalse
      ^ ")"
  | LIR.Cmp (left, right) ->
      "Cmp(" ^ prettyPrintLIRReg left ^ ", " ^ prettyPrintLIROperand right ^ ")"
  | LIR.Cset (dest, cond) ->
      prettyPrintLIRReg dest ^ " <- Cset(" ^ prettyPrintCondition cond ^ ")"
  | LIR.And (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- And(" ^ prettyPrintLIRReg left ^ ", "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.And_imm (dest, src, imm) ->
      prettyPrintLIRReg dest ^ " <- And_imm(" ^ prettyPrintLIRReg src ^ ", #"
      ^ Int64.to_string imm ^ ")"
  | LIR.Orr (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Orr(" ^ prettyPrintLIRReg left ^ ", "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.Eor (dest, left, right) ->
      prettyPrintLIRReg dest ^ " <- Eor(" ^ prettyPrintLIRReg left ^ ", "
      ^ prettyPrintLIRReg right ^ ")"
  | LIR.Lsl (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Lsl(" ^ prettyPrintLIRReg src ^ ", "
      ^ prettyPrintLIRReg shift ^ ")"
  | LIR.Lsr (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Lsr(" ^ prettyPrintLIRReg src ^ ", "
      ^ prettyPrintLIRReg shift ^ ")"
  | LIR.Asr (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Asr(" ^ prettyPrintLIRReg src ^ ", "
      ^ prettyPrintLIRReg shift ^ ")"
  | LIR.Lsl_imm (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Lsl_imm(" ^ prettyPrintLIRReg src ^ ", #"
      ^ string_of_int shift ^ ")"
  | LIR.Lsr_imm (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Lsr_imm(" ^ prettyPrintLIRReg src ^ ", #"
      ^ string_of_int shift ^ ")"
  | LIR.Asr_imm (dest, src, shift) ->
      prettyPrintLIRReg dest ^ " <- Asr_imm(" ^ prettyPrintLIRReg src ^ ", #"
      ^ string_of_int shift ^ ")"
  | LIR.Neg (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Neg(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Mvn (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Mvn(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Sxtb (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Sxtb(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Sxth (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Sxth(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Sxtw (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Sxtw(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Uxtb (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Uxtb(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Uxth (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Uxth(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Uxtw (dest, src) ->
      prettyPrintLIRReg dest ^ " <- Uxtw(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.Call (dest, funcName, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      prettyPrintLIRReg dest ^ " <- Call("
      ^ prettyPrintFunctionName functionNames funcName
      ^ ", [" ^ argStr ^ "])"
  | LIR.TailCall (funcName, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      "TailCall("
      ^ prettyPrintFunctionName functionNames funcName
      ^ ", [" ^ argStr ^ "])"
  | LIR.IndirectCall (dest, func, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      prettyPrintLIRReg dest ^ " <- IndirectCall(" ^ prettyPrintLIRReg func
      ^ ", [" ^ argStr ^ "])"
  | LIR.IndirectTailCall (func, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      "IndirectTailCall(" ^ prettyPrintLIRReg func ^ ", [" ^ argStr ^ "])"
  | LIR.ClosureAlloc (dest, funcName, captures) ->
      let capsStr = captures |> commaSeparated prettyPrintLIROperand in
      prettyPrintLIRReg dest ^ " <- ClosureAlloc("
      ^ prettyPrintFunctionName functionNames funcName
      ^ ", [" ^ capsStr ^ "])"
  | LIR.ClosureCall (dest, closure, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      prettyPrintLIRReg dest ^ " <- ClosureCall(" ^ prettyPrintLIRReg closure
      ^ ", [" ^ argStr ^ "])"
  | LIR.ClosureTailCall (closure, args) ->
      let argStr = args |> commaSeparated prettyPrintLIROperand in
      "ClosureTailCall(" ^ prettyPrintLIRReg closure ^ ", [" ^ argStr ^ "])"
  | LIR.SaveRegs (intRegs, floatRegs) ->
      let intStr =
        String.concat ", " (List.map prettyPrintLIRPhysReg intRegs)
      in
      let floatStr =
        String.concat ", " (List.map prettyPrintLIRPhysFPReg floatRegs)
      in
      "SaveRegs([" ^ intStr ^ "], [" ^ floatStr ^ "])"
  | LIR.RestoreRegs (intRegs, floatRegs) ->
      let intStr =
        String.concat ", " (List.map prettyPrintLIRPhysReg intRegs)
      in
      let floatStr =
        String.concat ", " (List.map prettyPrintLIRPhysFPReg floatRegs)
      in
      "RestoreRegs([" ^ intStr ^ "], [" ^ floatStr ^ "])"
  | LIR.ArgMoves moves ->
      let moveStrs =
        List.map
          (fun (dest, src) ->
            prettyPrintLIRPhysReg dest ^ " <- " ^ prettyPrintLIROperand src)
          moves
      in
      "ArgMoves(" ^ String.concat ", " moveStrs ^ ")"
  | LIR.TailArgMoves moves ->
      let moveStrs =
        List.map
          (fun (dest, src) ->
            prettyPrintLIRPhysReg dest ^ " <- " ^ prettyPrintLIROperand src)
          moves
      in
      "TailArgMoves(" ^ String.concat ", " moveStrs ^ ")"
  | LIR.FArgMoves moves ->
      let moveStrs =
        List.map
          (fun (dest, src) ->
            prettyPrintLIRPhysFPReg dest ^ " <- " ^ prettyPrintLIRFReg src)
          moves
      in
      "FArgMoves(" ^ String.concat ", " moveStrs ^ ")"
  | LIR.PrintInt64 reg -> "PrintInt64(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintUInt64 reg -> "PrintUInt64(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintBool reg -> "PrintBool(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintFloat freg -> "PrintFloat(" ^ prettyPrintLIRFReg freg ^ ")"
  | LIR.PrintString value ->
      "PrintString(str[" ^ escapeStringContent value ^ "], len="
      ^ string_of_int (Array.length (Text.scalars value))
      ^ ")"
  | LIR.StdoutWrite (_, value, appendNewline) ->
      "StdoutWrite("
      ^ prettyPrintLIROperand value
      ^ ", newline="
      ^ (if appendNewline then "True" else "False")
      ^ ")"
  | LIR.StdinReadLine (_, dest) ->
      prettyPrintLIRReg dest ^ " <- StdinReadLine()"
  | LIR.RuntimeError message ->
      "RuntimeError(\"" ^ escapeStringContent message ^ "\")"
  | LIR.RuntimeErrorString reg ->
      "RuntimeErrorString(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintChars chars ->
      let s = Text.ofScalars (Array.of_list chars) in
      "PrintChars(\"" ^ escapeStringContent s ^ "\")"
  | LIR.PrintBlob reg -> "PrintBlob(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintInt64NoNewline reg ->
      "PrintIntNoNewline(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintUInt64NoNewline reg ->
      "PrintUInt64NoNewline(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintBoolNoNewline reg ->
      "PrintBoolNoNewline(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintFloatNoNewline freg ->
      "PrintFloatNoNewline(" ^ prettyPrintLIRFReg freg ^ ")"
  | LIR.PrintHeapStringNoNewline reg ->
      "PrintHeapStringNoNewline(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.PrintList (listPtr, elemType) ->
      "PrintList(" ^ prettyPrintLIRReg listPtr ^ ", "
      ^ StructuralFormat.semanticType elemType
      ^ ")"
  | LIR.PrintSum (sumPtr, variants, transparentPayload) ->
      "PrintSum(" ^ prettyPrintLIRReg sumPtr ^ ", " ^ formatVariants variants
      ^ ", transparentPayload="
      ^ (if transparentPayload then "True" else "False")
      ^ ")"
  | LIR.PrintRecord (recordPtr, typeName, fields) ->
      "PrintRecord("
      ^ prettyPrintLIRReg recordPtr
      ^ ", " ^ typeName ^ ", " ^ formatFields fields ^ ")"
  | LIR.Exit -> "Exit"
  | LIR.FMov (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- FMov(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.FLoad (dest, value) ->
      prettyPrintLIRFReg dest ^ " <- FLoad(float["
      ^ FloatFormat.roundTrip value
      ^ "])"
  | LIR.FSpillLoad (dest, stackSlot) ->
      prettyPrintLIRFReg dest ^ " <- FSpillLoad(Stack "
      ^ string_of_int stackSlot ^ ")"
  | LIR.FSpillStore (stackSlot, src) ->
      "FSpillStore(Stack " ^ string_of_int stackSlot ^ ", "
      ^ prettyPrintLIRFReg src ^ ")"
  | LIR.FAdd (dest, left, right) ->
      prettyPrintLIRFReg dest ^ " <- FAdd(" ^ prettyPrintLIRFReg left ^ ", "
      ^ prettyPrintLIRFReg right ^ ")"
  | LIR.FSub (dest, left, right) ->
      prettyPrintLIRFReg dest ^ " <- FSub(" ^ prettyPrintLIRFReg left ^ ", "
      ^ prettyPrintLIRFReg right ^ ")"
  | LIR.FMul (dest, left, right) ->
      prettyPrintLIRFReg dest ^ " <- FMul(" ^ prettyPrintLIRFReg left ^ ", "
      ^ prettyPrintLIRFReg right ^ ")"
  | LIR.FMadd (dest, left, right, addend) ->
      prettyPrintLIRFReg dest ^ " <- FMadd(" ^ prettyPrintLIRFReg left ^ ", "
      ^ prettyPrintLIRFReg right ^ ", " ^ prettyPrintLIRFReg addend ^ ")"
  | LIR.FDiv (dest, left, right) ->
      prettyPrintLIRFReg dest ^ " <- FDiv(" ^ prettyPrintLIRFReg left ^ ", "
      ^ prettyPrintLIRFReg right ^ ")"
  | LIR.FNeg (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- FNeg(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.FAbs (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- FAbs(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.FSqrt (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- FSqrt(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.FCmp (left, right) ->
      "FCmp(" ^ prettyPrintLIRFReg left ^ ", " ^ prettyPrintLIRFReg right ^ ")"
  | LIR.Int64ToFloat (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- Int64ToFloat(" ^ prettyPrintLIRReg src
      ^ ")"
  | LIR.FloatToInt64 (dest, src) ->
      prettyPrintLIRReg dest ^ " <- FloatToInt64(" ^ prettyPrintLIRFReg src
      ^ ")"
  | LIR.FloatToBits (dest, src) ->
      prettyPrintLIRReg dest ^ " <- FloatToBits(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.GpToFp (dest, src) ->
      prettyPrintLIRFReg dest ^ " <- GpToFp(" ^ prettyPrintLIRReg src ^ ")"
  | LIR.FpToGp (dest, src) ->
      prettyPrintLIRReg dest ^ " <- FpToGp(" ^ prettyPrintLIRFReg src ^ ")"
  | LIR.HeapAlloc (dest, sizeBytes) ->
      prettyPrintLIRReg dest ^ " <- HeapAlloc(" ^ string_of_int sizeBytes ^ ")"
  | LIR.HeapStore (addr, offset, src, _valueType) ->
      "HeapStore(" ^ prettyPrintLIRReg addr ^ ", " ^ string_of_int offset ^ ", "
      ^ prettyPrintLIROperand src ^ ")"
  | LIR.HeapLoad (dest, addr, offset) ->
      prettyPrintLIRReg dest ^ " <- HeapLoad(" ^ prettyPrintLIRReg addr ^ ", "
      ^ string_of_int offset ^ ")"
  | LIR.RefCountInc (addr, payloadSize, kind, _) ->
      "RefCountInc(" ^ prettyPrintLIRReg addr ^ ", " ^ string_of_int payloadSize
      ^ ", " ^ prettyPrintLIRRcKind kind ^ ")"
  | LIR.RefCountDec (addr, payloadSize, kind, _) ->
      "RefCountDec(" ^ prettyPrintLIRReg addr ^ ", " ^ string_of_int payloadSize
      ^ ", " ^ prettyPrintLIRRcKind kind ^ ")"
  | LIR.StringConcat (dest, first, second, remaining) ->
      let operands =
        commaSeparated prettyPrintLIROperand (first :: second :: remaining)
      in
      prettyPrintLIRReg dest ^ " <- StringConcat(" ^ operands ^ ")"
  | LIR.CanonicalBufferEq (dest, kind, left, right) ->
      prettyPrintLIRReg dest ^ " <- CanonicalBufferEq["
      ^ prettyPrintCanonicalBufferKind kind
      ^ "](" ^ prettyPrintLIROperand left ^ ", "
      ^ prettyPrintLIROperand right
      ^ ")"
  | LIR.PrintHeapString reg -> "PrintHeapString(" ^ prettyPrintLIRReg reg ^ ")"
  | LIR.LoadFuncAddr (dest, funcName) ->
      prettyPrintLIRReg dest ^ " <- LoadFuncAddr(" ^ functionId funcName ^ ")"
  | LIR.FileReadBlob (dest, path) ->
      prettyPrintLIRReg dest ^ " <- FileReadBlob(" ^ prettyPrintLIROperand path
      ^ ")"
  | LIR.FileExists (dest, path) ->
      prettyPrintLIRReg dest ^ " <- FileExists(" ^ prettyPrintLIROperand path
      ^ ")"
  | LIR.FileWriteBlob (dest, path, content) ->
      prettyPrintLIRReg dest ^ " <- FileWriteBlob(" ^ prettyPrintLIROperand path
      ^ ", "
      ^ prettyPrintLIROperand content
      ^ ")"
  | LIR.FileAppendText (dest, path, content) ->
      prettyPrintLIRReg dest ^ " <- FileAppendText("
      ^ prettyPrintLIROperand path ^ ", "
      ^ prettyPrintLIROperand content
      ^ ")"
  | LIR.FileDelete (dest, path) ->
      prettyPrintLIRReg dest ^ " <- FileDelete(" ^ prettyPrintLIROperand path
      ^ ")"
  | LIR.FileCreateDirectory (dest, path) ->
      prettyPrintLIRReg dest ^ " <- FileCreateDirectory("
      ^ prettyPrintLIROperand path ^ ")"
  | LIR.FileSetExecutable (dest, path) ->
      prettyPrintLIRReg dest ^ " <- FileSetExecutable("
      ^ prettyPrintLIROperand path ^ ")"
  | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
      prettyPrintLIRReg dest ^ " <- FileWriteFromPtr("
      ^ prettyPrintLIROperand path ^ ", " ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg length ^ ")"
  | LIR.RawAlloc (dest, numBytes) ->
      prettyPrintLIRReg dest ^ " <- RawAlloc(" ^ prettyPrintLIRReg numBytes
      ^ ")"
  | LIR.MappedAlloc (dest, numBytes) ->
      prettyPrintLIRReg dest ^ " <- MappedAlloc(" ^ prettyPrintLIRReg numBytes
      ^ ")"
  | LIR.RawFree ptr -> "RawFree(" ^ prettyPrintLIRReg ptr ^ ")"
  | LIR.MappedFree ptr -> "MappedFree(" ^ prettyPrintLIRReg ptr ^ ")"
  | LIR.RawGet (dest, ptr, byteOffset) ->
      prettyPrintLIRReg dest ^ " <- RawGet(" ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg byteOffset
      ^ ")"
  | LIR.RawGetByte (dest, ptr, byteOffset) ->
      prettyPrintLIRReg dest ^ " <- RawGetByte(" ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg byteOffset
      ^ ")"
  | LIR.RawWriteWord (ptr, byteOffset, value) ->
      "RawWriteWord(" ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg byteOffset
      ^ ", " ^ prettyPrintLIRReg value ^ ")"
  | LIR.RawWriteByte (ptr, byteOffset, value) ->
      "RawWriteByte(" ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg byteOffset
      ^ ", " ^ prettyPrintLIRReg value ^ ")"
  | LIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
      "RawSlotInit(" ^ prettyPrintLIRReg ptr ^ ", "
      ^ prettyPrintLIRReg byteOffset
      ^ ", " ^ prettyPrintLIRReg value ^ ") : "
      ^ StructuralFormat.semanticType valueType
  | LIR.RefCountIncString str ->
      "RefCountIncString(" ^ prettyPrintLIROperand str ^ ")"
  | LIR.RefCountDecString str ->
      "RefCountDecString(" ^ prettyPrintLIROperand str ^ ")"
  | LIR.RefCountIncInt value ->
      "RefCountIncInt(" ^ prettyPrintLIROperand value ^ ")"
  | LIR.RefCountDecInt value ->
      "RefCountDecInt(" ^ prettyPrintLIROperand value ^ ")"
  | LIR.RefCountIncBlob bytes ->
      "RefCountIncBlob(" ^ prettyPrintLIROperand bytes ^ ")"
  | LIR.RefCountDecBlob bytes ->
      "RefCountDecBlob(" ^ prettyPrintLIROperand bytes ^ ")"
  | LIR.RandomInt64 dest -> prettyPrintLIRReg dest ^ " <- RandomInt64()"
  | LIR.DateTimeNow dest -> prettyPrintLIRReg dest ^ " <- DateTimeNow()"
  | LIR.Sleep (effectId, delayMs) ->
      "Sleep#" ^ string_of_int effectId ^ "(" ^ prettyPrintLIRFReg delayMs ^ ")"
  | LIR.CliNative (dest, operation, args) ->
      let argText = args |> commaSeparated prettyPrintLIROperand in
      prettyPrintLIRReg dest ^ " <- CliNative."
      ^ prettyPrintCliOperation operation
      ^ "(" ^ argText ^ ")"
  | LIR.FloatToString (dest, value) ->
      prettyPrintLIRReg dest ^ " <- FloatToString(" ^ prettyPrintLIRFReg value
      ^ ")"
  | LIR.CoverageHit exprId -> "CoverageHit(" ^ string_of_int exprId ^ ")"

(*
   Pretty-print symbolic LIR terminator
*)
let prettyPrintLIRTerminator term =
  match term with
  | LIR.Ret -> "Ret"
  | LIR.Branch (cond, trueLabel, falseLabel) ->
      "Branch(" ^ prettyPrintLIRReg cond ^ ", " ^ labelText trueLabel ^ ", "
      ^ labelText falseLabel ^ ")"
  | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) ->
      "BranchZero(" ^ prettyPrintLIRReg cond ^ ", " ^ labelText zeroLabel ^ ", "
      ^ labelText nonZeroLabel ^ ")"
  | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
      "BranchBitZero(" ^ prettyPrintLIRReg reg ^ ", #" ^ string_of_int bit
      ^ ", " ^ labelText zeroLabel ^ ", " ^ labelText nonZeroLabel ^ ")"
  | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
      "BranchBitNonZero(" ^ prettyPrintLIRReg reg ^ ", #" ^ string_of_int bit
      ^ ", " ^ labelText nonZeroLabel ^ ", " ^ labelText zeroLabel ^ ")"
  | LIR.CondBranch (cond, trueLabel, falseLabel) ->
      "CondBranch(" ^ prettyPrintCondition cond ^ ", " ^ labelText trueLabel
      ^ ", " ^ labelText falseLabel ^ ")"
  | LIR.Jump label -> "Jump(" ^ labelText label ^ ")"

(*
   Format symbolic LIR program with CFG structure
*)
let formatLIR (LIR.Program (functions, _, _)) =
  let names =
    FunctionIdMap.ofList
      (List.map
         (fun (func : LIR.functionDef) -> (func.LIR.id, func.LIR.name))
         functions)
  in
  List.map
    (fun (func : LIR.functionDef) ->
      let blocks =
        LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks
        |> List.map (fun (label, block) ->
            let instructions =
              List.map
                (fun instr -> "    " ^ prettyPrintLIRInstr names instr)
                block.LIR.instrs
              |> String.concat "\n"
            in
            "  " ^ labelText label ^ ":\n" ^ instructions ^ "\n    "
            ^ prettyPrintLIRTerminator block.LIR.terminator)
        |> String.concat "\n"
      in
      let calleeSaved =
        String.concat ", "
          (List.map prettyPrintLIRPhysReg func.LIR.usedCalleeSaved)
      in
      func.LIR.name ^ ":\n  StackSize: "
      ^ string_of_int func.LIR.stackSize
      ^ "\n  UsedCalleeSaved: [" ^ calleeSaved ^ "]\n" ^ blocks)
    functions
  |> String.concat "\n\n"

(*
   Format only matching LIR functions, optionally as block/instruction counts.
*)
let formatLIRDump filter summary (LIR.Program (functions, variants, records)) =
  let selected =
    List.filter
      (fun (func : LIR.functionDef) -> functionNameMatches filter func.LIR.name)
      functions
  in
  match (selected, summary) with
  | [], _ when Option.is_some filter -> noFunctionMatchText filter
  | _, true ->
      let lines =
        List.map
          (fun (func : LIR.functionDef) ->
            let instructionCount =
              LIR.LabelMap.fold
                (fun _ block total ->
                  Int32.add total (Int32.of_int (List.length block.LIR.instrs)))
                func.LIR.cfg.LIR.blocks 0l
            in
            func.LIR.name ^ ": "
            ^ string_of_int (LIR.LabelMap.cardinal func.LIR.cfg.LIR.blocks)
            ^ " blocks, "
            ^ Int32.to_string instructionCount
            ^ " instructions")
          selected
      in
      String.concat "\n"
        (("Functions: " ^ string_of_int (List.length selected)) :: lines)
  | _, false -> formatLIR (LIR.Program (selected, variants, records))
