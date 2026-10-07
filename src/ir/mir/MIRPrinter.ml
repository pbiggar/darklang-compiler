(* MIRPrinter.ml - Format MIR graphs and scoped or summarized dumps. *)
[@@@warning "-4"]
open IRPrinting
let functionId id = StructuralFormat.format (StructuralFormat.Union ("FunctionId", [StructuralFormat.Scalar (Printf.sprintf "%LuUL" (AST.functionIdValue id))]))
let prettyPrintFunctionName names id = match FunctionIdMap.tryFind id names with Some name -> name | None -> functionId id
(*
   Pretty-print MIR operand
*)
let prettyPrintMIROperand = function
 | MIR.Int64Const n -> Int64.to_string n
 | MIR.BoolConst b -> if b then "true" else "false"
 | MIR.FloatSymbol value -> "float[" ^ FloatFormat.roundTrip value ^ "]"
 | MIR.StringSymbol value -> "str[" ^ escapeStringContent value ^ "]"
 | MIR.Register (MIR.VReg n) -> "v" ^ string_of_int n
 | MIR.FuncAddr id -> "&" ^ functionId id
let prettyPrintMIROperandWithNames names = function MIR.FuncAddr id -> "&" ^ prettyPrintFunctionName names id | operand -> prettyPrintMIROperand operand
(*
   Pretty-print MIR operator
*)
let prettyPrintMIROp = function
 | MIR.Add -> "+"
 | MIR.Sub -> "-"
 | MIR.Mul -> "*"
 | MIR.Div -> "/"
 | MIR.Mod -> "%"
 | MIR.Shl -> "<<"
 | MIR.Shr -> ">>"
 | MIR.BitAnd -> "&"
 | MIR.BitOr -> "|"
 | MIR.BitXor -> "^"
 | MIR.Eq -> "=="
 | MIR.Neq -> "!="
 | MIR.Lt -> "<"
 | MIR.Gt -> ">"
 | MIR.Lte -> "<="
 | MIR.Gte -> ">="
 | MIR.And -> "&&"
 | MIR.Or -> "||"
(*
   Pretty-print MIR unary operator
*)
let prettyPrintMIRUnaryOp = function
 | MIR.Neg -> "-"
 | MIR.Not -> "!"
 | MIR.BitNot -> "~~~"
let prettyPrintMIRRcKind = function
 | MIR.GenericHeap -> "generic"
 | MIR.StreamHeap -> "stream"
 | MIR.TaggedList -> "list"
 | MIR.DictHeap -> "dict"
 | MIR.ClosureHeap -> "closure"
(*
   Pretty-print MIR virtual register
*)
let prettyPrintMIRVReg (MIR.VReg n) = "v" ^ string_of_int n
(*
   Pretty-print MIR label
*)
let prettyPrintMIRLabel (MIR.Label name) = name
let cliOperation = function
 | MIR.Execute -> "Execute"
 | MIR.RunProcess -> "RunProcess"
 | MIR.HostOS -> "HostOS"
 | MIR.HostArchitecture -> "HostArchitecture"
 | MIR.Hostname -> "Hostname"
 | MIR.GetEnv -> "GetEnv"
 | MIR.GetEnvironmentPacked -> "GetEnvironmentPacked"
 | MIR.SetEnv -> "SetEnv"
 | MIR.UnsetEnv -> "UnsetEnv"
 | MIR.DirectoryCurrent -> "DirectoryCurrent"
 | MIR.DirectoryListPacked -> "DirectoryListPacked"
 | MIR.FileIsDirectory -> "FileIsDirectory"
 | MIR.FileCreateExclusive -> "FileCreateExclusive"
 | MIR.GetArgv -> "GetArgv"
 | MIR.Kill -> "Kill"
 | MIR.GetPid -> "GetPid"
 | MIR.GetUid -> "GetUid"
 | MIR.CpuCount -> "CpuCount"
 | MIR.SpawnProcess -> "SpawnProcess"
 | MIR.ProcessIO -> "ProcessIO"
 | MIR.TerminateProcess -> "TerminateProcess"
 | MIR.SocketTcp4 -> "SocketTcp4"
 | MIR.SocketTcp6 -> "SocketTcp6"
 | MIR.SocketUdp4 -> "SocketUdp4"
 | MIR.SocketUdp6 -> "SocketUdp6"
 | MIR.SocketConnect4 -> "SocketConnect4"
 | MIR.SocketConnect6 -> "SocketConnect6"
 | MIR.SocketSend -> "SocketSend"
 | MIR.SocketReceive -> "SocketReceive"
 | MIR.SocketReceiveTimeout -> "SocketReceiveTimeout"
 | MIR.SocketSendTimeout -> "SocketSendTimeout"
 | MIR.SocketClose -> "SocketClose"
 | MIR.SocketBind4 -> "SocketBind4"
 | MIR.SocketListen -> "SocketListen"
 | MIR.SocketAccept -> "SocketAccept"
 | MIR.SocketCloexec -> "SocketCloexec"
 | MIR.SocketReuseAddress -> "SocketReuseAddress"
 | MIR.SocketPoll -> "SocketPoll"
 | MIR.SignalBlock -> "SignalBlock"
 | MIR.SignalRestore -> "SignalRestore"
 | MIR.SignalPending -> "SignalPending"
 | MIR.SignalWait -> "SignalWait"
 | MIR.MonotonicTime -> "MonotonicTime"
 | MIR.SecureRandomFill -> "SecureRandomFill"
let prettyPrintCanonicalBufferKind = ANFPrinter.prettyPrintCanonicalBufferKind
(*
   Pretty-print MIR instruction
*)
let prettyPrintMIRInstr functionNames instr =
 let prettyPrintMIROperand = prettyPrintMIROperandWithNames functionNames in
 match instr with
 | MIR.Mov (dest, src, valueType) -> let baseText = (prettyPrintMIRVReg dest) ^ " <- " ^ (prettyPrintMIROperand src) in appendTypeSuffix valueType baseText
 | MIR.BinOp (dest, op, left, right, operandType) -> (prettyPrintMIRVReg dest) ^ " <- " ^ (prettyPrintMIROperand left) ^ " " ^ (prettyPrintMIROp op) ^ " " ^ (prettyPrintMIROperand right) ^ " : " ^ (StructuralFormat.semanticType operandType)
 | MIR.UnaryOp (dest, op, src) -> (prettyPrintMIRVReg dest) ^ " <- " ^ (prettyPrintMIRUnaryOp op) ^ (prettyPrintMIROperand src)
 | MIR.Call (dest, funcName, args, _, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- Call(" ^ (prettyPrintFunctionName functionNames funcName) ^ ", [" ^ (argStr) ^ "])"
 | MIR.CanonicalBufferEq (dest, kind, left, right) -> (prettyPrintMIRVReg dest) ^ " <- CanonicalBufferEq[" ^ (prettyPrintCanonicalBufferKind kind) ^ "](" ^ (prettyPrintMIROperand left) ^ ", " ^ (prettyPrintMIROperand right) ^ ")"
 | MIR.TailCall (funcName, args, _, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in "TailCall(" ^ (prettyPrintFunctionName functionNames funcName) ^ ", [" ^ (argStr) ^ "])"
 | MIR.IndirectCall (dest, func, args, _, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- IndirectCall(" ^ (prettyPrintMIROperand func) ^ ", [" ^ (argStr) ^ "])"
 | MIR.IndirectTailCall (func, args, _, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in "IndirectTailCall(" ^ (prettyPrintMIROperand func) ^ ", [" ^ (argStr) ^ "])"
 | MIR.ClosureAlloc (dest, funcName, captures) -> let capsStr = captures |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- ClosureAlloc(" ^ (prettyPrintFunctionName functionNames funcName) ^ ", [" ^ (capsStr) ^ "])"
 | MIR.ClosureCall (dest, closure, args, _, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- ClosureCall(" ^ (prettyPrintMIROperand closure) ^ ", [" ^ (argStr) ^ "])"
 | MIR.ClosureTailCall (closure, args, _) -> let argStr = args |> commaSeparated prettyPrintMIROperand in "ClosureTailCall(" ^ (prettyPrintMIROperand closure) ^ ", [" ^ (argStr) ^ "])"
 | MIR.HeapAlloc (dest, sizeBytes) -> (prettyPrintMIRVReg dest) ^ " <- HeapAlloc(" ^ (string_of_int sizeBytes) ^ ")"
 | MIR.HeapStore (addr, offset, src, valueType) -> let baseText = "HeapStore(" ^ (prettyPrintMIRVReg addr) ^ ", " ^ (string_of_int offset) ^ ", " ^ (prettyPrintMIROperand src) ^ ")" in appendTypeSuffix valueType baseText
 | MIR.HeapLoad (dest, addr, offset, valueType) -> let baseText = (prettyPrintMIRVReg dest) ^ " <- HeapLoad(" ^ (prettyPrintMIRVReg addr) ^ ", " ^ (string_of_int offset) ^ ")" in appendTypeSuffix valueType baseText
 | MIR.StringConcat (dest, first, second, remaining) -> let operands = first :: second :: remaining |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- StringConcat(" ^ (operands) ^ ")"
 | MIR.RefCountInc (addr, payloadSize, kind, _) -> "RefCountInc(" ^ (prettyPrintMIRVReg addr) ^ ", size=" ^ (string_of_int payloadSize) ^ ", kind=" ^ (prettyPrintMIRRcKind kind) ^ ")"
 | MIR.RefCountDec (addr, payloadSize, kind, _) -> "RefCountDec(" ^ (prettyPrintMIRVReg addr) ^ ", size=" ^ (string_of_int payloadSize) ^ ", kind=" ^ (prettyPrintMIRRcKind kind) ^ ")"
 | MIR.Print (src, valueType) -> "Print(" ^ (prettyPrintMIROperand src) ^ ", type=" ^ (StructuralFormat.semanticType valueType) ^ ")"
 | MIR.StdoutWrite (_, src, appendNewline) -> "StdoutWrite(" ^ (prettyPrintMIROperand src) ^ ", newline=" ^ (if appendNewline then "True" else "False") ^ ")"
 | MIR.StdinReadLine dest -> (prettyPrintMIRVReg dest) ^ " <- StdinReadLine()"
 | MIR.RuntimeError message -> "RuntimeError(\"" ^ (escapeStringContent message) ^ "\")"
 | MIR.RuntimeErrorString message -> "RuntimeErrorString(" ^ (prettyPrintMIROperand message) ^ ")"
 | MIR.FileReadBlob (dest, path) -> (prettyPrintMIRVReg dest) ^ " <- FileReadBlob(" ^ (prettyPrintMIROperand path) ^ ")"
 | MIR.FileExists (dest, path) -> (prettyPrintMIRVReg dest) ^ " <- FileExists(" ^ (prettyPrintMIROperand path) ^ ")"
 | MIR.FileWriteBlob (dest, path, content) -> (prettyPrintMIRVReg dest) ^ " <- FileWriteBlob(" ^ (prettyPrintMIROperand path) ^ ", " ^ (prettyPrintMIROperand content) ^ ")"
 | MIR.FileAppendText (dest, path, content) -> (prettyPrintMIRVReg dest) ^ " <- FileAppendText(" ^ (prettyPrintMIROperand path) ^ ", " ^ (prettyPrintMIROperand content) ^ ")"
 | MIR.FileDelete (dest, path) -> (prettyPrintMIRVReg dest) ^ " <- FileDelete(" ^ (prettyPrintMIROperand path) ^ ")"
 | MIR.FileCreateDirectory (dest, path) -> (prettyPrintMIRVReg dest) ^ " <- FileCreateDirectory(" ^ (prettyPrintMIROperand path) ^ ")"
 | MIR.FileSetExecutable (dest, path) -> (prettyPrintMIRVReg dest) ^ " <- FileSetExecutable(" ^ (prettyPrintMIROperand path) ^ ")"
 | MIR.FileWriteFromPtr (dest, path, ptr, length) -> (prettyPrintMIRVReg dest) ^ " <- FileWriteFromPtr(" ^ (prettyPrintMIROperand path) ^ ", " ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand length) ^ ")"
 | MIR.FloatSqrt (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- FloatSqrt(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.FloatAbs (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- FloatAbs(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.FloatNeg (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- FloatNeg(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.Int64ToFloat (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- Int64ToFloat(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.FloatToInt64 (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- FloatToInt64(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.FloatToBits (dest, src) -> (prettyPrintMIRVReg dest) ^ " <- FloatToBits(" ^ (prettyPrintMIROperand src) ^ ")"
 | MIR.RawAlloc (dest, numBytes) -> (prettyPrintMIRVReg dest) ^ " <- RawAlloc(" ^ (prettyPrintMIROperand numBytes) ^ ")"
 | MIR.MappedAlloc (dest, numBytes) -> (prettyPrintMIRVReg dest) ^ " <- MappedAlloc(" ^ (prettyPrintMIROperand numBytes) ^ ")"
 | MIR.RawFree ptr -> "RawFree(" ^ (prettyPrintMIROperand ptr) ^ ")"
 | MIR.MappedFree ptr -> "MappedFree(" ^ (prettyPrintMIROperand ptr) ^ ")"
 | MIR.RawGet (dest, ptr, byteOffset, valueType) -> let baseText = (prettyPrintMIRVReg dest) ^ " <- RawGet(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand byteOffset) ^ ")" in appendTypeSuffix valueType baseText
 | MIR.RawGetByte (dest, ptr, byteOffset) -> (prettyPrintMIRVReg dest) ^ " <- RawGetByte(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand byteOffset) ^ ")"
 | MIR.RawWriteWord (ptr, byteOffset, value) -> "RawWriteWord(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand byteOffset) ^ ", " ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RawWriteByte (ptr, byteOffset, value) -> "RawWriteByte(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand byteOffset) ^ ", " ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RawSlotInit (ptr, byteOffset, value, valueType) -> let baseText = "RawSlotInit(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand byteOffset) ^ ", " ^ (prettyPrintMIROperand value) ^ ")" in appendTypeSuffix (Some valueType) baseText
 | MIR.StringToRawPtr (dest, value) -> (prettyPrintMIRVReg dest) ^ " <- StringToRawPtr(" ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RawPtrToString (dest, ptr) -> (prettyPrintMIRVReg dest) ^ " <- RawPtrToString(" ^ (prettyPrintMIROperand ptr) ^ ")"
 | MIR.BlobToRawPtr (dest, value) -> (prettyPrintMIRVReg dest) ^ " <- BlobToRawPtr(" ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RawPtrToBlob (dest, ptr) -> (prettyPrintMIRVReg dest) ^ " <- RawPtrToBlob(" ^ (prettyPrintMIROperand ptr) ^ ")"
 | MIR.DictToRawPtr (dest, dict) -> (prettyPrintMIRVReg dest) ^ " <- DictToRawPtr(" ^ (prettyPrintMIROperand dict) ^ ")"
 | MIR.RawPtrToDict (dest, ptr, tag) -> (prettyPrintMIRVReg dest) ^ " <- RawPtrToDict(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand tag) ^ ")"
 | MIR.ListToRawPtr (dest, list) -> (prettyPrintMIRVReg dest) ^ " <- ListToRawPtr(" ^ (prettyPrintMIROperand list) ^ ")"
 | MIR.RawPtrToList (dest, ptr, tag) -> (prettyPrintMIRVReg dest) ^ " <- RawPtrToList(" ^ (prettyPrintMIROperand ptr) ^ ", " ^ (prettyPrintMIROperand tag) ^ ")"
 | MIR.RefCountIncString str -> "RefCountIncString(" ^ (prettyPrintMIROperand str) ^ ")"
 | MIR.RefCountDecString str -> "RefCountDecString(" ^ (prettyPrintMIROperand str) ^ ")"
 | MIR.RefCountIncInt value -> "RefCountIncInt(" ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RefCountDecInt value -> "RefCountDecInt(" ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.RefCountIncBlob bytes -> "RefCountIncBlob(" ^ (prettyPrintMIROperand bytes) ^ ")"
 | MIR.RefCountDecBlob bytes -> "RefCountDecBlob(" ^ (prettyPrintMIROperand bytes) ^ ")"
 | MIR.RandomInt64 dest -> (prettyPrintMIRVReg dest) ^ " <- RandomInt64()"
 | MIR.DateTimeNow dest -> (prettyPrintMIRVReg dest) ^ " <- DateTimeNow()"
 | MIR.Sleep (effectId, dest, delayMs) -> (prettyPrintMIRVReg dest) ^ " <- Sleep#" ^ (string_of_int effectId) ^ "(" ^ (prettyPrintMIROperand delayMs) ^ ")"
 | MIR.CliNative (dest, operation, args) -> let argText = args |> commaSeparated prettyPrintMIROperand in (prettyPrintMIRVReg dest) ^ " <- CliNative." ^ (cliOperation operation) ^ "(" ^ (argText) ^ ")"
 | MIR.FloatToString (dest, value) -> (prettyPrintMIRVReg dest) ^ " <- FloatToString(" ^ (prettyPrintMIROperand value) ^ ")"
 | MIR.Phi (dest, sources, valueType) -> let srcStrs = commaSeparated (fun (operand, label) -> "(" ^ prettyPrintMIROperand operand ^ ", " ^ prettyPrintMIRLabel label ^ ")") sources in let baseText = prettyPrintMIRVReg dest ^ " <- Phi([" ^ srcStrs ^ "])" in appendTypeSuffix valueType baseText
 | MIR.CoverageHit exprId -> "CoverageHit(" ^ (string_of_int exprId) ^ ")"
(*
   Pretty-print MIR terminator
*)
let prettyPrintMIRTerminator functionNames term =
 let prettyPrintMIROperand = prettyPrintMIROperandWithNames functionNames in
 match term with
 | MIR.Ret operand -> "ret " ^ (prettyPrintMIROperand operand)
 | MIR.Branch (cond, trueLabel, falseLabel) -> "branch " ^ (prettyPrintMIROperand cond) ^ " ? " ^ (prettyPrintMIRLabel trueLabel) ^ " : " ^ (prettyPrintMIRLabel falseLabel)
 | MIR.Jump label -> "jump " ^ (prettyPrintMIRLabel label)
(*
   Format MIR program with CFG structure and names for external call targets.
*)
let formatMIRWithFunctionNames externalNames (MIR.Program (functions, _, _)) =
 let names = List.fold_left (fun names (func : MIR.functionDef) -> FunctionIdMap.add func.MIR.id func.MIR.name names) externalNames functions in
 let prettyBlock (block : MIR.basicBlock) = let label = "  " ^ prettyPrintMIRLabel block.MIR.label ^ ":" in let instructions = List.map (fun instr -> "    " ^ prettyPrintMIRInstr names instr) block.MIR.instrs in let terminator = "    " ^ prettyPrintMIRTerminator names block.MIR.terminator in String.concat "\n" (label :: (instructions @ [terminator])) in
 let prettyFunction (func : MIR.functionDef) =
  let entry = Option.map (fun block -> func.MIR.cfg.MIR.entry, block) (MIR.LabelMap.find_opt func.MIR.cfg.MIR.entry func.MIR.cfg.MIR.blocks) in
  let others = MIR.LabelMap.remove func.MIR.cfg.MIR.entry func.MIR.cfg.MIR.blocks |> MIR.LabelMap.bindings |> List.sort (fun (a, _) (b, _) -> StringOrder.compare (prettyPrintMIRLabel a) (prettyPrintMIRLabel b)) in
  let blocks = (match entry with Some block -> block :: others | None -> others) |> List.map (fun (_, block) -> prettyBlock block) |> String.concat "\n" in
  "Function " ^ func.MIR.name ^ ":\n" ^ (if blocks = "" then "  <empty>" else blocks) in
 List.map prettyFunction functions |> String.concat "\n\n"
let formatMIR program = formatMIRWithFunctionNames FunctionIdMap.empty program
(*
   Format only matching MIR functions, optionally as block/instruction counts.
*)
let formatMIRDump filter summary (MIR.Program (functions, variants, records)) =
 let selected = List.filter (fun (func : MIR.functionDef) -> functionNameMatches filter func.MIR.name) functions in
 match selected, summary with [], _ when Option.is_some filter -> noFunctionMatchText filter | _, true ->
  let lines = List.map (fun (func : MIR.functionDef) -> let count = MIR.LabelMap.fold (fun _ block count -> Int32.to_int (Int32.add (Int32.of_int count) (Int32.of_int (List.length block.MIR.instrs)))) func.MIR.cfg.MIR.blocks 0 in func.MIR.name ^ ": " ^ string_of_int (MIR.LabelMap.cardinal func.MIR.cfg.MIR.blocks) ^ " blocks, " ^ string_of_int count ^ " instructions") selected in String.concat "\n" (("Functions: " ^ string_of_int (List.length selected)) :: lines)
 | _, false -> formatMIR (MIR.Program (selected, variants, records))
