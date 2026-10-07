(* ANFPrinter.ml - Format ANF functions and scoped or summarized dumps. *)
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

let prettyPrintFunctionName names id =
  match FunctionIdMap.tryFind id names with
  | Some name -> name
  | None -> functionId id

let tempId (ANF.TempId id) =
  StructuralFormat.format
    (StructuralFormat.Union
       ("TempId", [ StructuralFormat.Scalar (string_of_int id) ]))

(*
   Pretty-print ANF atom
*)
let prettyPrintANFAtom = function
  | ANF.UnitLiteral -> "()"
  | ANF.IntLiteral n -> ANF.sizedIntToString n
  | ANF.BoolLiteral b -> if b then "true" else "false"
  | ANF.StringLiteral s -> "\"" ^ escapeStringContent s ^ "\""
  | ANF.FloatLiteral f -> FloatFormat.roundTrip f
  | ANF.Var (ANF.TempId n) -> "t" ^ string_of_int n
  | ANF.FuncRef id -> "&" ^ functionId id

let prettyPrintANFAtomWithNames names = function
  | ANF.FuncRef id -> "&" ^ prettyPrintFunctionName names id
  | atom -> prettyPrintANFAtom atom

(*
   Pretty-print ANF binary operator
*)
let prettyPrintANFOp = function
  | ANF.Add -> "+"
  | ANF.Sub -> "-"
  | ANF.Mul -> "*"
  | ANF.Div -> "/"
  | ANF.Mod -> "%"
  | ANF.Shl -> "<<"
  | ANF.Shr -> ">>"
  | ANF.BitAnd -> "&"
  | ANF.BitOr -> "|"
  | ANF.BitXor -> "^"
  | ANF.Eq -> "=="
  | ANF.Neq -> "!="
  | ANF.Lt -> "<"
  | ANF.Gt -> ">"
  | ANF.Lte -> "<="
  | ANF.Gte -> ">="
  | ANF.And -> "&&"
  | ANF.Or -> "||"

(*
   Pretty-print ANF unary operator
*)
let prettyPrintANFUnaryOp = function
  | ANF.Neg -> "-"
  | ANF.Not -> "!"
  | ANF.BitNot -> "~~~"

let prettyPrintANFRcKind = function
  | MemoryModel.GenericHeap -> "generic"
  | MemoryModel.StreamHeap -> "stream"
  | MemoryModel.TaggedList -> "list"
  | MemoryModel.DictHeap -> "dict"
  | MemoryModel.ClosureHeap -> "closure"

let prettyPrintCanonicalBufferKind = function
  | MemoryModel.Utf8String -> "utf8-string"
  | MemoryModel.NullableUtf8String -> "nullable-utf8-string"
  | MemoryModel.NullableGraphemeCluster -> "nullable-grapheme-cluster"
  | MemoryModel.GraphemeCluster -> "grapheme-cluster"

let cliOperation = function
  | ANF.Execute -> "Execute"
  | ANF.RunProcess -> "RunProcess"
  | ANF.HostOS -> "HostOS"
  | ANF.HostArchitecture -> "HostArchitecture"
  | ANF.Hostname -> "Hostname"
  | ANF.GetEnv -> "GetEnv"
  | ANF.GetEnvironmentPacked -> "GetEnvironmentPacked"
  | ANF.SetEnv -> "SetEnv"
  | ANF.UnsetEnv -> "UnsetEnv"
  | ANF.DirectoryCurrent -> "DirectoryCurrent"
  | ANF.DirectoryListPacked -> "DirectoryListPacked"
  | ANF.FileIsDirectory -> "FileIsDirectory"
  | ANF.FileCreateExclusive -> "FileCreateExclusive"
  | ANF.GetArgv -> "GetArgv"
  | ANF.Kill -> "Kill"
  | ANF.GetPid -> "GetPid"
  | ANF.GetUid -> "GetUid"
  | ANF.CpuCount -> "CpuCount"
  | ANF.SpawnProcess -> "SpawnProcess"
  | ANF.ProcessIO -> "ProcessIO"
  | ANF.TerminateProcess -> "TerminateProcess"
  | ANF.SocketTcp4 -> "SocketTcp4"
  | ANF.SocketTcp6 -> "SocketTcp6"
  | ANF.SocketUdp4 -> "SocketUdp4"
  | ANF.SocketUdp6 -> "SocketUdp6"
  | ANF.SocketConnect4 -> "SocketConnect4"
  | ANF.SocketConnect6 -> "SocketConnect6"
  | ANF.SocketSend -> "SocketSend"
  | ANF.SocketReceive -> "SocketReceive"
  | ANF.SocketReceiveTimeout -> "SocketReceiveTimeout"
  | ANF.SocketSendTimeout -> "SocketSendTimeout"
  | ANF.SocketClose -> "SocketClose"
  | ANF.SocketBind4 -> "SocketBind4"
  | ANF.SocketListen -> "SocketListen"
  | ANF.SocketAccept -> "SocketAccept"
  | ANF.SocketCloexec -> "SocketCloexec"
  | ANF.SocketReuseAddress -> "SocketReuseAddress"
  | ANF.SocketPoll -> "SocketPoll"
  | ANF.SignalBlock -> "SignalBlock"
  | ANF.SignalRestore -> "SignalRestore"
  | ANF.SignalPending -> "SignalPending"
  | ANF.SignalWait -> "SignalWait"
  | ANF.MonotonicTime -> "MonotonicTime"
  | ANF.SecureRandomFill -> "SecureRandomFill"

(*
   Pretty-print ANF complex expression
*)
let prettyPrintANFCExpr functionNames expression =
  let prettyPrintANFAtom = prettyPrintANFAtomWithNames functionNames in
  match expression with
  | ANF.Atom atom -> prettyPrintANFAtom atom
  | ANF.TypedAtom (atom, typ) ->
      prettyPrintANFAtom atom ^ " : " ^ StructuralFormat.semanticType typ
  | ANF.Prim (op, left, right) ->
      prettyPrintANFAtom left ^ " " ^ prettyPrintANFOp op ^ " "
      ^ prettyPrintANFAtom right
  | ANF.UnaryPrim (op, operand) ->
      prettyPrintANFUnaryOp op ^ prettyPrintANFAtom operand
  | ANF.Call (funcName, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      prettyPrintFunctionName functionNames funcName ^ "(" ^ argStr ^ ")"
  | ANF.CanonicalBufferEq (kind, left, right) ->
      "CanonicalBufferEq["
      ^ prettyPrintCanonicalBufferKind kind
      ^ "](" ^ prettyPrintANFAtom left ^ ", " ^ prettyPrintANFAtom right ^ ")"
  | ANF.BorrowedCall (funcName, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "borrowed "
      ^ prettyPrintFunctionName functionNames funcName
      ^ "(" ^ argStr ^ ")"
  | ANF.IndirectCall (func, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "IndirectCall(" ^ prettyPrintANFAtom func ^ ", [" ^ argStr ^ "])"
  | ANF.ClosureAlloc (funcName, captures) ->
      let capsStr = captures |> commaSeparated prettyPrintANFAtom in
      "ClosureAlloc("
      ^ prettyPrintFunctionName functionNames funcName
      ^ ", [" ^ capsStr ^ "])"
  | ANF.ClosureCall (closure, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "ClosureCall(" ^ prettyPrintANFAtom closure ^ ", [" ^ argStr ^ "])"
  | ANF.IfValue (cond, thenAtom, elseAtom) ->
      "if " ^ prettyPrintANFAtom cond ^ " then "
      ^ prettyPrintANFAtom thenAtom
      ^ " else "
      ^ prettyPrintANFAtom elseAtom
  | ANF.TupleAlloc elems ->
      let elemsStr = elems |> commaSeparated prettyPrintANFAtom in
      "(" ^ elemsStr ^ ")"
  | ANF.TupleGet (tupleAtom, index) ->
      prettyPrintANFAtom tupleAtom ^ "." ^ string_of_int index
  | ANF.RecordAlloc (descriptor, fields) ->
      let fieldsText = fields |> commaSeparated prettyPrintANFAtom in
      "RecordAlloc(" ^ descriptor.ANF.runtimeTypeName ^ ", [" ^ fieldsText
      ^ "])"
  | ANF.RecordGet (descriptor, recordAtom, index) ->
      "RecordGet(" ^ descriptor.ANF.runtimeTypeName ^ ", "
      ^ prettyPrintANFAtom recordAtom
      ^ ", " ^ string_of_int index ^ ")"
  | ANF.RecordClone (descriptor, recordAtom, fields) ->
      let fieldsText = fields |> commaSeparated prettyPrintANFAtom in
      "RecordClone(" ^ descriptor.ANF.runtimeTypeName ^ ", "
      ^ prettyPrintANFAtom recordAtom
      ^ ", [" ^ fieldsText ^ "])"
  | ANF.RecordReuse (_, descriptor, recordAtom, fields) ->
      let fieldsText = fields |> commaSeparated prettyPrintANFAtom in
      "RecordReuse(" ^ descriptor.ANF.runtimeTypeName ^ ", "
      ^ prettyPrintANFAtom recordAtom
      ^ ", [" ^ fieldsText ^ "])"
  | ANF.RefCountInc (atom, payloadSize, kind, _) ->
      "rc_inc(" ^ prettyPrintANFAtom atom ^ ", size="
      ^ string_of_int payloadSize ^ ", kind=" ^ prettyPrintANFRcKind kind ^ ")"
  | ANF.RefCountDec (atom, payloadSize, kind, _) ->
      "rc_dec(" ^ prettyPrintANFAtom atom ^ ", size="
      ^ string_of_int payloadSize ^ ", kind=" ^ prettyPrintANFRcKind kind ^ ")"
  | ANF.StringConcat (first, second, remaining) ->
      String.concat " ++ "
        (List.map prettyPrintANFAtom (first :: second :: remaining))
  | ANF.Print (atom, valueType) ->
      "print(" ^ prettyPrintANFAtom atom ^ ", type="
      ^ StructuralFormat.semanticType valueType
      ^ ")"
  | ANF.StdoutWrite (atom, appendNewline) ->
      "stdout_write(" ^ prettyPrintANFAtom atom ^ ", newline="
      ^ (if appendNewline then "True" else "False")
      ^ ")"
  | ANF.StdinReadLine -> "stdin_read_line()"
  | ANF.RuntimeError message ->
      "runtime_error(\"" ^ escapeStringContent message ^ "\")"
  | ANF.RuntimeErrorString message ->
      "runtime_error_string(" ^ prettyPrintANFAtom message ^ ")"
  | ANF.FileReadBlob path -> "FileReadBlob(" ^ prettyPrintANFAtom path ^ ")"
  | ANF.FileExists path -> "FileExists(" ^ prettyPrintANFAtom path ^ ")"
  | ANF.FileWriteBlob (path, content) ->
      "FileWriteBlob(" ^ prettyPrintANFAtom path ^ ", "
      ^ prettyPrintANFAtom content ^ ")"
  | ANF.FileAppendText (path, content) ->
      "FileAppendText(" ^ prettyPrintANFAtom path ^ ", "
      ^ prettyPrintANFAtom content ^ ")"
  | ANF.FileDelete path -> "FileDelete(" ^ prettyPrintANFAtom path ^ ")"
  | ANF.FileCreateDirectory path ->
      "FileCreateDirectory(" ^ prettyPrintANFAtom path ^ ")"
  | ANF.FileSetExecutable path ->
      "FileSetExecutable(" ^ prettyPrintANFAtom path ^ ")"
  | ANF.FileWriteFromPtr (path, ptr, length) ->
      "FileWriteFromPtr(" ^ prettyPrintANFAtom path ^ ", "
      ^ prettyPrintANFAtom ptr ^ ", " ^ prettyPrintANFAtom length ^ ")"
  | ANF.RawAlloc numBytes -> "RawAlloc(" ^ prettyPrintANFAtom numBytes ^ ")"
  | ANF.MappedAlloc numBytes ->
      "MappedAlloc(" ^ prettyPrintANFAtom numBytes ^ ")"
  | ANF.RawFree ptr -> "RawFree(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.MappedFree ptr -> "MappedFree(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.RawGet (ptr, byteOffset, valueType) ->
      let baseText =
        "RawGet(" ^ prettyPrintANFAtom ptr ^ ", "
        ^ prettyPrintANFAtom byteOffset
        ^ ")"
      in
      appendTypeSuffix valueType baseText
  | ANF.RawTake (ptr, byteOffset, valueType) ->
      let baseText =
        "RawTake(" ^ prettyPrintANFAtom ptr ^ ", "
        ^ prettyPrintANFAtom byteOffset
        ^ ")"
      in
      appendTypeSuffix valueType baseText
  | ANF.RawGetByte (ptr, byteOffset) ->
      "RawGetByte(" ^ prettyPrintANFAtom ptr ^ ", "
      ^ prettyPrintANFAtom byteOffset
      ^ ")"
  | ANF.RawWriteWord (ptr, byteOffset, value) ->
      "RawWriteWord(" ^ prettyPrintANFAtom ptr ^ ", "
      ^ prettyPrintANFAtom byteOffset
      ^ ", " ^ prettyPrintANFAtom value ^ ")"
  | ANF.RawWriteByte (ptr, byteOffset, value) ->
      "RawWriteByte(" ^ prettyPrintANFAtom ptr ^ ", "
      ^ prettyPrintANFAtom byteOffset
      ^ ", " ^ prettyPrintANFAtom value ^ ")"
  | ANF.RawSlotInit (ptr, byteOffset, value, valueType) ->
      let baseText =
        "RawSlotInit(" ^ prettyPrintANFAtom ptr ^ ", "
        ^ prettyPrintANFAtom byteOffset
        ^ ", " ^ prettyPrintANFAtom value ^ ")"
      in
      appendTypeSuffix (Some valueType) baseText
  | ANF.StringToRawPtr value ->
      "StringToRawPtr(" ^ prettyPrintANFAtom value ^ ")"
  | ANF.RawPtrToString ptr -> "RawPtrToString(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.BlobToRawPtr value -> "BlobToRawPtr(" ^ prettyPrintANFAtom value ^ ")"
  | ANF.RawPtrToBlob ptr -> "RawPtrToBlob(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.RawPtrToInt128 ptr -> "RawPtrToInt128(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.RawPtrToUInt128 ptr -> "RawPtrToUInt128(" ^ prettyPrintANFAtom ptr ^ ")"
  | ANF.DictToRawPtr dict -> "DictToRawPtr(" ^ prettyPrintANFAtom dict ^ ")"
  | ANF.RawPtrToDict (ptr, tag, dictType) ->
      "RawPtrToDict(" ^ prettyPrintANFAtom ptr ^ ", " ^ prettyPrintANFAtom tag
      ^ ") : "
      ^ StructuralFormat.semanticType dictType
  | ANF.ListToRawPtr list -> "ListToRawPtr(" ^ prettyPrintANFAtom list ^ ")"
  | ANF.FixedBlockToRawPtr value ->
      "FixedBlockToRawPtr(" ^ prettyPrintANFAtom value ^ ")"
  | ANF.RawPtrToList (ptr, tag, listType) ->
      "RawPtrToList(" ^ prettyPrintANFAtom ptr ^ ", " ^ prettyPrintANFAtom tag
      ^ ") : "
      ^ StructuralFormat.semanticType listType
  | ANF.FloatSqrt atom -> "FloatSqrt(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.FloatAbs atom -> "FloatAbs(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.FloatNeg atom -> "FloatNeg(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.Int64ToFloat atom -> "Int64ToFloat(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.FloatToInt64 atom -> "FloatToInt64(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.FloatToBits atom -> "FloatToBits(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.FloatToString atom -> "FloatToString(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.RefCountIncString str ->
      "RefCountIncString(" ^ prettyPrintANFAtom str ^ ")"
  | ANF.RefCountDecString str ->
      "RefCountDecString(" ^ prettyPrintANFAtom str ^ ")"
  | ANF.RefCountIncInt value ->
      "RefCountIncInt(" ^ prettyPrintANFAtom value ^ ")"
  | ANF.RefCountDecInt value ->
      "RefCountDecInt(" ^ prettyPrintANFAtom value ^ ")"
  | ANF.RefCountIncBlob bytes ->
      "RefCountIncBlob(" ^ prettyPrintANFAtom bytes ^ ")"
  | ANF.RefCountDecBlob bytes ->
      "RefCountDecBlob(" ^ prettyPrintANFAtom bytes ^ ")"
  | ANF.RandomInt64 -> "RandomInt64()"
  | ANF.DateTimeNow -> "DateTimeNow()"
  | ANF.Sleep delayMs -> "Sleep(" ^ prettyPrintANFAtom delayMs ^ ")"
  | ANF.CliNative (operation, args) ->
      let argText = args |> commaSeparated prettyPrintANFAtom in
      "CliNative." ^ cliOperation operation ^ "(" ^ argText ^ ")"
  | ANF.TailCall (funcName, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "TailCall("
      ^ prettyPrintFunctionName functionNames funcName
      ^ ", [" ^ argStr ^ "])"
  | ANF.IndirectTailCall (func, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "IndirectTailCall(" ^ prettyPrintANFAtom func ^ ", [" ^ argStr ^ "])"
  | ANF.ClosureTailCall (closure, args) ->
      let argStr = args |> commaSeparated prettyPrintANFAtom in
      "ClosureTailCall(" ^ prettyPrintANFAtom closure ^ ", [" ^ argStr ^ "])"

(*
   Pretty-print ANF expression
*)
let rec prettyPrintANFExpr functionNames expression =
  let prettyPrintANFAtom = prettyPrintANFAtomWithNames functionNames in
  let recurse = prettyPrintANFExpr functionNames in
  match expression with
  | ANF.Return atom -> "return " ^ prettyPrintANFAtom atom
  | ANF.Jump (target, atom) ->
      "jump " ^ tempId target ^ "(" ^ prettyPrintANFAtom atom ^ ")"
  | ANF.Join (parameter, continuation, entry) ->
      "join " ^ tempId parameter.ANF.id ^ ": "
      ^ StructuralFormat.semanticType parameter.ANF.typ
      ^ " =\n" ^ recurse continuation ^ "\nin\n" ^ recurse entry
  | ANF.Let (var, cexpr, body) ->
      let cexprStr = prettyPrintANFCExpr functionNames cexpr in
      let bodyStr = recurse body in
      "let " ^ tempId var ^ " = " ^ cexprStr ^ "\n" ^ bodyStr
  | ANF.If (cond, thenBranch, elseBranch) ->
      let condStr = prettyPrintANFAtom cond in
      let thenStr = recurse thenBranch in
      let elseStr = recurse elseBranch in
      "if " ^ condStr ^ " then\n" ^ thenStr ^ "\nelse\n" ^ elseStr

(*
   Format ANF program in a pinned format
*)
let formatANF (ANF.Program (functions, mainExpr)) =
  let names =
    FunctionIdMap.ofList
      (List.map
         (fun (func : ANF.functionDef) -> (func.ANF.id, func.ANF.name))
         functions)
  in
  let texts =
    List.map
      (fun (func : ANF.functionDef) ->
        "Function " ^ func.ANF.name ^ ":\n"
        ^ prettyPrintANFExpr names func.ANF.body)
      functions
    |> String.concat "\n\n"
  in
  let main = prettyPrintANFExpr names mainExpr in
  if functions = [] then main else texts ^ "\n\nMain:\n" ^ main

(*
   Format one ANF function while resolving calls against the supplied names.
*)
let formatANFFunction names (func : ANF.functionDef) =
  "Function " ^ func.ANF.name ^ ":\n" ^ prettyPrintANFExpr names func.ANF.body

(*
   Format only matching ANF functions, optionally as a compact inventory.
*)
let formatANFDump filter summary (ANF.Program (functions, mainExpr)) =
  let selected =
    List.filter
      (fun (func : ANF.functionDef) -> functionNameMatches filter func.ANF.name)
      functions
  in
  match (selected, summary) with
  | [], _ when Option.is_some filter -> noFunctionMatchText filter
  | _, true ->
      String.concat "\n"
        (("Functions: " ^ string_of_int (List.length selected))
        :: List.map (fun (func : ANF.functionDef) -> func.ANF.name) selected)
  | _, false -> formatANF (ANF.Program (selected, mainExpr))
