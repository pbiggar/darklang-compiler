(*
   Primitives.fs - Resolve intrinsic calls and primitive source representations.
*)
[@@@warning "-4"]
module M = StringOrder.Map
module S = StringOrder.Set
module C = CheckedAST
(*
   Variant lookup - maps variant names to (type name, type params, tag index, field types)
*)
type variantLookup = (string * string list * int * AST.semanticType list) M.t
(*
   Qualified sum cases grouped once when conversion registries are built.
*)
type sumCase = {typeParams : string list; tag : int; fields : AST.semanticType list}
type sumRepresentationIndex = sumCase M.t M.t
type sumMetadata = {names : S.t; cases : sumRepresentationIndex}
let eqHelperDispatchMarker = "__dark_internal_eq_helper_dispatch"
let materializeFunctionComparisonPlan left right = function
 | [a; b] -> C.Let (C.LPVariable left, a, C.Let (C.LPVariable right, b,
 C.If (C.BinOp (AST.Eq, C.TupleAccess (C.Local left, 1), C.TupleAccess (C.Local right, 1)),
 C.IndirectApply (C.TupleAccess (C.Local left, 1), NonEmptyList.fromList [C.Local left; C.Local right]), C.BoolLiteral false)))
 | _ -> Crash.crash "Function comparison plan expected exactly two operands"
let canonicalBufferKindForType = function AST.TString -> Some MemoryModel.Utf8String | AST.TChar -> Some MemoryModel.GraphemeCluster | _ -> None
let materializeComparisonPlan resolve target = function
 | [a; b] -> (match target with
   | AST.TList _ | AST.TDict _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> C.Call (resolve (ComparisonPlanning.eqHelperName target), NonEmptyList.fromList [a; b])
   | AST.TFunction _ -> Crash.crash "Function comparison bindings must be allocated from the checked symbol table"
   | AST.TInt -> C.Call (resolve "Darklang.Stdlib.Int.__equals", NonEmptyList.fromList [a; b])
   | _ -> C.BinOp (AST.Eq, a, b))
 | _ -> Crash.crash "Comparison plan expected exactly two operands"
let sumRepresentationIndex variants = M.fold (fun name (owner, typeParams, tag, fields) index ->
 if String.starts_with ~prefix:(owner ^ ".") name then
 let cases = Option.value (M.find_opt owner index) ~default:M.empty in M.add owner (M.add name {typeParams; tag; fields} cases) index else index) variants M.empty
let mergeSumCaseIndexes base overlay = M.fold (fun owner cases combined ->
 let existing = Option.value (M.find_opt owner combined) ~default:M.empty in
 M.add owner (M.fold M.add cases existing) combined) overlay base
(*
   Every constructible source value has a one-word native root. A unary
   single-case sum can share that root without reserving a tag or sentinel.
*)
let canUseTransparentPayload = MemoryPlanning.canUseTransparentSumPayload
let rec substituteTransparentPayloadType subst typ =
 let recurse = substituteTransparentPayloadType subst in match typ with
 | AST.TVar name -> (match M.find_opt name subst with Some value -> value | None -> Crash.crash ("Transparent sum payload variable '" ^ name ^ "' is not declared"))
 | AST.TTuple values -> AST.TTuple (List.map recurse values)
 | AST.TRecord (name, args) -> AST.TRecord (name, List.map recurse args)
 | AST.TSum (name, args) -> AST.TSum (name, List.map recurse args)
 | AST.TList value -> AST.TList (recurse value) | AST.TStream value -> AST.TStream (recurse value)
 | AST.TDict (key, value) -> AST.TDict (recurse key, recurse value)
 | AST.TFunction (args, result) -> AST.TFunction (List.map recurse args, recurse result) | _ -> typ
let concreteFields args case = List.map (substituteTransparentPayloadType (M.of_list (List.combine case.typeParams args))) case.fields
let transparentSumPayloadType name args sums = match M.find_opt name sums with
 | Some cases when M.cardinal cases = 1 ->
   let _, case = M.min_binding cases in
   if List.length case.typeParams <> List.length args then None else
   (match concreteFields args case with [payload] when canUseTransparentPayload payload -> Some payload | _ -> None)
 | _ -> None
(*
   These payloads always have a nonzero managed root, even when their contents
   are empty. Lists, dictionaries, streams, and arbitrary Int do not qualify.
*)
let canUseNullaryZeroForPayload = function AST.TString | AST.TChar | AST.TBlob | AST.TInt128 | AST.TUInt128 | AST.TTuple _ | AST.TRecord _ -> true | _ -> false
let unaryPayloadOfTwoCaseSum name args sums =
 let unary = match M.find_opt name sums with
  | Some cases when M.cardinal cases = 2 ->
    (match List.map snd (M.bindings cases) with
     | [nullary; unary] when nullary.fields = [] && List.length unary.fields = 1 -> Some unary
     | [unary; nullary] when nullary.fields = [] && List.length unary.fields = 1 -> Some unary | _ -> None)
  | _ -> None in
 match unary with Some case when List.length case.typeParams = List.length args -> (match concreteFields args case with [value] -> Some value | _ -> None) | _ -> None
let nullablePointerSumPayloadType name args sums = match unaryPayloadOfTwoCaseSum name args sums with Some value when canUseNullaryZeroForPayload value -> Some value | _ -> None
(*
   List nodes use tags 1-3 over an eight-byte-aligned root. Zero remains
   the valid empty list; the otherwise unused bare tag 4 denotes absence.
*)
let spareImmediateSumSentinel name args sums = match unaryPayloadOfTwoCaseSum name args sums with
 | Some AST.TUnit -> Some 1L | Some AST.TBool -> Some 2L | Some (AST.TInt8 | AST.TUInt8) -> Some 256L
 | Some (AST.TInt16 | AST.TUInt16) -> Some 65536L | Some (AST.TInt32 | AST.TUInt32) -> Some 4294967296L | Some (AST.TList _) -> Some 4L | _ -> None
let sumPayloadExpr source atom sums = match source with
 | AST.TSum (name, args) ->
   let payload = match transparentSumPayloadType name args sums with Some _ as value -> value | None ->
    (match nullablePointerSumPayloadType name args sums with Some _ as value -> value | None -> if Option.is_some (spareImmediateSumSentinel name args sums) then unaryPayloadOfTwoCaseSum name args sums else None) in
   (match payload with Some typ -> ANF.TypedAtom (atom, typ) | None -> ANF.TupleGet (atom, 1))
 | _ -> ANF.TupleGet (atom, 1)
let sumTypeNamesFromVariantLookup variants = M.fold (fun _ (owner, _, _, _) names -> S.add owner names) variants S.empty
let sumMetadataFromVariantLookup variants = {names = sumTypeNamesFromVariantLookup variants; cases = sumRepresentationIndex variants}
let mergeSumMetadata base overlay = {names = S.union base.names overlay.names; cases = mergeSumCaseIndexes base.cases overlay.cases}
let tryFindRecordTypeNameById id (names : C.semanticMetadata) = C.TypeIdMap.find_opt id names.C.typeNames
let tryFindSumTypeNameById = tryFindRecordTypeNameById
let tryFindVariantForType name typ variants =
 let bare () = M.find_opt name variants in match typ with
 | AST.TSum (owner, _) | AST.TRecord (owner, _) -> (match M.find_opt (owner ^ "." ^ name) variants with Some _ as value -> value | None -> bare ())
 | _ -> bare ()
let tryFindVariantByTag owner tag sums = Option.bind (M.find_opt owner sums) (fun cases ->
 Option.map (fun (_, case) -> owner, case.typeParams, case.tag, case.fields) (List.find_opt (fun (_, case) -> case.tag = tag) (M.bindings cases)))
(*
   Checked constructors already identify their owner, case, and runtime tag.
   Use that identity directly instead of scanning every registered variant.
*)
let tryFindVariantByConstructorId expected owner constructor variants =
 if AST.compareTypeId (AST.constructorIdOwner constructor) expected <> 0 then None else
 let name = AST.constructorIdValue constructor in
 let found = match M.find_opt (owner ^ "." ^ name) variants with Some _ as value -> value | None -> M.find_opt name variants in
 match found with Some (declared, _, tag, _) when declared = owner && tag = AST.constructorRuntimeTag constructor -> found | _ -> None
let tryFindVariantForTypeById constructor source names variants = match source with
 | AST.TSum (owner, _) | AST.TRecord (owner, _) ->
   let id = AST.constructorIdOwner constructor in
   if tryFindSumTypeNameById id names <> Some owner then None else tryFindVariantByConstructorId id owner constructor variants
 | _ -> None
let constructorReferenceMatches owner name (reference : C.constructorReference) names variants =
 match M.find_opt name variants with
 | Some (declared, _, tag, _) -> declared = owner && AST.compareTypeId (AST.constructorIdOwner reference.C.constructorId) reference.C.typeId = 0 && tryFindSumTypeNameById reference.C.typeId names = Some owner && AST.constructorRuntimeTag reference.C.constructorId = tag
 | None -> false
let int128ToCanonicalString = Z.to_string
let uint128ToCanonicalString = Z.to_string
let words value = Z.to_int64 (Z.signed_extract value 0 64), Z.to_int64 (Z.signed_extract value 64 64)
let int128Construction resolve value = let low, high = words value in ANF.Call (resolve "Darklang.Stdlib.Int128.__fromWords", [ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])
let uint128Construction resolve value = let low, high = words value in ANF.Call (resolve "Darklang.Stdlib.UInt128.__fromWords", [ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])
let int128LiteralComparison resolve atom value = let low, high = words value in ANF.Call (resolve "Darklang.Stdlib.Int128.__equalsWords", [atom; ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])
let uint128LiteralComparison resolve atom value = let low, high = words value in ANF.Call (resolve "Darklang.Stdlib.UInt128.__equalsWords", [atom; ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])
(*
   Convert AST.SemanticType to a string for specialization keys
*)
let rec typeToString typ =
 let join separator values = String.concat separator (List.map typeToString values) in match typ with
 | AST.TInt64 -> "i64" | AST.TInt128 -> "i128" | AST.TInt -> "int" | AST.TInt32 -> "i32" | AST.TInt16 -> "i16" | AST.TInt8 -> "i8"
 | AST.TUInt64 -> "u64" | AST.TUInt128 -> "u128" | AST.TUInt32 -> "u32" | AST.TUInt16 -> "u16" | AST.TUInt8 -> "u8"
 | AST.TBool -> "bool" | AST.TString -> "str" | AST.TBlob -> "blob" | AST.TChar -> "char" | AST.TDateTime -> "datetime" | AST.TFloat64 -> "f64" | AST.TUnit -> "unit" | AST.TNever -> "runtime_error" | AST.TInternalRawPtr -> "ptr"
 | AST.TVar name | AST.TInferenceVar (name, _) -> name
 | AST.TRecord (name, args) -> name ^ (if args = [] then "" else "<" ^ join "," args ^ ">")
 | AST.TSum (name, args) -> name ^ "<" ^ join "," args ^ ">"
 | AST.TList value -> "List<" ^ typeToString value ^ ">" | AST.TStream value -> "Stream<" ^ typeToString value ^ ">"
 | AST.TDict (key, value) -> "Dict<" ^ typeToString key ^ "," ^ typeToString value ^ ">"
 | AST.TFunction (args, result) -> "(" ^ join "," args ^ ")->" ^ typeToString result
 | AST.TTuple values -> "(" ^ join "*" values ^ ")"
(*
   Convert a literal pattern into an ANF sized integer
*)
let patternLiteralToSizedInt = function
 | C.PInt64 n -> Some (ANF.Int64 n) | C.PInt8Literal n -> Some (ANF.Int8 n) | C.PInt16Literal n -> Some (ANF.Int16 n)
 | C.PInt32Literal n -> Some (ANF.Int32 n) | C.PUInt8Literal n -> Some (ANF.UInt8 n) | C.PUInt16Literal n -> Some (ANF.UInt16 n)
 | C.PUInt32Literal n -> Some (ANF.UInt32 n) | C.PUInt64Literal n -> Some (ANF.UInt64 n) | _ -> None

(*
   Try to convert a function call to a file I/O intrinsic CExpr
   Returns Some CExpr if it's a file intrinsic, None otherwise
*)
let tryFileIntrinsic name args = match name, args with
 | "Darklang.Stdlib.File.currentDirectory", ([] | [ANF.UnitLiteral]) ->
 Some (ANF.CliNative (ANF.DirectoryCurrent, []))
 | "Darklang.Stdlib.File.listDirectoryPacked", [pathAtom] ->
 Some (ANF.CliNative (ANF.DirectoryListPacked, [pathAtom]))
 | "Darklang.Stdlib.File.readBlob", [pathAtom] ->
 Some (ANF.FileReadBlob pathAtom)
 | "Darklang.Stdlib.File.exists", [pathAtom] ->
 Some (ANF.FileExists pathAtom)
 | "Darklang.Stdlib.File.isDirectory", [pathAtom] ->
 Some (ANF.CliNative (ANF.FileIsDirectory, [pathAtom]))
 | "Darklang.Stdlib.File.writeBlob", [pathAtom; contentAtom] ->
 Some (ANF.FileWriteBlob (pathAtom, contentAtom))
 | "Darklang.Stdlib.File.appendText", [pathAtom; contentAtom] ->
 Some (ANF.FileAppendText (pathAtom, contentAtom))
 | "Darklang.Stdlib.File.delete", [pathAtom] ->
 Some (ANF.FileDelete pathAtom)
 | "Darklang.Stdlib.File.createDirectory", [pathAtom] ->
 Some (ANF.FileCreateDirectory pathAtom)
 | "Darklang.Stdlib.File.setExecutable", [pathAtom] ->
 Some (ANF.FileSetExecutable pathAtom)
 | "Darklang.Stdlib.File.writeFromPtr", [pathAtom; ptrAtom; lengthAtom] ->
 Some (ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom))
 | _ -> None

(*
   Convert the public console builtins into explicit ordered effects.
*)
let tryPresentationIntrinsic name args = match name, args with
 | "Builtin.print", [value] -> Some (ANF.StdoutWrite (value, false))
 | "Builtin.printLine", [value] -> Some (ANF.StdoutWrite (value, true))
 | "Builtin.stdinReadLine", [ANF.UnitLiteral] -> Some ANF.StdinReadLine
 | _ -> None

(*
   Try to convert a function call to a Float intrinsic CExpr
   Returns Some CExpr if it's a Float intrinsic, None otherwise
   NOTE: Float.toString is now implemented in Dark, not as an intrinsic
*)
let tryFloatIntrinsic name args = match name, args with
 | "Darklang.Stdlib.Float.sqrt", [xAtom] ->
 Some (ANF.FloatSqrt xAtom)
 | "Darklang.Stdlib.Float.negate", [xAtom] ->
 Some (ANF.FloatNeg xAtom)
 | "Darklang.Stdlib.Int64.toFloat", [xAtom] ->
 Some (ANF.Int64ToFloat xAtom)
 | "Darklang.Stdlib.Float.__toBits", [xAtom] ->
 Some (ANF.FloatToBits xAtom)
 | "Darklang.Stdlib.Float.__toInt64Unchecked", [xAtom] ->
 Some (ANF.FloatToInt64 xAtom)
 | _ -> None

let tryCliIntrinsic name args = match name, args with
 | "Darklang.Stdlib.Cli.__sleep", [delay] -> Some (ANF.Sleep delay)
 | _ ->
   let operation = match name with
    | "Darklang.Stdlib.Cli.__execute" -> Some ANF.Execute
    | "Darklang.Stdlib.Cli.__runProcess" -> Some ANF.RunProcess
    | "Darklang.Stdlib.Cli.__hostOSCode" -> Some ANF.HostOS
    | "Darklang.Stdlib.Cli.__hostArchitectureCode" -> Some ANF.HostArchitecture
    | "Darklang.Stdlib.Cli.__hostname" -> Some ANF.Hostname
    | "Darklang.Stdlib.Cli.__getenv" -> Some ANF.GetEnv
    | "Darklang.Stdlib.Cli.__createExclusive" -> Some ANF.FileCreateExclusive
    | "Darklang.Stdlib.Cli.__environmentPacked" -> Some ANF.GetEnvironmentPacked
    | "Darklang.Stdlib.Cli.__setenv" -> Some ANF.SetEnv
    | "Darklang.Stdlib.Cli.__unsetenv" -> Some ANF.UnsetEnv
    | "Darklang.Stdlib.Cli.__argv" -> Some ANF.GetArgv
    | "Darklang.Stdlib.Cli.__kill" -> Some ANF.Kill
    | "Darklang.Stdlib.Cli.__getpid" -> Some ANF.GetPid
    | "Darklang.Stdlib.Cli.__getuid" -> Some ANF.GetUid
    | "Darklang.Stdlib.Cli.__cpuCount" -> Some ANF.CpuCount
    | "Darklang.Stdlib.Cli.__spawnProcess" -> Some ANF.SpawnProcess
    | "Darklang.Stdlib.Cli.__processIO" -> Some ANF.ProcessIO
    | "Darklang.Stdlib.Cli.__terminateProcess" -> Some ANF.TerminateProcess
    | "Darklang.Stdlib.Network.__tcp4Socket" -> Some ANF.SocketTcp4
    | "Darklang.Stdlib.Network.__tcp6Socket" -> Some ANF.SocketTcp6
    | "Darklang.Stdlib.Network.__udp4Socket" -> Some ANF.SocketUdp4
    | "Darklang.Stdlib.Network.__udp6Socket" -> Some ANF.SocketUdp6
    | "Darklang.Stdlib.Network.__connect4" -> Some ANF.SocketConnect4
    | "Darklang.Stdlib.Network.__connect6" -> Some ANF.SocketConnect6
    | "Darklang.Stdlib.Network.__send" -> Some ANF.SocketSend
    | "Darklang.Stdlib.Network.__receive" -> Some ANF.SocketReceive
    | "Darklang.Stdlib.Network.__receiveTimeout" -> Some ANF.SocketReceiveTimeout
    | "Darklang.Stdlib.Network.__sendTimeout" -> Some ANF.SocketSendTimeout
    | "Darklang.Stdlib.Network.__close" -> Some ANF.SocketClose
    | "Darklang.Stdlib.Network.__bind4" -> Some ANF.SocketBind4
    | "Darklang.Stdlib.Network.__listen" -> Some ANF.SocketListen
    | "Darklang.Stdlib.Network.__accept" -> Some ANF.SocketAccept
    | "Darklang.Stdlib.Network.__cloexec" -> Some ANF.SocketCloexec
    | "Darklang.Stdlib.Network.__reuseAddress" -> Some ANF.SocketReuseAddress
    | "Darklang.Stdlib.Network.__poll" -> Some ANF.SocketPoll
    | "Darklang.Stdlib.Network.__signalBlock" -> Some ANF.SignalBlock
    | "Darklang.Stdlib.Network.__signalRestore" -> Some ANF.SignalRestore
    | "Darklang.Stdlib.Network.__signalPending" -> Some ANF.SignalPending
    | "Darklang.Stdlib.Network.__signalWait" -> Some ANF.SignalWait
    | "Darklang.Stdlib.Network.__monotonic" -> Some ANF.MonotonicTime
    | "Darklang.Stdlib.Crypto.__secureRandomFill" -> Some ANF.SecureRandomFill
    | _ -> None
   in Option.map (fun operation -> ANF.CliNative (operation, args)) operation

let normalizeNullaryIntrinsicArgs = function [ANF.UnitLiteral] -> [] | values -> values
(*
   Parse a mangled type name (from typeToMangledName) into an AST type.
   Returns Error if the mangled form is ambiguous or unsupported.
   `TNever` is mangled as two underscore-separated tokens.
*)
let tryParseMangledTypeWithSumTypeNames sumNames mangled =
 let named name args = if S.mem name sumNames then AST.TSum (name, args) else AST.TRecord (name, args) in
 let primitive = function
  | "i8" -> Some AST.TInt8
  | "i16" -> Some AST.TInt16
  | "i32" -> Some AST.TInt32
  | "i64" -> Some AST.TInt64
  | "i128" -> Some AST.TInt128
  | "int" -> Some AST.TInt
  | "u8" -> Some AST.TUInt8
  | "u16" -> Some AST.TUInt16
  | "u32" -> Some AST.TUInt32
  | "u64" -> Some AST.TUInt64
  | "u128" -> Some AST.TUInt128
  | "bool" -> Some AST.TBool
  | "f64" -> Some AST.TFloat64
  | "str" -> Some AST.TString
  | "blob" -> Some AST.TBlob
  | "char" -> Some AST.TChar
  | "datetime" -> Some AST.TDateTime
  | "unit" -> Some AST.TUnit
  | "rawptr" -> Some AST.TInternalRawPtr
  | _ -> None
 in
 let firstIsLower text = match Text.first text with
  | None -> false | Some character -> Uucp.Gc.general_category character = `Ll in
 let rec parseType = function
 | [] -> []
 | "runtime" :: "error" :: rest -> [AST.TNever, rest]
 | token :: rest -> (match token with
   | "list" -> List.map (fun (value, rest) -> AST.TList value, rest) (parseType rest)
   | "stream" -> List.map (fun (value, rest) -> AST.TStream value, rest) (parseType rest)
   | "dict" -> List.concat_map (fun (key, rest) -> List.map (fun (value, rest) -> AST.TDict (key, value), rest) (parseType rest)) (parseType rest)
   | token when String.starts_with ~prefix:"tup" token && String.length token > 3 ->
     (match Text.tryParseInt32 (String.sub token 3 (String.length token - 3)) with
      | Some count when count >= 0l -> List.map (fun (values, rest) -> AST.TTuple values, rest) (parseExactly (Int32.to_int count) rest)
      | _ -> [])
   | "tup" -> List.map (fun (values, rest) -> AST.TTuple values, rest) (parseTupleElems rest)
   | "fn" -> parseFunction rest
   | _ -> (match primitive token with
     | Some value -> [value, rest]
     | None when String.contains token '$' || firstIsLower token -> [AST.TVar token, rest]
     | None -> (named token [], rest) :: List.map (fun (args, rest) -> named token args, rest) (parseTupleElems rest)))
 and parseExactly count tokens =
  if count = 0 then [[], tokens] else List.concat_map (fun (first, rest) -> List.map (fun (values, rest) -> first :: values, rest) (parseExactly (count - 1) rest)) (parseType tokens)
 and parseTupleElems tokens =
  List.concat_map (fun (first, rest) -> ([first], rest) :: List.map (fun (values, rest) -> first :: values, rest) (parseTupleElems rest)) (parseType tokens)
 and parseFunction tokens =
  let rec split accumulated = function [] -> None | "to" :: rest -> Some (List.rev accumulated, rest) | token :: rest -> split (token :: accumulated) rest in
  match split [] tokens with
  | None -> []
  | Some (parameters, result) ->
    let complete values = List.filter_map (fun (value, rest) -> if rest = [] then Some value else None) values in
    let parameters = complete (parseTupleElems parameters) and results = complete (parseType result) in
    List.concat_map (fun parameters -> List.map (fun result -> AST.TFunction (parameters, result), []) results) parameters in
 match List.filter (fun (_, rest) -> rest = []) (parseType (String.split_on_char '_' mangled)) with
 | [value, _] -> Ok value | [] -> Error ("Could not parse mangled type: " ^ mangled) | _ -> Error ("Ambiguous mangled type: " ^ mangled)
let tryParseMangledType variants = tryParseMangledTypeWithSumTypeNames (sumTypeNamesFromVariantLookup variants)
(*
   Parse mangled type names used by monomorphized raw intrinsics.
*)
let tryParseMangledTypeForRawIntrinsic names mangled = Result.to_option (tryParseMangledTypeWithSumTypeNames names mangled)
(*
   Canonical named APIs whose AOT implementation maps directly to backend
   Boolean and fixed-width integer primitives.
*)
let tryCanonicalPrimitiveIntrinsic name args = match name, args with
 | "Darklang.Stdlib.Bool.not", [value] -> Some (ANF.UnaryPrim (ANF.Not, value))
 | _ ->
   let modules = S.of_list ["Darklang.Stdlib.Int8"; "Darklang.Stdlib.Int16"; "Darklang.Stdlib.Int32"; "Darklang.Stdlib.Int64"; "Darklang.Stdlib.UInt8"; "Darklang.Stdlib.UInt16"; "Darklang.Stdlib.UInt32"; "Darklang.Stdlib.UInt64"] in
   let reversed = List.rev (String.split_on_char '.' name) in
   let operation = List.hd reversed and owner = String.concat "." (List.rev (List.tl reversed)) in
   if not (S.mem owner modules) then None else match operation, args with
   | "bitwiseAnd", [a; b] -> Some (ANF.Prim (ANF.BitAnd, a, b))
   | "bitwiseOr", [a; b] -> Some (ANF.Prim (ANF.BitOr, a, b))
   | "bitwiseXor", [a; b] -> Some (ANF.Prim (ANF.BitXor, a, b))
   | "shiftLeft", [a; b] -> Some (ANF.Prim (ANF.Shl, a, b))
   | "shiftRight", [a; b] -> Some (ANF.Prim (ANF.Shr, a, b))
   | "bitwiseNot", [value] -> Some (ANF.UnaryPrim (ANF.BitNot, value)) | _ -> None
let releaseRuntimeSmall pointer =
 (* LowerListRegions.releaseRuntimeSmall has no dependency on region IR.
    Keep its complete allocator-capacity contract here until region lowering
    imports this shared helper. The RC word follows the 28-element capacity. *)
 let payload = 24 + 28 * 8 in
 let metadata = Some {MemoryModel.releasePlanCacheKey = None; sourceType = None; releasePlan = Some (MemoryModel.RootRelease (payload, MemoryModel.GenericHeap, MemoryModel.NoPayloadRelease))} in
 ANF.RefCountDec (pointer, payload, MemoryModel.GenericHeap, metadata)
(*
   Try to convert a function call to a raw memory intrinsic CExpr
   These are internal-only functions for implementing HAMT data structures
   Returns Some CExpr if it's a raw memory intrinsic, None otherwise
   Note: __raw_get and __raw_slot_init are generic and become monomorphized names like
   __raw_get_i64, __raw_get_str, __raw_slot_init_i64, etc.
   Read single byte at offset, returns Int64 (zero-extended)
   IMPORTANT: Must come before the generic __raw_get_* pattern
   Generic __raw_get<v> monomorphizes to __raw_get_<mangled-type>.
   Preserve recovered value type so downstream RC/codegen can handle heap payloads correctly.
   Write single byte at offset
   Write one unmanaged machine word. This intentionally has no typed edge semantics.
   Generic __raw_slot_init<v> monomorphizes to __raw_slot_init_<mangled-type>.
   Preserve recovered value type so codegen can update ownership for stored heap values.
   String refcount intrinsics (for Dict with string keys)
   Dynamic buffer backing-pointer views and raw allocation adoption.
   A preceding RuntimeError never returns, but its unreachable
   continuation still has the ANF Unit placeholder.
   Dict intrinsics - for type-safe Dict<k, v> operations
   __empty_dict<k, v> returns 0 (null pointer)
   __dict_is_null<k, v> checks if pointer is 0
   __dict_get_tag<k, v> extracts low 2 bits (dict & 3)
   __dict_to_rawptr<k, v> clears tag bits (dict & -4)
   __rawptr_to_dict<k, v> combines pointer + tag (ptr | tag)
   List intrinsics for the direct-payload skew RAL implementation.
   __list_is_null<a> checks if list pointer is 0 (empty)
   __list_get_tag<a> extracts the low three skew-node tag bits.
   __list_to_rawptr<a> clears tag bits (list & -8) to get raw pointer
   __rawptr_to_list<a> combines pointer + tag (ptr | tag) to create tagged list
   __list_empty<a> returns 0 (the null skew-list root).
*)
let tryRawMemoryIntrinsic resolve sumNames name arguments =
 let args = normalizeNullaryIntrinsicArgs arguments in
 let suffix prefix name = if String.starts_with ~prefix:(prefix ^ "_") name then Some (String.sub name (String.length prefix + 1) (String.length name - String.length prefix - 1)) else None in
 let valueType prefix name = Option.bind (suffix prefix name) (tryParseMangledTypeForRawIntrinsic sumNames) in
 let family prefix name = name = prefix || String.starts_with ~prefix:(prefix ^ "_") name in
 let dictType name = match suffix "__rawptr_to_dict" name with
  | None -> AST.TDict (AST.TVar "k", AST.TVar "v")
  | Some suffix -> (match tryParseMangledTypeForRawIntrinsic sumNames ("dict_" ^ suffix) with Some value -> value | None -> Crash.crash ("Could not recover Dict type from intrinsic '" ^ name ^ "'")) in
 let listType name = match suffix "__rawptr_to_list" name with
  | None -> AST.TList (AST.TVar "a")
  | Some suffix -> (match tryParseMangledTypeForRawIntrinsic sumNames suffix with Some value -> AST.TList value | None -> Crash.crash ("Could not recover List type from intrinsic '" ^ name ^ "'")) in
 match name, args with
 | "__raw_alloc", [n] -> Some (ANF.RawAlloc n) | "__mapped_alloc", [n] -> Some (ANF.MappedAlloc n)
 | "__raw_free", [ptr] -> Some (ANF.RawFree ptr) | "__mapped_free", [ptr] -> Some (ANF.MappedFree ptr)
 | "__list_array_release_small", [ptr] -> Some (releaseRuntimeSmall ptr)
 | "__raw_get_byte", [ptr; offset] -> Some (ANF.RawGetByte (ptr, offset))
 | name, [ptr; offset] when family "__raw_get" name -> Some (ANF.RawGet (ptr, offset, valueType "__raw_get" name))
 | name, [ptr; offset] when family "__raw_take" name -> Some (ANF.RawTake (ptr, offset, valueType "__raw_take" name))
 | "__raw_write_byte", [ptr; offset; value] -> Some (ANF.RawWriteByte (ptr, offset, value))
 | "__raw_write_word", [ptr; offset; value] -> Some (ANF.RawWriteWord (ptr, offset, value))
 | name, [ptr; offset; value] when family "__raw_slot_init" name ->
   (match valueType "__raw_slot_init" name with Some typ -> Some (ANF.RawSlotInit (ptr, offset, value, typ)) | None -> Crash.crash "__raw_slot_init requires a concrete slot type")
 | "__refcount_inc_string", [value] -> Some (ANF.RefCountIncString value)
 | "__refcount_dec_string", [value] -> Some (ANF.RefCountDecString value)
 | "__string_to_rawptr", [value] -> Some (ANF.StringToRawPtr value)
 | "__rawptr_to_string", [value] -> Some (ANF.RawPtrToString value)
 | ("__int_to_rawptr" | "__int128_to_rawptr" | "__uint128_to_rawptr"), [value] -> Some (ANF.TypedAtom (value, AST.TInternalRawPtr))
 | "__rawptr_to_int", [value] -> Some (ANF.TypedAtom (value, AST.TInt))
 | "__rawptr_to_int128", [value] -> Some (ANF.RawPtrToInt128 value)
 | "__rawptr_to_uint128", [value] -> Some (ANF.RawPtrToUInt128 value)
 | "__string_concat_raw", [left; right] ->
   let representable = function ANF.UnitLiteral -> ANF.StringLiteral "" | value -> value in Some (ANF.StringConcat (representable left, representable right, []))
 | ("__int_to_word" | "__uint64_to_int64_bits" | "__uint8_to_int64" | "__uint16_to_int64" | "__uint32_to_int64"), [value] -> Some (ANF.TypedAtom (value, AST.TInt64))
 | "__word_to_int", [value] -> Some (ANF.TypedAtom (value, AST.TInt))
 | ("__int64_to_uint64_bits" | "__int64_to_int8" | "__int64_to_int16" | "__int64_to_int32" | "__int64_to_uint8" | "__int64_to_uint16" | "__int64_to_uint32"), [value] -> Some (ANF.Atom value)
 | "__int128_to_int", [value] -> Some (ANF.Call (resolve "Darklang.Stdlib.Int128.__toInt", [value]))
 | "__uint128_to_int", [value] -> Some (ANF.Call (resolve "Darklang.Stdlib.UInt128.__toInt", [value]))
 | "__int_to_int128", [value] -> Some (ANF.Call (resolve "Darklang.Stdlib.Int128.__fromInt", [value]))
 | "__int_to_uint128", [value] -> Some (ANF.Call (resolve "Darklang.Stdlib.UInt128.__fromInt", [value]))
 | "__blob_to_rawptr", [value] -> Some (ANF.BlobToRawPtr value)
 | "__rawptr_to_blob", [value] -> Some (ANF.RawPtrToBlob value)
 | name, [value] when family "__stream_to_rawptr" name -> Some (ANF.TypedAtom (value, AST.TInternalRawPtr))
 | name, [value] when family "__rawptr_to_stream" name ->
   let typ = match suffix "__rawptr_to_stream" name with None -> AST.TStream (AST.TVar "a")
    | Some suffix -> (match tryParseMangledTypeForRawIntrinsic sumNames suffix with Some value -> AST.TStream value | None -> Crash.crash ("Could not recover Stream type from intrinsic '" ^ name ^ "'")) in Some (ANF.TypedAtom (value, typ))
 | name, [] when family "__empty_dict" name -> Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))
 | name, [value] when family "__dict_is_null" name -> Some (ANF.Prim (ANF.Eq, value, ANF.IntLiteral (ANF.Int64 0L)))
 | name, [value] when family "__dict_get_tag" name -> Some (ANF.Prim (ANF.BitAnd, value, ANF.IntLiteral (ANF.Int64 3L)))
 | name, [value] when family "__dict_to_rawptr" name -> Some (ANF.DictToRawPtr value)
 | name, [ptr; tag] when family "__rawptr_to_dict" name -> Some (ANF.RawPtrToDict (ptr, tag, dictType name))
 | name, [value] when family "__list_is_null" name -> Some (ANF.Prim (ANF.Eq, value, ANF.IntLiteral (ANF.Int64 0L)))
 | name, [value] when family "__list_get_tag" name -> Some (ANF.Prim (ANF.BitAnd, value, ANF.IntLiteral (ANF.Int64 7L)))
 | name, [value] when family "__list_to_rawptr" name -> Some (ANF.ListToRawPtr value)
 | name, [ptr; tag] when family "__rawptr_to_list" name -> Some (ANF.RawPtrToList (ptr, tag, listType name))
 | name, [] when family "__list_empty" name -> Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))
 | _ -> None
(*
   Try to convert a function call to a random intrinsic CExpr
   Returns Some CExpr if it's a random intrinsic, None otherwise
*)
let tryRandomIntrinsic name args = match name, normalizeNullaryIntrinsicArgs args with "Darklang.Stdlib.Int.__randomInt64Word", [] -> Some ANF.RandomInt64 | _ -> None
(*
   Try to convert a function call to a DateTime intrinsic CExpr.
*)
let tryDateTimeIntrinsic name args = match name, normalizeNullaryIntrinsicArgs args with
 | "Darklang.Stdlib.DateTime.__now", [] -> Some ANF.DateTimeNow
 | "Darklang.Stdlib.DateTime.__fromUnixTimeTicks", [value] -> Some (ANF.TypedAtom (value, AST.TDateTime))
 | "Darklang.Stdlib.DateTime.__toUnixTimeTicks", [value] -> Some (ANF.TypedAtom (value, AST.TInt64)) | _ -> None
let isBuiltinUnwrapName name = name = "Builtin.unwrap"
let isBuiltinTestRuntimeErrorName name = name = "Builtin.testRuntimeError"
(*
   Source programs use `crash`; the older builtin is retained for tests only.
*)
let isSourceCrashName name = name = "Builtin.crash"
let isRuntimeFailureName name = isBuiltinTestRuntimeErrorName name || isSourceCrashName name
(*
   Record metadata retained through lowering. Declared parameter order cannot
   be reconstructed from fields because parameters may be phantom.
*)
let unwrapErrorPayloadToString = function
 | C.UnitLiteral -> Some "()" | C.Int64Literal value -> Some (Int64.to_string value)
 | C.Int128Literal value | C.UInt128Literal value -> Some (Z.to_string value)
 | C.Int8Literal value | C.Int16Literal value | C.UInt8Literal value | C.UInt16Literal value -> Some (string_of_int value)
 | C.Int32Literal value -> Some (Int32.to_string value)
 | C.UInt32Literal value -> Some (Int64.to_string value)
 | C.UInt64Literal value -> Some (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))
 | C.BoolLiteral value -> Some (if value then "true" else "false") | C.FloatLiteral value -> Some (FloatFormat.roundTrip value)
 | C.StringLiteral value | C.CharLiteral value -> Some value | _ -> None
