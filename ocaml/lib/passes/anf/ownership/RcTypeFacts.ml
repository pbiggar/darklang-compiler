(* TypeFacts.fs - Track ANF value types and immutable context projections for RC insertion. *)
[@@@warning "-4"]
module A = ANF
module R = TypeRegistries
module M = StringOrder.Map
module TempMap = InliningCommon.TempMap
(*
   Immutable registry projections and memoized type plans shared by every
   function processed in one RC insertion pass. TypeContext itself is copied
   as local TempId information changes, so keep the reusable planning state in
   a reference object carried by those copies.
*)
type rcTypePlanningContext = {mutable recordRegistries : ((string * AST.semanticType) list M.t * string list M.t) option; shapes : (AST.semanticType, MemoryModel.rcShape) Hashtbl.t; metadata : (AST.semanticType, MemoryModel.rcMetadata) Hashtbl.t}
(*
   Type context for inferring types during RC insertion
   Maps TempId -> Type for values we've seen
   Maps TempId -> function name for closures (to resolve closure call return types)
   Registry projections and canonical ownership plans shared across local
   TypeContext copies for this pass.
*)
type typeContext = {typeReg : R.typeRegistry; variantLookup : LoweringPrimitives.variantLookup; sumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : R.functionRegistry; funcParams : (string * AST.semanticType) list M.t; tempTypes : AST.semanticType TempMap.t; closureFuncs : AST.functionId TempMap.t; typePlanning : rcTypePlanningContext}
let createRcTypePlanningContext () = {recordRegistries = None; shapes = Hashtbl.create 32; metadata = Hashtbl.create 32}
(*
   Create initial context from conversion result
*)
let createContext (result : AST_to_ANF.conversionResult) =
 let A.Program (functions, _) = result.AST_to_ANF.program in
 {typeReg = result.AST_to_ANF.typeReg; variantLookup = result.AST_to_ANF.variantLookup; sumShapeReg = result.AST_to_ANF.rcSumShapeReg;
  funcReg = AST_to_ANF.extendFunctionRegistryWithConverted result.AST_to_ANF.funcReg functions; funcParams = result.AST_to_ANF.funcParams;
  tempTypes = TempMap.empty; closureFuncs = TempMap.empty; typePlanning = createRcTypePlanningContext ()}
let withTempTypes ctx types = {ctx with tempTypes = types}
(*
   Add a closure TempId -> function name mapping to context
*)
let addClosureFunc ctx id func = {ctx with closureFuncs = TempMap.add id func ctx.closureFuncs}
(*
   Try to get the function name of a closure from its TempId
*)
let tryGetClosureFunc ctx = function A.Var id -> TempMap.find_opt id ctx.closureFuncs | _ -> None
(*
   Try to get the type of a TempId
*)
let tryGetType ctx id = TempMap.find_opt id ctx.tempTypes
(*
   Try to get a function's return type from the function registry
*)
let tryGetFuncReturnTypeFromReg ctx id = match FunctionIdMap.tryFind id ctx.funcReg with Some (_, AST.TFunction (_, ret)) -> Some ret | Some (_, typ) -> Some typ | None -> None
(*
   Infer the type of an atom (best-effort)
*)
let inferAtomType ctx = function
 | A.UnitLiteral -> Some AST.TUnit | A.IntLiteral value -> Some (A.sizedIntToType value)
 | A.BoolLiteral _ -> Some AST.TBool | A.StringLiteral _ -> Some AST.TString | A.FloatLiteral _ -> Some AST.TFloat64
 | A.Var id -> tryGetType ctx id | A.FuncRef id -> Option.map snd (FunctionIdMap.tryFind id ctx.funcReg)
let isIntegerType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true | _ -> false
let inferArithmeticType left right = match left, right with
 | Some AST.TFloat64, _ | _, Some AST.TFloat64 -> Some AST.TFloat64
 | Some left, Some right when left = right && isIntegerType left -> Some left
 | Some left, None when isIntegerType left -> Some left
 | None, Some right when isIntegerType right -> Some right | _ -> None
let isHeapLikeForBitwiseTagging = function AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _ -> true | _ -> false
let starts prefix text = String.starts_with ~prefix text
(*
   Return types for monomorphized intrinsics that are not always present in FuncReg
*)
let tryGetMonomorphizedIntrinsicReturnType ctx name =
 let parse mangled = Result.to_option (LoweringPrimitives.tryParseMangledType ctx.variantLookup mangled) in
 let suffix prefix = String.sub name (String.length prefix) (String.length name - String.length prefix) in
 if starts "__raw_get_" name then parse (suffix "__raw_get_")
 else if starts "__raw_take_" name then parse (suffix "__raw_take_")
 else if starts "__raw_slot_init_" name then Some AST.TUnit
 else if starts "__hash_" name then Some AST.TInt64
 else if starts "__key_eq_" name then Some AST.TBool
 else if starts "__empty_dict_" name then Some AST.TInt64
 else if starts "__dict_is_null_" name then Some AST.TBool
 else if starts "__dict_get_tag_" name then Some AST.TInt64
 else if starts "__dict_to_rawptr_" name then Some AST.TInternalRawPtr
 else if starts "__rawptr_to_dict_" name then parse ("dict_" ^ suffix "__rawptr_to_dict_")
 else if starts "__list_is_null_" name then Some AST.TBool
 else if starts "__list_get_tag_" name then Some AST.TInt64
 else if starts "__list_to_rawptr_" name then Some AST.TInternalRawPtr
 else if starts "__rawptr_to_list_" name then Option.map (fun typ -> AST.TList typ) (parse (suffix "__rawptr_to_list_"))
 else None
let tryItem index values = if index < 0 then None else List.nth_opt values index
(*
   Infer the type of a CExpr in the given context
   Constructor descriptors retain the nominal sum identity for reuse, while
   ownership remains variant-specific as it was for tuple-backed sums. This
   avoids a dynamic sum release when the concrete payload layout is known.
   Use the explicit type annotation
   Binary ops return int or bool depending on op
   Pointer-tagging lowerings use bitwise ops over tagged heap values and masks.
   The result is a scalar tag/masked pointer value, not a heap object ownership value.
   Preserve the operand type instead of assuming Int64.
   This keeps sized integer semantics (e.g. UInt8) intact.
   Float intrinsics
   The erased list-pattern helper returns Int64 at the ABI boundary, but
   ownership follows the concrete element type of its list argument.
   Return type from function registry (with special-case inference for stdlib list/tuple helpers)
   Tail calls have same return type as regular calls
   Look up the function's type to get its return type
   Raw code pointers are pointer-sized integers after projection
   from the internal function-closure layout.
   Same as IndirectCall
   Closure payload size is resolved by the backend from the function
   pointer, so ownership insertion must preserve the source function type
   instead of treating the closure as an ordinary fixed block.
   Prefer the concrete closure target when available; otherwise use
   the closure value's function type carried by the ANF type registry.
   Same as ClosureCall
   Infer element types and create TTuple
   Get element type from tuple type
   Record fields - look up field type
   List Cons cells are (tag, head, tail) - index 1 is head, index 2 is tail
   tag
   head element
   tail is same list type
   Sum type layout: [tag:8][payload:8]
   index 0 = tag (Int64), index 1 = payload
   Payload type depends on variant, but for simple cases like Option<T>,
   the payload type is the first type argument
   Closures are typed as TFunction but laid out as tuples:
   [func_ptr:8][cap1:8][cap2:8]...
   Index 0 is the function pointer (Int64), rest are captures
   All closure slots are pointer-sized
   String concatenation returns a string
   Result<Blob, String>
   Bool
   Result<Unit, String>
   Returns Bool (success/failure)
   Raw memory intrinsics (no ref counting - manually managed)
   Returns raw pointer
   Returns unit
   Returns 1-byte value (zero-extended)
   Dynamic buffer refcount intrinsics
   Return analysis annotation for AExpr nodes
*)
let inferCExprType ctx expr =
 let fixedBlockType (descriptor : A.recordDescriptor) = match descriptor.A.valueType with AST.TSum _ -> AST.TTuple (List.map snd descriptor.A.fields) | typ -> typ in
 let ret = tryGetFuncReturnTypeFromReg ctx in
 let atom = inferAtomType ctx in
 let indirect = function A.Var id -> (match tryGetType ctx id with Some (AST.TFunction (_, typ)) -> Some typ | Some AST.TInternalRawPtr | Some AST.TInt64 -> Some AST.TBool | _ -> None) | _ -> None in
 let closure value = match tryGetClosureFunc ctx value with Some id -> ret id | None -> (match value with A.Var id -> (match tryGetType ctx id with Some (AST.TFunction (_, typ)) -> Some typ | _ -> None) | _ -> None) in
 let posix payload = Some (AST.TSum ("Darklang.Stdlib.Result.Result", [payload; AST.TRecord ("Darklang.Stdlib.Cli.NativePosixError", [])])) in
 match expr with
 | A.Atom value -> atom value
 | A.TypedAtom (_, typ) -> Some typ
 | A.Prim (op, left, right) ->
   (match op with A.Add | A.Sub | A.Mul | A.Div -> inferArithmeticType (atom left) (atom right)
   | A.Mod | A.Shl | A.Shr | A.BitAnd | A.BitOr | A.BitXor ->
     let left, right = atom left, atom right in
     let bitwise = match op with A.BitAnd | A.BitOr | A.BitXor -> true | _ -> false in
     (match left, right with
     | Some left, _ when bitwise && isHeapLikeForBitwiseTagging left -> Some AST.TInt64
     | _, Some right when bitwise && isHeapLikeForBitwiseTagging right -> Some AST.TInt64
     | Some left, Some right when left = right && isIntegerType left -> Some left
     | Some left, None when isIntegerType left -> Some left
     | None, Some right when isIntegerType right -> Some right
     | Some left, _ -> Some left | None, Some right -> Some right | None, None -> None)
   | A.Eq | A.Neq | A.Lt | A.Gt | A.Lte | A.Gte | A.And | A.Or -> Some AST.TBool)
 | A.CanonicalBufferEq _ -> Some AST.TBool
 | A.UnaryPrim (op, value) -> (match op with A.Neg -> (match atom value with Some AST.TFloat64 -> Some AST.TFloat64 | Some _ -> Some AST.TInt64 | None -> None) | A.Not -> Some AST.TBool | A.BitNot -> atom value)
 | A.FloatSqrt _ | A.FloatAbs _ | A.FloatNeg _ | A.Int64ToFloat _ -> Some AST.TFloat64
 | A.FloatToInt64 _ | A.RandomInt64 -> Some AST.TInt64
 | A.FloatToBits _ -> Some AST.TUInt64
 | A.FloatToString _ | A.StdinReadLine -> Some AST.TString
 | A.DateTimeNow -> Some AST.TDateTime
 | A.Sleep _ | A.StdoutWrite _ -> Some AST.TUnit
 | A.CliNative (op, _) -> (match op with
   | A.Execute | A.ProcessIO | A.TerminateProcess -> Some (AST.TRecord ("Darklang.Stdlib.Cli.NativeOutput", []))
   | A.RunProcess -> Some (AST.TRecord ("Darklang.Stdlib.Cli.NativeProcessOutput", []))
   | A.GetEnv | A.GetArgv -> Some (AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TString]))
   | A.Kill | A.SetEnv | A.UnsetEnv -> posix AST.TUnit
   | A.Hostname -> posix AST.TString
   | A.GetEnvironmentPacked | A.DirectoryCurrent | A.DirectoryListPacked -> Some AST.TString
   | A.FileIsDirectory -> Some AST.TBool
   | A.HostOS | A.HostArchitecture | A.GetPid | A.GetUid | A.CpuCount | A.SpawnProcess | A.FileCreateExclusive
   | A.SocketTcp4 | A.SocketTcp6 | A.SocketUdp4 | A.SocketUdp6 | A.SocketConnect4 | A.SocketConnect6
   | A.SocketSend | A.SocketReceive | A.SocketReceiveTimeout | A.SocketSendTimeout | A.SocketClose | A.SecureRandomFill -> Some AST.TInt64)
 | A.IfValue (_, yes, _) -> atom yes
 | A.BorrowedCall (func, [value]) when Option.fold ~none:false ~some:(fun (name, _) -> starts "Darklang.Stdlib.List.__headUnsafe" name) (FunctionIdMap.tryFind func ctx.funcReg) ->
   (match atom value with Some (AST.TList elem) -> Some elem | _ -> ret func)
 | A.Call (func, args) | A.BorrowedCall (func, args) ->
   let displayName = Option.map fst (FunctionIdMap.tryFind func ctx.funcReg) in
   let fallbackList value = match atom value with Some (AST.TList elem) -> Some (AST.TSum ("Darklang.Stdlib.Option.Option", [elem])) | _ -> None in
   (match displayName, args with
   | Some name, [value; _] when starts "Darklang.Stdlib.List.getAt" name || starts "Darklang.Stdlib.List.__getAt" name -> (match ret func with Some typ -> Some typ | None -> fallbackList value)
   | Some name, [value] when starts "Darklang.Stdlib.List.head" name || starts "Darklang.Stdlib.List.__head" name -> (match ret func with Some typ -> Some typ | None -> fallbackList value)
   | Some name, [value] when starts "Darklang.Stdlib.List.tail" name || starts "Darklang.Stdlib.List.__tail" name ->
     (match atom value with Some (AST.TList elem) when starts "Darklang.Stdlib.List.tail" name -> Some (AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TList elem])) | Some (AST.TList elem) -> Some (AST.TList elem) | _ -> ret func)
   | Some name, [value] when starts "Darklang.Stdlib.Tuple2.first" name ->
     (match ret func with Some typ -> Some typ | None -> (match atom value with Some (AST.TTuple (typ :: _)) -> Some typ | _ -> None))
   | Some name, [value] when starts "Darklang.Stdlib.Tuple2.second" name ->
     (match ret func with Some typ -> Some typ | None -> (match atom value with Some (AST.TTuple (_ :: typ :: _)) -> Some typ | _ -> None))
   | _ -> (match ret func with Some typ -> Some typ | None -> Option.bind displayName (tryGetMonomorphizedIntrinsicReturnType ctx)))
 | A.TailCall (func, _) -> ret func
 | A.IndirectCall (value, _) | A.IndirectTailCall (value, _) -> indirect value
 | A.ClosureAlloc (func, _) -> (match FunctionIdMap.tryFind func ctx.funcReg with
   | Some (_, (AST.TFunction _ as typ)) -> Some typ
   | Some (name, typ) -> Crash.crash ("RefCountInsertion: ClosureAlloc target '" ^ name ^ "' has non-function type " ^ HostStructuralFormat.semanticType typ)
   | None -> let ordinal = AST.functionIdValue func in
     let ordinal = Z.to_string (if ordinal < 0L then Z.add (Z.of_int64 ordinal) (Z.shift_left Z.one 64) else Z.of_int64 ordinal) in
     Crash.crash ("RefCountInsertion: ClosureAlloc target '" ^ ordinal ^ "' not found in function registry"))
 | A.ClosureCall (value, _) | A.ClosureTailCall (value, _) -> closure value
 | A.TupleAlloc values ->
   Some (AST.TTuple (List.map (fun value -> match atom value with Some typ -> typ | None ->
    match value with A.Var (A.TempId id) -> Crash.crash ("RefCountInsertion: type not found for temp TempId " ^ string_of_int id ^ " in TupleAlloc")
    | A.FuncRef id -> Crash.crash ("RefCountInsertion: type not found for function " ^ HostStructuralFormat.format (AST.DiagnosticFormatting.func id) ^ " in TupleAlloc")
    | A.UnitLiteral | A.IntLiteral _ | A.BoolLiteral _ | A.StringLiteral _ | A.FloatLiteral _ -> assert false) values))
 | A.RecordAlloc (descriptor, _) | A.RecordClone (descriptor, _, _) | A.RecordReuse (_, descriptor, _, _) -> Some (fixedBlockType descriptor)
 | A.RecordGet (descriptor, _, index) -> Option.map snd (tryItem index descriptor.A.fields)
 | A.TupleGet (value, index) -> (match value with A.Var id -> (match tryGetType ctx id with
   | Some (AST.TTuple types) when index < List.length types -> Some (List.nth types index)
   | Some (AST.TRecord (name, _)) -> (match M.find_opt name ctx.typeReg with Some info when index < List.length info.R.fields -> Some (snd (List.nth info.R.fields index)) | _ -> None)
   | Some (AST.TList elem) -> (match index with 0 -> Some AST.TInt64 | 1 -> Some elem | 2 -> Some (AST.TList elem) | _ -> None)
   | Some (AST.TSum (_, args)) -> (match index, args with 0, _ -> Some AST.TInt64 | 1, [typ] -> Some typ | _ -> None)
   | Some (AST.TFunction _) -> Some AST.TInt64 | _ -> None) | _ -> None)
 | A.StringConcat _ -> Some AST.TString
 | A.FileReadBlob _ -> Some (AST.TSum ("Darklang.Stdlib.Result.Result", [AST.TBlob; AST.TString]))
 | A.FileExists _ | A.FileWriteFromPtr _ -> Some AST.TBool
 | A.FileWriteBlob _ | A.FileAppendText _ | A.FileDelete _ | A.FileCreateDirectory _ | A.FileSetExecutable _ -> Some (AST.TSum ("Darklang.Stdlib.Result.Result", [AST.TUnit; AST.TString]))
 | A.RawAlloc _ | A.MappedAlloc _ | A.StringToRawPtr _ | A.BlobToRawPtr _ | A.DictToRawPtr _ | A.ListToRawPtr _ | A.FixedBlockToRawPtr _ -> Some AST.TInternalRawPtr
 | A.RawGet (_, _, typ) | A.RawTake (_, _, typ) -> typ
 | A.RawGetByte _ -> Some AST.TInt64
 | A.RawPtrToString _ -> Some AST.TString | A.RawPtrToBlob _ -> Some AST.TBlob
 | A.RawPtrToInt128 _ -> Some AST.TInt128 | A.RawPtrToUInt128 _ -> Some AST.TUInt128
 | A.RawPtrToDict (_, _, typ) | A.RawPtrToList (_, _, typ) -> Some typ
 | A.RefCountInc _ | A.RefCountDec _ | A.Print _ | A.RawFree _ | A.MappedFree _ | A.RawWriteWord _ | A.RawWriteByte _ | A.RawSlotInit _
 | A.RefCountIncString _ | A.RefCountDecString _ | A.RefCountIncBlob _ | A.RefCountDecBlob _ | A.RefCountIncInt _ | A.RefCountDecInt _ | A.RuntimeError _ | A.RuntimeErrorString _ -> Some AST.TUnit
