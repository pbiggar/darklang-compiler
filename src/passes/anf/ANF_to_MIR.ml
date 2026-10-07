(*
   ANF_to_MIR.ml - MIR Transformation (Pass 3)
   Transforms ANF into MIR with Control Flow Graph (CFG).
   Algorithm:
   - Converts ANF expressions into MIR CFG with basic blocks
   - Maps ANF temporary variables to MIR virtual registers
   - Converts ANF If expressions into conditional branches with basic blocks
   - Each basic block has a label, instructions, and a terminator
   Example (with if):
   if x then 10 else 20
   →
   entry:
   branch x, then_block, else_block
   then_block:
   v0 <- 10
   jump join_block
   else_block:
   v1 <- 20
   join_block:
   v2 <- phi(v0, v1)  // (simplified - actual implementation uses registers)
   ret v2
   CFG builder state - includes lookups to avoid mutable module-level state
   which would cause race conditions in parallel test execution
   Fresh MIR registers start above this function's source TempIds and must use ExtraTypeMap.
   Function identity -> return type
   For generating unique labels per function
   Parameter VRegs for self-recursive tail call loop optimization
   VReg IDs that hold float values
   Closure temp -> function identity for return type lookup
   Coverage support
*)
[@@@warning "-4"]

module TempMap = RcTypeFacts.TempMap
module TempSet = ANFEffects.TempSet
module SM = StringOrder.Map
module LM = MIR.LabelMap
module IS = MIR.IntSet

let addInt a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mulInt a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let ( let* ) = Result.bind
let ( let+ ) value action = Result.map action value
let tempText (ANF.TempId id) = "TempId " ^ string_of_int id

let labelText (MIR.Label name) =
  "Label " ^ StructuralFormat.format (StructuralFormat.Text name)

let functionText id = StructuralFormat.format (AST.DiagnosticFormatting.func id)
let tryItem index values = if index < 0 then None else List.nth_opt values index

let item index values =
  match tryItem index values with
  | Some value -> value
  | None ->
      invalid_arg
        "The index was outside the range of elements in the list. (Parameter \
         'index')"

let zip3 a b c = List.map2 (fun (x, y) z -> (x, y, z)) (List.combine a b) c

let mapFold action state values =
  let reversed, state =
    List.fold_left
      (fun (result, state) value ->
        let mapped, next = action state value in
        (mapped :: result, next))
      ([], state) values
  in
  (List.rev reversed, state)

(*
   Helper to create VariantInfo record
*)
let mkVariantInfo name tag fields : MIR.variantInfo =
  {
    MIR.name;
    tag;
    payload =
      (match fields with
      | [] -> None
      | [ field ] -> Some field
      | _ -> Some (AST.TTuple fields));
    fieldCount = List.length fields;
  }

(*
   Helper to create TypeVariants record
*)
let mkTypeVariants typeParams variants : MIR.typeVariants =
  { MIR.typeParams; variants }

(*
   Helper to create RecordField record
*)
let mkRecordField name typ : MIR.recordField = { MIR.name; typ }

(*
   Build VariantRegistry from VariantLookup
   VariantLookup: variantName -> (typeName, typeParams, tagIndex, payloadType)
   VariantRegistry: typeName -> TypeVariants (with named record types)
   Qualified entries are canonical and cannot be overwritten by a
   same-named case from another nominal type. Short entries exist only
   for source resolution.
*)
let buildVariantRegistry (variantLookup : LoweringPrimitives.variantLookup) =
  let entries = SM.bindings variantLookup in
  let canonicalEntries =
    List.filter_map
      (fun (variantName, (typeName, typeParams, tag, payload)) ->
        let prefix = typeName ^ "." in
        if String.starts_with ~prefix variantName then
          Some
            ( typeName,
              typeParams,
              ( String.sub variantName (String.length prefix)
                  (String.length variantName - String.length prefix),
                tag,
                payload ) )
        else None)
      entries
  in
  let canonicalTypes =
    StringOrder.Set.of_list
      (List.map (fun (name, _, _) -> name) canonicalEntries)
  in
  let shortEntries =
    List.filter_map
      (fun (variantName, (typeName, typeParams, tag, payload)) ->
        if StringOrder.Set.mem typeName canonicalTypes then None
        else Some (typeName, typeParams, (variantName, tag, payload)))
      entries
  in
  let groups, order =
    List.fold_left
      (fun (groups, order) ((name, _, _) as entry) ->
        let previous = SM.find_opt name groups in
        ( SM.add name (entry :: Option.value ~default:[] previous) groups,
          if Option.is_none previous then name :: order else order ))
      (SM.empty, [])
      (canonicalEntries @ shortEntries)
  in
  List.fold_left
    (fun result name ->
      let entries = List.rev (SM.find name groups) in
      match entries with
      | [] ->
          Crash.crash
            ("ANF_to_MIR: variant group for type '" ^ name ^ "' had no variants")
      | (_, typeParams, _) :: rest ->
          if List.exists (fun (_, other, _) -> other <> typeParams) rest then
            Crash.crash
              ("ANF_to_MIR: inconsistent type parameters in variant registry \
                for type: " ^ name)
          else
            let variants =
              List.map
                (fun (_, _, (name, tag, payload)) ->
                  mkVariantInfo name tag payload)
                entries
              |> List.stable_sort (fun left right ->
                  Int.compare left.MIR.tag right.MIR.tag)
            in
            SM.add name (mkTypeVariants typeParams variants) result)
    SM.empty (List.rev order)

(*
   Build RecordRegistry from TypeReg
   TypeReg: typeName -> (fieldName, fieldType) list
   RecordRegistry: typeName -> RecordField list
*)
let buildRecordRegistry typeReg =
  SM.map (List.map (fun (name, typ) -> mkRecordField name typ)) typeReg

(*
   Append a list of instructions (in order) to a reversed instruction list
*)
let appendInstrsRev instrs revInstrs = List.rev instrs @ revInstrs

(*
   Map ANF TempId to MIR virtual register
*)
let tempToVReg (ANF.TempId id) = MIR.VReg id

(*
   Find the maximum TempId in an atom (returns -1 if no TempId)
*)
let maxTempIdInAtom = function ANF.Var (ANF.TempId id) -> id | _ -> -1

(*
   Find the maximum TempId across atoms without allocating an intermediate list
*)
let maxTempIdInAtoms atoms =
  List.fold_left
    (fun largest atom -> max largest (maxTempIdInAtom atom))
    (-1) atoms

let maxTempIdWithAtoms first rest =
  max (maxTempIdInAtom first) (maxTempIdInAtoms rest)

(*
   Convert ANF.BinOp to MIR.BinOp
*)
let convertBinOp (op : ANF.binOp) =
  match op with
  | ANF.Add -> MIR.Add
  | ANF.Sub -> MIR.Sub
  | ANF.Mul -> MIR.Mul
  | ANF.Div -> MIR.Div
  | ANF.Mod -> MIR.Mod
  | ANF.Shl -> MIR.Shl
  | ANF.Shr -> MIR.Shr
  | ANF.BitAnd -> MIR.BitAnd
  | ANF.BitOr -> MIR.BitOr
  | ANF.BitXor -> MIR.BitXor
  | ANF.Eq -> MIR.Eq
  | ANF.Neq -> MIR.Neq
  | ANF.Lt -> MIR.Lt
  | ANF.Gt -> MIR.Gt
  | ANF.Lte -> MIR.Lte
  | ANF.Gte -> MIR.Gte
  | ANF.And -> MIR.And
  | ANF.Or -> MIR.Or

(*
   Convert ANF.UnaryOp to MIR.UnaryOp
*)
let convertUnaryOp (op : ANF.unaryOp) =
  match op with
  | ANF.Neg -> MIR.Neg
  | ANF.Not -> MIR.Not
  | ANF.BitNot -> MIR.BitNot

let convertCliOperation (operation : ANF.cliOperation) =
  match operation with
  | ANF.Execute -> MIR.Execute
  | ANF.RunProcess -> MIR.RunProcess
  | ANF.HostOS -> MIR.HostOS
  | ANF.HostArchitecture -> MIR.HostArchitecture
  | ANF.Hostname -> MIR.Hostname
  | ANF.GetEnv -> MIR.GetEnv
  | ANF.GetEnvironmentPacked -> MIR.GetEnvironmentPacked
  | ANF.SetEnv -> MIR.SetEnv
  | ANF.UnsetEnv -> MIR.UnsetEnv
  | ANF.DirectoryCurrent -> MIR.DirectoryCurrent
  | ANF.DirectoryListPacked -> MIR.DirectoryListPacked
  | ANF.FileIsDirectory -> MIR.FileIsDirectory
  | ANF.FileCreateExclusive -> MIR.FileCreateExclusive
  | ANF.GetArgv -> MIR.GetArgv
  | ANF.Kill -> MIR.Kill
  | ANF.GetPid -> MIR.GetPid
  | ANF.GetUid -> MIR.GetUid
  | ANF.CpuCount -> MIR.CpuCount
  | ANF.SpawnProcess -> MIR.SpawnProcess
  | ANF.ProcessIO -> MIR.ProcessIO
  | ANF.TerminateProcess -> MIR.TerminateProcess
  | ANF.SocketTcp4 -> MIR.SocketTcp4
  | ANF.SocketTcp6 -> MIR.SocketTcp6
  | ANF.SocketUdp4 -> MIR.SocketUdp4
  | ANF.SocketUdp6 -> MIR.SocketUdp6
  | ANF.SocketConnect4 -> MIR.SocketConnect4
  | ANF.SocketConnect6 -> MIR.SocketConnect6
  | ANF.SocketSend -> MIR.SocketSend
  | ANF.SocketReceive -> MIR.SocketReceive
  | ANF.SocketReceiveTimeout -> MIR.SocketReceiveTimeout
  | ANF.SocketSendTimeout -> MIR.SocketSendTimeout
  | ANF.SocketClose -> MIR.SocketClose
  | ANF.SocketBind4 -> MIR.SocketBind4
  | ANF.SocketListen -> MIR.SocketListen
  | ANF.SocketAccept -> MIR.SocketAccept
  | ANF.SocketCloexec -> MIR.SocketCloexec
  | ANF.SocketReuseAddress -> MIR.SocketReuseAddress
  | ANF.SocketPoll -> MIR.SocketPoll
  | ANF.SignalBlock -> MIR.SignalBlock
  | ANF.SignalRestore -> MIR.SignalRestore
  | ANF.SignalPending -> MIR.SignalPending
  | ANF.SignalWait -> MIR.SignalWait
  | ANF.MonotonicTime -> MIR.MonotonicTime
  | ANF.SecureRandomFill -> MIR.SecureRandomFill
  | ANF.PosixOpenAt -> MIR.PosixOpenAt
  | ANF.PosixRead -> MIR.PosixRead
  | ANF.PosixWrite -> MIR.PosixWrite
  | ANF.PosixClose -> MIR.PosixClose
  | ANF.PosixSeek -> MIR.PosixSeek
  | ANF.PosixStatAt -> MIR.PosixStatAt
  | ANF.PosixGetCwd -> MIR.PosixGetCwd
  | ANF.PosixChdir -> MIR.PosixChdir
  | ANF.PosixMkdirAt -> MIR.PosixMkdirAt
  | ANF.PosixUnlinkAt -> MIR.PosixUnlinkAt
  | ANF.PosixRenameAt -> MIR.PosixRenameAt
  | ANF.PosixChmodAt -> MIR.PosixChmodAt
  | ANF.PosixChmodAt2 -> MIR.PosixChmodAt2
  | ANF.PosixUtimesAt -> MIR.PosixUtimesAt
  | ANF.PosixSetAttributesAt -> MIR.PosixSetAttributesAt
  | ANF.PosixSymlinkAt -> MIR.PosixSymlinkAt
  | ANF.PosixReadlinkAt -> MIR.PosixReadlinkAt
  | ANF.PosixFlock -> MIR.PosixFlock
  | ANF.PosixGetDents -> MIR.PosixGetDents

(*
   Precomputed descriptions for primitive ops (avoids formatting on hot path)
*)
let binOpDescription (op : ANF.binOp) =
  match op with
  | ANF.Add -> "Prim Add"
  | ANF.Sub -> "Prim Sub"
  | ANF.Mul -> "Prim Mul"
  | ANF.Div -> "Prim Div"
  | ANF.Mod -> "Prim Mod"
  | ANF.Shl -> "Prim Shl"
  | ANF.Shr -> "Prim Shr"
  | ANF.BitAnd -> "Prim BitAnd"
  | ANF.BitOr -> "Prim BitOr"
  | ANF.BitXor -> "Prim BitXor"
  | ANF.Eq -> "Prim Eq"
  | ANF.Neq -> "Prim Neq"
  | ANF.Lt -> "Prim Lt"
  | ANF.Gt -> "Prim Gt"
  | ANF.Lte -> "Prim Lte"
  | ANF.Gte -> "Prim Gte"
  | ANF.And -> "Prim And"
  | ANF.Or -> "Prim Or"

(*
   Precomputed descriptions for unary ops (avoids formatting on hot path)
*)
let unaryOpDescription (op : ANF.unaryOp) =
  match op with
  | ANF.Neg -> "UnaryPrim Neg"
  | ANF.Not -> "UnaryPrim Not"
  | ANF.BitNot -> "UnaryPrim BitNot"

(*
   Find the maximum TempId in a CExpr
   No atoms, so no TempIds
*)
let maxTempIdInCExpr (cexpr : ANF.cExpr) =
  match cexpr with
  | ANF.Atom atom -> maxTempIdInAtom atom
  | ANF.TypedAtom (atom, _) -> maxTempIdInAtom atom
  | ANF.Prim (_, left, right) ->
      max (maxTempIdInAtom left) (maxTempIdInAtom right)
  | ANF.UnaryPrim (_, atom) -> maxTempIdInAtom atom
  | ANF.IfValue (cond, thenVal, elseVal) ->
      max (maxTempIdInAtom cond)
        (max (maxTempIdInAtom thenVal) (maxTempIdInAtom elseVal))
  | ANF.Call (_, args) | ANF.BorrowedCall (_, args) -> maxTempIdInAtoms args
  | ANF.TailCall (_, args) -> maxTempIdInAtoms args
  | ANF.IndirectCall (func, args) -> maxTempIdWithAtoms func args
  | ANF.IndirectTailCall (func, args) -> maxTempIdWithAtoms func args
  | ANF.TupleAlloc atoms -> maxTempIdInAtoms atoms
  | ANF.TupleGet (tuple, _) -> maxTempIdInAtom tuple
  | ANF.RecordAlloc (_, fields) -> maxTempIdInAtoms fields
  | ANF.RecordGet (_, record, _) -> maxTempIdInAtom record
  | ANF.RecordClone (_, record, fields) | ANF.RecordReuse (_, _, record, fields)
    ->
      maxTempIdWithAtoms record fields
  | ANF.StringConcat (first, second, remaining) ->
      maxTempIdInAtoms (first :: second :: remaining)
  | ANF.CanonicalBufferEq (_, left, right) ->
      max (maxTempIdInAtom left) (maxTempIdInAtom right)
  | ANF.RefCountInc (atom, _, _, _) -> maxTempIdInAtom atom
  | ANF.RefCountDec (atom, _, _, _) -> maxTempIdInAtom atom
  | ANF.Print (atom, _) -> maxTempIdInAtom atom
  | ANF.StdoutWrite (atom, _) -> maxTempIdInAtom atom
  | ANF.StdinReadLine -> -1
  | ANF.RuntimeError _ -> -1
  | ANF.RuntimeErrorString atom -> maxTempIdInAtom atom
  | ANF.ClosureAlloc (_, captures) -> maxTempIdInAtoms captures
  | ANF.ClosureCall (closure, args) -> maxTempIdWithAtoms closure args
  | ANF.ClosureTailCall (closure, args) -> maxTempIdWithAtoms closure args
  | ANF.FileReadBlob path -> maxTempIdInAtom path
  | ANF.FileExists path -> maxTempIdInAtom path
  | ANF.FileWriteBlob (path, content) ->
      max (maxTempIdInAtom path) (maxTempIdInAtom content)
  | ANF.FileAppendText (path, content) ->
      max (maxTempIdInAtom path) (maxTempIdInAtom content)
  | ANF.FileDelete path -> maxTempIdInAtom path
  | ANF.FileCreateDirectory path -> maxTempIdInAtom path
  | ANF.FileSetExecutable path -> maxTempIdInAtom path
  | ANF.FileWriteFromPtr (path, ptr, length) ->
      max (maxTempIdInAtom path)
        (max (maxTempIdInAtom ptr) (maxTempIdInAtom length))
  | ANF.RawAlloc numBytes -> maxTempIdInAtom numBytes
  | ANF.MappedAlloc numBytes -> maxTempIdInAtom numBytes
  | ANF.RawFree ptr -> maxTempIdInAtom ptr
  | ANF.MappedFree ptr -> maxTempIdInAtom ptr
  | ANF.RawGet (ptr, offset, _) | ANF.RawTake (ptr, offset, _) ->
      max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
  | ANF.RawGetByte (ptr, offset) ->
      max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
  | ANF.RawWriteWord (ptr, offset, value) ->
      max (maxTempIdInAtom ptr)
        (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
  | ANF.RawWriteByte (ptr, offset, value) ->
      max (maxTempIdInAtom ptr)
        (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
  | ANF.RawSlotInit (ptr, offset, value, _) ->
      max (maxTempIdInAtom ptr)
        (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
  | ANF.StringToRawPtr value -> maxTempIdInAtom value
  | ANF.RawPtrToString ptr -> maxTempIdInAtom ptr
  | ANF.BlobToRawPtr value -> maxTempIdInAtom value
  | ANF.RawPtrToBlob ptr -> maxTempIdInAtom ptr
  | ANF.RawPtrToInt128 ptr -> maxTempIdInAtom ptr
  | ANF.RawPtrToUInt128 ptr -> maxTempIdInAtom ptr
  | ANF.DictToRawPtr dict -> maxTempIdInAtom dict
  | ANF.RawPtrToDict (ptr, tag, _) ->
      max (maxTempIdInAtom ptr) (maxTempIdInAtom tag)
  | ANF.ListToRawPtr list -> maxTempIdInAtom list
  | ANF.FixedBlockToRawPtr value -> maxTempIdInAtom value
  | ANF.RawPtrToList (ptr, tag, _) ->
      max (maxTempIdInAtom ptr) (maxTempIdInAtom tag)
  | ANF.FloatSqrt atom -> maxTempIdInAtom atom
  | ANF.FloatAbs atom -> maxTempIdInAtom atom
  | ANF.FloatNeg atom -> maxTempIdInAtom atom
  | ANF.Int64ToFloat atom -> maxTempIdInAtom atom
  | ANF.FloatToInt64 atom -> maxTempIdInAtom atom
  | ANF.FloatToBits atom -> maxTempIdInAtom atom
  | ANF.RefCountIncString str -> maxTempIdInAtom str
  | ANF.RefCountDecString str -> maxTempIdInAtom str
  | ANF.RefCountIncBlob bytes -> maxTempIdInAtom bytes
  | ANF.RefCountDecBlob bytes -> maxTempIdInAtom bytes
  | ANF.RefCountIncInt value -> maxTempIdInAtom value
  | ANF.RefCountDecInt value -> maxTempIdInAtom value
  | ANF.RandomInt64 -> -1
  | ANF.DateTimeNow -> -1
  | ANF.Sleep delayMs -> maxTempIdInAtom delayMs
  | ANF.CliNative (_, args) -> maxTempIdInAtoms args
  | ANF.FloatToString atom -> maxTempIdInAtom atom

(*
   Find the maximum TempId in an AExpr
*)
let rec maxTempIdInAExpr = function
  | ANF.Let (ANF.TempId id, cexpr, body) ->
      max id (max (maxTempIdInCExpr cexpr) (maxTempIdInAExpr body))
  | ANF.Return atom -> maxTempIdInAtom atom
  | ANF.Jump (ANF.TempId target, atom) -> max target (maxTempIdInAtom atom)
  | ANF.Join (param, continuation, entry) ->
      let (ANF.TempId id) = param.ANF.id in
      max id (max (maxTempIdInAExpr continuation) (maxTempIdInAExpr entry))
  | ANF.If (cond, yes, no) ->
      max (maxTempIdInAtom cond)
        (max (maxTempIdInAExpr yes) (maxTempIdInAExpr no))

(*
   Find the maximum TempId in a function
*)
let maxTempIdInFunction (func : ANF.functionDef) =
  max
    (List.fold_left
       (fun largest (param : ANF.typedParam) ->
         let (ANF.TempId id) = param.ANF.id in
         max largest id)
       (-1) func.ANF.typedParams)
    (maxTempIdInAExpr func.ANF.body)

(*
   Find the maximum TempId in an ANF program
*)
let maxTempIdInProgram (ANF.Program (functions, main)) =
  max
    (List.fold_left
       (fun largest func -> max largest (maxTempIdInFunction func))
       (-1) functions)
    (maxTempIdInAExpr main)

(*
   Helper to check if an atom is a float value
*)
let isFloatAtom floatRegs = function
  | ANF.FloatLiteral _ -> true
  | ANF.Var (ANF.TempId id) -> IS.mem id floatRegs
  | _ -> false

(*
   Helper to check if a CExpr produces a float value
   returnTypeReg: map from function name to return type (for checking Call results)
   Comparisons and boolean ops always produce Bool, not Float
   Arithmetic ops produce float if either operand is float
   IfValue produces a float if either branch produces a float
   (then and else should have the same type, so we check then)
   Check if the called function returns a float
   Indirect calls - we don't know the return type, assume not float
   Closure calls - we don't know the return type, assume not float
*)
let cexprProducesFloat floatRegs returnTypeReg = function
  | ANF.Prim
      ( ( ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte | ANF.And
        | ANF.Or ),
        _,
        _ ) ->
      false
  | ANF.Prim (_, left, right) ->
      isFloatAtom floatRegs left || isFloatAtom floatRegs right
  | ANF.FloatSqrt _ | ANF.FloatAbs _ | ANF.FloatNeg _ | ANF.Int64ToFloat _ ->
      true
  | ANF.Atom atom -> isFloatAtom floatRegs atom
  | ANF.IfValue (_, yes, _) -> isFloatAtom floatRegs yes
  | ANF.Call (name, _) | ANF.BorrowedCall (name, _) | ANF.TailCall (name, _) ->
      FunctionIdMap.tryFind name returnTypeReg = Some AST.TFloat64
  | _ -> false

(*
   Build a map from function name to return type for all functions
   ANF functions are already typed, so their declared return types are authoritative.
   externalReturnTypes: return types for functions not in `functions` (e.g., specialized functions compiled elsewhere)
*)
let buildReturnTypeReg functions externalTypes =
  List.fold_left
    (fun map (func : ANF.functionDef) ->
      FunctionIdMap.add func.ANF.id func.ANF.returnType map)
    (FunctionIdMap.map (fun _ (_, typ) -> typ) externalTypes)
    functions

(*
   Return type for monomorphized intrinsics not tracked in the return type registry
*)
let tryGetIntrinsicReturnType name =
  if name = "Builtin.pmFindValuesByValueType" then
    Some (AST.TList (AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])))
  else if name = "Builtin.pmGetLocationsByValue" then
    Some
      (AST.TList
         (AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])))
  else if String.starts_with ~prefix:"__raw_get_" name then
    Crash.crash
      ("ANF_to_MIR: monomorphized raw_get return type missing from registry: "
     ^ name)
  else if String.starts_with ~prefix:"__raw_take_" name then
    Crash.crash
      ("ANF_to_MIR: monomorphized raw_take return type missing from registry: "
     ^ name)
  else
    List.find_map
      (fun (prefix, typ) ->
        if String.starts_with ~prefix name then Some typ else None)
      [
        ("__stream_to_rawptr_", AST.TInternalRawPtr);
        ("__raw_slot_init_", AST.TUnit);
        ("__hash_", AST.TInt64);
        ("__key_eq_", AST.TBool);
        ("__empty_dict_", AST.TInt64);
        ("__dict_is_null_", AST.TBool);
        ("__dict_get_tag_", AST.TInt64);
        ("__dict_to_rawptr_", AST.TInternalRawPtr);
        ("__rawptr_to_dict_", AST.TDict (AST.TVar "k", AST.TVar "v"));
        ("__list_is_null_", AST.TBool);
        ("__list_get_tag_", AST.TInt64);
        ("__list_to_rawptr_", AST.TInternalRawPtr);
        ("__rawptr_to_list_", AST.TList (AST.TVar "a"));
      ]

type cfgBuilder = {
  blocks : MIR.basicBlock LM.t;
  joins : (MIR.label * AST.semanticType) TempMap.t;
  joinIncoming : (MIR.operand * MIR.label) list TempMap.t;
  selfTailIncoming : (MIR.label * MIR.operand list) list;
  labelGen : MIR.labelGen;
  regGen : MIR.regGen;
  typeById : ANF.typeMap;
  sourceTempIdMax : int;
  extraTypeMap : AST.semanticType TempMap.t;
  typeReg : (string * AST.semanticType) list SM.t;
  returnTypeReg : AST.semanticType FunctionIdMap.t;
  functionNames : string FunctionIdMap.t;
  funcId : AST.functionId;
  funcName : string;
  paramRegs : MIR.vReg list;
  floatRegs : IS.t;
  closureFuncs : AST.functionId TempMap.t;
  enableCoverage : bool;
  exprIdGen : ANF.exprIdGen;
  coverageMapping : ANF.coverageMapping;
}

(*
   Lookup a TempId by raw integer id, checking extra types for newly created regs
*)
let tryFindTypeById builder id =
  match TempMap.find_opt (ANF.TempId id) builder.extraTypeMap with
  | Some typ -> Some typ
  | None when id >= 0 && id <= builder.sourceTempIdMax ->
      ANF.TypeMap.tryFind (ANF.TempId id) builder.typeById
  | None -> None

(*
   Lookup a TempId, checking extra types for newly created regs
*)
let tryFindType builder (ANF.TempId id) = tryFindTypeById builder id

(*
   Convert ANF Atom to MIR Operand using lookups from builder
   Returns Error if float/string lookup fails (internal invariant violation)
   Unit is represented as 0
*)
let atomToOperand (_builder : cfgBuilder) = function
  | ANF.UnitLiteral -> Ok (MIR.Int64Const 0L)
  | ANF.IntLiteral n -> Ok (MIR.Int64Const (ANF.sizedIntToInt64 n))
  | ANF.BoolLiteral b -> Ok (MIR.BoolConst b)
  | ANF.FloatLiteral f -> Ok (MIR.FloatSymbol f)
  | ANF.StringLiteral s -> Ok (MIR.StringSymbol s)
  | ANF.Var tid -> Ok (MIR.Register (tempToVReg tid))
  | ANF.FuncRef name -> Ok (MIR.FuncAddr name)

let rcKindToMIR = function
  | MemoryModel.GenericHeap -> MIR.GenericHeap
  | MemoryModel.StreamHeap -> MIR.StreamHeap
  | MemoryModel.TaggedList -> MIR.TaggedList
  | MemoryModel.DictHeap -> MIR.DictHeap
  | MemoryModel.ClosureHeap -> MIR.ClosureHeap

(*
   Get the type of an ANF Atom (for generating type-specific instructions)
   Use the actual type from SizedInt
   Check if this VReg is known to hold a float
   TypeMap is populated by RefCountInsertion. If we reach
   here, a later pass created a TempId without tracking it.
   Function addresses are pointer-sized
*)
let atomType builder = function
  | ANF.UnitLiteral -> AST.TUnit
  | ANF.IntLiteral n -> ANF.sizedIntToType n
  | ANF.BoolLiteral _ -> AST.TBool
  | ANF.StringLiteral _ -> AST.TString
  | ANF.FloatLiteral _ -> AST.TFloat64
  | ANF.FuncRef _ -> AST.TInt64
  | ANF.Var (ANF.TempId id) -> (
      if IS.mem id builder.floatRegs then AST.TFloat64
      else
        match tryFindTypeById builder id with
        | Some typ -> typ
        | None ->
            Crash.crash
              (Printf.sprintf
                 "atomType: unknown type for TempId %d - TempId created after \
                  RefCountInsertion?"
                 id))

(*
   Get the operand type for a binary operation (checks both operands)
   If either operand is float, the operation is float
*)
let binOpType builder left right =
  let leftType = atomType builder left in
  let rightType = atomType builder right in
  match (leftType, rightType) with
  | AST.TFloat64, _ | _, AST.TFloat64 -> AST.TFloat64
  | _ -> leftType

(*
   Resolve the result type for a closure call.
   A closure temp may still have its concrete allocation target in ClosureFuncs;
   higher-order values passed through parameters or containers are resolved from
   the already-required ANF result temp type.
*)
let closureCallReturnType builder tempId closure =
  let resultType () =
    match tryFindType builder tempId with
    | Some (AST.TFunction (_, ret)) -> ret
    | Some typ -> typ
    | None ->
        Crash.crash ("ClosureCall: Return type not found for " ^ tempText tempId)
  in
  match closure with
  | ANF.Var id -> (
      match TempMap.find_opt id builder.closureFuncs with
      | Some name -> (
          match FunctionIdMap.tryFind name builder.returnTypeReg with
          | Some typ -> typ
          | None -> resultType ())
      | None -> resultType ())
  | _ -> resultType ()

let directCallReturnType builder funcName =
  match FunctionIdMap.tryFind funcName builder.returnTypeReg with
  | Some typ -> typ
  | None -> (
      let name =
        match FunctionIdMap.tryFind funcName builder.functionNames with
        | Some name -> name
        | None ->
            Crash.crash "MIR lowering lost direct-call function name metadata"
      in
      match tryGetIntrinsicReturnType name with
      | Some typ -> typ
      | None when String.starts_with ~prefix:"__dark_eq_" name -> AST.TBool
      | None when String.starts_with ~prefix:"Builtin.pmEvaluateValue_" name ->
          let prefix = "Builtin.pmEvaluateValue_" in
          let suffix =
            String.sub name (String.length prefix)
              (String.length name - String.length prefix)
          in
          let typ =
            match suffix with
            | "i8" -> AST.TInt8
            | "i16" -> AST.TInt16
            | "i32" -> AST.TInt32
            | "i64" -> AST.TInt64
            | "i128" -> AST.TInt128
            | "int" -> AST.TInt
            | "u8" -> AST.TUInt8
            | "u16" -> AST.TUInt16
            | "u32" -> AST.TUInt32
            | "u64" -> AST.TUInt64
            | "u128" -> AST.TUInt128
            | "bool" -> AST.TBool
            | "f64" -> AST.TFloat64
            | "str" -> AST.TString
            | "blob" -> AST.TBlob
            | "char" -> AST.TChar
            | "datetime" -> AST.TDateTime
            | "unit" -> AST.TUnit
            | nominal when SM.mem nominal builder.typeReg ->
                AST.TRecord (nominal, [])
            | nominal -> AST.TSum (nominal, [])
          in
          AST.TSum ("Darklang.Stdlib.Option.Option", [ typ ])
      | None ->
          Crash.crash
            ("ANF_to_MIR: Return type not found for function identity " ^ name))

(*
   List is a Cons cell: (tag, head, tail) - index 1 is head
   Function returning list - extract list element type.
   Sum type: [tag:8][payload:8], index 1 is payload.
*)
let tupleGetDestType builder tempId aliasType tupleId index =
  match aliasType with
  | Some AST.TFloat64 -> Some AST.TFloat64
  | _ -> (
      match tryFindType builder tempId with
      | Some AST.TFloat64 -> Some AST.TFloat64
      | _ -> (
          match tryFindType builder tupleId with
          | Some (AST.TTuple elems) when index < List.length elems ->
              if item index elems = AST.TFloat64 then Some AST.TFloat64
              else None
          | Some (AST.TList elem) | Some (AST.TFunction (_, AST.TList elem)) ->
              if index = 1 && elem = AST.TFloat64 then Some AST.TFloat64
              else None
          | Some (AST.TSum (_, [ AST.TFloat64 ])) when index = 1 ->
              Some AST.TFloat64
          | _ -> None))

let inferSimpleCExprDestType builder tempId aliasType = function
  | ANF.Atom atom -> Some (atomType builder atom)
  | ANF.TypedAtom (_, typ) -> Some typ
  | ANF.Prim
      ( ( ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte | ANF.And
        | ANF.Or ),
        _,
        _ ) ->
      Some AST.TBool
  | ANF.Prim (_, left, right) -> Some (binOpType builder left right)
  | ANF.CanonicalBufferEq _ -> Some AST.TBool
  | ANF.UnaryPrim (ANF.Not, _) -> Some AST.TBool
  | ANF.UnaryPrim (_, atom) -> Some (atomType builder atom)
  | ANF.Call (name, _) | ANF.BorrowedCall (name, _) ->
      Some (directCallReturnType builder name)
  | ANF.IndirectCall (func, _) -> (
      match atomType builder func with
      | AST.TFunction (_, ret) -> Some ret
      | AST.TInternalRawPtr | AST.TInt64 -> Some AST.TBool
      | typ ->
          Crash.crash
            ("IndirectCall: Expected TFunction type for func, got "
            ^ StructuralFormat.semanticType typ))
  | ANF.ClosureCall (closure, _) ->
      Some (closureCallReturnType builder tempId closure)
  | ANF.TupleGet (ANF.Var tuple, index) ->
      tupleGetDestType builder tempId aliasType tuple index
  | ANF.RecordAlloc (desc, _)
  | ANF.RecordClone (desc, _, _)
  | ANF.RecordReuse (_, desc, _, _) ->
      Some desc.ANF.valueType
  | ANF.RecordGet (desc, _, index) ->
      Option.map snd (tryItem index desc.ANF.fields)
  | ANF.StringToRawPtr _ | ANF.BlobToRawPtr _ | ANF.DictToRawPtr _
  | ANF.ListToRawPtr _ | ANF.FixedBlockToRawPtr _ ->
      Some AST.TInternalRawPtr
  | ANF.RawPtrToString _ -> Some AST.TString
  | ANF.RawPtrToBlob _ -> Some AST.TBlob
  | ANF.RawPtrToInt128 _ -> Some AST.TInt128
  | ANF.RawPtrToUInt128 _ -> Some AST.TUInt128
  | ANF.RawPtrToDict (_, _, typ) | ANF.RawPtrToList (_, _, typ) -> Some typ
  | ANF.FloatSqrt _ | ANF.FloatAbs _ | ANF.FloatNeg _ | ANF.Int64ToFloat _ ->
      Some AST.TFloat64
  | ANF.FloatToInt64 _ -> Some AST.TInt64
  | ANF.FloatToBits _ -> Some AST.TUInt64
  | _ -> None

(*
   Get the type of an MIR operand (for generating type-specific instructions)
   Function addresses are pointer-sized
   Check if this VReg is known to hold a float or has a tracked type
*)
let operandType builder = function
  | MIR.Int64Const _ | MIR.FuncAddr _ -> AST.TInt64
  | MIR.BoolConst _ -> AST.TBool
  | MIR.FloatSymbol _ -> AST.TFloat64
  | MIR.StringSymbol _ -> AST.TString
  | MIR.Register (MIR.VReg id) -> (
      if IS.mem id builder.floatRegs then AST.TFloat64
      else
        match tryFindTypeById builder id with
        | Some typ -> typ
        | None ->
            Crash.crash (Printf.sprintf "operandType: missing type for v%d" id))

let cliOperationName = function
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
  | ANF.PosixOpenAt -> "PosixOpenAt"
  | ANF.PosixRead -> "PosixRead"
  | ANF.PosixWrite -> "PosixWrite"
  | ANF.PosixClose -> "PosixClose"
  | ANF.PosixSeek -> "PosixSeek"
  | ANF.PosixStatAt -> "PosixStatAt"
  | ANF.PosixGetCwd -> "PosixGetCwd"
  | ANF.PosixChdir -> "PosixChdir"
  | ANF.PosixMkdirAt -> "PosixMkdirAt"
  | ANF.PosixUnlinkAt -> "PosixUnlinkAt"
  | ANF.PosixRenameAt -> "PosixRenameAt"
  | ANF.PosixChmodAt -> "PosixChmodAt"
  | ANF.PosixChmodAt2 -> "PosixChmodAt2"
  | ANF.PosixUtimesAt -> "PosixUtimesAt"
  | ANF.PosixSetAttributesAt -> "PosixSetAttributesAt"
  | ANF.PosixSymlinkAt -> "PosixSymlinkAt"
  | ANF.PosixReadlinkAt -> "PosixReadlinkAt"
  | ANF.PosixFlock -> "PosixFlock"
  | ANF.PosixGetDents -> "PosixGetDents"

(*
   Generate description for a CExpr (for coverage mapping)
*)
let cexprDescription cexpr =
  match cexpr with
  | ANF.Atom _ -> "Atom"
  | ANF.TypedAtom _ -> "TypedAtom"
  | ANF.Prim (op, _, _) -> binOpDescription op
  | ANF.UnaryPrim (op, _) -> unaryOpDescription op
  | ANF.IfValue _ -> "IfValue"
  | ANF.Call (name, _) -> "Call " ^ functionText name
  | ANF.BorrowedCall (name, _) -> "BorrowedCall " ^ functionText name
  | ANF.TailCall (name, _) -> "TailCall " ^ functionText name
  | ANF.IndirectCall _ -> "IndirectCall"
  | ANF.IndirectTailCall _ -> "IndirectTailCall"
  | ANF.ClosureAlloc (name, _) -> "ClosureAlloc " ^ functionText name
  | ANF.ClosureCall _ -> "ClosureCall"
  | ANF.ClosureTailCall _ -> "ClosureTailCall"
  | ANF.TupleAlloc _ -> "TupleAlloc"
  | ANF.TupleGet _ -> "TupleGet"
  | ANF.RecordAlloc (descriptor, _) ->
      "RecordAlloc " ^ descriptor.ANF.runtimeTypeName
  | ANF.RecordGet (descriptor, _, _) ->
      "RecordGet " ^ descriptor.ANF.runtimeTypeName
  | ANF.RecordClone (descriptor, _, _) ->
      "RecordClone " ^ descriptor.ANF.runtimeTypeName
  | ANF.RecordReuse (_, descriptor, _, _) ->
      "RecordReuse " ^ descriptor.ANF.runtimeTypeName
  | ANF.StringConcat _ -> "StringConcat"
  | ANF.CanonicalBufferEq _ -> "CanonicalBufferEq"
  | ANF.RefCountInc _ -> "RefCountInc"
  | ANF.RefCountDec _ -> "RefCountDec"
  | ANF.Print _ -> "Print"
  | ANF.StdoutWrite _ -> "StdoutWrite"
  | ANF.StdinReadLine -> "StdinReadLine"
  | ANF.RuntimeError _ -> "RuntimeError"
  | ANF.RuntimeErrorString _ -> "RuntimeErrorString"
  | ANF.FileReadBlob _ -> "FileReadBlob"
  | ANF.FileExists _ -> "FileExists"
  | ANF.FileWriteBlob _ -> "FileWriteBlob"
  | ANF.FileAppendText _ -> "FileAppendText"
  | ANF.FileDelete _ -> "FileDelete"
  | ANF.FileCreateDirectory _ -> "FileCreateDirectory"
  | ANF.FileSetExecutable _ -> "FileSetExecutable"
  | ANF.FileWriteFromPtr _ -> "FileWriteFromPtr"
  | ANF.FloatSqrt _ -> "FloatSqrt"
  | ANF.FloatAbs _ -> "FloatAbs"
  | ANF.FloatNeg _ -> "FloatNeg"
  | ANF.Int64ToFloat _ -> "Int64ToFloat"
  | ANF.FloatToInt64 _ -> "FloatToInt64"
  | ANF.FloatToBits _ -> "FloatToBits"
  | ANF.RawAlloc _ -> "RawAlloc"
  | ANF.MappedAlloc _ -> "MappedAlloc"
  | ANF.RawFree _ -> "RawFree"
  | ANF.MappedFree _ -> "MappedFree"
  | ANF.RawGet _ -> "RawGet"
  | ANF.RawTake _ -> "RawTake"
  | ANF.RawGetByte _ -> "RawGetByte"
  | ANF.RawWriteWord _ -> "RawWriteWord"
  | ANF.RawWriteByte _ -> "RawWriteByte"
  | ANF.RawSlotInit _ -> "RawSlotInit"
  | ANF.StringToRawPtr _ -> "StringToRawPtr"
  | ANF.RawPtrToString _ -> "RawPtrToString"
  | ANF.BlobToRawPtr _ -> "BlobToRawPtr"
  | ANF.RawPtrToBlob _ -> "RawPtrToBlob"
  | ANF.RawPtrToInt128 _ -> "RawPtrToInt128"
  | ANF.RawPtrToUInt128 _ -> "RawPtrToUInt128"
  | ANF.DictToRawPtr _ -> "DictToRawPtr"
  | ANF.RawPtrToDict _ -> "RawPtrToDict"
  | ANF.ListToRawPtr _ -> "ListToRawPtr"
  | ANF.FixedBlockToRawPtr _ -> "FixedBlockToRawPtr"
  | ANF.RawPtrToList _ -> "RawPtrToList"
  | ANF.RefCountIncString _ -> "RefCountIncString"
  | ANF.RefCountDecString _ -> "RefCountDecString"
  | ANF.RefCountIncBlob _ -> "RefCountIncBlob"
  | ANF.RefCountDecBlob _ -> "RefCountDecBlob"
  | ANF.RefCountIncInt _ -> "RefCountIncInt"
  | ANF.RefCountDecInt _ -> "RefCountDecInt"
  | ANF.RandomInt64 -> "RandomInt64"
  | ANF.DateTimeNow -> "DateTimeNow"
  | ANF.Sleep _ -> "Sleep"
  | ANF.CliNative (operation, _) -> "CliNative " ^ cliOperationName operation
  | ANF.FloatToString _ -> "FloatToString"

(*
   Generate coverage instrumentation for an expression
   Returns: (CoverageHit instruction option, updated builder with new ExprId)
*)
let withCoverage builder cexpr =
  if builder.enableCoverage then
    let exprId, exprIdGen = ANF.freshExprId builder.exprIdGen in
    let description = builder.funcName ^ ": " ^ cexprDescription cexpr in
    let coverageMapping =
      ANF.addCoverageEntry exprId description builder.coverageMapping
    in
    ([ MIR.CoverageHit exprId ], { builder with exprIdGen; coverageMapping })
  else ([], builder)

(*
   Collect cleanup operations that must run before a self-tailcall loop jump.
   Expected shape after TailCallDetection:
   Let(callTmp, TailCall(...), Let(_, RefCountDec..., ... Return(callTmp)))
*)
let rec collectSelfTailCallCleanup builder callTempId = function
  | ANF.Return (ANF.Var tid) when tid = callTempId -> Ok []
  | ANF.Let (_, ANF.RefCountDec (ANF.Var tid, size, kind, typ), rest) ->
      let+ instrs = collectSelfTailCallCleanup builder callTempId rest in
      MIR.RefCountDec (tempToVReg tid, size, rcKindToMIR kind, typ) :: instrs
  | ANF.Let (_, ANF.RefCountDec _, _) ->
      Error
        "Internal error: RefCountDec in self-tailcall cleanup on non-variable"
  | ANF.Let (_, ANF.RefCountDecString atom, rest) ->
      let* operand = atomToOperand builder atom in
      let+ instrs = collectSelfTailCallCleanup builder callTempId rest in
      MIR.RefCountDecString operand :: instrs
  | ANF.Let (_, ANF.RefCountDecBlob atom, rest) ->
      let* operand = atomToOperand builder atom in
      let+ instrs = collectSelfTailCallCleanup builder callTempId rest in
      MIR.RefCountDecBlob operand :: instrs
  | ANF.Let (_, ANF.RefCountDecInt atom, rest) ->
      let* operand = atomToOperand builder atom in
      let+ instrs = collectSelfTailCallCleanup builder callTempId rest in
      MIR.RefCountDecInt operand :: instrs
  | _ ->
      Error
        ("Internal error: unexpected expression after self tailcall in "
       ^ builder.funcName)

(*
   RC insertion places owned loop-state releases immediately before the call
   so tail-call validation can account for them. Pull that contiguous suffix
   back out before lowering: arguments must be captured, and overlaps retained,
   before any obsolete state is released.
*)
let collectPreSelfTailCallCleanup instrsRev =
  let isCleanup = function
    | MIR.RefCountDec _ | MIR.RefCountDecString _ | MIR.RefCountDecBlob _ ->
        true
    | _ -> false
  in
  let rec loop cleanup remaining =
    match remaining with
    | instr :: rest when isCleanup instr -> loop (instr :: cleanup) rest
    | _ -> (cleanup, remaining)
  in
  loop [] instrsRev

(*
   Transfer cleanup-owned edges that also occur in the next argument vector.
   The first destination adopts the existing edge, so its decrement disappears;
   only additional destinations require retains.
*)
let transferOverlappingArgOwnership argOperands cleanupInstrs existingInstrsRev
    =
  let decInfos =
    MIR.VRegMap.of_list
      (List.filter_map
         (function
           | MIR.RefCountDec (vreg, size, kind, typ) ->
               Some (vreg, (size, kind, typ))
           | _ -> None)
         cleanupInstrs)
  in
  let aliases =
    List.fold_left
      (fun aliases -> function
        | MIR.Mov (dest, MIR.Register src, _) ->
            MIR.VRegMap.add dest src aliases
        | _ -> aliases)
      MIR.VRegMap.empty existingInstrsRev
  in
  let rec findTarget vreg visited =
    if MIR.VRegSet.mem vreg visited then None
    else
      match MIR.VRegMap.find_opt vreg decInfos with
      | Some info -> Some (vreg, info)
      | None -> (
          match MIR.VRegMap.find_opt vreg aliases with
          | Some next -> findTarget next (MIR.VRegSet.add vreg visited)
          | None -> None)
  in
  let overlapCounts =
    List.fold_left
      (fun counts -> function
        | MIR.Register vreg -> (
            match findTarget vreg MIR.VRegSet.empty with
            | Some (target, _) ->
                MIR.VRegMap.update target
                  (fun value -> Some (addInt (Option.value ~default:0 value) 1))
                  counts
            | None -> counts)
        | _ -> counts)
      MIR.VRegMap.empty argOperands
  in
  let overlapIncs =
    List.concat_map
      (function
        | MIR.RefCountDec (vreg, size, kind, typ) ->
            let edges =
              max 0
                (addInt
                   (Option.value ~default:0
                      (MIR.VRegMap.find_opt vreg overlapCounts))
                   (-1))
            in
            List.init edges (fun _ -> MIR.RefCountInc (vreg, size, kind, typ))
        | _ -> [])
      cleanupInstrs
  in
  let cleanup =
    List.filter
      (function
        | MIR.RefCountDec (vreg, _, _, _) ->
            not (MIR.VRegMap.mem vreg overlapCounts)
        | _ -> true)
      cleanupInstrs
  in
  (overlapIncs, cleanup)

(*
   Only value-producing exits may be redirected into an enclosing value join.
   A terminal transfer has no result register or patchable return block.
*)
type exprExit = Returned of MIR.operand * MIR.label | Terminated

(*
   Redirect a value exit without inspecting a label's spelling or fabricating
   an operand for a path that cannot reach the continuation.
*)
let redirectReturn joinLabel exit builder =
  match exit with
  | Terminated -> Ok (builder, [])
  | Returned (operand, label) -> (
      match LM.find_opt label builder.blocks with
      | Some ({ MIR.terminator = MIR.Ret _; _ } as block) ->
          Ok
            ( {
                builder with
                blocks =
                  LM.add label
                    { block with MIR.terminator = MIR.Jump joinLabel }
                    builder.blocks;
              },
              [ (operand, label) ] )
      | Some _ -> Error "ANF to MIR: value exit does not end in a return"
      | None -> Error "ANF to MIR: value exit block is missing")

let simpleCExprInstrs builder tempId destReg destType cexpr =
  let operand = atomToOperand builder in
  let unary action atom =
    let+ op = operand atom in
    [ action op ]
  in
  let binary action left right =
    let* l = operand left in
    let+ r = operand right in
    [ action l r ]
  in
  let ternary action first second third =
    let* a = operand first in
    let* b = operand second in
    let+ c = operand third in
    [ action a b c ]
  in
  let arguments args = ResultList.sequenceResults (List.map operand args) in
  let storeFields fields =
    ResultList.sequenceResults
      (List.mapi
         (fun index field ->
           let typ = atomType builder field in
           let valueType =
             if typ = AST.TFloat64 then Some AST.TFloat64 else None
           in
           let+ op = operand field in
           MIR.HeapStore (destReg, mulInt index 8, op, valueType))
         fields)
  in
  match cexpr with
  | ANF.Atom atom ->
      let typ = atomType builder atom in
      unary (fun op -> MIR.Mov (destReg, op, Some typ)) atom
  | ANF.TypedAtom (atom, typ) ->
      unary (fun op -> MIR.Mov (destReg, op, Some typ)) atom
  | ANF.Prim (op, left, right) ->
      let typ = binOpType builder left right in
      binary
        (fun l r -> MIR.BinOp (destReg, convertBinOp op, l, r, typ))
        left right
  | ANF.UnaryPrim (op, atom) ->
      let typ = atomType builder atom in
      unary
        (fun operand ->
          match op with
          | ANF.Not -> MIR.UnaryOp (destReg, convertUnaryOp op, operand)
          | ANF.Neg ->
              MIR.BinOp (destReg, MIR.Sub, MIR.Int64Const 0L, operand, typ)
          | ANF.BitNot ->
              MIR.BinOp (destReg, MIR.BitXor, operand, MIR.Int64Const (-1L), typ))
        atom
  | ANF.Call (name, args) | ANF.BorrowedCall (name, args) ->
      let types = List.map (atomType builder) args in
      let typ = directCallReturnType builder name in
      let+ ops = arguments args in
      [ MIR.Call (destReg, name, ops, types, typ) ]
  | ANF.IndirectCall (func, args) ->
      let types = List.map (atomType builder) args in
      let typ =
        match atomType builder func with
        | AST.TFunction (_, typ) -> typ
        | AST.TInternalRawPtr | AST.TInt64 -> AST.TBool
        | other ->
            Crash.crash
              ("IndirectCall: Expected TFunction type for func, got "
              ^ StructuralFormat.semanticType other)
      in
      let* funcOp = operand func in
      let+ ops = arguments args in
      [ MIR.IndirectCall (destReg, funcOp, ops, types, typ) ]
  | ANF.ClosureAlloc (name, captures) ->
      let size = mulInt (addInt 1 (List.length captures)) 8 in
      let alloc = MIR.HeapAlloc (destReg, size) in
      let storeFunc = MIR.HeapStore (destReg, 0, MIR.FuncAddr name, None) in
      let+ stores =
        ResultList.sequenceResults
          (List.mapi
             (fun index cap ->
               let typ = atomType builder cap in
               let valueType =
                 if typ = AST.TFloat64 then Some AST.TFloat64 else None
               in
               let+ op = operand cap in
               MIR.HeapStore (destReg, mulInt (addInt index 1) 8, op, valueType))
             captures)
      in
      alloc :: storeFunc :: stores
  | ANF.ClosureCall (closure, args) ->
      let types = List.map (atomType builder) args in
      let typ = closureCallReturnType builder tempId closure in
      let* closureOp = operand closure in
      let+ ops = arguments args in
      [ MIR.ClosureCall (destReg, closureOp, ops, types, typ) ]
  | ANF.TailCall (name, args) ->
      let types = List.map (atomType builder) args in
      let typ = directCallReturnType builder name in
      let+ ops = arguments args in
      [ MIR.TailCall (name, ops, types, typ) ]
  | ANF.IndirectTailCall (func, args) ->
      let types = List.map (atomType builder) args in
      let typ =
        match atomType builder func with
        | AST.TFunction (_, typ) -> typ
        | AST.TInternalRawPtr | AST.TInt64 -> AST.TBool
        | other ->
            Crash.crash
              ("IndirectTailCall: Expected TFunction type for func, got "
              ^ StructuralFormat.semanticType other)
      in
      let* funcOp = operand func in
      let+ ops = arguments args in
      [ MIR.IndirectTailCall (funcOp, ops, types, typ) ]
  | ANF.ClosureTailCall (closure, args) ->
      let types = List.map (atomType builder) args in
      let* closureOp = operand closure in
      let+ ops = arguments args in
      [ MIR.ClosureTailCall (closureOp, ops, types) ]
  | ANF.TupleAlloc elems ->
      let alloc = MIR.HeapAlloc (destReg, mulInt (List.length elems) 8) in
      let+ stores = storeFields elems in
      alloc :: stores
  | ANF.TupleGet (tuple, index) -> (
      match tuple with
      | ANF.Var tid ->
          let typ =
            match destType with
            | Some AST.TFloat64 -> Some AST.TFloat64
            | _ -> None
          in
          Ok [ MIR.HeapLoad (destReg, tempToVReg tid, mulInt index 8, typ) ]
      | _ ->
          Error
            "Internal error: Tuple access on non-variable (ANF invariant \
             violated)")
  | ANF.RecordAlloc (_, fields) | ANF.RecordClone (_, _, fields) ->
      let alloc = MIR.HeapAlloc (destReg, mulInt (List.length fields) 8) in
      let+ stores = storeFields fields in
      alloc :: stores
  | ANF.RecordReuse (_, desc, record, fields) -> (
      match record with
      | ANF.Var source ->
          let typ = desc.ANF.valueType in
          let+ stores = storeFields fields in
          MIR.Mov (destReg, MIR.Register (tempToVReg source), Some typ)
          :: stores
      | _ ->
          Error
            "Internal error: Record reuse on non-variable (ANF invariant \
             violated)")
  | ANF.RecordGet (_, record, index) -> (
      match record with
      | ANF.Var tid ->
          let typ =
            match destType with
            | Some AST.TFloat64 -> Some AST.TFloat64
            | _ -> None
          in
          Ok [ MIR.HeapLoad (destReg, tempToVReg tid, mulInt index 8, typ) ]
      | _ ->
          Error
            "Internal error: Record access on non-variable (ANF invariant \
             violated)")
  | ANF.IfValue _ ->
      Error "Internal error: IfValue should have been handled in outer match"
  | ANF.RefCountInc (atom, size, kind, typ) -> (
      match atom with
      | ANF.Var tid ->
          Ok [ MIR.RefCountInc (tempToVReg tid, size, rcKindToMIR kind, typ) ]
      | _ -> Error "Internal error: RefCountInc on non-variable")
  | ANF.RefCountDec (atom, size, kind, typ) -> (
      match atom with
      | ANF.Var tid ->
          Ok [ MIR.RefCountDec (tempToVReg tid, size, rcKindToMIR kind, typ) ]
      | _ -> Error "Internal error: RefCountDec on non-variable")
  | ANF.Print (atom, typ) -> unary (fun op -> MIR.Print (op, typ)) atom
  | ANF.StdoutWrite (atom, newline) ->
      let (MIR.VReg effectId) = destReg in
      let+ op = operand atom in
      [
        MIR.StdoutWrite (effectId, op, newline);
        MIR.Mov (destReg, MIR.Int64Const 0L, Some AST.TUnit);
      ]
  | ANF.StdinReadLine -> Ok [ MIR.StdinReadLine destReg ]
  | ANF.RuntimeError message -> Ok [ MIR.RuntimeError message ]
  | ANF.RuntimeErrorString atom ->
      unary (fun op -> MIR.RuntimeErrorString op) atom
  | ANF.StringConcat (first, second, rest) -> (
      let+ ops = ResultList.mapResults operand (first :: second :: rest) in
      match ops with
      | a :: b :: remaining -> [ MIR.StringConcat (destReg, a, b, remaining) ]
      | _ -> Crash.crash "StringConcat lost its required operands")
  | ANF.CanonicalBufferEq (kind, left, right) ->
      binary (fun l r -> MIR.CanonicalBufferEq (destReg, kind, l, r)) left right
  | ANF.FileReadBlob atom ->
      unary (fun op -> MIR.FileReadBlob (destReg, op)) atom
  | ANF.FileExists atom -> unary (fun op -> MIR.FileExists (destReg, op)) atom
  | ANF.FileWriteBlob (path, content) ->
      binary (fun p c -> MIR.FileWriteBlob (destReg, p, c)) path content
  | ANF.FileAppendText (path, content) ->
      binary (fun p c -> MIR.FileAppendText (destReg, p, c)) path content
  | ANF.FileDelete atom -> unary (fun op -> MIR.FileDelete (destReg, op)) atom
  | ANF.FileCreateDirectory atom ->
      unary (fun op -> MIR.FileCreateDirectory (destReg, op)) atom
  | ANF.FileSetExecutable atom ->
      unary (fun op -> MIR.FileSetExecutable (destReg, op)) atom
  | ANF.FileWriteFromPtr (path, ptr, length) ->
      ternary
        (fun p a l -> MIR.FileWriteFromPtr (destReg, p, a, l))
        path ptr length
  | ANF.RawAlloc atom -> unary (fun op -> MIR.RawAlloc (destReg, op)) atom
  | ANF.MappedAlloc atom -> unary (fun op -> MIR.MappedAlloc (destReg, op)) atom
  | ANF.RawFree atom -> unary (fun op -> MIR.RawFree op) atom
  | ANF.MappedFree atom -> unary (fun op -> MIR.MappedFree op) atom
  | ANF.RawGet (ptr, offset, typ) | ANF.RawTake (ptr, offset, typ) ->
      binary (fun p o -> MIR.RawGet (destReg, p, o, typ)) ptr offset
  | ANF.RawGetByte (ptr, offset) ->
      binary (fun p o -> MIR.RawGetByte (destReg, p, o)) ptr offset
  | ANF.RawWriteWord (ptr, offset, value) ->
      ternary (fun p o v -> MIR.RawWriteWord (p, o, v)) ptr offset value
  | ANF.RawWriteByte (ptr, offset, value) ->
      ternary (fun p o v -> MIR.RawWriteByte (p, o, v)) ptr offset value
  | ANF.RawSlotInit (ptr, offset, value, typ) ->
      ternary (fun p o v -> MIR.RawSlotInit (p, o, v, typ)) ptr offset value
  | ANF.StringToRawPtr atom ->
      unary (fun op -> MIR.StringToRawPtr (destReg, op)) atom
  | ANF.RawPtrToString atom ->
      unary (fun op -> MIR.RawPtrToString (destReg, op)) atom
  | ANF.BlobToRawPtr atom ->
      unary (fun op -> MIR.BlobToRawPtr (destReg, op)) atom
  | ANF.RawPtrToBlob atom ->
      unary (fun op -> MIR.RawPtrToBlob (destReg, op)) atom
  | ANF.RawPtrToInt128 atom ->
      unary (fun op -> MIR.Mov (destReg, op, Some AST.TInt128)) atom
  | ANF.RawPtrToUInt128 atom ->
      unary (fun op -> MIR.Mov (destReg, op, Some AST.TUInt128)) atom
  | ANF.DictToRawPtr atom ->
      unary (fun op -> MIR.DictToRawPtr (destReg, op)) atom
  | ANF.RawPtrToDict (ptr, tag, _) ->
      binary (fun p t -> MIR.RawPtrToDict (destReg, p, t)) ptr tag
  | ANF.ListToRawPtr atom ->
      unary (fun op -> MIR.ListToRawPtr (destReg, op)) atom
  | ANF.FixedBlockToRawPtr atom ->
      unary (fun op -> MIR.Mov (destReg, op, Some AST.TInternalRawPtr)) atom
  | ANF.RawPtrToList (ptr, tag, _) ->
      binary (fun p t -> MIR.RawPtrToList (destReg, p, t)) ptr tag
  | ANF.FloatSqrt atom -> unary (fun op -> MIR.FloatSqrt (destReg, op)) atom
  | ANF.FloatAbs atom -> unary (fun op -> MIR.FloatAbs (destReg, op)) atom
  | ANF.FloatNeg atom -> unary (fun op -> MIR.FloatNeg (destReg, op)) atom
  | ANF.Int64ToFloat atom ->
      unary (fun op -> MIR.Int64ToFloat (destReg, op)) atom
  | ANF.FloatToInt64 atom ->
      unary (fun op -> MIR.FloatToInt64 (destReg, op)) atom
  | ANF.FloatToBits atom -> unary (fun op -> MIR.FloatToBits (destReg, op)) atom
  | ANF.RefCountIncString atom ->
      unary (fun op -> MIR.RefCountIncString op) atom
  | ANF.RefCountDecString atom ->
      unary (fun op -> MIR.RefCountDecString op) atom
  | ANF.RefCountIncBlob atom -> unary (fun op -> MIR.RefCountIncBlob op) atom
  | ANF.RefCountDecBlob atom -> unary (fun op -> MIR.RefCountDecBlob op) atom
  | ANF.RefCountIncInt atom -> unary (fun op -> MIR.RefCountIncInt op) atom
  | ANF.RefCountDecInt atom -> unary (fun op -> MIR.RefCountDecInt op) atom
  | ANF.RandomInt64 -> Ok [ MIR.RandomInt64 destReg ]
  | ANF.DateTimeNow -> Ok [ MIR.DateTimeNow destReg ]
  | ANF.Sleep atom ->
      let (MIR.VReg effectId) = destReg in
      unary (fun op -> MIR.Sleep (effectId, destReg, op)) atom
  | ANF.CliNative (op, args) ->
      let+ operands = ResultList.mapResults operand args in
      [ MIR.CliNative (destReg, convertCliOperation op, operands) ]
  | ANF.FloatToString atom ->
      unary (fun op -> MIR.FloatToString (destReg, op)) atom

(*
   Convert ANF sequencing to CFG once, whether at function scope or in a branch.
   Returned blocks are complete CFG blocks; an enclosing join may redirect only
   the returned exit. Terminal transfers are never patched.
   Return: end current block with Ret terminator
   Self-recursive tail call: emit arg capture + cleanup + param update + Jump to loop header
   This must come before the general Let case to take precedence
   Phi nodes carry type info, so this works for both int and float parameters.
   To handle register swaps correctly (e.g., swapInt(b, a, n-1)),
   we need temps only when an argument directly references a parameter.
   Example where temps are needed: args = [b, a] for params [a, b]
   - Arg 0 is param b (VReg 1), needs capture before a is overwritten
   - Arg 1 is param a (VReg 0), needs capture before b is overwritten
   Example where temps are NOT needed: args = [n-1, acc+n] for params [n, acc]
   - Arg 0 is a computed temp (VReg 10xxx), not a direct param reference
   - Arg 1 is a computed temp (VReg 10xxx), not a direct param reference
   We only need temps if ANY argument is a direct param reference AND
   that param will be written to by another assignment.
   Use temps to avoid swap issues
   First capture all arg values into temps
   The loop header phis select the captured arguments.
   Let binding: handle based on cexpr type
   IfValue requires control flow blocks
   1. End current block with branch on condition
   Both predecessor operands become sources of the join's single definition.
   Add coverage instrumentation for the IfValue expression
   Current block ends with branch (after coverage hit)
   Determine the type of the if result (then/else should have same type)
   Simple CExpr: add instruction(s) to current block, continue
   Track if dest is float type for later builder update
   Use the explicit type annotation (for pattern matching with correct types)
   Comparison and boolean ops produce Bool, not the operand type
   Use typed subtraction so sized integers are truncated correctly downstream.
   x XOR -1 is equivalent to bitwise-not and preserves integer width via operandType.
   Allocate closure: (func_addr, cap1, cap2, ...)
   func_ptr + captures
   Store function pointer at offset 0 (always int/pointer type)
   Store captured values at offsets 8, 16, ... tracking value type for floats
   Call through closure: extract func_ptr, call with (closure, args...)
   Non-self-recursive tail call (self-recursive handled specially above)
   Emits TailCall instruction with full epilogue + branch
   Indirect tail call: no destination register
   Closure tail call: no destination register
   Allocate heap space: 8 bytes per element
   Store each element at its offset, tracking value type for float handling
   Tuple should always be a variable in ANF
   This case is handled above; reaching here indicates a bug
   FloatToBits copies float bits to UInt64 (produces integer, not float)
   Add coverage instrumentation if enabled
   Update FloatRegs if this dest is a float
*)
let rec convertExpr resultType expr currentLabel currentInstrsRev builder =
  match expr with
  | ANF.Jump (target, value) -> (
      match TempMap.find_opt target builder.joins with
      | None ->
          Error
            ("ANF to MIR: jump target " ^ tempText target
           ^ " is not in lexical scope")
      | Some (label, _) ->
          let+ operand = atomToOperand builder value in
          let block : MIR.basicBlock =
            {
              MIR.label = currentLabel;
              instrs = List.rev currentInstrsRev;
              terminator = MIR.Jump label;
            }
          in
          let incoming =
            Option.value ~default:[]
              (TempMap.find_opt target builder.joinIncoming)
          in
          ( Terminated,
            {
              builder with
              blocks = LM.add currentLabel block builder.blocks;
              joinIncoming =
                TempMap.add target
                  ((operand, currentLabel) :: incoming)
                  builder.joinIncoming;
            } ))
  | ANF.Join (param, continuation, entry) -> (
      let label, labelGen =
        MIR.freshLabelWithPrefix builder.funcName builder.labelGen
      in
      let entryBuilder =
        {
          builder with
          labelGen;
          joins = TempMap.add param.ANF.id (label, param.ANF.typ) builder.joins;
        }
      in
      let* exit, afterEntry =
        convertExpr resultType entry currentLabel currentInstrsRev entryBuilder
      in
      match exit with
      | Returned _ ->
          Error
            "ANF to MIR: a join entry must transfer control, not return a \
             function value"
      | Terminated ->
          let incoming =
            List.rev
              (Option.value ~default:[]
                 (TempMap.find_opt param.ANF.id afterEntry.joinIncoming))
          in
          let phi =
            MIR.Phi (tempToVReg param.ANF.id, incoming, Some param.ANF.typ)
          in
          let continuationBuilder =
            {
              afterEntry with
              joins = builder.joins;
              extraTypeMap =
                TempMap.add param.ANF.id param.ANF.typ afterEntry.extraTypeMap;
            }
          in
          convertExpr resultType continuation label [ phi ] continuationBuilder)
  | ANF.Return atom ->
      let* operand = atomToOperand builder atom in
      let block : MIR.basicBlock =
        {
          MIR.label = currentLabel;
          instrs = List.rev currentInstrsRev;
          terminator = MIR.Ret operand;
        }
      in
      Ok
        ( Returned (operand, currentLabel),
          { builder with blocks = LM.add currentLabel block builder.blocks } )
  | ANF.Let (callTempId, ANF.TailCall (funcName, args), rest)
    when funcName = builder.funcId ->
      let* postCleanup = collectSelfTailCallCleanup builder callTempId rest in
      let argTypes = List.map (atomType builder) args in
      let* argOperands =
        ResultList.sequenceResults (List.map (atomToOperand builder) args)
      in
      let loopLabel = MIR.Label (builder.funcName ^ "_body") in
      let paramSet = MIR.VRegSet.of_list builder.paramRegs in
      let needsTemps =
        List.exists
          (function
            | MIR.Register vreg -> MIR.VRegSet.mem vreg paramSet | _ -> false)
          argOperands
      in
      let captureInstrs, loopArguments, regGen =
        if needsTemps then
          let reversed, regGen =
            List.fold_left
              (fun (reversed, regGen) _ ->
                let temp, next = MIR.freshReg regGen in
                (temp :: reversed, next))
              ([], builder.regGen) argOperands
          in
          let temps = List.rev reversed in
          let captures =
            List.map
              (fun (temp, operand, typ) -> MIR.Mov (temp, operand, Some typ))
              (zip3 temps argOperands argTypes)
          in
          (captures, List.map (fun reg -> MIR.Register reg) temps, regGen)
        else ([], argOperands, builder.regGen)
      in
      let preCleanup, beforeCleanup =
        collectPreSelfTailCallCleanup currentInstrsRev
      in
      let incs, cleanup =
        transferOverlappingArgOwnership argOperands (preCleanup @ postCleanup)
          beforeCleanup
      in
      let instrsRev =
        beforeCleanup
        |> appendInstrsRev captureInstrs
        |> appendInstrsRev incs |> appendInstrsRev cleanup
      in
      let block : MIR.basicBlock =
        {
          MIR.label = currentLabel;
          instrs = List.rev instrsRev;
          terminator = MIR.Jump loopLabel;
        }
      in
      Ok
        ( Terminated,
          {
            builder with
            blocks = LM.add currentLabel block builder.blocks;
            regGen;
            selfTailIncoming =
              (currentLabel, loopArguments) :: builder.selfTailIncoming;
          } )
  | ANF.Let (tempId, cexpr, rest) -> (
      let destReg = tempToVReg tempId in
      let aliasType =
        match (cexpr, rest) with
        | ( (ANF.TupleGet _ | ANF.RecordGet _),
            ANF.Let (_, ANF.TypedAtom (ANF.Var sourceId, typ), _) )
          when sourceId = tempId ->
            Some typ
        | _ -> None
      in
      match cexpr with
      | ANF.IfValue (cond, yes, no) ->
          let coverage, builder = withCoverage builder cexpr in
          let* condOp = atomToOperand builder cond in
          let* yesOp = atomToOperand builder yes in
          let* noOp = atomToOperand builder no in
          let thenLabel, gen1 =
            MIR.freshLabelWithPrefix builder.funcName builder.labelGen
          in
          let elseLabel, gen2 =
            MIR.freshLabelWithPrefix builder.funcName gen1
          in
          let joinLabel, gen3 =
            MIR.freshLabelWithPrefix builder.funcName gen2
          in
          let currentBlock : MIR.basicBlock =
            {
              MIR.label = currentLabel;
              instrs = List.rev (appendInstrsRev coverage currentInstrsRev);
              terminator = MIR.Branch (condOp, thenLabel, elseLabel);
            }
          in
          let typ = atomType builder yes in
          let thenBlock : MIR.basicBlock =
            {
              MIR.label = thenLabel;
              instrs = [];
              terminator = MIR.Jump joinLabel;
            }
          in
          let elseBlock : MIR.basicBlock =
            {
              MIR.label = elseLabel;
              instrs = [];
              terminator = MIR.Jump joinLabel;
            }
          in
          let blocks =
            builder.blocks
            |> LM.add currentLabel currentBlock
            |> LM.add thenLabel thenBlock |> LM.add elseLabel elseBlock
          in
          let (ANF.TempId destId) = tempId in
          let builder = { builder with blocks; labelGen = gen3 } in
          let builder =
            if typ = AST.TFloat64 then
              { builder with floatRegs = IS.add destId builder.floatRegs }
            else builder
          in
          let phi =
            MIR.Phi
              (destReg, [ (yesOp, thenLabel); (noOp, elseLabel) ], Some typ)
          in
          convertExpr resultType rest joinLabel [ phi ] builder
      | _ ->
          let destType =
            inferSimpleCExprDestType builder tempId aliasType cexpr
          in
          let* instrs =
            simpleCExprInstrs builder tempId destReg destType cexpr
          in
          let coverage, builder = withCoverage builder cexpr in
          let builder =
            match cexpr with
            | ANF.ClosureAlloc (name, _) ->
                {
                  builder with
                  closureFuncs = TempMap.add tempId name builder.closureFuncs;
                }
            | _ -> builder
          in
          let newInstrsRev =
            appendInstrsRev (coverage @ instrs) currentInstrsRev
          in
          let (MIR.VReg destId) = destReg in
          let builder =
            match destType with
            | Some typ ->
                {
                  builder with
                  extraTypeMap = TempMap.add tempId typ builder.extraTypeMap;
                }
            | None -> builder
          in
          let builder =
            match destType with
            | Some AST.TFloat64 ->
                { builder with floatRegs = IS.add destId builder.floatRegs }
            | _ -> builder
          in
          convertExpr resultType rest currentLabel newInstrsRev builder)
  | ANF.If (cond, yes, no) -> (
      let* condOp = atomToOperand builder cond in
      let thenLabel, gen1 =
        MIR.freshLabelWithPrefix builder.funcName builder.labelGen
      in
      let elseLabel, gen2 = MIR.freshLabelWithPrefix builder.funcName gen1 in
      let joinLabel, gen3 = MIR.freshLabelWithPrefix builder.funcName gen2 in
      let resultReg, regGen = MIR.freshReg builder.regGen in
      let currentBlock : MIR.basicBlock =
        {
          MIR.label = currentLabel;
          instrs = List.rev currentInstrsRev;
          terminator = MIR.Branch (condOp, thenLabel, elseLabel);
        }
      in
      let branched =
        {
          builder with
          blocks = LM.add currentLabel currentBlock builder.blocks;
          labelGen = gen3;
          regGen;
        }
      in
      let* thenExit, afterThen =
        convertExpr resultType yes thenLabel [] branched
      in
      let* elseExit, afterElse =
        convertExpr resultType no elseLabel [] afterThen
      in
      match (thenExit, elseExit) with
      | Terminated, Terminated -> Ok (Terminated, afterElse)
      | _ ->
          let* afterFirst, thenSources =
            redirectReturn joinLabel thenExit afterElse
          in
          let+ redirected, elseSources =
            redirectReturn joinLabel elseExit afterFirst
          in
          let result = MIR.Register resultReg in
          let joinBlock : MIR.basicBlock =
            {
              MIR.label = joinLabel;
              instrs =
                [
                  MIR.Phi (resultReg, thenSources @ elseSources, Some resultType);
                ];
              terminator = MIR.Ret result;
            }
          in
          let (MIR.VReg resultId) = resultReg in
          let joined =
            {
              redirected with
              blocks = LM.add joinLabel joinBlock redirected.blocks;
              extraTypeMap =
                TempMap.add (ANF.TempId resultId) resultType
                  redirected.extraTypeMap;
              floatRegs =
                (if resultType = AST.TFloat64 then
                   IS.add resultId redirected.floatRegs
                 else redirected.floatRegs);
            }
          in
          (Returned (result, joinLabel), joined))

(*
   MIR phi sources for Float values must be registers before LIR lowering.
*)
let materializeFloatPhiSources regGen floatRegs blocks =
  let rewriteInstruction (additions, nextReg, floats) = function
    | MIR.Phi (dest, sources, Some AST.TFloat64) ->
        let sources, state =
          mapFold
            (fun (added, currentReg, currentFloats) (source, fromLabel) ->
              match source with
              | MIR.Register _ ->
                  ((source, fromLabel), (added, currentReg, currentFloats))
              | _ ->
                  let temp, followingReg = MIR.freshReg currentReg in
                  let (MIR.VReg id) = temp in
                  let current =
                    Option.value ~default:[] (LM.find_opt fromLabel added)
                  in
                  ( (MIR.Register temp, fromLabel),
                    ( LM.add fromLabel
                        (current @ [ MIR.Mov (temp, source, Some AST.TFloat64) ])
                        added,
                      followingReg,
                      IS.add id currentFloats ) ))
            (additions, nextReg, floats)
            sources
        in
        (MIR.Phi (dest, sources, Some AST.TFloat64), state)
    | instruction -> (instruction, (additions, nextReg, floats))
  in
  let rewritten, (additions, _, updatedFloats) =
    LM.fold
      (fun label block (rewritten, state) ->
        let instrs, next = mapFold rewriteInstruction state block.MIR.instrs in
        (LM.add label { block with MIR.instrs } rewritten, next))
      blocks
      (LM.empty, (LM.empty, regGen, floatRegs))
  in
  ( LM.mapi
      (fun label block ->
        let extra = Option.value ~default:[] (LM.find_opt label additions) in
        { block with MIR.instrs = block.MIR.instrs @ extra })
      rewritten,
    updatedFloats )

(*
   Lower the explicit post-ANF block graph without creating mutable MIR
   register definitions at joins. CExpr expansion still uses convertExpr so
   operation-specific layout and coverage handling stay in one place.
   LIR reserves virtual IDs below 4000 for ABI and spill scratch values.
*)
let convertSSAANFFunction (ssaFunc : SSAANF.functionDef) typeById typeReg
    returnTypeReg functionNames enableCoverage =
  let mirLabel (SSAANF.Label id) =
    MIR.Label
      (ssaFunc.SSAANF.name
      ^ if id = 0 then "_body" else "_ssa_" ^ string_of_int id)
  in
  let paramMax =
    List.fold_left
      (fun largest (param : ANF.typedParam) ->
        let (ANF.TempId id) = param.ANF.id in
        max largest id)
      (-1) ssaFunc.SSAANF.typedParams
  in
  let maxId =
    SSAANF.LabelMap.fold
      (fun _ block largest ->
        let withParams =
          List.fold_left
            (fun current (param : ANF.typedParam) ->
              let (ANF.TempId id) = param.ANF.id in
              max current id)
            largest block.SSAANF.parameters
        in
        let withOperations =
          List.fold_left
            (fun current (ANF.TempId id, operation) ->
              max current (max id (maxTempIdInCExpr operation)))
            withParams block.SSAANF.operations
        in
        let terminalMax =
          match block.SSAANF.terminator with
          | SSAANF.Return value -> maxTempIdInAtom value
          | SSAANF.Jump (_, args) -> maxTempIdInAtoms args
          | SSAANF.Branch (cond, _, _) -> maxTempIdInAtom cond
        in
        max withOperations terminalMax)
      ssaFunc.SSAANF.blocks paramMax
  in
  let bodyParamRegs =
    List.map
      (fun (param : ANF.typedParam) -> tempToVReg param.ANF.id)
      ssaFunc.SSAANF.typedParams
  in
  let hasSelfTail =
    SSAANF.LabelMap.exists
      (fun _ block ->
        List.exists
          (function
            | _, ANF.TailCall (target, _) -> target = ssaFunc.SSAANF.id
            | _ -> false)
          block.SSAANF.operations)
      ssaFunc.SSAANF.blocks
  in
  let firstExpansionReg = max 4000 (addInt maxId 1) in
  let inputRegs, regGen =
    if hasSelfTail then
      mapFold
        (fun current _ -> MIR.freshReg current)
        (MIR.RegGen firstExpansionReg) ssaFunc.SSAANF.typedParams
    else (bodyParamRegs, MIR.RegGen firstExpansionReg)
  in
  let inputsById =
    TempMap.of_list
      (List.combine
         (List.map
            (fun (param : ANF.typedParam) -> param.ANF.id)
            ssaFunc.SSAANF.typedParams)
         inputRegs)
  in
  let inputReg id =
    match TempMap.find_opt id inputsById with
    | Some reg -> reg
    | None ->
        Crash.crash ("SSA ANF to MIR: missing parameter input " ^ tempText id)
  in
  let parameterFloatRegs =
    SSAANF.LabelMap.fold
      (fun _ block floats ->
        List.fold_left
          (fun current (param : ANF.typedParam) ->
            if param.ANF.typ = AST.TFloat64 then
              let (ANF.TempId id) = param.ANF.id in
              IS.add id current
            else current)
          floats block.SSAANF.parameters)
      ssaFunc.SSAANF.blocks IS.empty
  in
  let floatRegs =
    List.fold_left
      (fun floats ((param : ANF.typedParam), MIR.VReg bodyId, MIR.VReg inputId)
         ->
        if param.ANF.typ = AST.TFloat64 then
          floats |> IS.add bodyId |> IS.add inputId
        else floats)
      parameterFloatRegs
      (zip3 ssaFunc.SSAANF.typedParams bodyParamRegs inputRegs)
  in
  let initialBuilder =
    {
      regGen;
      joins = TempMap.empty;
      joinIncoming = TempMap.empty;
      selfTailIncoming = [];
      labelGen = MIR.initialLabelGen;
      blocks = LM.empty;
      typeById;
      sourceTempIdMax = maxId;
      extraTypeMap =
        List.fold_left
          (fun types (param : ANF.typedParam) ->
            TempMap.add param.ANF.id param.ANF.typ types)
          ssaFunc.SSAANF.freshValueTypes ssaFunc.SSAANF.typedParams;
      typeReg;
      returnTypeReg;
      functionNames;
      funcId = ssaFunc.SSAANF.id;
      funcName = ssaFunc.SSAANF.name;
      paramRegs = bodyParamRegs;
      floatRegs;
      closureFuncs = TempMap.empty;
      enableCoverage;
      exprIdGen = ANF.initialExprIdGen;
      coverageMapping = ANF.emptyCoverageMapping;
    }
  in
  let paramIds =
    TempSet.of_list
      (List.map
         (fun (param : ANF.typedParam) -> param.ANF.id)
         ssaFunc.SSAANF.typedParams)
  in
  let rec splitEntryRetains = function
    | (_, ANF.RefCountInc (ANF.Var id, size, kind, typ)) :: rest
      when TempSet.mem id paramIds ->
        let following, body = splitEntryRetains rest in
        ( MIR.RefCountInc (inputReg id, size, rcKindToMIR kind, typ) :: following,
          body )
    | (_, ANF.RefCountIncString (ANF.Var id)) :: rest
      when TempSet.mem id paramIds ->
        let following, body = splitEntryRetains rest in
        (MIR.RefCountIncString (MIR.Register (inputReg id)) :: following, body)
    | (_, ANF.RefCountIncBlob (ANF.Var id)) :: rest when TempSet.mem id paramIds
      ->
        let following, body = splitEntryRetains rest in
        (MIR.RefCountIncBlob (MIR.Register (inputReg id)) :: following, body)
    | (_, ANF.RefCountIncInt (ANF.Var id)) :: rest when TempSet.mem id paramIds
      ->
        let following, body = splitEntryRetains rest in
        (MIR.RefCountIncInt (MIR.Register (inputReg id)) :: following, body)
    | operations -> ([], operations)
  in
  let entryBlock =
    match
      SSAANF.LabelMap.find_opt ssaFunc.SSAANF.entry ssaFunc.SSAANF.blocks
    with
    | Some block -> block
    | None -> Crash.crash "SSA ANF to MIR: missing entry block"
  in
  let entryRetains, entryOperations =
    splitEntryRetains entryBlock.SSAANF.operations
  in
  let blocksToLower =
    SSAANF.LabelMap.add ssaFunc.SSAANF.entry
      { entryBlock with SSAANF.operations = entryOperations }
      ssaFunc.SSAANF.blocks
  in
  let patchReturn label terminator builder =
    match LM.find_opt label builder.blocks with
    | Some ({ MIR.terminator = MIR.Ret _; _ } as block) ->
        Ok
          {
            builder with
            blocks = LM.add label { block with MIR.terminator } builder.blocks;
          }
    | Some _ -> Error "SSA ANF to MIR: block exit is not a return"
    | None -> Error "SSA ANF to MIR: block exit is missing"
  in
  let lowerBlock _ block result =
    let* builder, incoming = result in
    let finalAtom =
      match block.SSAANF.terminator with
      | SSAANF.Return value -> value
      | SSAANF.Jump _ | SSAANF.Branch _ -> ANF.UnitLiteral
    in
    let expr =
      List.fold_right
        (fun (id, operation) rest -> ANF.Let (id, operation, rest))
        block.SSAANF.operations (ANF.Return finalAtom)
    in
    let* exit, after =
      convertExpr ssaFunc.SSAANF.returnType expr
        (mirLabel block.SSAANF.label)
        [] builder
    in
    match (block.SSAANF.terminator, exit) with
    | SSAANF.Return _, _ -> Ok (after, incoming)
    | SSAANF.Jump (target, args), Returned (_, exitLabel) ->
        let* operands =
          ResultList.sequenceResults (List.map (atomToOperand after) args)
        in
        let+ patched =
          patchReturn exitLabel (MIR.Jump (mirLabel target)) after
        in
        let current =
          Option.value ~default:[] (SSAANF.LabelMap.find_opt target incoming)
        in
        ( patched,
          SSAANF.LabelMap.add target ((exitLabel, operands) :: current) incoming
        )
    | SSAANF.Branch (condition, ifTrue, ifFalse), Returned (_, exitLabel) ->
        let* operand = atomToOperand after condition in
        let+ patched =
          patchReturn exitLabel
            (MIR.Branch (operand, mirLabel ifTrue, mirLabel ifFalse))
            after
        in
        (patched, incoming)
    | _ -> Error "SSA ANF to MIR: a control-flow edge has no value exit"
  in
  let* lowered, incoming =
    SSAANF.LabelMap.fold lowerBlock blocksToLower
      (Ok (initialBuilder, SSAANF.LabelMap.empty))
  in
  let addBlockParameters _ block result =
    let* blocks = result in
    let label = mirLabel block.SSAANF.label in
    match LM.find_opt label blocks with
    | None -> Error ("SSA ANF to MIR: missing lowered block " ^ labelText label)
    | Some mirBlock ->
        let sources =
          List.rev
            (Option.value ~default:[]
               (SSAANF.LabelMap.find_opt block.SSAANF.label incoming))
        in
        if
          List.exists
            (fun (_, args) ->
              List.length args <> List.length block.SSAANF.parameters)
            sources
        then
          Error
            ("SSA ANF to MIR: edge arguments do not match " ^ labelText label)
        else
          let phis =
            List.mapi
              (fun index (param : ANF.typedParam) ->
                let phiSources =
                  List.map
                    (fun (fromLabel, args) ->
                      match tryItem index args with
                      | Some arg -> (arg, fromLabel)
                      | None ->
                          Crash.crash
                            "SSA ANF to MIR: verified edge lost an argument")
                    sources
                in
                MIR.Phi (tempToVReg param.ANF.id, phiSources, Some param.ANF.typ))
              block.SSAANF.parameters
          in
          Ok
            (LM.add label
               { mirBlock with MIR.instrs = phis @ mirBlock.MIR.instrs }
               blocks)
  in
  let+ withBlockParameters =
    SSAANF.LabelMap.fold addBlockParameters blocksToLower (Ok lowered.blocks)
  in
  let trueEntryLabel = MIR.Label (ssaFunc.SSAANF.name ^ "_entry") in
  let bodyEntryLabel = mirLabel ssaFunc.SSAANF.entry in
  let withLoopParameters =
    if not hasSelfTail then withBlockParameters
    else
      let header =
        match LM.find_opt bodyEntryLabel withBlockParameters with
        | Some block -> block
        | None -> Crash.crash "SSA ANF to MIR: missing loop header"
      in
      let backedges = List.rev lowered.selfTailIncoming in
      let phis =
        List.mapi
          (fun index ((param : ANF.typedParam), bodyReg, input) ->
            let sources =
              (MIR.Register input, trueEntryLabel)
              :: List.map
                   (fun (fromLabel, args) ->
                     match tryItem index args with
                     | Some arg -> (arg, fromLabel)
                     | None ->
                         Crash.crash
                           "SSA ANF to MIR: missing self-tail argument")
                   backedges
            in
            MIR.Phi (bodyReg, sources, Some param.ANF.typ))
          (zip3 ssaFunc.SSAANF.typedParams bodyParamRegs inputRegs)
      in
      LM.add bodyEntryLabel
        { header with MIR.instrs = phis @ header.MIR.instrs }
        withBlockParameters
  in
  let blocksWithFloatSources, finalFloatRegs =
    materializeFloatPhiSources lowered.regGen lowered.floatRegs
      withLoopParameters
  in
  let trueEntry : MIR.basicBlock =
    {
      MIR.label = trueEntryLabel;
      instrs = entryRetains;
      terminator = MIR.Jump bodyEntryLabel;
    }
  in
  let typedParams =
    List.map
      (fun (reg, (param : ANF.typedParam)) ->
        ({ MIR.reg; typ = param.ANF.typ } : MIR.typedMIRParam))
      (List.combine inputRegs ssaFunc.SSAANF.typedParams)
  in
  let entry, blocks =
    if ssaFunc.SSAANF.name = "_start" && entryRetains = [] && not hasSelfTail
    then (bodyEntryLabel, blocksWithFloatSources)
    else (trueEntryLabel, LM.add trueEntryLabel trueEntry blocksWithFloatSources)
  in
  ({
     MIR.id = ssaFunc.SSAANF.id;
     name = ssaFunc.SSAANF.name;
     typedParams;
     returnType = ssaFunc.SSAANF.returnType;
     cfg = { MIR.entry; blocks };
     floatRegs = finalFloatRegs;
   }
    : MIR.functionDef)

let convertANFFunctionWithTailCalls (anfFunc : ANF.functionDef) typeMap typeReg
    returnTypeReg functionNames enableCoverage recursiveMembers enableTCO =
  let* ssaFunc =
    SSAANF.convertFunction (maxTempIdInFunction anfFunc) typeMap anfFunc
  in
  let withTailCalls =
    if enableTCO then SSATailCallDetection.detect recursiveMembers ssaFunc
    else ssaFunc
  in
  convertSSAANFFunction withTailCalls typeMap typeReg returnTypeReg
    functionNames enableCoverage

let convertANFFunction anfFunc typeMap typeReg returnTypeReg functionNames
    enableCoverage =
  convertANFFunctionWithTailCalls anfFunc typeMap typeReg returnTypeReg
    functionNames enableCoverage FunctionIdMap.empty true

(*
   Convert ANF program to MIR program
   mainExprType: the type of the main expression (used for _start's return type)
   variantLookup: mapping from variant names to type info (for enum printing)
   typeReg: mapping from record type names to field info (for record printing, converted to RecordRegistry)
   externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
   Each function gets its own RegGen for deterministic VReg assignment.
   Build return type registry for all functions (needed for caller to know return type)
   Phase 2: Convert all functions to MIR
   Each function gets its own RegGen starting from (maxTempId + 1) for deterministic compilation
   The main expression uses the same SSA boundary as named functions.
   Build recordRegistry from typeRegForRecords (converts tuples to RecordField records)
*)
let toMIR (ANF.Program (functions, mainExpr)) typeMap typeReg mainExprType
    variantLookup typeRegForRecords enableCoverage externalReturnTypes
    functionNames =
  let returnTypeReg = buildReturnTypeReg functions externalReturnTypes in
  let startId =
    match
      Seq.find_map
        (fun (id, name) -> if name = "_start" then Some id else None)
        (FunctionIdMap.toSeq functionNames)
    with
    | Some id -> id
    | None -> Crash.crash "MIR start function has no allocated identity"
  in
  let* mirFuncs =
    ResultList.mapResults
      (fun func ->
        convertANFFunction func typeMap typeReg returnTypeReg functionNames
          enableCoverage)
      functions
  in
  let startFuncANF : ANF.functionDef =
    {
      ANF.id = startId;
      name = "_start";
      typedParams = [];
      returnType = mainExprType;
      returnOwnership = ANF.OwnedReturn;
      body = mainExpr;
    }
  in
  let* startFunc =
    convertANFFunction startFuncANF typeMap typeReg returnTypeReg functionNames
      enableCoverage
  in
  let allFuncs = mirFuncs @ [ startFunc ] in
  let variantRegistry = buildVariantRegistry variantLookup in
  let recordRegistry = buildRecordRegistry typeRegForRecords in
  Ok (MIR.Program (allFuncs, variantRegistry, recordRegistry))

(*
   Convert ANF program to MIR (functions only, no _start)
   Use for stdlib where there's no real main expression to convert.
   Returns just the function list, variant registry, and record registry without wrapping in MIR.Program.
   externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
   Each function gets its own RegGen for deterministic VReg assignment.
*)
let toMIRFunctionsOnlyInternal phaseRecorder projectedRegistries tailCallConfig
    (ANF.Program (functions, _)) typeMap typeReg variantLookup typeRegForRecords
    enableCoverage returnTypeReg functionNames =
  let startPhase () =
    Option.map
      (fun _ -> Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)
      phaseRecorder
  in
  let recordPhase name timer =
    match (phaseRecorder, timer) with
    | Some record, Some start ->
        let elapsed =
          (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start
        in
        record name elapsed
    | _ -> ()
  in
  let members, enabled =
    Option.value ~default:(FunctionIdMap.empty, true) tailCallConfig
  in
  let ssaTimer = startPhase () in
  let ssaResult =
    ResultList.mapResults
      (fun func ->
        SSAANF.convertFunction (maxTempIdInFunction func) typeMap func)
      functions
  in
  recordPhase "ANF -> SSA Function Conversion" ssaTimer;
  let* ssaFunctions = ssaResult in
  let tailCallTimer = startPhase () in
  let withTailCalls =
    if enabled then List.map (SSATailCallDetection.detect members) ssaFunctions
    else ssaFunctions
  in
  recordPhase "Tail Call Detection" tailCallTimer;
  let conversionTimer = startPhase () in
  let+ mirFuncs =
    ResultList.mapResults
      (fun func ->
        convertSSAANFFunction func typeMap typeReg returnTypeReg functionNames
          enableCoverage)
      withTailCalls
  in
  recordPhase "SSA ANF -> MIR Function Conversion" conversionTimer;
  let registryTimer = startPhase () in
  let variantRegistry, recordRegistry =
    match projectedRegistries with
    | Some registries -> registries
    | None ->
        let variants = buildVariantRegistry variantLookup in
        let records = buildRecordRegistry typeRegForRecords in
        (variants, records)
  in
  recordPhase "ANF -> MIR Registry Projection" registryTimer;
  (mirFuncs, variantRegistry, recordRegistry)

(*
   Lower functions whose ownership operations have already been inserted on
   SSA ANF blocks. No ANF or MIR SSA reconstruction is performed here.
*)
let toMIRSSAFunctionsOnlyWithTrace phaseRecorder projectedRegistries
    recursiveMembers enableTCO functions typeMap typeReg variantLookup
    typeRegForRecords enableCoverage returnTypeReg functionNames =
  let withTailCalls =
    if enableTCO then
      List.map (SSATailCallDetection.detect recursiveMembers) functions
    else functions
  in
  let+ mirFuncs =
    ResultList.mapResults
      (fun func ->
        convertSSAANFFunction func typeMap typeReg returnTypeReg functionNames
          enableCoverage)
      withTailCalls
  in
  Option.iter
    (fun record -> record "SSA ANF -> MIR Function Conversion" 0.0)
    phaseRecorder;
  let variantRegistry, recordRegistry =
    match projectedRegistries with
    | Some registries -> registries
    | None ->
        let variants = buildVariantRegistry variantLookup in
        let records = buildRecordRegistry typeRegForRecords in
        (variants, records)
  in
  (mirFuncs, variantRegistry, recordRegistry)

let toMIRFunctionsOnly (ANF.Program (functions, _) as program) typeMap typeReg
    variantLookup typeRegForRecords enableCoverage externalReturnTypes
    functionNames =
  let returnTypeReg = buildReturnTypeReg functions externalReturnTypes in
  toMIRFunctionsOnlyInternal None None None program typeMap typeReg
    variantLookup typeRegForRecords enableCoverage returnTypeReg functionNames

let toMIRFunctionsOnlyWithTrace phaseRecorder projectedRegistries
    recursiveMembers enableTCO program typeMap typeReg variantLookup
    typeRegForRecords enableCoverage returnTypeReg functionNames =
  toMIRFunctionsOnlyInternal phaseRecorder projectedRegistries
    (Some (recursiveMembers, enableTCO))
    program typeMap typeReg variantLookup typeRegForRecords enableCoverage
    returnTypeReg functionNames
