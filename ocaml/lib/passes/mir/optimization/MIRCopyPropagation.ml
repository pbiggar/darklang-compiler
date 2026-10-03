(* CopyPropagation.fs - Resolve and propagate MIR copy equivalences. *)
[@@@warning "-4"]
open MIR
(*
   Replace uses of copy destinations with their sources
   For: dest = src, replace all uses of dest with src
*)
type copyMap = operand VRegMap.t
(*
   SSA phis are a leading block prefix. Later passes either retain that
   invariant or replace a phi with an ordinary move, so stop as soon as the
   prefix ends instead of rescanning every instruction in every fixed-point
   iteration merely to rediscover the same destinations.
   Don't add if dest is a phi destination or already in map
   Constant propagation: track constant moves too
   Only propagate if this is for integer/bool types, not for heap types like strings
   This prevents incorrectly propagating Int64Const 0L to string variables
   Don't propagate for TString, TList, etc.
   Trivial phi with single register source
   Check if all sources are the same register
*)
let buildCopyMap (cfg : cfg) =
 let rec leading dests = function Phi (dest, _, _) :: rest -> leading (VRegSet.add dest dests) rest | _ -> dests in
 let phis = LabelMap.fold (fun _ block dests -> leading dests block.instrs) cfg.blocks VRegSet.empty in
 let scalar = function None | Some AST.TInt64 | Some AST.TInt32 | Some AST.TInt16 | Some AST.TInt8 | Some AST.TUInt64 | Some AST.TUInt32 | Some AST.TUInt16 | Some AST.TUInt8 | Some AST.TBool | Some AST.TUnit -> true | _ -> false in
 LabelMap.fold (fun _ block copies -> List.fold_left (fun copies instruction -> match instruction with
 | Mov (dest, Register source, _) when dest <> source -> if VRegSet.mem dest phis || VRegMap.mem dest copies then copies else VRegMap.add dest (Register source) copies
 | Mov (dest, (Int64Const _ as source), typ) -> if VRegSet.mem dest phis || VRegMap.mem dest copies || not (scalar typ) then copies else VRegMap.add dest source copies
 | Mov (dest, (BoolConst _ as source), _) -> if VRegSet.mem dest phis || VRegMap.mem dest copies then copies else VRegMap.add dest source copies
 | Phi (dest, [(Register source, _)], _) when dest <> source -> if VRegMap.mem dest copies then copies else VRegMap.add dest (Register source) copies
 | Phi (dest, sources, _) -> if VRegMap.mem dest copies then copies else (match sources with (Register source, _) :: rest when dest <> source && List.for_all (fun (operand, _) -> operand = Register source) rest -> VRegMap.add dest (Register source) copies | _ -> copies)
 | _ -> copies) copies block.instrs) cfg.blocks VRegMap.empty
(*
   Transitively resolve a copy chain (with cycle detection)
   Cycle detected, stop here
*)
let resolveCopy copies operand = let rec resolve visited = function Register reg as operand -> if VRegSet.mem reg visited then operand else (match VRegMap.find_opt reg copies with Some next -> resolve (VRegSet.add reg visited) next | None -> operand) | operand -> operand in resolve VRegSet.empty operand
(*
   Resolve every copy destination once so operand propagation only needs one lookup.
*)
let resolveCopyMap copies = VRegMap.mapi (fun dest _ -> resolveCopy copies (Register dest)) copies
(*
   Apply copy propagation to an operand
*)
let propagateCopyOperand copies = function Register reg as operand -> Option.value ~default:operand (VRegMap.find_opt reg copies) | operand -> operand
(*
   Apply copy propagation to an instruction
   RuntimeError continuations use an integer zero sentinel even when
   their unreachable result flows through a float-typed use.
   RuntimeError continuations can carry the unreachable Unit
   sentinel through an inlined string-typed parameter. Keep the
   register form so dead post-error code remains representable.
   Don't propagate copies into phi sources - phis are merge points and their
   sources represent values flowing from specific predecessor blocks
*)
let propagateCopyInstr copies (instruction : instr) =
 let p = propagateCopyOperand copies in
 let typedP typ operand = let propagated = p operand in match typ, propagated with AST.TFloat64, Int64Const bits -> FloatSymbol (Int64.float_of_bits bits) | AST.TFloat64, Register _ | AST.TFloat64, FloatSymbol _ -> propagated | AST.TFloat64, _ -> operand | _ -> propagated in
 let rec callArgs args types = match args, types with [], [] -> [] | arg :: args, typ :: types -> let arg = typedP typ arg in arg :: callArgs args types | _ -> Crash.crash "MIR call argument and type counts differ" in
 let reg addr = match p (Register addr) with Register reg -> reg | _ -> addr in
 let stringOperand operand = match p operand with (Register _ | StringSymbol _) as propagated -> propagated | _ -> operand in
 match instruction with
 | Mov (dest, source, typ) -> Mov (dest, (match typ with Some typ -> typedP typ source | None -> p source), typ)
 | BinOp (dest, op, left, right, typ) -> let left, right = match op, typ with (Sub | Eq | Neq | Lt | Gt | Lte | Gte), AST.TFloat64 -> typedP typ left, typedP typ right | _ -> p left, p right in BinOp (dest, op, left, right, typ)
 | HeapStore (addr, offset, source, typ) -> HeapStore (reg addr, offset, p source, typ)
 | HeapLoad (dest, addr, offset, typ) -> HeapLoad (dest, reg addr, offset, typ)
 | RefCountInc (addr, size, kind, typ) -> RefCountInc (reg addr, size, kind, typ)
 | RefCountDec (addr, size, kind, typ) -> RefCountDec (reg addr, size, kind, typ)
 | StringConcat (dest, first, second, remaining) -> StringConcat (dest, stringOperand first, stringOperand second, List.map stringOperand remaining)
| UnaryOp (dest, op, src) -> UnaryOp (dest, op, p src)
| Call (dest, name, args, argTypes, retType) -> Call (dest, name, callArgs args argTypes, argTypes, retType)
| TailCall (name, args, argTypes, retType) -> TailCall (name, callArgs args argTypes, argTypes, retType)
| IndirectCall (dest, func, args, argTypes, retType) -> IndirectCall (dest, p func, callArgs args argTypes, argTypes, retType)
| IndirectTailCall (func, args, argTypes, retType) -> IndirectTailCall (p func, callArgs args argTypes, argTypes, retType)
| ClosureAlloc (dest, name, captures) -> ClosureAlloc (dest, name, List.map p captures)
| ClosureCall (dest, closure, args, argTypes, retType) -> ClosureCall (dest, p closure, callArgs args argTypes, argTypes, retType)
| ClosureTailCall (closure, args, argTypes) -> ClosureTailCall (p closure, callArgs args argTypes, argTypes)
| HeapAlloc (dest, size) -> HeapAlloc (dest, size)
| CanonicalBufferEq (dest, kind, left, right) -> CanonicalBufferEq (dest, kind, p left, p right)
| Print (src, vt) -> Print (p src, vt)
| StdoutWrite (effectId, src, appendNewline) -> StdoutWrite (effectId, p src, appendNewline)
| StdinReadLine dest -> StdinReadLine dest
| FileReadBlob (dest, path) -> FileReadBlob (dest, p path)
| FileExists (dest, path) -> FileExists (dest, p path)
| FileWriteBlob (dest, path, content) -> FileWriteBlob (dest, p path, p content)
| FileAppendText (dest, path, content) -> FileAppendText (dest, p path, p content)
| FileDelete (dest, path) -> FileDelete (dest, p path)
| FileCreateDirectory (dest, path) -> FileCreateDirectory (dest, p path)
| FileSetExecutable (dest, path) -> FileSetExecutable (dest, p path)
| FileWriteFromPtr (dest, path, ptr, length) -> FileWriteFromPtr (dest, p path, p ptr, p length)
| Phi (dest, sources, valueType) -> Phi (dest, sources, valueType)
| RawAlloc (dest, numBytes) -> RawAlloc (dest, p numBytes)
| MappedAlloc (dest, numBytes) -> MappedAlloc (dest, p numBytes)
| RawFree ptr -> RawFree (p ptr)
| MappedFree ptr -> MappedFree (p ptr)
| RawGet (dest, ptr, byteOffset, valueType) -> RawGet (dest, p ptr, p byteOffset, valueType)
| RawGetByte (dest, ptr, byteOffset) -> RawGetByte (dest, p ptr, p byteOffset)
| StringToRawPtr (dest, value) -> StringToRawPtr (dest, p value)
| RawPtrToString (dest, ptr) -> RawPtrToString (dest, p ptr)
| BlobToRawPtr (dest, value) -> BlobToRawPtr (dest, p value)
| RawPtrToBlob (dest, ptr) -> RawPtrToBlob (dest, p ptr)
| DictToRawPtr (dest, dict) -> DictToRawPtr (dest, p dict)
| RawPtrToDict (dest, ptr, tag) -> RawPtrToDict (dest, p ptr, p tag)
| ListToRawPtr (dest, list) -> ListToRawPtr (dest, p list)
| RawPtrToList (dest, ptr, tag) -> RawPtrToList (dest, p ptr, p tag)
| RawWriteWord (ptr, byteOffset, value) -> RawWriteWord (p ptr, p byteOffset, p value)
| RawWriteByte (ptr, byteOffset, value) -> RawWriteByte (p ptr, p byteOffset, p value)
| RawSlotInit (ptr, byteOffset, value, valueType) -> RawSlotInit (p ptr, p byteOffset, p value, valueType)
| FloatSqrt (dest, src) -> FloatSqrt (dest, p src)
| FloatAbs (dest, src) -> FloatAbs (dest, p src)
| FloatNeg (dest, src) -> FloatNeg (dest, p src)
| Int64ToFloat (dest, src) -> Int64ToFloat (dest, p src)
| FloatToInt64 (dest, src) -> FloatToInt64 (dest, p src)
| FloatToBits (dest, src) -> FloatToBits (dest, p src)
| RefCountIncString str -> RefCountIncString (p str)
| RefCountDecString str -> RefCountDecString (p str)
| RefCountIncBlob bytes -> RefCountIncBlob (p bytes)
| RefCountDecBlob bytes -> RefCountDecBlob (p bytes)
| RefCountIncInt value -> RefCountIncInt (p value)
| RefCountDecInt value -> RefCountDecInt (p value)
| RandomInt64 dest -> RandomInt64 dest
| DateTimeNow dest -> DateTimeNow dest
| Sleep (effectId, dest, delayMs) -> Sleep (effectId, dest, p delayMs)
| CliNative (dest, operation, args) -> CliNative (dest, operation, List.map p args)
| FloatToString (dest, value) -> FloatToString (dest, p value)
| RuntimeError message -> RuntimeError message
| RuntimeErrorString message -> RuntimeErrorString (p message)
| CoverageHit exprId -> CoverageHit exprId
(*
   Apply copy propagation to terminator
*)
let propagateCopyTerminator copies (terminator : terminator) = let p = propagateCopyOperand copies in match terminator with Ret operand -> Ret (p operand) | Branch (condition, yes, no) -> Branch (p condition, yes, no) | Jump label -> Jump label
