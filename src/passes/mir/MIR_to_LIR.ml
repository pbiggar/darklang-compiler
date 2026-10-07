(*
   MIR_to_LIR.fs - Instruction Selection (Pass 4)
   Transforms MIR CFG into symbolic LIR CFG (string/float constants remain symbolic).
   Instruction selection algorithm:
   - Converts MIR basic blocks to LIR basic blocks
   - Selects target-neutral LIR operations for each MIR operation
   - Handles LIR operand constraints that later backends lower to real instructions:
   - ADD/SUB: support 12-bit immediates, left operand must be register
   - MUL/SDIV: both operands must be registers
   - Inserts MOV instructions to load immediates when needed
   - Converts MIR terminators to LIR terminators
   - Preserves CFG structure (labels, branches, jumps)
*)
let convertCliOperation operation = (match operation with
| MIR.Execute -> (LIR.Execute)
| MIR.RunProcess -> (LIR.RunProcess)
| MIR.HostOS -> (LIR.HostOS)
| MIR.HostArchitecture -> (LIR.HostArchitecture)
| MIR.Hostname -> (LIR.Hostname)
| MIR.GetEnv -> (LIR.GetEnv)
| MIR.GetEnvironmentPacked -> (LIR.GetEnvironmentPacked)
| MIR.SetEnv -> (LIR.SetEnv)
| MIR.UnsetEnv -> (LIR.UnsetEnv)
| MIR.DirectoryCurrent -> (LIR.DirectoryCurrent)
| MIR.DirectoryListPacked -> (LIR.DirectoryListPacked)
| MIR.FileIsDirectory -> (LIR.FileIsDirectory)
| MIR.FileCreateExclusive -> (LIR.FileCreateExclusive)
| MIR.GetArgv -> (LIR.GetArgv)
| MIR.Kill -> (LIR.Kill)
| MIR.GetPid -> (LIR.GetPid)
| MIR.GetUid -> (LIR.GetUid)
| MIR.CpuCount -> (LIR.CpuCount)
| MIR.SpawnProcess -> (LIR.SpawnProcess)
| MIR.ProcessIO -> (LIR.ProcessIO)
| MIR.TerminateProcess -> (LIR.TerminateProcess)
| MIR.SocketTcp4 -> (LIR.SocketTcp4)
| MIR.SocketTcp6 -> (LIR.SocketTcp6)
| MIR.SocketUdp4 -> (LIR.SocketUdp4)
| MIR.SocketUdp6 -> (LIR.SocketUdp6)
| MIR.SocketConnect4 -> (LIR.SocketConnect4)
| MIR.SocketConnect6 -> (LIR.SocketConnect6)
| MIR.SocketSend -> (LIR.SocketSend)
| MIR.SocketReceive -> (LIR.SocketReceive)
| MIR.SocketReceiveTimeout -> (LIR.SocketReceiveTimeout)
| MIR.SocketSendTimeout -> (LIR.SocketSendTimeout)
| MIR.SocketClose -> (LIR.SocketClose)
| MIR.SocketBind4 -> LIR.SocketBind4
| MIR.SocketListen -> LIR.SocketListen
| MIR.SocketAccept -> LIR.SocketAccept
| MIR.SocketCloexec -> LIR.SocketCloexec
| MIR.SocketReuseAddress -> LIR.SocketReuseAddress
| MIR.SocketPoll -> LIR.SocketPoll
| MIR.SignalBlock -> LIR.SignalBlock
| MIR.SignalRestore -> LIR.SignalRestore
| MIR.SignalPending -> LIR.SignalPending
| MIR.SignalWait -> LIR.SignalWait
| MIR.MonotonicTime -> LIR.MonotonicTime
| MIR.SecureRandomFill -> (LIR.SecureRandomFill))

[@@@warning "-4"]
module M=MIR
module L=LIR
module SM=StringOrder.Map
let ( let* ) = Result.bind
let ( let+ ) result action = Result.map action result
let addInt a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mulInt a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let rec take count values=if count=0 then [] else match values with head::rest when count>0 -> head::take (count-1) rest | _ -> invalid_arg (Printf.sprintf "The input sequence has an insufficient number of elements.\nThe list was %d elements shorter than the index" count)
let rec skip count values=if count=0 then values else match values with _::rest when count>0 -> skip (count-1) rest | _ -> invalid_arg "The input sequence has an insufficient number of elements. (Parameter 'list')"
let distinct values=let _,reversed=List.fold_left (fun (seen,output) value -> if List.mem value seen then seen,output else value::seen,value::output) ([],[]) values in List.rev reversed
(*
   Convert MIR.VReg to LIR.Reg (virtual)
*)
let vregToLIRReg (M.VReg id)=L.Virtual id
(*
   Convert MIR.VReg to LIR.FReg (virtual float register)
*)
let vregToLIRFReg (M.VReg id)=L.FVirtual id
(*
   Convert MIR.Operand to LIR.Operand
   Booleans as 0/1
   Function address (for higher-order functions)
*)
let convertOperand=function M.Int64Const n -> L.Imm n | M.BoolConst b -> L.Imm (if b then 1L else 0L) | M.FloatSymbol value -> L.FloatSymbol value | M.StringSymbol value -> L.StringSymbol value | M.Register reg -> L.Reg (vregToLIRReg reg) | M.FuncAddr name -> L.FuncAddr name
(*
   Apply type substitution - replaces type variables with concrete types
   Build substitution map from type params to type args
   Unbound - keep as-is
   Concrete types unchanged
*)
let applyTypeSubst typeParams typeArgs typ=
 let subst=if List.length typeParams=List.length typeArgs then SM.of_list (List.combine typeParams typeArgs) else Crash.crash (Printf.sprintf "applyTypeSubst: type argument mismatch: params=%d, args=%d" (List.length typeParams) (List.length typeArgs)) in
 let rec substitute typ=match typ with
 | AST.TVar name | AST.TInferenceVar (_,name) -> Option.value ~default:typ (SM.find_opt name subst)
 | AST.TFunction (params,ret) -> AST.TFunction (List.map substitute params,substitute ret)
 | AST.TTuple elems -> AST.TTuple (List.map substitute elems)
 | AST.TList elem -> AST.TList (substitute elem)
 | AST.TDict (key,value) -> AST.TDict (substitute key,substitute value)
 | AST.TSum (name,args) -> AST.TSum (name,List.map substitute args)
 | _ -> typ in substitute typ
let collectTypeVars typ=
 let rec collect=function AST.TVar name | AST.TInferenceVar (_,name) -> [name] | AST.TFunction (params,ret) -> List.concat_map collect params@collect ret | AST.TTuple elems -> List.concat_map collect elems | AST.TList elem -> collect elem | AST.TDict (key,value) -> collect key@collect value | AST.TRecord (_,args) | AST.TSum (_,args) -> List.concat_map collect args | _ -> [] in distinct (collect typ)
let inferRecordTypeParamsFromFields fields=distinct (List.concat_map (fun (field:M.recordField) -> collectTypeVars field.M.typ) fields)
type integerErrorLabels={divideByZero:L.label;moduloByZero:L.label;moduloNegativeDivisor:L.label}
type tempState={nextRegId:int;nextFRegId:int}
let freshTempReg state=L.Virtual state.nextRegId,{state with nextRegId=addInt state.nextRegId 1}
let freshTempFReg state=L.FVirtual state.nextFRegId,{state with nextFRegId=addInt state.nextFRegId 1}
let rcSumShapeRegistryFromVariantRegistry variantRegistry=SM.map (fun (variants:M.typeVariants) -> ({MemoryModel.typeParams=variants.M.typeParams;payloads=List.map (fun variant -> variant.M.tag,variant.M.payload) (List.stable_sort (fun left right -> Int.compare left.M.tag right.M.tag) variants.M.variants);unaryPayloadTags=MemoryModel.IntSet.of_list (List.filter_map (fun variant -> if variant.M.fieldCount=1 then Some variant.M.tag else None) variants.M.variants)} : MemoryModel.rcSumShapeInfo)) variantRegistry
type printRcContext={recordFields:(string*AST.semanticType) list SM.t;recordTypeParams:string list SM.t;sumShapes:MemoryModel.rcSumShapeRegistry}
let printRcContextFromMirRegistries variantRegistry recordRegistry=
 let recordFields=SM.map (List.map (fun (field:M.recordField) -> field.M.name,field.M.typ)) recordRegistry in
 {recordFields;recordTypeParams=MemoryPlanning.inferredRecordTypeParamsRegistry recordFields;sumShapes=rcSumShapeRegistryFromVariantRegistry variantRegistry}
let rcMetadataForPrintType ctx typ=
 let releasePlan=MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.recordFields ctx.sumShapes typ in
 {MemoryModel.releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan;releasePlan=Some releasePlan;sourceType=Some typ}
let releasePrintedValueFromReg ctx reg typ=
 let shape=MemoryPlanning.rcShapeOfTypeWithSums ctx.recordFields ctx.recordTypeParams ctx.sumShapes typ in
 match MemoryPlanning.rcShapeReleaseOperation shape with
 | Some MemoryModel.DynamicStringBuffer -> if MemoryPlanning.isNullablePointerSumType ctx.sumShapes typ then [L.RefCountDecInt (L.Reg reg)] else [L.RefCountDecString (L.Reg reg)]
 | Some MemoryModel.DynamicIntBuffer -> [L.RefCountDecInt (L.Reg reg)]
 | Some MemoryModel.DynamicBlobBuffer -> [L.RefCountDecBlob (L.Reg reg)]
 | Some (MemoryModel.FixedSizeRoot (size,kind)) -> let kind=match kind with MemoryModel.GenericHeap -> L.GenericHeap | MemoryModel.StreamHeap -> L.StreamHeap | MemoryModel.TaggedList -> L.TaggedList | MemoryModel.DictHeap -> L.DictHeap | MemoryModel.ClosureHeap -> L.ClosureHeap in [L.RefCountDec (reg,size,kind,Some (rcMetadataForPrintType ctx typ))]
 | None -> []
let releasePrintedValue ctx src typ=match src with M.Register reg -> releasePrintedValueFromReg ctx (vregToLIRReg reg) typ | _ -> []
(*
   Ensure operand is in a register (may need to load immediate)
   Need to load constant into a temporary register
   Load boolean (0 or 1) into register
   Load float into FP register, then move bits to GP register
   String references are not used as operands in arithmetic operations
   Load function address into register using ADR instruction
*)
let ensureInRegister operand state=match operand with
 | M.Int64Const n -> let reg,next=freshTempReg state in Ok ([L.Mov (reg,L.Imm n)],reg,next)
 | M.BoolConst b -> let reg,next=freshTempReg state in Ok ([L.Mov (reg,L.Imm (if b then 1L else 0L))],reg,next)
 | M.FloatSymbol value -> let reg,afterReg=freshTempReg state in let freg,next=freshTempFReg afterReg in Ok ([L.FLoad (freg,value);L.FpToGp (reg,freg)],reg,next)
 | M.StringSymbol _ -> Error "Internal error: Cannot use string literal as arithmetic operand"
 | M.Register reg -> Ok ([],vregToLIRReg reg,state)
 | M.FuncAddr name -> let reg,next=freshTempReg state in Ok ([L.LoadFuncAddr (reg,name)],reg,next)
(*
   Ensure a Blob handle is in a register. Blob.empty reuses the immutable
   dynamic-buffer literal pool; nullable Blob sums also compare against zero.
*)
let ensureBlobInRegister operand state=match operand with
 | M.StringSymbol value -> let reg,next=freshTempReg state in Ok ([L.Mov (reg,L.StringSymbol value)],reg,next)
 | M.Int64Const 0L -> let reg,next=freshTempReg state in Ok ([L.Mov (reg,L.Imm 0L)],reg,next)
 | M.Register reg -> Ok ([],vregToLIRReg reg,state)
 | _ -> Error "Internal error: Blob handle must be a literal or register"
(*
   Ensure float operand is in an FP register
   Load float constant into FP register
   Float value already in a virtual register - treat it as FVirtual
*)
let ensureInFRegister operand state=match operand with
 | M.FloatSymbol value -> let reg,next=freshTempFReg state in Ok ([L.FLoad (reg,value)],reg,next)
 | M.Register reg -> Ok ([],vregToLIRFReg reg,state)
 | M.Int64Const _ | M.BoolConst _ -> Error "Internal error: Cannot use integer/boolean as float operand"
 | M.StringSymbol _ -> Error "Internal error: Cannot use string as float operand"
 | M.FuncAddr _ -> Error "Internal error: Cannot use function address as float operand"
(*
   Generate truncation instruction for sized integer arithmetic
   After a 64-bit operation, this sign/zero extends the result to the target width
   to ensure proper overflow behavior (e.g., 127y + 1y = -128)
   Sign-extend byte
   Sign-extend halfword
   Sign-extend word
   Zero-extend byte
   Zero-extend halfword
   Zero-extend word
   No truncation needed for 64-bit
   Non-integer types
*)
let truncateForType reg=function AST.TInt8 -> [L.Sxtb (reg,reg)] | AST.TInt16 -> [L.Sxth (reg,reg)] | AST.TInt32 -> [L.Sxtw (reg,reg)] | AST.TUInt8 -> [L.Uxtb (reg,reg)] | AST.TUInt16 -> [L.Uxth (reg,reg)] | AST.TUInt32 -> [L.Uxtw (reg,reg)] | _ -> []
let shouldCheckNegativeDivisor=function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 -> true | _ -> false
let isUnsignedIntegerType=function AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true | _ -> false
let shiftCountMask=function AST.TInt8 | AST.TUInt8 -> 7L | AST.TInt16 | AST.TUInt16 -> 15L | AST.TInt32 | AST.TUInt32 -> 31L | _ -> 63L
(*
   ARM64 and x86-64 register shifts use the low six count bits in hardware.
   For native 64-bit operands that is exactly the language mask, so emitting
   an explicit AND is redundant. Narrow integers retain an explicit mask
   because their declared count width is smaller than the machine width.
*)
let usesNativeVariableShiftMask=function AST.TInt64 | AST.TUInt64 -> true | _ -> false
let binOpName=function M.Add -> "Add" | M.Sub -> "Sub" | M.Mul -> "Mul" | M.Div -> "Div" | M.Mod -> "Mod" | M.Shl -> "Shl" | M.Shr -> "Shr" | M.BitAnd -> "BitAnd" | M.BitOr -> "BitOr" | M.BitXor -> "BitXor" | M.Eq -> "Eq" | M.Neq -> "Neq" | M.Lt -> "Lt" | M.Gt -> "Gt" | M.Lte -> "Lte" | M.Gte -> "Gte" | M.And -> "And" | M.Or -> "Or"
let comparisonCondition typ op=match op,isUnsignedIntegerType typ with M.Eq,_ -> L.EQ | M.Neq,_ -> L.NE | M.Lt,true -> L.ULT | M.Gt,true -> L.UGT | M.Lte,true -> L.ULE | M.Gte,true -> L.UGE | M.Lt,false -> L.LT | M.Gt,false -> L.GT | M.Lte,false -> L.LE | M.Gte,false -> L.GE | _ -> Crash.crash ("comparisonCondition: non-comparison op "^binOpName op)
(*
   The modulus contract has already established a positive
   divisor. Convert the truncating hardware remainder to a
   Euclidean one by adding divisor & signMask(remainder). The
   quotient register is dead after MSUB, so reuse it as scratch.
*)
let buildIntegerModuloParts dest left right typ state=
 let* leftInstrs,leftReg,afterLeft=ensureInRegister left state in
 let* rightInstrs,rightReg,afterRight=ensureInRegister right afterLeft in
 let quotReg,afterQuot=freshTempReg afterRight in let trunc=truncateForType dest typ in
 let modulo=if isUnsignedIntegerType typ then [L.Udiv (quotReg,leftReg,rightReg);L.Msub (dest,quotReg,rightReg,leftReg)]@trunc else [L.Sdiv (quotReg,leftReg,rightReg);L.Msub (dest,quotReg,rightReg,leftReg);L.Asr_imm (quotReg,dest,63);L.And (quotReg,rightReg,quotReg);L.Add (dest,dest,L.Reg quotReg)]@trunc in
 Ok (leftInstrs@rightInstrs,rightReg,modulo,afterQuot)
let buildFloatArgMoves args destRegs state=
 if args=[] then Ok ([],state) else
 let rec loop remaining regs currentState loads pairs=match remaining,regs with
 | [],_ -> Ok (List.concat (List.rev loads),List.rev pairs,currentState)
 | _,[] -> Error "Internal error: not enough float arg registers"
 | arg::rest,dest::regTail -> match arg with
 | M.FloatSymbol value -> let temp,next=freshTempFReg currentState in loop rest regTail next ([L.FLoad (temp,value)]::loads) ((dest,temp)::pairs)
 | M.Register reg -> loop rest regTail currentState loads ((dest,vregToLIRFReg reg)::pairs)
 | _ -> Error "Internal error: float arg must be a float literal or register" in
 let+ loads,pairs,next=loop args destRegs state [] [] in loads@[L.FArgMoves pairs],next

let selectBinOp dest op left right typ state=
 let lirDest=vregToLIRReg dest in let lirFDest=vregToLIRFReg dest in let rightOp=convertOperand right in
 match typ with
 | AST.TFloat64 ->
 (match op with
 | M.Mod -> Error "Float modulo not yet supported"
 | M.And | M.Or -> Error "Boolean operations not supported on floats"
 | M.Shl | M.Shr | M.BitAnd | M.BitOr | M.BitXor -> Error "Bitwise operations not supported on floats"
 | M.Add | M.Sub | M.Mul | M.Div | M.Eq | M.Neq | M.Lt | M.Gt | M.Lte | M.Gte ->
 let* leftInstrs,leftReg,afterLeft=ensureInFRegister left state in
 let* rightInstrs,rightReg,next=ensureInFRegister right afterLeft in
 let instrs=match op with
 | M.Add -> [L.FAdd (lirFDest,leftReg,rightReg)] | M.Sub -> [L.FSub (lirFDest,leftReg,rightReg)] | M.Mul -> [L.FMul (lirFDest,leftReg,rightReg)] | M.Div -> [L.FDiv (lirFDest,leftReg,rightReg)]
 | M.Eq -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.EQ)] | M.Neq -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.NE)] | M.Lt -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.LT)] | M.Gt -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.GT)] | M.Lte -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.LE)] | M.Gte -> [L.FCmp (leftReg,rightReg);L.Cset (lirDest,L.GE)]
 | M.Mod | M.And | M.Or | M.Shl | M.Shr | M.BitAnd | M.BitOr | M.BitXor -> assert false in
 Ok (leftInstrs@rightInstrs@instrs,next))
 | _ -> let trunc=truncateForType lirDest typ in
 let twoRegisters action truncate=let* li,lr,after=ensureInRegister left state in let+ ri,rr,next=ensureInRegister right after in li@ri@[action lr rr]@(if truncate then trunc else []),next in
 match op with
 | M.Add -> let+ li,lr,next=ensureInRegister left state in li@[L.Add (lirDest,lr,rightOp)]@trunc,next
 | M.Sub -> (match left with M.Int64Const 0L -> let+ ri,rr,next=ensureInRegister right state in ri@[L.Neg (lirDest,rr)]@trunc,next | _ -> let+ li,lr,next=ensureInRegister left state in li@[L.Sub (lirDest,lr,rightOp)]@trunc,next)
 | M.Mul -> twoRegisters (fun l r -> L.Mul (lirDest,l,r)) true
 | M.Div -> twoRegisters (fun l r -> if isUnsignedIntegerType typ then L.Udiv (lirDest,l,r) else L.Sdiv (lirDest,l,r)) true
 | M.Mod -> let+ loads,_,modulo,next=buildIntegerModuloParts lirDest left right typ state in loads@modulo,next
 | M.Eq | M.Neq ->
 let ensure=if typ=AST.TBlob then ensureBlobInRegister else ensureInRegister in
 let* li,lr,after=ensure left state in
 let condition=if op=M.Eq then L.EQ else L.NE in
 if typ=AST.TBlob then let+ ri,rr,next=ensure right after in li@ri@[L.Cmp (lr,L.Reg rr);L.Cset (lirDest,condition)],next
 else Ok (li@[L.Cmp (lr,rightOp);L.Cset (lirDest,condition)],after)
 | M.Lt | M.Gt | M.Lte | M.Gte -> let+ li,lr,next=ensureInRegister left state in li@[L.Cmp (lr,rightOp);L.Cset (lirDest,comparisonCondition typ op)],next
 | M.And | M.Or -> twoRegisters (fun l r -> if op=M.And then L.And (lirDest,l,r) else L.Orr (lirDest,l,r)) false
 | M.Shl -> let* li,lr,afterLeft=ensureInRegister left state in
 (match right with M.Int64Const n -> let masked=Int64.logand n (shiftCountMask typ) in Ok (li@[L.Lsl_imm (lirDest,lr,Int64.to_int masked)]@trunc,afterLeft)
 | _ -> let* ri,rr,afterRight=ensureInRegister right afterLeft in if usesNativeVariableShiftMask typ then Ok (li@ri@[L.Lsl (lirDest,lr,rr)]@trunc,afterRight) else let masked,next=freshTempReg afterRight in Ok (li@ri@[L.And_imm (masked,rr,shiftCountMask typ);L.Lsl (lirDest,lr,masked)]@trunc,next))
 | M.Shr -> let* li,lr,afterLeft=ensureInRegister left state in
 (match right with M.Int64Const n -> let masked=Int64.to_int (Int64.logand n (shiftCountMask typ)) in let instr=if isUnsignedIntegerType typ then L.Lsr_imm (lirDest,lr,masked) else L.Asr_imm (lirDest,lr,masked) in Ok (li@[instr]@trunc,afterLeft)
 | _ -> let* ri,rr,afterRight=ensureInRegister right afterLeft in if usesNativeVariableShiftMask typ then let instr=if isUnsignedIntegerType typ then L.Lsr (lirDest,lr,rr) else L.Asr (lirDest,lr,rr) in Ok (li@ri@[instr]@trunc,afterRight) else let masked,next=freshTempReg afterRight in let instr=if isUnsignedIntegerType typ then L.Lsr (lirDest,lr,masked) else L.Asr (lirDest,lr,masked) in Ok (li@ri@[L.And_imm (masked,rr,shiftCountMask typ);instr]@trunc,next))
 | M.BitAnd -> let* li,lr,afterLeft=ensureInRegister left state in
 let isMask n=n>0L && Int64.logand n (Int64.add n 1L)=0L in
 (match right with M.Int64Const n when isMask n -> Ok (li@[L.And_imm (lirDest,lr,n)]@trunc,afterLeft) | _ -> let+ ri,rr,next=ensureInRegister right afterLeft in li@ri@[L.And (lirDest,lr,rr)]@trunc,next)
 | M.BitOr -> twoRegisters (fun l r -> L.Orr (lirDest,l,r)) true
 | M.BitXor -> twoRegisters (fun l r -> L.Eor (lirDest,l,r)) true
let selectUnaryOp dest op src state=
 let lirDest=vregToLIRReg dest in match op with
 | M.Neg -> (match src with M.FloatSymbol value -> Ok ([L.FLoad (L.FPhysical L.D1,value);L.FNeg (L.FPhysical L.D0,L.FPhysical L.D1)],state) | _ -> let+ loads,reg,next=ensureInRegister src state in loads@[L.Mov (lirDest,L.Imm 0L);L.Sub (lirDest,lirDest,L.Reg reg)],next)
 | M.Not -> let+ loads,reg,next=ensureInRegister src state in loads@[L.Mov (lirDest,L.Imm 1L);L.Sub (lirDest,lirDest,L.Reg reg)],next
 | M.BitNot -> let+ loads,reg,next=ensureInRegister src state in loads@[L.Mvn (lirDest,reg)],next

let intArgRegs=[L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7]
let floatArgRegs=[L.D0;L.D1;L.D2;L.D3;L.D4;L.D5;L.D6;L.D7]
let setupDirectArgs tail closure args argTypes state=
 let argsWithTypes=List.combine args argTypes in
 let intArgs=List.filter (fun (_,typ) -> typ<>AST.TFloat64) argsWithTypes in
 let floatArgs=List.filter (fun (_,typ) -> typ=AST.TFloat64) argsWithTypes in
 let intMoves=match closure with
 | None -> if intArgs=[] then [] else let pairs=List.map (fun (arg,reg) -> reg,convertOperand arg) (List.combine (List.map fst intArgs) (take (List.length intArgs) intArgRegs)) in [if tail then L.TailArgMoves pairs else L.ArgMoves pairs]
 | Some closureReg -> let closureMove=L.X0,L.Reg closureReg in
 let pairs=if intArgs=[] then [] else List.map (fun (arg,reg) -> reg,convertOperand arg) (List.combine (List.map fst intArgs) (take (List.length intArgs) (skip 1 intArgRegs))) in [if tail then L.TailArgMoves (closureMove::pairs) else L.ArgMoves (closureMove::pairs)] in
 let floatResult=if floatArgs=[] then Ok ([],state) else let operands=List.map fst floatArgs in let dests=take (List.length floatArgs) floatArgRegs in buildFloatArgMoves operands dests state in
 intMoves,floatResult
let directReturnMoves dest typ=
 if typ=AST.TFloat64 then [L.FMov (L.FVirtual (-1),L.FPhysical L.D0)],[L.FMov (vregToLIRFReg dest,L.FVirtual (-1))]
 else let reg=vregToLIRReg dest in [],(match reg with L.Physical L.X0 -> [] | _ -> [L.Mov (reg,L.Reg (L.Physical L.X0))])
let selectDirectCall dest name args argTypes returnType state=
 let lirDest=vregToLIRReg dest in
 let save=[L.SaveRegs ([],[])] in
 let intMoves,floatMovesResult=setupDirectArgs false None args argTypes state in
 let call=L.Call (lirDest,name,List.map convertOperand args) in
 let restore=[L.RestoreRegs ([],[])] in
 let before,after=directReturnMoves dest returnType in
 let+ floatMoves,next=floatMovesResult in save@intMoves@floatMoves@[call]@before@restore@after,next
let selectTailCall name args argTypes state=
 let intMoves,floatMovesResult=setupDirectArgs true None args argTypes state in
 let call=L.TailCall (name,List.map convertOperand args) in
 let+ floatMoves,next=floatMovesResult in intMoves@floatMoves@[call],next
let functionAddressInstrs func=match convertOperand func with L.Reg reg -> [L.Mov (L.Physical L.X9,L.Reg reg)] | L.FuncAddr name -> [L.LoadFuncAddr (L.Physical L.X9,name)] | other -> [L.Mov (L.Physical L.X9,other)]
let indirectArgs tail args=if args=[] then [] else let pairs=List.map (fun (arg,reg) -> reg,convertOperand arg) (List.combine args (take (List.length args) intArgRegs)) in [if tail then L.TailArgMoves pairs else L.ArgMoves pairs]
let selectIndirectCall dest func args returnType state=
 let lirDest=vregToLIRReg dest in
 let loads=functionAddressInstrs func in let moves=indirectArgs false args in
 let call=L.IndirectCall (lirDest,L.Physical L.X9,List.map convertOperand args) in
 let moveResult=if returnType=AST.TFloat64 then [L.FMov (vregToLIRFReg dest,L.FPhysical L.D0)] else match lirDest with L.Physical L.X0 -> [] | _ -> [L.Mov (lirDest,L.Reg (L.Physical L.X0))] in
 Ok ([L.SaveRegs ([],[])]@loads@moves@[call;L.RestoreRegs ([],[])]@moveResult,state)
let selectIndirectTailCall func args state=
 let loads=functionAddressInstrs func in let moves=indirectArgs true args in let call=L.IndirectTailCall (L.Physical L.X9,List.map convertOperand args) in Ok (loads@moves@[call],state)
let selectClosureAlloc dest name captures state=
 let reg=vregToLIRReg dest in let size=mulInt (addInt 1 (List.length captures)) 8 in
 let alloc=L.HeapAlloc (reg,size) in let store=L.HeapStore (reg,0,L.FuncAddr name,None) in
 let stores=List.mapi (fun i cap -> L.HeapStore (reg,mulInt (addInt i 1) 8,convertOperand cap,None)) captures in Ok (alloc::store::stores,state)
let selectClosureCall dest closure args argTypes returnType state=
 let lirDest=vregToLIRReg dest in let closureOp=convertOperand closure in let closureReg=L.Physical L.X10 in
 let load=match closureOp with L.Reg reg -> L.Mov (closureReg,L.Reg reg) | other -> L.Mov (closureReg,other) in
 let intMoves,floatMovesResult=setupDirectArgs false (Some closureReg) args argTypes state in
 let loadFunc=L.HeapLoad (L.Physical L.X9,L.Physical L.X0,0) in let call=L.ClosureCall (lirDest,L.Physical L.X9,List.map convertOperand args) in
 let before,after=directReturnMoves dest returnType in
 let+ floatMoves,next=floatMovesResult in [L.SaveRegs ([],[]);load]@intMoves@floatMoves@[loadFunc;call]@before@[L.RestoreRegs ([],[])]@after,next
let selectClosureTailCall closure args argTypes state=
 let closureOp=convertOperand closure in let closureReg=L.Physical L.X10 in
 let load=match closureOp with L.Reg reg -> L.Mov (closureReg,L.Reg reg) | other -> L.Mov (closureReg,other) in
 let intMoves,floatMovesResult=setupDirectArgs true (Some closureReg) args argTypes state in
 let loadFunc=L.HeapLoad (L.Physical L.X9,L.Physical L.X0,0) in let call=L.ClosureTailCall (L.Physical L.X9,List.map convertOperand args) in
 let+ floatMoves,next=floatMovesResult in [load]@intMoves@floatMoves@[loadFunc;call],next

let operandValue=function
 | M.Int64Const n -> StructuralValue.Union ("Int64Const",[StructuralValue.Scalar (Int64.to_string n^"L")])
 | M.BoolConst b -> StructuralValue.Union ("BoolConst",[StructuralValue.Scalar (string_of_bool b)])
 | M.FloatSymbol value -> StructuralValue.Union ("FloatSymbol",[StructuralValue.Scalar (FloatFormat.structural value)])
 | M.StringSymbol value -> StructuralValue.Union ("StringSymbol",[StructuralValue.Text value])
 | M.Register (M.VReg id) -> StructuralValue.Union ("Register",[StructuralValue.Union ("VReg",[StructuralValue.Scalar (string_of_int id)])])
 | M.FuncAddr name -> StructuralValue.Union ("FuncAddr",[AST.DiagnosticFormatting.func name])
let operandText operand=StructuralFormat.format (operandValue operand)
let selectPrint src typ variants records ctx state=
 let finish instrs=Ok ([L.SaveRegs ([],[])]@instrs@[L.RestoreRegs ([],[])]@releasePrintedValue ctx src typ,state) in
 let finishReg reg instrs=Ok ([L.SaveRegs ([],[])]@instrs@[L.RestoreRegs ([],[])]@releasePrintedValueFromReg ctx reg typ,state) in
 let moveTo reg=match convertOperand src with L.Reg actual when actual=reg -> [] | other -> [L.Mov (reg,other)] in
 let x0=L.Physical L.X0 in let x19=L.Physical L.X19 in
 match typ with
 | AST.TBool -> finish (moveTo x0@[L.PrintBool x0])
 | AST.TDateTime -> Error "DateTime values must be rendered through Stdlib.DateTime.toString before print lowering"
 | AST.TStream _ -> Error "Stream values must be rendered opaquely before print lowering"
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 -> finish (moveTo x0@[L.PrintInt64 x0])
 | AST.TUInt64 -> finish (moveTo x0@[L.PrintUInt64 x0])
 | AST.TFloat64 -> (match src with M.FloatSymbol value -> finish [L.FLoad (L.FPhysical L.D0,value);L.PrintFloat (L.FPhysical L.D0)] | M.Register reg -> finish [L.FMov (L.FPhysical L.D0,vregToLIRFReg reg);L.PrintFloat (L.FPhysical L.D0)] | _ -> Error "Internal error: unexpected operand type for float print")
 | AST.TString | AST.TChar | AST.TInt -> (match src with M.StringSymbol value -> finish [L.PrintString value] | M.Register reg -> finish [L.PrintHeapString (vregToLIRReg reg)] | other -> Error ("Print: Unexpected operand type for string: "^operandText other))
 | AST.TInt128 | AST.TUInt128 -> Error "128-bit values must be rendered to String before print lowering"
 | AST.TTuple elems ->
 let saveAddr=[L.Mov (x19,convertOperand src)] in
 let elemInstrs=List.concat (List.mapi (fun index elemType ->
 let load=L.HeapLoad (x0,x19,mulInt index 8) in let separator=if index>0 then [L.PrintChars [44;32]] else [] in
 let print=match elemType with
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 -> [L.PrintInt64NoNewline x0]
 | AST.TUInt64 -> [L.PrintUInt64NoNewline x0]
 | AST.TBool -> [L.PrintBoolNoNewline x0]
 | AST.TFloat64 -> [L.GpToFp (L.FPhysical L.D0,x0);L.PrintFloatNoNewline (L.FPhysical L.D0)]
 | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 -> [L.PrintHeapStringNoNewline x0]
 | AST.TList _ -> Crash.crash ("Unsupported nested list tuple element type for printing: "^StructuralFormat.semanticType elemType)
 | other -> Crash.crash ("Unsupported tuple element type for printing: "^StructuralFormat.semanticType other) in separator@[load]@print) elems) in
 finishReg x19 (saveAddr@[L.PrintChars [40]]@elemInstrs@[L.PrintChars [41;10]])
 | AST.TList elem when elem=AST.TInt128 || elem=AST.TUInt128 -> Crash.crash ("Unsupported Int128/UInt128 list element type for printing: "^StructuralFormat.semanticType elem)
 | AST.TList elem -> finishReg x19 (moveTo x19@[L.PrintList (x19,elem)])
 | AST.TSum (name,args) -> (match SM.find_opt name variants with
 | None -> Crash.crash ("Missing sum variant metadata for printing sum type '"^name^"'")
 | Some (variants:M.typeVariants) ->
 let substituted=List.map (fun variant -> let payload=Option.map (applyTypeSubst variants.M.typeParams args) variant.M.payload in variant.M.name,variant.M.tag,payload) variants.M.variants in
 let moves=moveTo x19 in
 let transparent=variants.M.typeParams=[] && (match substituted with [_,_,Some (AST.TInt64|AST.TUInt64|AST.TBool|AST.TString|AST.TChar)] -> true | _ -> false) in
 finishReg x19 (moves@[L.PrintSum (x19,substituted,transparent)]))
 | AST.TRecord (name,args) -> (match SM.find_opt name records with
 | None -> Error ("Print: Record type '"^name^"' not found in recordRegistry")
 | Some fields -> let moves=moveTo x19 in let params=inferRecordTypeParamsFromFields fields in
 let fields=List.map (fun (field:M.recordField) -> let typ=if List.length params=List.length args then applyTypeSubst params args field.M.typ else Crash.crash (Printf.sprintf "Record print type argument mismatch for '%s': params=%d, args=%d" name (List.length params) (List.length args)) in field.M.name,typ) fields in finishReg x19 (moves@[L.PrintRecord (x19,name,fields)]))
 | AST.TDict _ -> finish (moveTo x0@[L.PrintInt64 x0])
 | AST.TUnit | AST.TNever -> finish [L.PrintChars [40;41;10]]
 | AST.TFunction _ | AST.TInternalRawPtr -> finish (moveTo x0@[L.PrintInt64 x0])
 | AST.TBlob -> finishReg x19 (moveTo x19@[L.PrintBlob x19])
 | AST.TVar _ | AST.TInferenceVar _ -> Error "Internal error: type variable reached MIR_to_LIR (should be monomorphized)"

(*
   Convert MIR instruction to LIR instructions
   floatRegs: Set of VReg IDs that hold float values (from MIR.Function.FloatRegs)
   Check if this is a float move - either by valueType or by source operand type
   Float move - use FP registers
   Load float constant
   Move between float registers
   Reinterpret GP register bits as float (e.g., heap-loaded float payloads)
   Integer/other move
   Check if this is a float operation
   Float operations - use FP registers and instructions
   Float comparisons - result goes in integer register
   Integer operations - existing logic
   Note: After each arithmetic operation, we truncate to the target width
   to ensure proper overflow behavior (e.g., 127y + 1y = -128 for Int8)
   ADD can have immediate or register as right operand
   Left operand must be in a register
   Negation is subtraction from zero and maps directly to a native
   instruction on both supported architectures.
   SUB can have immediate or register as right operand
   MUL requires both operands in registers
   Division requires both operands in registers.
   Comparisons: CMP + CSET sequence
   Boolean operations (bitwise for 0/1 values)
   Bitwise operators (also need truncation for proper overflow)
   Check if shift amount is a constant (0-63)
   Check if right operand is a valid bitmask immediate (power-of-2 minus 1)
   These are values like 0x1, 0x3, 0x7, 0xF, etc. (ones run from bit 0)
   Check if source is a float - use FP negation
   Float negation: load float into D1, negate into D0
   Integer negation: 0 - src
   Boolean NOT: 1 - src (since booleans are 0 or 1)
   Bitwise NOT: flip all bits using MVN instruction
   ARM64 calling convention (AAPCS64):
   - Integer arguments in X0-X7 (using separate counter)
   - Float arguments in D0-D7 (using separate counter)
   - Return value in X0 (int) or D0 (float)
   IMPORTANT: Save caller-saved registers BEFORE setting up arguments
   Empty placeholder - register allocator will fill in the actual registers to save
   Separate args into int and float based on argTypes
   Generate ArgMoves for integer arguments
   Call instruction
   Restore caller-saved registers after the call
   Empty placeholder - register allocator will fill in the actual registers to restore
   Move return value from X0 or D0 to destination based on return type
   D16 is reserved from allocation and survives this call's RestoreRegs.
   Keep the return there while restoring a live D0.
   This handles the case where destFReg maps to D0 (which would be clobbered by RestoreRegs).
   Float return: value is in D0, use reserved D16 as intermediate
   Integer return: value is in X0
   Tail call optimization: Skip SaveRegs/RestoreRegs, use B instead of BL
   Generate TailArgMoves for integer arguments (uses temp registers, no SaveRegs)
   Generate FArgMoves for float arguments
   Tail call instruction (no SaveRegs/RestoreRegs)
   Indirect call through function pointer (BLR instruction)
   Similar to direct call but uses function address in register
   Save caller-saved registers
   IMPORTANT: Load function address into X9 FIRST, before setting up arguments.
   The function pointer might be in X0-X7 which will be overwritten by argument moves.
   Always copy to X9 in case the source register is overwritten by arg moves
   Load operand into X9
   Use ArgMoves for parallel move - handles register clobbering correctly
   Call through X9 (always, since we always copy to X9 now)
   Restore caller-saved registers
   Float return: value is in D0, move to FVirtual
   Indirect tail call: use BR instead of BLR, no SaveRegs/RestoreRegs
   Load function address into X9 FIRST
   Use TailArgMoves for parallel move (uses temp registers, no SaveRegs)
   Indirect tail call through X9
   Allocate closure: (func_addr, cap1, cap2, ...)
   This is similar to TupleAlloc but first element is a function address
   Store function pointer at offset 0 (always int/pointer type)
   Store captured values at offsets 8, 16, ... (assume int/pointer for captures)
   Call through closure: extract func_ptr from closure[0], call with (closure, args...)
   Load closure into a temp register first
   Use X10 for closure (not an arg register)
   Generate ArgMoves for closure (X0) and integer arguments (X1-X7)
   Load function pointer from closure[0] into X9
   IMPORTANT: This must come AFTER argMoves because ArgMoves may use X9 as a temp
   (e.g., StringSymbol conversion uses X9 for ADRP/ADD_label)
   After ArgMoves, X0 contains the closure, so we load from [X0, 0]
   D16 is reserved from allocation and survives RestoreRegs.
   Closure tail call: skip SaveRegs/RestoreRegs, use BR
   Generate TailArgMoves for closure (X0) and integer arguments (X1-X7)
   IMPORTANT: This must come AFTER argMoves because TailArgMoves may use X9 as a temp
   After TailArgMoves, X0 contains the closure, so we load from [X0, 0]
   For float values, we need to move the float bits from FReg to GP register
   since HeapStore uses GP registers. Use FpToGp to transfer bits.
   Float in FVirtual register - move its bits through an allocated GP
   temporary. A fixed scratch register can alias the scratch used to
   reload a spilled destination address on x86-64.
   After FpToGp, value is in GP register, so use None for valueType
   (otherwise CodeGen would try to treat the GP register as a float register)
   Float load: load into integer register, then move bits to float register
   Use temp register for heap load
   Integer/other load
   Printers clobber caller-saved registers; preserve other live values
   before consuming the printed ownership root.
   Float needs to be in D0 for printing
   Literal float - load into D0
   Computed float - it's in an FVirtual register, move to D0 for printing
   String/Char printing uses PrintString for pool strings, PrintHeapString for heap strings.
   Char is stored as a string at runtime (single EGC).
   Heap string (from concatenation): use PrintHeapString
   Tuple printing: (elem1, elem2, ...)
   Use X19 (callee-saved) to hold tuple address throughout printing
   since PrintChars clobbers caller-saved registers (X0-X3)
   Generate instructions to print each element
   Use no-newline versions for tuple elements
   Load directly to X0 for printing
   ", "
   Float is in X0 as raw bits, move to D0 for printing
   Combine: save addr + "(" + elements + ")\n"
   Print list as [elem1, elem2, ...]
   Sum type printing: look up variants and generate PrintSum
   Apply type substitution to payload types
   Move sum pointer to X19 (callee-saved for print operations)
   Print record with field names and values
   Move record address to callee-saved X19 (preserved through syscalls)
   Convert RecordField list to tuple format for LIR
   Dict: print address for now
   Unit: print "()" with newline
   Runtime-error expressions are normalized to Unit before print insertion,
   but keep this branch explicit for exhaustiveness.
   Functions shouldn't be printed, but just print address
   Raw pointer: print address
   Blob: render as the interpreter-compatible ephemeral marker.
   Type variables should be monomorphized away before reaching LIR
   ptr and length must be in registers
   numBytes must be in a register for LIR
   ptr must be in a register
   Both ptr and byteOffset must be in registers
   Float load: load raw bits into GP register, then move to FP register
   Use temp register for raw get
   All three operands must be in registers
   Handle StringSymbol specially - use Mov which CodeGen handles (converts to heap format)
   Float store: ensure value is in FP register, then convert to GP for storage
   Use temp register for FpToGp
   src is an integer operand that needs to be in an integer register
   src is a float operand that needs to be in a float register
   FloatToBits uses FpToGp (bit copy, not conversion)
   The long-standing argv helper already returns through the
   requested destination and is performance-critical at benchmark
   startup. Keep its established lowering separate from the new
   inline native operations, whose syscall and allocation scratch
   registers require explicit caller preservation.
   Ensure value is in an FP register
   Convert MIR.Phi to LIR.Phi (int) or LIR.FPhi (float)
   Check if this is a float phi by:
   1. valueType is Some TFloat64 (set by SSA for parameters), OR
   2. destination VReg is in floatRegs (set during MIR generation and SSA renaming)
   Float phi uses FReg (FVirtual) registers
   Integer phi uses Reg (Virtual) registers
*)
let selectInstr arch instr variantRegistry recordRegistry printRcContext floatRegs state=
 let ensure=ensureInRegister in
 let integerUnary action src=let+ loads,reg,next=ensure src state in loads@[action reg],next in
 let floatUnary action src=let+ loads,reg,next=ensureInFRegister src state in loads@[action reg],next in
 let integerBinary action left right=let* li,lr,after=ensure left state in let+ ri,rr,next=ensure right after in li@ri@[action lr rr],next in
 let integerTernary action first second third=let* ai,ar,afterFirst=ensure first state in let* bi,br,afterSecond=ensure second afterFirst in let+ ci,cr,next=ensure third afterSecond in ai@bi@ci@[action ar br cr],next in
 let pure value=Ok ([value],state) in
 match instr with
 | M.Mov (dest,src,valueType) -> let isFloat=match valueType with Some AST.TFloat64 -> true | _ -> (match src with M.FloatSymbol _ -> true | _ -> false) in
 if isFloat then let fdest=vregToLIRFReg dest in (match src with M.FloatSymbol value -> pure (L.FLoad (fdest,value)) | M.Register ((M.VReg id) as reg) -> if M.IntSet.mem id floatRegs then pure (L.FMov (fdest,vregToLIRFReg reg)) else pure (L.GpToFp (fdest,vregToLIRReg reg)) | _ -> Error "Internal error: non-float operand in float Mov") else pure (L.Mov (vregToLIRReg dest,convertOperand src))
 | M.BinOp (dest,op,left,right,typ) -> selectBinOp dest op left right typ state
 | M.UnaryOp (dest,op,src) -> selectUnaryOp dest op src state
 | M.Call (dest,name,args,argTypes,ret) -> selectDirectCall dest name args argTypes ret state
 | M.TailCall (name,args,argTypes,_) -> selectTailCall name args argTypes state
 | M.IndirectCall (dest,func,args,_,ret) -> selectIndirectCall dest func args ret state
 | M.IndirectTailCall (func,args,_,_) -> selectIndirectTailCall func args state
 | M.ClosureAlloc (dest,name,captures) -> selectClosureAlloc dest name captures state
 | M.ClosureCall (dest,closure,args,argTypes,ret) -> selectClosureCall dest closure args argTypes ret state
 | M.ClosureTailCall (closure,args,argTypes) -> selectClosureTailCall closure args argTypes state
 | M.HeapAlloc (dest,size) -> pure (L.HeapAlloc (vregToLIRReg dest,size))
 | M.HeapStore (addr,offset,src,valueType) -> let addr=vregToLIRReg addr in
 (match src,valueType with M.Register reg,Some AST.TFloat64 -> let freg=vregToLIRFReg reg in let temp,next=match arch with Platform.ARM64 -> L.Physical L.X9,state | Platform.X86_64 -> freshTempReg state in Ok ([L.FpToGp (temp,freg);L.HeapStore (addr,offset,L.Reg temp,None)],next) | _ -> pure (L.HeapStore (addr,offset,convertOperand src,valueType)))
 | M.HeapLoad (dest,addr,offset,valueType) -> let addr=vregToLIRReg addr in (match valueType with Some AST.TFloat64 -> let temp=L.Physical L.X9 in Ok ([L.HeapLoad (temp,addr,offset);L.GpToFp (vregToLIRFReg dest,temp)],state) | _ -> pure (L.HeapLoad (vregToLIRReg dest,addr,offset)))
 | M.RefCountInc (addr,size,kind,typ) -> let kind=match kind with M.GenericHeap -> L.GenericHeap | M.StreamHeap -> L.StreamHeap | M.TaggedList -> L.TaggedList | M.DictHeap -> L.DictHeap | M.ClosureHeap -> L.ClosureHeap in pure (L.RefCountInc (vregToLIRReg addr,size,kind,typ))
 | M.RefCountDec (addr,size,kind,typ) -> let kind=match kind with M.GenericHeap -> L.GenericHeap | M.StreamHeap -> L.StreamHeap | M.TaggedList -> L.TaggedList | M.DictHeap -> L.DictHeap | M.ClosureHeap -> L.ClosureHeap in pure (L.RefCountDec (vregToLIRReg addr,size,kind,typ))
 | M.Print (src,typ) -> selectPrint src typ variantRegistry recordRegistry printRcContext state
 | M.RuntimeError message -> pure (L.RuntimeError message)
 | M.RuntimeErrorString message -> (match convertOperand message with L.Reg reg -> pure (L.RuntimeErrorString reg) | _ -> Error "Internal error: dynamic runtime error message must be held in a register")
 | M.StringConcat (dest,first,second,rest) -> pure (L.StringConcat (vregToLIRReg dest,convertOperand first,convertOperand second,List.map convertOperand rest))
 | M.CanonicalBufferEq (dest,kind,left,right) -> pure (L.CanonicalBufferEq (vregToLIRReg dest,kind,convertOperand left,convertOperand right))
 | M.StdoutWrite (effectId,value,newline) -> pure (L.StdoutWrite (effectId,convertOperand value,newline))
 | M.StdinReadLine ((M.VReg effectId) as dest) -> pure (L.StdinReadLine (effectId,vregToLIRReg dest))
 | M.FileReadBlob (dest,path) -> pure (L.FileReadBlob (vregToLIRReg dest,convertOperand path))
 | M.FileExists (dest,path) -> pure (L.FileExists (vregToLIRReg dest,convertOperand path))
 | M.FileWriteBlob (dest,path,content) -> pure (L.FileWriteBlob (vregToLIRReg dest,convertOperand path,convertOperand content))
 | M.FileAppendText (dest,path,content) -> pure (L.FileAppendText (vregToLIRReg dest,convertOperand path,convertOperand content))
 | M.FileDelete (dest,path) -> pure (L.FileDelete (vregToLIRReg dest,convertOperand path))
 | M.FileCreateDirectory (dest,path) -> pure (L.FileCreateDirectory (vregToLIRReg dest,convertOperand path))
 | M.FileSetExecutable (dest,path) -> pure (L.FileSetExecutable (vregToLIRReg dest,convertOperand path))
 | M.FileWriteFromPtr (dest,path,ptr,length) -> let dest=vregToLIRReg dest in let path=convertOperand path in integerBinary (fun ptr length -> L.FileWriteFromPtr (dest,path,ptr,length)) ptr length
 | M.RawAlloc (dest,size) -> integerUnary (fun reg -> L.RawAlloc (vregToLIRReg dest,reg)) size
 | M.MappedAlloc (dest,size) -> let dest=vregToLIRReg dest in let+ loads,reg,next=ensure size state in loads@[L.SaveRegs ([],[]);L.MappedAlloc (L.Physical L.X0,reg);L.RestoreRegs ([],[]);L.Mov (dest,L.Reg (L.Physical L.X0))],next
 | M.RawFree ptr -> integerUnary (fun reg -> L.RawFree reg) ptr
 | M.MappedFree ptr -> let+ loads,reg,next=ensure ptr state in loads@[L.SaveRegs ([],[]);L.MappedFree reg;L.RestoreRegs ([],[])],next
 | M.RawGet (dest,ptr,offset,valueType) -> let* pi,pr,afterPtr=ensure ptr state in let+ oi,or_,next=ensure offset afterPtr in
 let instrs=match valueType with Some AST.TFloat64 -> let temp=L.Physical L.X9 in [L.RawGet (temp,pr,or_);L.GpToFp (vregToLIRFReg dest,temp)] | _ -> [L.RawGet (vregToLIRReg dest,pr,or_)] in pi@oi@instrs,next
 | M.RawGetByte (dest,ptr,offset) -> integerBinary (fun p o -> L.RawGetByte (vregToLIRReg dest,p,o)) ptr offset
 | M.RawWriteWord (ptr,offset,value) -> integerTernary (fun p o v -> L.RawWriteWord (p,o,v)) ptr offset value
 | M.RawWriteByte (ptr,offset,value) -> integerTernary (fun p o v -> L.RawWriteByte (p,o,v)) ptr offset value
 | M.RawSlotInit (ptr,offset,value,typ) -> let* pi,pr,afterPtr=ensure ptr state in let* oi,or_,afterOffset=ensure offset afterPtr in
 (match value with M.StringSymbol _ -> let temp,next=freshTempReg afterOffset in let mov=L.Mov (temp,convertOperand value) in Ok (pi@oi@[mov;L.RawSlotInit (pr,or_,temp,typ)],next) | _ ->
 if typ=AST.TFloat64 then let+ vi,vr,next=ensureInFRegister value afterOffset in let temp=L.Physical L.X9 in pi@oi@vi@[L.FpToGp (temp,vr);L.RawSlotInit (pr,or_,temp,typ)],next
 else let+ vi,vr,next=ensure value afterOffset in pi@oi@vi@[L.RawSlotInit (pr,or_,vr,typ)],next)
 | M.StringToRawPtr (dest,value) | M.RawPtrToString (dest,value) | M.BlobToRawPtr (dest,value) | M.RawPtrToBlob (dest,value) -> pure (L.Mov (vregToLIRReg dest,convertOperand value))
 | M.DictToRawPtr (dest,dict) -> let* di,dr,after=ensure dict state in let+ mi,mr,next=ensure (M.Int64Const (-4L)) after in di@mi@[L.And (vregToLIRReg dest,dr,mr)],next
 | M.RawPtrToDict (dest,ptr,tag) -> integerBinary (fun p t -> L.Orr (vregToLIRReg dest,p,t)) ptr tag
 | M.ListToRawPtr (dest,value) -> integerUnary (fun reg -> L.And_imm (vregToLIRReg dest,reg,-8L)) value
 | M.RawPtrToList (dest,ptr,tag) -> integerBinary (fun p t -> L.Orr (vregToLIRReg dest,p,t)) ptr tag
 | M.FloatSqrt (dest,src) -> floatUnary (fun reg -> L.FSqrt (vregToLIRFReg dest,reg)) src
 | M.FloatAbs (dest,src) -> floatUnary (fun reg -> L.FAbs (vregToLIRFReg dest,reg)) src
 | M.FloatNeg (dest,src) -> floatUnary (fun reg -> L.FNeg (vregToLIRFReg dest,reg)) src
 | M.Int64ToFloat (dest,src) -> integerUnary (fun reg -> L.Int64ToFloat (vregToLIRFReg dest,reg)) src
 | M.FloatToInt64 (dest,src) -> floatUnary (fun reg -> L.FloatToInt64 (vregToLIRReg dest,reg)) src
 | M.FloatToBits (dest,src) -> floatUnary (fun reg -> L.FloatToBits (vregToLIRReg dest,reg)) src
 | M.RefCountIncString value -> pure (L.RefCountIncString (convertOperand value))
 | M.RefCountDecString value -> pure (L.RefCountDecString (convertOperand value))
 | M.RefCountIncBlob value -> pure (L.RefCountIncBlob (convertOperand value))
 | M.RefCountDecBlob value -> pure (L.RefCountDecBlob (convertOperand value))
 | M.RefCountIncInt value -> pure (L.RefCountIncInt (convertOperand value))
 | M.RefCountDecInt value -> pure (L.RefCountDecInt (convertOperand value))
 | M.RandomInt64 dest -> pure (L.RandomInt64 (vregToLIRReg dest))
 | M.DateTimeNow dest -> pure (L.DateTimeNow (vregToLIRReg dest))
 | M.Sleep (effectId,dest,delay) -> let+ loads,reg,next=ensureInFRegister delay state in loads@[L.SaveRegs ([],[]);L.Sleep (effectId,reg);L.RestoreRegs ([],[]);L.Mov (vregToLIRReg dest,L.Imm 0L)],next
 | M.CliNative (dest,operation,args) -> let operation=convertCliOperation operation in let args=List.map convertOperand args in
 (match operation with L.GetArgv -> pure (L.CliNative (vregToLIRReg dest,operation,args)) | _ -> Ok ([L.SaveRegs ([],[]);L.CliNative (L.Physical L.X0,operation,args);L.RestoreRegs ([],[]);L.Mov (vregToLIRReg dest,L.Reg (L.Physical L.X0))],state))
 | M.FloatToString (dest,value) -> floatUnary (fun reg -> L.FloatToString (vregToLIRReg dest,reg)) value
 | M.CoverageHit id -> pure (L.CoverageHit id)
 | M.Phi ((M.VReg destId) as dest,sources,typ) ->
 let isFloat=match typ with Some AST.TFloat64 -> true | _ -> M.IntSet.mem destId floatRegs in
 if isFloat then let rec buildSources=function [] -> Ok [] | (op,M.Label label)::rest -> (match op with M.Register reg -> let+ tail=buildSources rest in (vregToLIRFReg reg,L.Label label)::tail | _ -> Error ("FPhi source must be a register, got: "^operandText op)) in
 let+ sources=buildSources sources in [L.FPhi (vregToLIRFReg dest,sources)],state
 else let sources=List.map (fun (op,M.Label label) -> convertOperand op,L.Label label) sources in pure (L.Phi (vregToLIRReg dest,sources,typ))

(*
   Convert MIR terminator to LIR terminator
   For Branch, need to convert operand to register (may add instructions)
   Printing is now handled by MIR.Print instruction, not in terminator
   Load float into D0 for return
   Return bool as 0/1
   Return string symbol address in X0 so callers can use the value.
   Top-level printing is handled by MIR.Print, not return lowering.
   Float return - move to D0 via FMov
   Integer/other return - move operand to X0
   Convert MIR.Label to LIR.Label
   Condition must be in a register for ARM64 branch instructions
*)
let selectTerminator terminator returnType state=match terminator with
 | M.Ret operand -> (match operand with
 | M.FloatSymbol value -> Ok ([L.FLoad (L.FPhysical L.D0,value)],L.Ret,state)
 | M.BoolConst b -> Ok ([L.Mov (L.Physical L.X0,L.Imm (if b then 1L else 0L))],L.Ret,state)
 | M.StringSymbol _ -> Ok ([L.Mov (L.Physical L.X0,convertOperand operand)],L.Ret,state)
 | M.Register reg when returnType=AST.TFloat64 -> Ok ([L.FMov (L.FPhysical L.D0,vregToLIRFReg reg)],L.Ret,state)
 | _ -> Ok ([L.Mov (L.Physical L.X0,convertOperand operand)],L.Ret,state))
 | M.Branch (cond,M.Label yes,M.Label no) -> let+ instrs,reg,next=ensureInRegister cond state in instrs,L.Branch (reg,L.Label yes,L.Label no),next
 | M.Jump (M.Label label) -> Ok ([],L.Jump (L.Label label),state)
(*
   Convert MIR label to LIR label
*)
let convertLabel (M.Label label)=L.Label label
let maxVRegId (M.VReg id) currentMax=max currentMax id
let maxVRegIdFromOperand operand currentMax=match operand with M.Register reg -> maxVRegId reg currentMax | _ -> currentMax
let maxVRegIdsFromOperands operands currentMax=List.fold_left (fun largest operand -> maxVRegIdFromOperand operand largest) currentMax operands

let maxVRegIdFromInstr instr currentMax = (match instr with
| MIR.Mov (dest, src, _) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand src)
| MIR.BinOp (dest, _, left, right, _) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand left |> maxVRegIdFromOperand right)
| MIR.UnaryOp (dest, _, src) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand src)
| MIR.Call (dest, _, args, _, _) -> (currentMax |> maxVRegId dest |> maxVRegIdsFromOperands args)
| MIR.TailCall (_, args, _, _) -> (currentMax |> maxVRegIdsFromOperands args)
| MIR.IndirectCall (dest, func, args, _, _) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand func |> maxVRegIdsFromOperands args)
| MIR.IndirectTailCall (func, args, _, _) -> (currentMax |> maxVRegIdFromOperand func |> maxVRegIdsFromOperands args)
| MIR.ClosureAlloc (dest, _, captures) -> (currentMax |> maxVRegId dest |> maxVRegIdsFromOperands captures)
| MIR.ClosureCall (dest, closure, args, _, _) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand closure |> maxVRegIdsFromOperands args)
| MIR.ClosureTailCall (closure, args, _) -> (currentMax |> maxVRegIdFromOperand closure |> maxVRegIdsFromOperands args)
| MIR.HeapAlloc (dest, _) -> (maxVRegId dest currentMax)
| MIR.HeapStore (addr, _, src, _) -> (currentMax |> maxVRegId addr |> maxVRegIdFromOperand src)
| MIR.HeapLoad (dest, addr, _, _) -> (currentMax |> maxVRegId dest |> maxVRegId addr)
| MIR.StringConcat (dest, first, second, remaining) -> (currentMax |> maxVRegId dest |> maxVRegIdsFromOperands (first :: second :: remaining))
| MIR.CanonicalBufferEq (dest, _, left, right) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand left |> maxVRegIdFromOperand right)
| MIR.RefCountInc (addr, _, _, _) | MIR.RefCountDec (addr, _, _, _) -> (maxVRegId addr currentMax)
| MIR.Print (src, _) -> (maxVRegIdFromOperand src currentMax)
| MIR.StdoutWrite (_, src, _) -> (maxVRegIdFromOperand src currentMax)
| MIR.StdinReadLine dest -> (maxVRegId dest currentMax)
| MIR.RuntimeError _ -> (currentMax)
| MIR.RuntimeErrorString message -> (maxVRegIdFromOperand message currentMax)
| MIR.FileReadBlob (dest, path) | MIR.FileExists (dest, path) | MIR.FileDelete (dest, path) | MIR.FileCreateDirectory (dest, path) | MIR.FileSetExecutable (dest, path) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand path)
| MIR.FileWriteBlob (dest, path, content) | MIR.FileAppendText (dest, path, content) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand path |> maxVRegIdFromOperand content)
| MIR.FileWriteFromPtr (dest, path, ptr, length) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand path |> maxVRegIdFromOperand ptr |> maxVRegIdFromOperand length)
| MIR.FloatSqrt (dest, src) | MIR.FloatAbs (dest, src) | MIR.FloatNeg (dest, src) | MIR.Int64ToFloat (dest, src) | MIR.FloatToInt64 (dest, src) | MIR.FloatToBits (dest, src) | MIR.RawAlloc (dest, src) | MIR.MappedAlloc (dest, src) | MIR.StringToRawPtr (dest, src) | MIR.RawPtrToString (dest, src) | MIR.BlobToRawPtr (dest, src) | MIR.RawPtrToBlob (dest, src) | MIR.DictToRawPtr (dest, src) | MIR.ListToRawPtr (dest, src) | MIR.FloatToString (dest, src) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand src)
| MIR.RawFree ptr | MIR.MappedFree ptr | MIR.RefCountIncString ptr | MIR.RefCountDecString ptr | MIR.RefCountIncBlob ptr | MIR.RefCountDecBlob ptr -> (maxVRegIdFromOperand ptr currentMax)
| MIR.RefCountIncInt ptr | MIR.RefCountDecInt ptr -> (maxVRegIdFromOperand ptr currentMax)
| MIR.RawGet (dest, ptr, byteOffset, _) | MIR.RawGetByte (dest, ptr, byteOffset) | MIR.RawPtrToDict (dest, ptr, byteOffset) | MIR.RawPtrToList (dest, ptr, byteOffset) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand ptr |> maxVRegIdFromOperand byteOffset)
| MIR.RawWriteWord (ptr, byteOffset, value) -> (currentMax |> maxVRegIdFromOperand ptr |> maxVRegIdFromOperand byteOffset |> maxVRegIdFromOperand value)
| MIR.RawWriteByte (ptr, byteOffset, value) | MIR.RawSlotInit (ptr, byteOffset, value, _) -> (currentMax |> maxVRegIdFromOperand ptr |> maxVRegIdFromOperand byteOffset |> maxVRegIdFromOperand value)
| MIR.RandomInt64 dest | MIR.DateTimeNow dest -> (maxVRegId dest currentMax)
| MIR.Sleep (_, dest, delayMs) -> (currentMax |> maxVRegId dest |> maxVRegIdFromOperand delayMs)
| MIR.CliNative (dest, _, args) -> (currentMax |> maxVRegId dest |> maxVRegIdsFromOperands args)
| MIR.Phi (dest, sources, _) -> List.fold_left (fun largest (operand,_) -> maxVRegIdFromOperand operand largest) (maxVRegId dest currentMax) sources
| MIR.CoverageHit _ -> (currentMax))

let maxVRegIdFromTerminator terminator currentMax=match terminator with M.Ret op | M.Branch (op,_,_) -> maxVRegIdFromOperand op currentMax | M.Jump _ -> currentMax
(*
   LIR reserves virtual IDs through 3999 for spill and ABI temporaries.
*)
let initTempState (func:M.functionDef)=
 let maxParam=List.fold_left (fun largest (param:M.typedMIRParam) -> maxVRegId param.M.reg largest) (-1) func.M.typedParams in
 let maxReg=M.LabelMap.fold (fun _ block largest -> maxVRegIdFromTerminator block.M.terminator (List.fold_left (fun largest instr -> maxVRegIdFromInstr instr largest) largest block.M.instrs)) func.M.cfg.M.blocks maxParam in
 let maxFloat=M.IntSet.fold max func.M.floatRegs (-1) in
 {nextRegId=max 4000 (addInt maxReg 1);nextFRegId=max 4000 (addInt maxFloat 1)}
let integerErrorBlock label message : L.basicBlock={L.label;instrs=[L.RuntimeError message];terminator=L.Ret}
(*
   A statically non-zero divisor needs no runtime guard. Besides
   removing a redundant branch, this preserves the constant as a
   loop invariant for later optimization passes.
   Constant divisors that satisfy the public modulus contract do
   not need a runtime guard.
   The hot path uses one comparison and branch. Only
   the cold invalid path distinguishes the canonical
   zero and negative-divisor error messages.
   RuntimeError exits the process. Lowering unreachable
   instructions can otherwise reject a dead print whose
   placeholder operand has no printable representation.
*)
let selectBlocksWithModuloChecks arch (_functionName:string) (block:M.basicBlock) variants records ctx returnType floatRegs errorLabels state=
 let M.Label baseLabel=block.M.label in
 let rec loop instrs counter currentLabel currentInstrsRev blocksRev currentState=match instrs with
 | [] -> Ok (blocksRev,counter,currentLabel,currentInstrsRev,currentState)
 | instr::rest -> match instr with
 | M.BinOp (_,M.Div,_,M.Int64Const divisor,typ) when typ<>AST.TFloat64 && divisor<>0L ->
 let* lirInstrs,next=selectInstr arch instr variants records ctx floatRegs currentState in
 let reversed=List.fold_left (fun reversed instr -> instr::reversed) currentInstrsRev lirInstrs in loop rest counter currentLabel reversed blocksRev next
 | M.BinOp (_,M.Div,_,right,typ) when typ<>AST.TFloat64 ->
 let* rightInstrs,rightReg,afterRight=ensureInRegister right currentState in
 let* divInstrs,next=selectInstr arch instr variants records ctx floatRegs afterRight in
 let nextLabel=L.Label (baseLabel^"_div_cont_"^string_of_int counter) in
 let checkBlock : L.basicBlock={L.label=currentLabel;instrs=List.rev currentInstrsRev@rightInstrs@[L.Cmp (rightReg,L.Imm 0L)];terminator=L.CondBranch (L.EQ,errorLabels.divideByZero,nextLabel)} in
 loop rest (addInt counter 1) nextLabel (List.rev divInstrs) (checkBlock::blocksRev) next
 | M.BinOp (_,M.Mod,_,M.Int64Const divisor,typ) when typ<>AST.TFloat64 && ((isUnsignedIntegerType typ && divisor<>0L) || (shouldCheckNegativeDivisor typ && divisor>0L)) ->
 let* lirInstrs,next=selectInstr arch instr variants records ctx floatRegs currentState in
 let reversed=List.fold_left (fun reversed instr -> instr::reversed) currentInstrsRev lirInstrs in loop rest counter currentLabel reversed blocksRev next
 | M.BinOp (dest,M.Mod,left,right,typ) when typ<>AST.TFloat64 ->
 let* loads,rightReg,modInstrs,next=buildIntegerModuloParts (vregToLIRReg dest) left right typ currentState in
 let nextLabel=L.Label (baseLabel^"_mod_cont_"^string_of_int counter) in let negative=shouldCheckNegativeDivisor typ in let invalidLabel=L.Label (baseLabel^"_mod_invalid_"^string_of_int counter) in
 let zeroCheck : L.basicBlock={L.label=currentLabel;instrs=List.rev currentInstrsRev@loads@[L.Cmp (rightReg,L.Imm 0L)];terminator=(if negative then L.CondBranch (L.LE,invalidLabel,nextLabel) else L.CondBranch (L.EQ,errorLabels.moduloByZero,nextLabel))} in
 let checks=if negative then let invalidCheck : L.basicBlock={L.label=invalidLabel;instrs=[L.Cmp (rightReg,L.Imm 0L)];terminator=L.CondBranch (L.EQ,errorLabels.moduloByZero,errorLabels.moduloNegativeDivisor)} in invalidCheck::zeroCheck::blocksRev else zeroCheck::blocksRev in
 loop rest (addInt counter 1) nextLabel (List.rev modInstrs) checks next
 | _ -> let* lirInstrs,next=selectInstr arch instr variants records ctx floatRegs currentState in
 let reversed=List.fold_left (fun reversed instr -> instr::reversed) currentInstrsRev lirInstrs in
 match instr with M.RuntimeError _ | M.RuntimeErrorString _ -> Ok (blocksRev,counter,currentLabel,reversed,next) | _ -> loop rest counter currentLabel reversed blocksRev next in
 let* blocksRev,_,currentLabel,reversed,after=loop block.M.instrs 0 (convertLabel block.M.label) [] [] state in
 let+ termInstrs,terminator,next=selectTerminator block.M.terminator returnType after in
 let final : L.basicBlock={L.label=currentLabel;instrs=List.rev reversed@termInstrs;terminator} in List.rev (final::blocksRev),currentLabel,next
(*
   Convert MIR CFG to LIR CFG
*)
let selectCFG arch functionName (cfg:M.cfg) variants records ctx returnType floatRegs errorLabels state=
 let entry=convertLabel cfg.M.entry in
 match M.LabelMap.find_opt cfg.M.entry cfg.M.blocks with
 | None -> let M.Label label=cfg.M.entry in Error ("MIR to LIR: missing entry block "^StructuralFormat.format (StructuralValue.Union ("Label",[StructuralValue.Text label])))
 | Some _ ->
 let rec build remaining currentState blocksAcc labelsAcc=match remaining with
 | [] -> Ok (List.concat (List.rev blocksAcc),L.LabelMap.of_list labelsAcc,currentState)
 | (_,block)::rest -> let* blocks,finalLabel,next=selectBlocksWithModuloChecks arch functionName block variants records ctx returnType floatRegs errorLabels currentState in build rest next (blocks::blocksAcc) ((convertLabel block.M.label,finalLabel)::labelsAcc) in
 let* lirBlocks,labelMap,_=build (M.LabelMap.bindings cfg.M.blocks) state [] [] in
 let referenced=L.LabelSet.of_list (List.concat_map (fun block -> match block.L.terminator with L.Branch (_,yes,no) | L.BranchZero (_,yes,no) | L.BranchBitZero (_,_,yes,no) | L.BranchBitNonZero (_,_,yes,no) | L.CondBranch (_,yes,no) -> [yes;no] | L.Jump label -> [label] | L.Ret -> []) lirBlocks) in
 let errors=[errorLabels.divideByZero,"Cannot divide by 0";errorLabels.moduloByZero,"Cannot evaluate modulus against 0";errorLabels.moduloNegativeDivisor,"Cannot evaluate modulus against a negative number"] in
 let blocks=lirBlocks@List.map (fun (label,message) -> integerErrorBlock label message) (List.filter (fun (label,_) -> L.LabelSet.mem label referenced) errors) in
 let seen,duplicate=List.fold_left (fun (seen,duplicate) block -> L.LabelSet.add block.L.label seen,duplicate || L.LabelSet.mem block.L.label seen) (L.LabelSet.empty,false) blocks in
 let _=seen in
 if duplicate then Error "Internal error: duplicate LIR labels after modulo check insertion" else
 let remap label=Option.value ~default:label (L.LabelMap.find_opt label labelMap) in
 let remapPhi=function L.Phi (dest,sources,typ) -> L.Phi (dest,List.map (fun (op,pred) -> op,remap pred) sources,typ) | L.FPhi (dest,sources) -> L.FPhi (dest,List.map (fun (src,pred) -> src,remap pred) sources) | instr -> instr in
 let blocks=List.map (fun block -> let instrs=List.map remapPhi block.L.instrs in {block with L.instrs=instrs}) blocks in
 Ok ({L.entry;blocks=L.LabelMap.of_list (List.map (fun block -> block.L.label,block) blocks)} : L.cfg)

let argRegisterCounts types=List.fold_left (fun (ints,floats) typ -> if typ=AST.TFloat64 then ints,addInt floats 1 else addInt ints 1,floats) (0,0) types
let validateRegisterBankLimits description ints floats=if ints>8 then Some (Printf.sprintf "%s has %d integer/pointer arguments, but only 8 are supported (ARM64 calling convention limit)" description ints) else if floats>8 then Some (Printf.sprintf "%s has %d float arguments, but only 8 are supported (ARM64 calling convention limit)" description floats) else None
(*
   Check if any function exceeds ARM64 parameter register-bank limits.
*)
let checkParameterLimits functions=match List.find_map (fun (func:M.functionDef) -> let ints,floats=argRegisterCounts (List.map (fun (param:M.typedMIRParam) -> param.M.typ) func.M.typedParams) in validateRegisterBankLimits ("Function '"^func.M.name^"'") ints floats) functions with Some error -> Error error | None -> Ok ()
(*
   Check if any function call exceeds ARM64 argument register-bank limits.
*)
let checkCallArgLimits functions=
 let validateCall description types=let ints,floats=argRegisterCounts types in if ints>8 || floats>8 then validateRegisterBankLimits (description ()) ints floats else None in
 let validateClosure description types=let ints,floats=argRegisterCounts types in let ints=addInt ints 1 in if ints>8 || floats>8 then validateRegisterBankLimits (description ()) ints floats else None in
 let checkBlock block=List.find_map (function
 | M.Call (_,name,_,types,_) -> validateCall (fun () -> Printf.sprintf "Call to '%Lu'" (AST.functionIdValue name)) types
 | M.TailCall (name,_,types,_) -> validateCall (fun () -> Printf.sprintf "Tail call to '%Lu'" (AST.functionIdValue name)) types
 | M.IndirectCall (_,_,_,types,_) -> validateCall (fun () -> "Indirect call") types
 | M.IndirectTailCall (_,_,types,_) -> validateCall (fun () -> "Indirect tail call") types
 | M.ClosureCall (_,_,_,types,_) -> validateClosure (fun () -> "Closure call") types
 | M.ClosureTailCall (_,_,types) -> validateClosure (fun () -> "Closure tail call") types
 | _ -> None) block.M.instrs in
 match List.find_map (fun (func:M.functionDef) -> List.find_map (fun (_,block) -> checkBlock block) (M.LabelMap.bindings func.M.cfg.M.blocks)) functions with Some error -> Error error | None -> Ok ()
(*
   Convert MIR functions to LIR for a concrete target architecture.
   Pre-check: verify all functions have ≤8 parameters and calls have ≤8 arguments
   Convert each MIR function to LIR
   Convert MIR TypedParams to LIR TypedLIRParams
   Will be determined by register allocation
*)
let convertFunctionsForWithTrace phaseRecorder arch functions variants records ctx=
 let startPhase ()=Option.map (fun _ -> (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)) phaseRecorder in
 let recordPhase name timer=match phaseRecorder,timer with Some record,Some start -> let elapsed=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)-.start in record name elapsed | _ -> () in
 let parameterTimer=startPhase () in let parameterCheck=checkParameterLimits functions in recordPhase "MIR -> LIR Parameter Limit Check" parameterTimer;
 let* ()=parameterCheck in
 let callTimer=startPhase () in let callCheck=checkCallArgLimits functions in recordPhase "MIR -> LIR Call Argument Limit Check" callTimer;
 let* ()=callCheck in
 let convertFunc (func:M.functionDef)=
 let errors={divideByZero=L.Label ("__divide_by_zero_error_"^func.M.name);moduloByZero=L.Label ("__modulo_by_zero_error_"^func.M.name);moduloNegativeDivisor=L.Label ("__modulo_negative_divisor_error_"^func.M.name)} in
 let tempState=initTempState func in
 let+ cfg=selectCFG arch func.M.name func.M.cfg variants records ctx func.M.returnType func.M.floatRegs errors tempState in
 let typedParams=List.map (fun (param:M.typedMIRParam) -> let M.VReg id=param.M.reg in ({L.reg=L.Virtual id;typ=param.M.typ} : L.typedLIRParam)) func.M.typedParams in
 ({L.id=func.M.id;name=func.M.name;typedParams;cfg;stackSize=0;usedCalleeSaved=[];codegenFacts=None} : L.functionDef) in
 let functionTimer=startPhase () in let converted=ResultList.mapResults convertFunc functions in recordPhase "MIR -> LIR Function Conversion" functionTimer;converted
(*
   Convert only MIR functions when the caller already owns the projected type
   registries. This avoids rebuilding registry representations that would be
   immediately discarded by function-only compilation paths.
*)
let toLIRFunctionsForWithTrace phaseRecorder arch (M.Program (functions,variants,records))=
 let ctx=printRcContextFromMirRegistries variants records in convertFunctionsForWithTrace phaseRecorder arch functions variants records ctx
(*
   Convert MIR functions while reusing the RC registries that produced them.
   This avoids reconstructing whole-program print-release registries inside
   instruction lowering.
*)
let toLIRFunctionsForWithTraceAndRcRegistries phaseRecorder arch recordFields recordTypeParams sumShapes (M.Program (functions,variants,records))=
 let ctx={recordFields;recordTypeParams;sumShapes} in convertFunctionsForWithTrace phaseRecorder arch functions variants records ctx
(*
   Convert a MIR program to LIR for a concrete target architecture.
*)
let toLIRForWithTrace phaseRecorder arch ((M.Program (_,variants,records)) as program)=
 let startPhase ()=Option.map (fun _ -> (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)) phaseRecorder in
 let recordPhase name timer=match phaseRecorder,timer with Some record,Some start -> let elapsed=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)-.start in record name elapsed | _ -> () in
 let+ functions=toLIRFunctionsForWithTrace phaseRecorder arch program in
 let timer=startPhase () in
 let variants=SM.map (fun (variants:M.typeVariants) -> let values=List.map (fun (variant:M.variantInfo) -> ({L.name=variant.M.name;tag=variant.M.tag;payload=variant.M.payload;fieldCount=variant.M.fieldCount} : L.variantInfo)) variants.M.variants in ({L.typeParams=variants.M.typeParams;variants=values} : L.typeVariants)) variants in
 let records=SM.map (List.map (fun (field:M.recordField) -> field.M.name,field.M.typ)) records in
 recordPhase "MIR -> LIR Registry Projection" timer;L.Program (functions,variants,records)
let toLIRFor arch program=toLIRForWithTrace None arch program
(*
   ARM64 remains the default for target-neutral pass fixtures.
*)
let toLIR program=toLIRFor Platform.ARM64 program
